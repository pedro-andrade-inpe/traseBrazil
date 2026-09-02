
beefFile1 <- getFile("BRAZIL_BEEF_2.0.2_pc\\BRAZIL_BEEF_2.0.2_pc.2015.csv")
beefFile2 <- getFile("BRAZIL_BEEF_2.0.2_pc\\BRAZIL_BEEF_2.0.2_pc.2016.csv")
beefFile3 <- getFile("BRAZIL_BEEF_2.0.2_pc\\BRAZIL_BEEF_2.0.2_pc.2017.csv")

csv1 <- read.csv(beefFile1, as.is = TRUE)
csv2 <- read.csv(beefFile2, as.is = TRUE)
csv3 <- read.csv(beefFile3, as.is = TRUE)

csv <- rbind(csv1, csv2, csv3) %>%
  mutate(Value = BEEF_EQUIVALENT_TONNES / CONVERSION) %>%
  mutate(EXPORTER.GROUP = stringr::str_trim(EXPORTER.GROUP))

csv %>%
  group_by(YEAR) %>%
  summarise(Value = sum(Value))

# no invalid exporter.group in this version
csv$EXPORTER.GROUP %>% unique() %>% sort()

csv$COUNTRY.OF.DESTINATION %>% unique() %>% sort()

unknown_country <- csv %>%
  dplyr::filter(COUNTRY.OF.DESTINATION == "UNKNOWN COUNTRY")

unknown_country %>%
  group_by(YEAR) %>%
  summarise(Value = sum(Value))
#YEAR   Value
#<int>   <dbl>
#1  2015 0.225  
#2  2016 0.0173 
#3  2017 0.00632

geocodes <- csv$TRASE_GEOCODE %>% unique() %>% sort()

# names to be removed
geocodes[1] # BR-11-AGGREGATED (like these there are others - 11 is an IBGE's state code)
geocodes[2924] # BR-XXXXXXX

csv <- csv %>%
  dplyr::mutate(TRASE_GEOCODE = str_replace(TRASE_GEOCODE, ".*AGGREGATED$", "AGGREGATED")) %>%
  dplyr::mutate(TRASE_GEOCODE = str_replace(TRASE_GEOCODE, ".*X$", "AGGREGATED"))

remove_aggregated <- csv %>%
  dplyr::filter(TRASE_GEOCODE == "AGGREGATED") 

remove_aggregated %>%
  group_by(YEAR) %>%
  summarise(Value = sum(Value))
#YEAR Value
#<int> <dbl>
#1  2015  226.
#2  2016  238.
#3  2017  225.

csv_clean <- csv %>%
  dplyr::filter(TRASE_GEOCODE != "AGGREGATED") %>%
  dplyr::filter(COUNTRY.OF.DESTINATION != "UNKNOWN COUNTRY")


both <- cbind(csv_clean %>% group_by(YEAR) %>% summarise(Value = sum(Value)),
      csv %>% group_by(YEAR) %>% summarise(Value = sum(Value)))


both$Diff <- both[,2] - both[,4]
both$Perc <- both[,2] / both[,4] - 1
both
