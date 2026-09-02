
soyFile1 <- getFile("BRAZIL_SOY_2.6.0_pc\\BRAZIL_SOY_2.6.0_pc.2015.csv")
soyFile2 <- getFile("BRAZIL_SOY_2.6.0_pc\\BRAZIL_SOY_2.6.0_pc.2016.csv")
soyFile3 <- getFile("BRAZIL_SOY_2.6.0_pc\\BRAZIL_SOY_2.6.0_pc.2017.csv")

csv1 <- read.csv(soyFile1, as.is = TRUE)
csv2 <- read.csv(soyFile2, as.is = TRUE)
csv3 <- read.csv(soyFile3, as.is = TRUE)

csv <- rbind(csv1, csv2, csv3) %>%
  mutate(Value = SOY_EQUIVALENT_TONNES / CONVERSION) %>%
  mutate(EXPORTER.GROUP = stringr::str_trim(EXPORTER.GROUP)) %>%
  dplyr::mutate(TRASE_GEOCODE = str_replace(TRASE_GEOCODE, ".*X$", "AGGREGATED"))
  
csv %>%
  group_by(YEAR) %>%
  summarise(Value = sum(Value))

# no invalid exporter.group in this version
csv$EXPORTER.GROUP %>% unique() %>% sort()

# no invalid country.of.first.import
csv$COUNTRY.OF.FIRST.IMPORT %>% unique() %>% sort()

geocodes <- csv$TRASE_GEOCODE %>% unique() %>% sort()

# names to be removed
geocodes[1] # AGGREGATED

remove_aggregated <- csv %>%
  dplyr::filter(TRASE_GEOCODE == "AGGREGATED") 

remove_aggregated %>%
  group_by(YEAR) %>%
  summarise(Value = sum(Value))
#   YEAR Value
#<int> <dbl>
#1  2015 6335.
#2  2016 7090.
#3  2017 8351.

csv_clean <- csv %>%
  dplyr::filter(TRASE_GEOCODE != "AGGREGATED")


both <- cbind(csv_clean %>% group_by(YEAR) %>% summarise(Value = sum(Value)),
      csv %>% group_by(YEAR) %>% summarise(Value = sum(Value)))


both$Diff <- both[,2] - both[,4]
both$Perc <- both[,2] / both[,4] - 1
both
