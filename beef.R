
source("common.R")

#######################################################
cat("Reading CSV files\n")
#######################################################

beefFile1 <- getFile("BRAZIL_BEEF_2.0.1_pc\\BRAZIL_BEEF_2.0.1_pc.2015.csv")
beefFile2 <- getFile("BRAZIL_BEEF_2.0.1_pc\\BRAZIL_BEEF_2.0.1_pc.2016.csv")
beefFile3 <- getFile("BRAZIL_BEEF_2.0.1_pc\\BRAZIL_BEEF_2.0.1_pc.2017.csv")

csv1 <- read.csv(beefFile1, as.is = TRUE)
csv2 <- read.csv(beefFile2, as.is = TRUE)
csv3 <- read.csv(beefFile3, as.is = TRUE)

csv <- rbind(csv1, csv2, csv3) %>%
  mutate(Value = BEEF_EQUIVALENT_TONNES / CONVERSION) %>%
  mutate(EXPORTER.GROUP = stringr::str_trim(EXPORTER.GROUP)) %>%
  #mergeBeefIMPORTER.GROUP(0.95, "beef") %>%
  mapCOUNTRY() %>%
  mapMUNICIPALITY() %>%
  distributeUnknownValue() %>%
  distributeAggregated() %>%
  dplyr::select(YEAR, code, EXPORTER, EXPORTER.GROUP, TRASE_GEOCODE, COUNTRY, Value)

load(getFile("2020-07-17-BRA_BEEF_SEIPCS.rdata"))
  
prod <- prod %>%
  dplyr::mutate(TRASE_GEOCODE = paste0("BR-", GEOCODE)) %>%
  dplyr::mutate(PRODUCTION_KTONS_YR = CW_PRODUCTION_TONS_5_YR / 1000 / 5)

res <- csv %>%
  dplyr::group_by(YEAR, TRASE_GEOCODE, code) %>%
  dplyr::summarise(Value = sum(Value), .groups = "drop") %>%
  dplyr::full_join(prod, by = c("TRASE_GEOCODE", "YEAR")) %>%
  dplyr::mutate(Internal = PRODUCTION_KTONS_YR - Value)

difference <- res %>% dplyr::filter(Internal < 0) %>%
  dplyr::group_by(YEAR) %>%
  summarize(Value = sum(Internal))
#<dbl>  <dbl>
# 1  2015 -105. 
# 2  2016 -113. 
# 3  2017  -99.6

sum(difference$Value)/ sum(prod$PRODUCTION_KTONS_YR) * 100
# -0.9252067

internal <- res %>%
  dplyr::filter(Internal > 0) %>%
  dplyr::mutate(Value = Internal) %>%
  dplyr::select(-CW_PRODUCTION_TONS_5_YR, -GEOCODE, -Internal, -PRODUCTION_KTONS_YR) %>%
  dplyr::mutate(COUNTRY = "Brazil") %>%
  dplyr::mutate(EXPORTER = "DOMESTIC_CONSUMPTION") %>%
  dplyr::mutate(EXPORTER.GROUP = "DOMESTIC_CONSUMPTION") %>%
  dplyr::relocate(names(csv))

assertthat::assert_that(all.equal(names(csv), names(internal)))

csv <- rbind(csv, internal)

### PAROU AQUI
shp <- getMunicipalities()

checkMunicipalities(csv, shp)

#######################################################
cat("Joining and exporting data\n")
#######################################################

gms <- buildGmsByPairs(csv)

euFile <- getFile("eu-countries.csv")
eu <- read.csv(euFile)

gms_eu <- gms %>%
  dplyr::mutate(country = ifelse(country %in% !!eu$country, "EU", country)) %>%
  dplyr::filter(country == "EU") %>%
  dplyr::group_by(ID, EXPORTER.GROUP, country, year) %>%
  dplyr::summarise(value = sum(value), .groups = "drop") 

gms <- rbind(gms, gms_eu) %>%
  dplyr::arrange(ID, EXPORTER.GROUP, year)

writeGmsByPairs(gms, "beef-exporter")

csv$EXPORTER.GROUP %>% 
  unique() %>%
  sort() %>% 
  write.table(getFile(paste0("result/trase-exporter-beef.txt")), quote = FALSE, row.names = FALSE, col.names = FALSE)

