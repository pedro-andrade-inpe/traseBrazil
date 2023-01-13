
source("common.R")

#######################################################
cat("Reading CSV files\n")
#######################################################

soyFile <- getFile("BRAZIL_SOY_2.5.1_pc\\BRAZIL_SOY_2.5.1_pc.csv")

csv <- read.csv(soyFile, as.is = TRUE) %>%
  dplyr::filter(YEAR >= 2015) %>%
  mutate(Value = SOY_EQUIVALENT_TONNES / CONVERSION) %>%
  mutate(EXPORTER.GROUP = stringr::str_trim(EXPORTER.GROUP)) %>%
  #mergeSoyIMPORTER.GROUP(0.999, "soy") %>%
  mapCOUNTRY() %>%
  mapMUNICIPALITY() %>%
  distributeUnknownValue() %>%
  dplyr::select(YEAR, code, EXPORTER, EXPORTER.GROUP, TRASE_GEOCODE, COUNTRY, Value)

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

writeGmsByPairs(gms, "soybeans-exporter")

csv$EXPORTER.GROUP %>% 
  unique() %>%
  sort() %>% 
  write.table(getFile(paste0("result/trase-exporter-soy.txt")), quote = FALSE, row.names = FALSE, col.names = FALSE)
