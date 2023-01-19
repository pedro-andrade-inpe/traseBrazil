
source("common.R")

#######################################################
cat("Reading CSV files\n")
#######################################################

alldata <- c()

for(year in 2015:2020){
  soyFile <- getFile(paste0("BRAZIL_SOY_2.6.0_pc\\BRAZIL_SOY_2.6.0_pc.", year, ".csv"))
  mdata <- read.csv(soyFile, as.is = TRUE)
  alldata <- rbind(alldata, mdata)
}

csv <- alldata %>%
  mutate(Value = SOY_EQUIVALENT_TONNES / CONVERSION) 

before <- csv %>% dplyr::group_by(YEAR) %>%
  summarize(Before = sum(Value))

csv <- csv %>%
  mutate(EXPORTER.GROUP = stringr::str_trim(EXPORTER.GROUP)) %>%
  dplyr::filter(EXPORTER.GROUP != "UNKNOWN") %>%
  mutate(COUNTRY = stringr::str_trim(COUNTRY.OF.FIRST.IMPORT)) %>%
  dplyr::filter(COUNTRY != "UNKNOWN COUNTRY") %>%
  mutate(MUNICIPALITY = stringr::str_trim(MUNICIPALITY.OF.PRODUCTION)) %>%
  dplyr::filter(MUNICIPALITY != "UNKNOWN") %>%
  mapCOUNTRY() %>%
  mapMUNICIPALITY() %>%
  dplyr::select(YEAR, EXPORTER, code, EXPORTER.GROUP, TRASE_GEOCODE, COUNTRY, Value)

after <- csv %>% dplyr::group_by(YEAR) %>%
  summarize(After = sum(Value))

comparison <- cbind(before, after) %>%
  .[,-3] %>%
  dplyr::mutate(Reduction = Before - After, Perc = After / Before) %>%
  round(2)

#YEAR    Before     After Reduction Perc
#1 2015  97464.94  91130.42   6334.51 0.94
#2 2016  96394.82  89304.65   7090.17 0.93
#3 2017 114732.10 106381.26   8350.84 0.93
#4 2018 117912.45  99841.45  18071.00 0.85
#5 2019 114316.83  98050.36  16266.47 0.86
#6 2020 121797.71 101259.71  20538.00 0.83


csv %>%
  dplyr::filter(EXPORTER.GROUP == "DOMESTIC CONSUMPTION") %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(Value = sum(Value))

csv %>%
  dplyr::filter(EXPORTER.GROUP != "DOMESTIC CONSUMPTION") %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(Value = sum(Value))

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
  paste0("\"", ., "\"") %>%
  unique() %>%
  sort() %>% 
  write.table(getFile(paste0("result/trase-exporter-soy.txt")), quote = FALSE, row.names = FALSE, col.names = FALSE)

csv$COUNTRY %>% 
  paste0("\"", ., "\"") %>%
  unique() %>%
  sort() %>% 
  write.table(getFile(paste0("result/trase-country-soy.txt")), quote = FALSE, row.names = FALSE, col.names = FALSE)

