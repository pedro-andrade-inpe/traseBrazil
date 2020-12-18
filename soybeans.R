
DATA_DIR <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2020\\rplus\\trase\\"
getFile <- function(file) paste0(DATA_DIR, file)

source(getFile("common.R"))

#######################################################
cat("Reading CSV files\n")
#######################################################

soyFile <- getFile("BRAZIL_SOY_2.5.1_pc\\BRAZIL_SOY_2.5.1_pc.csv")

csv <- read.csv(soyFile, as.is = TRUE) %>%
  dplyr::filter(YEAR >= 2015) %>%
  mutate(Value = SOY_EQUIVALENT_TONNES / CONVERSION) %>%
  mutate(IMPORTER.GROUP = stringr::str_trim(IMPORTER.GROUP)) %>%
  mergeSoyIMPORTER.GROUP(0.999, "soy") %>%
  mapCOUNTRY() %>%
  mapMUNICIPALITY() %>%
  distributeUnknownValue()

shp <- getMunicipalities()

checkMunicipalities(csv, shp)

#######################################################
cat("Joining and exporting data\n")
#######################################################

gms <- buildGmsByPairs(csv)




writeGmsByPairs(gms, "soybeans")

csv$IMPORTER.GROUP %>% 
  unique() %>%
  sort() %>% 
  write.table(getFile(paste0("result/trase-importer-soy.txt")), quote = FALSE, row.names = FALSE, col.names = FALSE)
