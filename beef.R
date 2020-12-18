
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
  mutate(IMPORTER.GROUP = stringr::str_trim(IMPORTER.GROUP)) %>%
  mergeBeefIMPORTER.GROUP(0.95, "beef") %>%
  mapCOUNTRY() %>%
  mapMUNICIPALITY() %>%
  distributeUnknownValue() %>%
  distributeAggregated()

shp <- getMunicipalities()

checkMunicipalities(csv, shp)

####################################################
####################################################
# national production using pta joining with Trase by state

pta <- read.csv(getFile("ibge/pta_tabela1092_abate_bovino.csv"), sep = ";", skip = 3)
pta <- pta[71:97, -(c(2:74, 87:96))] # 2015 to 2017

for(i in 2:13)
  suppressWarnings(pta[,i] <- as.numeric(pta[,i]))

names(pta) <- c("uf", paste0("t", 1:12))

myconversion <- CONVERSION * 1e3 # the data is in kg, 1e3 converts it to ton

pta <- pta %>%
  replace(is.na(.), 0) %>%
  mutate(pta_b_2015 = round((t1 + t2 + t3 + t4) / myconversion, 2)) %>%
  mutate(pta_b_2016 = round((t5 + t6 + t7 + t8) / myconversion, 2)) %>%
  mutate(pta_b_2017 = round((t9 + t10 + t11 + t12) / myconversion, 2)) %>%
  select(uf, pta_b_2015, pta_b_2016, pta_b_2017) %>%
  arrange(uf)
   
pta$uf <- tolower(unlist(mystates)[-28])

full_state_data <- csv %>%
  mutate(uf = tolower(Sigla)) %>%
  group_by(uf, YEAR) %>%
  filter(code != "unknown-us") %>%
  summarize(beef = round(sum(Value), 2), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = YEAR, values_from = beef, names_prefix = "trase_b_") %>%
  merge(pta, by = "uf") %>%
  replace(is.na(.), 0) %>%
  # compute internal consumption
  mutate(br_b_2015 = round(pta_b_2015 - trase_b_2015, 2)) %>%
  mutate(br_b_2016 = round(pta_b_2016 - trase_b_2016, 2)) %>%
  mutate(br_b_2017 = round(pta_b_2017 - trase_b_2017, 2)) %>%
  mutate(export_2015 = round(trase_b_2015 / br_b_2015 * 100, 2)) %>%
  mutate(export_2016 = round(trase_b_2016 / br_b_2016 * 100, 2)) %>%
  mutate(export_2017 = round(trase_b_2017 / br_b_2017 * 100, 2))

#View(full_state_data)

# spread over municipalities using PPM cattle

ppm <- read.csv(getFile("ibge/ppm_tabela3939_efetivo_rebanho_bovino.csv"), sep = ";", skip = 3)
ppm <- ppm[-1, ]
ppm
####################################################
####################################################

#######################################################
cat("Joining and exporting data\n")
#######################################################

countryGms <- buildGmsByPairs(csv)
writeGmsByPairs(countryGms, "beef")


csv$IMPORTER.GROUP %>% 
  unique() %>%
  sort() %>% 
  write.table(getFile(paste0("result/trase-importer-beef.txt")), quote = FALSE, row.names = FALSE, col.names = FALSE)
