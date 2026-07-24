DATA_DIR <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2020\\rplus\\trase\\"
getFile <- function(file) paste0(DATA_DIR, file)

#######################################################
cat("Reading CSV file\n")
#######################################################

soyFile <- read.csv("C:/Users/pedro/Dropbox/pesquisa/2026/aline/trase/brazil_soy_v2_6_1_composite.csv")

#### adicinoei recente para testar glencore

result <- soyFile %>%
  dplyr::filter(str_detect(exporter, "GLENCORE"))

sum(result$volume) / sum(soyFile$volume) * 100

result$year %>% unique()

#### adicinoei recente para testar glencore

csv <- soyFile %>%
  mutate(Value = volume) %>%
  mutate(EXPORTER_GROUP = stringr::str_trim(exporter_group)) %>%
  mutate(ECONOMIC_BLOCK = stringr::str_trim(economic_bloc)) %>%
  dplyr::mutate(municipality_of_production_trase_id = str_replace(municipality_of_production_trase_id, ".*X$", "AGGREGATED")) %>%
  dplyr::filter(municipality_of_production_trase_id != "AGGREGATED") %>%
  mutate(MUNICIPALITY = stringr::str_trim(municipality_of_production_trase_id) %>% substring(4)) %>%
  dplyr::select(year, EXPORTER_GROUP, ECONOMIC_BLOCK, MUNICIPALITY, Value)

require(geobr)

shp <- geobr::read_municipality(year = 2020) %>% 
  dplyr::mutate(MUNICIPALITY = paste0(code_muni))

csv2 <- csv %>%
  mutate(
    ECONOMIC_BLOCK = recode(
      ECONOMIC_BLOCK,
      "CHINA (HONG KONG)" = "CHINA",
      "CHINA (MAINLAND)" = "CHINA"
    )) %>%
  mutate(EXPORTER_GROUP = case_when(
    EXPORTER_GROUP %in% c("ADM", "AMAGGI", "BUNGE", "CARGILL", "COFCO", "LOUIS DREYFUS", "VITERRA BV", "GLENCORE") ~ EXPORTER_GROUP,
    TRUE ~ "OTHER"
  )) %>%
  mutate(ECONOMIC_BLOCK = case_when(
    ECONOMIC_BLOCK %in% c("BRAZIL", "EUROPEAN UNION", "CHINA") ~ ECONOMIC_BLOCK,
    TRUE ~ "OTHER"
  )) 

muniToSimU <- read.csv(getFile("muni-to-simu.csv")) %>% # read from file exported by createRelations.R
  dplyr::mutate(MUNICIPALITY = paste0(code_muni)) %>%
  dplyr::select(ID, MUNICIPALITY, area)

muniToSimU <- muniToSimU %>%
  group_by(ID) %>%
  slice_max(area, n = 1, with_ties = FALSE) %>%
  ungroup()

muniToSimU$area <- 1

#######################################################
cat("Processing economic block\n")
#######################################################
csv_economic_block <- csv2 %>%
  dplyr::group_by(ECONOMIC_BLOCK, year, MUNICIPALITY) %>%
  dplyr::summarise(Value = sum(Value), .groups = "drop")

result <- csv_economic_block %>%
  left_join(
    muniToSimU,
    by = "MUNICIPALITY",
    relationship = "many-to-many"
  ) %>%
  mutate(value_id = Value * area) %>%
  group_by(ECONOMIC_BLOCK, year, ID) %>%
  summarise(value = sum(value_id, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(ID)

res <- paste0("Brazil.", result$ID, ".\"", result$ECONOMIC_BLOCK, "\".", result$year, "\t", result$value) %>%
  c("/", ";") %>%
  data.frame()

colnames(res) <- paste0("PARAMETER ",
                        "SOY_TRASE\n(COUNTRY,SimUID,SOY_TRASE_DESTINATION,ALLYEAR) ",
                        " sourcing in ton per SimU\n/")

write.table(res, "trase-soy-economic-block.gms", row.names = FALSE, quote = FALSE)

#######################################################
cat("Processing exporter group\n")
#######################################################
csv_exporter_group <- csv2 %>%
  dplyr::filter(ECONOMIC_BLOCK != "BRAZIL") %>%
  dplyr::group_by(EXPORTER_GROUP, year, MUNICIPALITY) %>%
  dplyr::summarise(Value = sum(Value), .groups = "drop") 

result <- csv_exporter_group %>%
  left_join(
    muniToSimU,
    by = "MUNICIPALITY",
    relationship = "many-to-many"
  ) %>%
  mutate(value_id = Value * area) %>%
  group_by(EXPORTER_GROUP, year, ID) %>%
  summarise(value = sum(value_id, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(ID)

res <- paste0("Brazil.", result$ID, ".\"", result$EXPORTER_GROUP, "\".", result$year, "\t", result$value) %>%
  c("/", ";") %>%
  data.frame()

colnames(res) <- paste0("PARAMETER ",
                        "SOY_TRASE\n(COUNTRY,SimUID,SOY_TRASE_TRADER,ALLYEAR) ",
                        " sourcing in ton per SimU\n/")

write.table(res, "trase-soy-exporter-group.gms", row.names = FALSE, quote = FALSE)

#######################################################
cat("Processing exporter group and economic block together\n")
#######################################################
csv_economic_block_exporter_group <- csv2 %>%
  dplyr::filter(ECONOMIC_BLOCK != "BRAZIL") %>%
  dplyr::group_by(ECONOMIC_BLOCK, EXPORTER_GROUP, year, MUNICIPALITY) %>%
  dplyr::summarise(Value = sum(Value), .groups = "drop")

result <- csv_economic_block_exporter_group %>%
  left_join(
    muniToSimU,
    by = "MUNICIPALITY",
    relationship = "many-to-many"
  ) %>%
  mutate(value_id = Value * area) %>%
  group_by(ECONOMIC_BLOCK, EXPORTER_GROUP, year, ID) %>%
  summarise(value = sum(value_id, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(ID)

res <- paste0("Brazil.", result$ID, ".\"", result$ECONOMIC_BLOCK, "\".\"", result$EXPORTER_GROUP, "\".", result$year, "\t", result$value) %>%
  c("/", ";") %>%
  data.frame()

colnames(res) <- paste0("PARAMETER ",
                        "SOY_TRASE\n(COUNTRY,SimUID,SOY_TRASE_DESTINATION,SOY_TRASE_TRADER,ALLYEAR) ",
                        " sourcing in ton per SimU\n/")

write.table(res, "trase-soy-economic-block-exporter-group.gms", row.names = FALSE, quote = FALSE)


#######################################################
cat("Exporting unique values\n")
#######################################################
csv2$EXPORTER_GROUP %>% 
  paste0("\"", ., "\"") %>%
  unique() %>%
  sort() %>% 
  write.table("trase_soy_trader.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

csv2$ECONOMIC_BLOCK %>% 
  paste0("\"", ., "\"") %>%
  unique() %>%
  sort() %>% 
  write.table("trase_soy_destionation.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

