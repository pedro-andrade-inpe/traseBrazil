DATA_DIR <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2020\\rplus\\trase\\"
getFile <- function(file) paste0(DATA_DIR, file)

require(dplyr)

#######################################################
cat("Reading CSV file\n")
#######################################################

beefFile <- read.csv("C:/Users/pedro/Dropbox/pesquisa/2026/aline/trase/brazil_beef_v2_2_2.csv")

csv <- beefFile %>%
  mutate(Value = volume) %>%
  mutate(EXPORTER_GROUP = stringr::str_trim(exporter_group)) %>%
  mutate(
    EXPORTER_GROUP = case_when(
      EXPORTER_GROUP %in% c("JBS", "MARFRIG", "MINERVA") ~ EXPORTER_GROUP,
      TRUE ~ "OTHER"
    )) %>%
  mutate(MUNICIPALITY = municipality_of_production_trase_id %>% substring(4)) %>%
  dplyr::filter(MUNICIPALITY != "XXXXXXX") %>%
  mutate(ECONOMIC_BLOCK = economic_bloc) %>%
  mutate(
    ECONOMIC_BLOCK = recode(
      ECONOMIC_BLOCK,
      "CHINA (HONG KONG)" = "CHINA",
      "CHINA (MAINLAND)" = "CHINA"
    )) %>%
  mutate(
    ECONOMIC_BLOCK = case_when(
      ECONOMIC_BLOCK %in% c("BRAZIL", "EUROPEAN UNION", "CHINA") ~ ECONOMIC_BLOCK,
      TRUE ~ "OTHER"
    )) %>%
  mutate(YEAR = year) %>%
  dplyr::select(YEAR, ECONOMIC_BLOCK, EXPORTER_GROUP, MUNICIPALITY, Value)

dados <- arrow::read_parquet("c:/users/pedro/downloads/cattle_production_4_annual_and_5YEAR_2025-10-02.parquet") %>%
  mutate(MUNICIPALITY = trase_id %>% substring(4)) %>%
  mutate(ECONOMIC_BLOCK = "BRAZIL") %>%
  mutate(EXPORTER_GROUP = "INTERNAL_CONSUMPTION") %>%
  mutate(Value = CW_PRODUCTION_TONS_5_YR / 5) %>%
  dplyr::select(YEAR, ECONOMIC_BLOCK, EXPORTER_GROUP, MUNICIPALITY, Value) %>%
  dplyr::filter(YEAR %in% 2011:2023)

export_mun <- csv %>%
  group_by(YEAR, MUNICIPALITY) %>%
  summarise(
    exportacao = sum(Value, na.rm = TRUE),
    .groups = "drop"
  )

consumo_interno <- dados %>%
  rename(producao_original = Value) %>%
  left_join(export_mun, by = c("YEAR", "MUNICIPALITY")) %>%
  mutate(
    exportacao = coalesce(exportacao, 0),
    aumento_producao = pmax(exportacao - producao_original, 0),
    producao_ajustada = pmax(producao_original, exportacao),
    consumo_interno = producao_ajustada - exportacao
  )

result <- consumo_interno %>%
  dplyr::mutate(Value = consumo_interno) %>%
  dplyr::select(YEAR, ECONOMIC_BLOCK, EXPORTER_GROUP, MUNICIPALITY, Value)

muniToSimU <- read.csv(getFile("muni-to-simu.csv")) %>% # read from file exported by createRelations.R
  dplyr::mutate(MUNICIPALITY = paste0(code_muni)) %>%
  dplyr::select(ID, MUNICIPALITY, area)

csv2 <- rbind(csv, result) %>%
  dplyr::filter(YEAR != 2018)


#######################################################
cat("Processing economic block\n")
#######################################################
csv_economic_block <- csv2 %>%
  dplyr::group_by(ECONOMIC_BLOCK, EXPORTER_GROUP, YEAR, MUNICIPALITY) %>%
  dplyr::summarise(Value = sum(Value), .groups = "drop")

result <- csv_economic_block %>%
  left_join(
    muniToSimU,
    by = "MUNICIPALITY",
    relationship = "many-to-many"
  ) %>%
  mutate(value_id = Value * area) %>%
  group_by(ECONOMIC_BLOCK, EXPORTER_GROUP, YEAR, ID) %>%
  summarise(value = sum(value_id, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(ID)

res <- paste0("Brazil.", result$ID, ".\"", result$ECONOMIC_BLOCK, "\".", result$EXPORTER_GROUP, "\".", result$YEAR, "\t", result$value) %>%
  c("/", ";") %>%
  data.frame()

colnames(res) <- paste0("PARAMETER ",
                        "BEEF_TRASE\n(COUNTRY,SimUID,BEEF_TRASE_DESTINATION,EXPORTER_GROUP,ALLYEAR) ",
                        " sourcing in ton per SimU\n/")

write.table(res, "trase-beef-economic-block.gms", row.names = FALSE, quote = FALSE)

#######################################################
cat("Processing exporter group\n")
#######################################################
csv_exporter_group <- csv2 %>%
  dplyr::filter(ECONOMIC_BLOCK != "BRAZIL") %>%
  dplyr::group_by(EXPORTER_GROUP, YEAR, MUNICIPALITY) %>%
  dplyr::summarise(Value = sum(Value), .groups = "drop") 

result <- csv_exporter_group %>%
  left_join(
    muniToSimU,
    by = "MUNICIPALITY",
    relationship = "many-to-many"
  ) %>%
  mutate(value_id = Value * area) %>%
  group_by(EXPORTER_GROUP, YEAR, ID) %>%
  summarise(value = sum(value_id, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(ID)

res <- paste0("Brazil.", result$ID, ".\"", result$EXPORTER_GROUP, "\".", result$YEAR, "\t", result$value) %>%
  c("/", ";") %>%
  data.frame()

colnames(res) <- paste0("PARAMETER ",
                        "BEEF_TRASE\n(COUNTRY,SimUID,BEEF_TRASE_TRADER,ALLYEAR) ",
                        " sourcing in ton per SimU\n/")

write.table(res, "trase-beef-exporter-group.gms", row.names = FALSE, quote = FALSE)

#######################################################
cat("Processing exporter group and economic block together\n")
#######################################################
csv_economic_block_exporter_group <- csv2 %>%
  dplyr::filter(ECONOMIC_BLOCK != "BRAZIL") %>%
  dplyr::group_by(ECONOMIC_BLOCK, EXPORTER_GROUP, YEAR, MUNICIPALITY) %>%
  dplyr::summarise(Value = sum(Value), .groups = "drop")

result <- csv_economic_block_exporter_group %>%
  left_join(
    muniToSimU,
    by = "MUNICIPALITY",
    relationship = "many-to-many"
  ) %>%
  mutate(value_id = Value * area) %>%
  group_by(ECONOMIC_BLOCK, EXPORTER_GROUP, YEAR, ID) %>%
  summarise(value = sum(value_id, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(ID)

res <- paste0("Brazil.", result$ID, ".\"", result$ECONOMIC_BLOCK, "\".\"", result$EXPORTER_GROUP, "\".", result$YEAR, "\t", result$value) %>%
  c("/", ";") %>%
  data.frame()

colnames(res) <- paste0("PARAMETER ",
                        "BEEF_TRASE\n(COUNTRY,SimUID,BEEF_TRASE_DESTINATION,BEEF_TRASE_TRADER,ALLYEAR) ",
                        " sourcing in ton per SimU\n/")

write.table(res, "trase-beef-economic-block-exporter-group.gms", row.names = FALSE, quote = FALSE)


#######################################################
cat("Exporting unique values\n")
#######################################################
csv2$EXPORTER_GROUP %>% 
  paste0("\"", ., "\"") %>%
  unique() %>%
  sort() %>% 
  write.table("trase_beef_trader.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

csv2$ECONOMIC_BLOCK %>% 
  paste0("\"", ., "\"") %>%
  unique() %>%
  sort() %>% 
  write.table("trase_beef_destination.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)



