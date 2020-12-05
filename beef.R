
DATA_DIR <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2020\\rplus\\trase\\"
getFile <- function(file) paste0(DATA_DIR, file)

source(getFile("common.R"))

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
  mergeBeefIMPORTER.GROUP(0.95) %>%
  mapCOUNTRY() %>%
  mapMUNICIPALITY()

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
  summarize(beef = round(sum(BEEF_EQUIVALENT_TONNES), 2), .groups = "drop") %>%
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
writeGmsByPairs(countryGms, getFile("result/trase-beef.gms"))


useless <- function(){
  muniToSimU <- read.csv(getFile("muni-to-simu.csv")) # read from file exported by createRelations.R
  years <- csv$YEAR %>% unique() %>% sort()
  
  for(year in years){
    csv_year <- csv %>%
      dplyr::filter(YEAR == !!year) %>%
      dplyr::select(-PRODUCT_DESCR, -FOB_USD, -TYPE, 
                    -CO2_EMISSIONS_CATTLE_DEFORESTATION_5_YEAR_ANNUAL_RISK,
                    -CO2_EMISSIONS_TERRITORIAL_DEFORESTATION_RISK_TCO2, 
                    -ZERO_DEFORESTATION_BRAZIL_BEEF, -TERRITORIAL_DEFORESTATION_RISK_HA,
                    -CATTLE_DEFORESTATION_5_YEAR_ANNUAL_RISK, -LAND_USE, -LOGISTICS.HUB, -BIOME,
                    -STATE, -EXPORTER, -EXPORTER.GROUP, -COUNTRY.OF.PRODUCTION, -IMPORTER, -Sigla,
                    -MUNICIPALITY) %>%
      dplyr::filter(code != "unknown-us")
    
    # generate data for countries
    countries <- csv_year$COUNTRY %>% unique() %>% sort()
    result <- data.frame()
    
    for(country in countries){
      cat(paste0("Processing ", year, " '", country, "'\n"))
      csv_country <- csv_year %>%
        dplyr::filter(COUNTRY == !!country)
      
      csv_muni <- dplyr::left_join(csv_country, shp, "code") 
      
      testthat::expect_true(all(csv_country$TRASE_GEOCODE == paste0("BR-", csv_muni$code_muni))) # check if everything is ok
      
      csv_muni <- csv_muni %>%
        dplyr::select(BEEF_EQUIVALENT_TONNES, code_muni)
      
      simu <- dplyr::inner_join(muniToSimU, csv_muni, "code_muni") %>%
        dplyr::mutate(value = area * BEEF_EQUIVALENT_TONNES) %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise(value = sum(value), .groups = "drop") %>%
        dplyr::mutate(country = !!country)
      
      result <- rbind(result, simu)
      
      testthat::expect_equal(sum(simu$value), sum(csv_country$BEEF_EQUIVALENT_TONNES))
    }
    
    testthat::expect_equal(sum(result$value), sum(csv_year$BEEF_EQUIVALENT_TONNES))
    
    total <- result %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::mutate(country = "ALLCOUNTRY")
    
    result <- rbind(result, total) %>%
      dplyr::mutate(value = round(value, 6)) %>%
      dplyr::filter(value > 0) %>%
      dplyr::arrange(ID)
    
    gmsStrings <- function(grouped, id, name, attr)
      return(paste0("Brazil.", grouped[[id]], ".", grouped[[name]], "\t", grouped[[attr]]))
    #  return(paste0("Brazil.", grouped[[id]], ".Alti_Any.Slp_Any.Soil_Any.", grouped[[name]], "\t", grouped[[attr]]))
    
    res <- data.frame(gmsStrings(result, "ID", "country", "value"))
    colnames(res) <- paste("Beef imported (carcass) by country for", year, "in kton by SimU ID\n\\")
    write.table(res, paste0("result/beef-country-", year, ".gms"), row.names = FALSE, quote = FALSE)
    
    # generate data for importers
    importers <- csv_year$IMPORTER.GROUP %>% unique() %>% sort()
    result <- data.frame()
    
    for(importer in importers){
      cat(paste0("Processing ", year, " '", importer, "'\n"))
      csv_importer <- csv_year %>%
        dplyr::filter(IMPORTER.GROUP == !!importer)
      
      csv_muni <- dplyr::left_join(csv_importer, shp, "code") 
      
      testthat::expect_true(all(csv_importer$TRASE_GEOCODE == paste0("BR-", csv_muni$code_muni))) # check if everything is ok
    
      csv_muni <- csv_muni %>%
        dplyr::select(BEEF_EQUIVALENT_TONNES, code_muni)
      
      simu <- dplyr::inner_join(muniToSimU, csv_muni, "code_muni") %>%
        dplyr::mutate(value = area * BEEF_EQUIVALENT_TONNES) %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise(value = sum(value), .groups = "drop") %>%
        dplyr::mutate(importer = !!importer)
      
      result <- rbind(result, simu)
      
      testthat::expect_equal(sum(simu$value), sum(csv_importer$BEEF_EQUIVALENT_TONNES))
    }
    
    testthat::expect_equal(sum(result$value), sum(csv_year$BEEF_EQUIVALENT_TONNES))
    
    total <- result %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::mutate(importer = "ALLIMPORTER")
    
    result <- rbind(result, total) %>%
      dplyr::mutate(value = round(value, 6)) %>%
      dplyr::filter(value > 0) %>%
      dplyr::arrange(ID)
    
    gmsStrings <- function(grouped, id, name, attr)
      return(paste0("Brazil.", grouped[[id]], ".", grouped[[name]], "\t", grouped[[attr]]))
    #  return(paste0("Brazil.", grouped[[id]], ".Alti_Any.Slp_Any.Soil_Any.", grouped[[name]], "\t", grouped[[attr]]))
    
    res <- data.frame(gmsStrings(result, "ID", "importer", "value"))
    colnames(res) <- paste("Beef imported (carcass) by importers for", year, "in kton by SimU ID\n\\")
    write.table(res, paste0("result/beef-importer-", year, ".gms"), row.names = FALSE, quote = FALSE)
  }
}
