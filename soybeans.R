
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
  mergeSoyIMPORTER.GROUP(0.999) %>%
  mapCOUNTRY() %>%
  mapMUNICIPALITY()

shp <- getMunicipalities()

checkMunicipalities(csv, shp)

#######################################################
cat("Joining and exporting data\n")
#######################################################



buildGms <- function(attribute){
  muniToSimU <- read.csv(getFile("muni-to-simu.csv")) # read from file exported by createRelations.R
  years <- csv$YEAR %>% unique() %>% sort()
  result <- data.frame()
  
  for(year in years){
    cat(paste("Processing year", year, "\n"))
    csv_year <- csv %>%
      dplyr::filter(YEAR == !!year) %>%
      dplyr::select(YEAR, IMPORTER.GROUP, !!attribute, TRASE_GEOCODE, Value, code) %>%
      dplyr::filter(code != "unknown-us")
    
    objects <- csv_year[[attribute]] %>% unique() %>% sort()
    
    result_simu <- data.frame()
    
    for(object in objects){
      csv_object <- csv_year %>%
        dplyr::filter(!!as.symbol(attribute) == !!object)
      
      csv_muni <- dplyr::left_join(csv_object, shp, "code") 
      
      testthat::expect_true(all(csv_object$TRASE_GEOCODE == paste0("BR-", csv_muni$code_muni))) # check if everything is ok
      
      csv_muni <- csv_muni %>%
        dplyr::select(Value, code_muni)
      
      simu <- dplyr::inner_join(muniToSimU, csv_muni, "code_muni") %>%
        dplyr::mutate(value = area * Value) %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise(value = sum(value), .groups = "drop") %>%
        dplyr::mutate(!!attribute := !!object) %>%
        dplyr::mutate(year = !!year)
      
      result_simu <- rbind(result_simu, simu)
      
      testthat::expect_equal(sum(simu$value), sum(csv_object$Value))
    }
  
    total <- result_simu %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::mutate(!!attribute := paste0("ALL", toupper(!!attribute))) %>%
      dplyr::mutate(year = !!year)
  
    result <- rbind(result, result_simu, total) 
  }
  return(result)
}

#countryGms <- buildGms("COUNTRY")
#importerGms <- buildGms("IMPORTER.GROUP")

countryGms <- buildGmsByPairs()
writeGmsByPairs(countryGms, getFile("result/trase-soy.gms"))


writeGms <- function(result, name, fileName){
  result <- result %>%
    dplyr::mutate(value = round(value, 6)) %>%
    dplyr::filter(value > 0) %>%
    dplyr::arrange(ID)
  
  gmsStrings <- function(grouped, id, name, attr)
    return(paste0("Brazil.", grouped[[id]], ".", grouped[[name]], ".", grouped$year, "\t", grouped[[attr]]))

  res <- data.frame(gmsStrings(result, "ID", name, "value"))
  write.table(res, paste0(fileName), row.names = FALSE, quote = FALSE)
}

