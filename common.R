require(dplyr)
require(sf)
require(geobr)
require(stringr)
require(assertthat)
require(colrow) #devtools::install_github("pedro-andrade-inpe/colrow")

CONVERSION <- 1e3 # thousand ton
COLROW_DATA_DIR <- "c:/Users/pedro/Dropbox/colrow/"
DATA_DIR <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2020\\rplus\\trase\\"

getFile <- function(file) paste0(DATA_DIR, file)

# distribute AGGREGATED values proportionally within the respective states
distributeAggregated <- function(csv){
  #######################################################
  cat("Distributing aggregated\n")
  #######################################################

  total1 <- sum(csv$Value)

  total <- csv %>%
    dplyr::group_by(COUNTRY, STATE, YEAR) %>%
    dplyr::summarise(Total = sum(Value), .groups = "drop")
  
  aggregatedValue <- csv %>%
    dplyr::filter(str_detect(MUNICIPALITY, "^AGGREGATED")) %>%
    dplyr::group_by(COUNTRY, STATE, YEAR) %>%
    dplyr::summarise(Value = sum(Value), .groups = "drop") %>%
    dplyr::left_join(total, by = c("COUNTRY", "STATE", "YEAR")) %>%
    dplyr::mutate(multiplier = 1 + Value / (Total - Value)) %>%
    dplyr::select(COUNTRY, STATE, YEAR, multiplier, Value)
  
  infCountries <- aggregatedValue %>%
    dplyr::filter(is.infinite(multiplier)) %>%
    dplyr::group_by(COUNTRY, STATE, YEAR) %>%
    dplyr::summarise(Value = sum(Value), .groups = "drop") %>%
    dplyr::arrange(COUNTRY)

  totalInf <- aggregatedValue %>%
    dplyr::filter(is.infinite(multiplier)) %>%
    `$`("Value") %>%
    sum() %>%
    round(2)
  
  cat(paste0("Production with origin aggregated by state and no municipality to be used ", totalInf, " (", round(totalInf / total1 * 100, 4), "% of the total)\n"))

  if(dim(aggregatedValue)[1] == 0) return(csv) # nothing to be distributed

  aggregatedValue <- aggregatedValue %>%
    dplyr::select(-Value)
  
  csv <- csv %>%
    dplyr::filter(!str_detect(MUNICIPALITY, "^AGGREGATED")) %>%
    dplyr::left_join(aggregatedValue, by = c("COUNTRY", "STATE", "YEAR")) %>%
    dplyr::filter(!is.infinite(multiplier)) %>%
    dplyr::mutate(multiplier = ifelse(is.na(multiplier), 1, multiplier)) %>%
    dplyr::mutate(Value = Value * multiplier) %>%
    dplyr::select(-multiplier)
  
  total2 <- sum(csv$Value)

  assertthat::are_equal(total1 - totalInf, total2, tol = 0.01)
  
  # some error because there are states with only aggregated municipalities, therefore
  # this data cannot be spread
  
  return(csv)
}

groupIMPORTER.GROUP <- function(csv, perc, groupedFile){
  #######################################################
  cat(paste0("Renaming out of ", perc * 100, "% as OTHER\n"))
  #######################################################
  
  ww <- csv %>% dplyr::select(Value, IMPORTER.GROUP) %>%
    group_by(IMPORTER.GROUP) %>%
    summarize(Value = sum(Value), .groups = "drop") %>%
    dplyr::mutate(Value = round(Value, 1)) %>%
    dplyr::relocate(Value, IMPORTER.GROUP) %>%
    dplyr::filter(IMPORTER.GROUP != "DOMESTIC CONSUMPTION") %>%
    dplyr::filter(IMPORTER.GROUP != "UNKNOWN")
  
  total <- sum(ww$Value)
  
  ww2 <- ww %>% 
    dplyr::arrange(desc(Value)) %>%
    dplyr::mutate(cumsum = cumsum(Value)) %>%
    dplyr::filter(cumsum <= total * perc)

  ww2$IMPORTER.GROUP %>% 
    unique() %>%
    sort() %>% 
    write.table(getFile(paste0("result/trase-joined-", groupedFile, "-as-other.txt")), quote = FALSE, row.names = FALSE, col.names = FALSE)

  # merging the less relevant companies
  csv <- csv %>%
    dplyr::mutate(IMPORTER.GROUP = ifelse(IMPORTER.GROUP %in% ww2$IMPORTER.GROUP, IMPORTER.GROUP, "OTHER"))

  return(csv)
}

mapCOUNTRY <- function(csv){
  #######################################################
  cat("Mapping countries from Trase to GLOBIOM\n")
  #######################################################
  
  csv$COUNTRY = str_to_title(csv$COUNTRY)
  csv$COUNTRY = gsub(" ", "", csv$COUNTRY)
  csv$COUNTRY = sub("China\\(HongKong\\)", "China", csv$COUNTRY)
  csv$COUNTRY = sub("China\\(Mainland\\)", "China", csv$COUNTRY)

  return(csv)
}

# check whether all municipalities match their names and geocode
# using the csv data and the municipalities from ibge
checkMunicipalities <- function(csv, shp){
  #######################################################
  cat("Matching shapefile and CSV\n")
  #######################################################
  
  err = list()

  for(i in 1:dim(csv)[1]){
    munic <- csv$MUNICIPALITY[i]
    
    pos <- which(shp$code_muni == as.numeric(munic))
    
    if(length(pos) != 1) {
      cat(paste0("Geocode does not match: ", munic, " at position ", i, "\n"))
      err[[munic]] = TRUE
    }
  }

  if(length(err) > 0) print(sort(names(err))) # municipalities with some problem to match Trase and IBGE
  assertthat::are_equal(length(err), 0)
}

buildGmsByPairs <- function(csv, shp){
  muniToSimU <- read.csv(getFile("muni-to-simu.csv")) # read from file exported by createRelations.R
  years <- csv$year %>% unique() %>% sort()
  result <- data.frame()
  
  shp <- shp %>% dplyr::mutate(MUNICIPALITY = paste0(code_muni))

  for(year in years){
    cat(paste("Processing year", year, "\n"))
    csv_year <- csv %>%
      dplyr::filter(year == !!year)

    countries <- csv_year$COUNTRY %>% unique() %>% sort()
    
    result_simu <- data.frame()
    
    for(country in countries){
      csv_object <- csv_year %>%
        dplyr::filter(COUNTRY == country)
      
      csv_muni <- dplyr::left_join(csv_object, shp, "MUNICIPALITY") 
      
      csv_muni <- csv_muni %>%
        dplyr::select(Value, code_muni, EXPORTER_GROUP)
      
      simu <- dplyr::inner_join(muniToSimU, csv_muni, "code_muni") %>%
        dplyr::mutate(value = area * Value) %>%
        dplyr::group_by(ID, EXPORTER_GROUP) %>%
        dplyr::summarise(value = sum(value), .groups = "drop") %>%
        dplyr::mutate(country = !!country) %>%
        dplyr::mutate(year = !!year)
      
      result_simu <- rbind(result_simu, simu)
      
      assertthat::are_equal(sum(simu$value), sum(csv_object$Value))
    }
    
    total <- result_simu %>%
      dplyr::group_by(ID, EXPORTER_GROUP) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::mutate(country = "TOTALCOUNTRY") %>%
      dplyr::mutate(year = !!year)
    
    result <- rbind(result, result_simu, total) 
  }
  return(result)
}

writeGmsByPairs <- function(result, name){
  result <- result %>%
    dplyr::mutate(value = round(value, 6)) %>%
    dplyr::filter(value > 0) %>%
    dplyr::arrange(ID)
  
  res <- paste0("Brazil.", result$ID, ".\"", result$EXPORTER_GROUP, "\".\"", result$country, "\".", result$year, "\t", result$value) %>%
    c("/", ";") %>%
    data.frame()

  colnames(res) <- paste0("PARAMETER ",
                          name,
                          "_TRASE\n(COUNTRY,SimUID,ALLTRADER,ALLCOUNTRY,ALLSCENYEAR) ",
                          name,
                          " sourcing in kton per SimU\n/")
  
  write.table(res, getFile(paste0("result/trase-", name , ".gms")), row.names = FALSE, quote = FALSE)
}
