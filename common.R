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

cleanSpecialCharacters <- function(csv){
  csv$COUNTRY = sub("\\.", "_", csv$COUNTRY)

  csv$EXPORTER.GROUP = sub("\\.", "_", csv$EXPORTER.GROUP)
  csv$EXPORTER.GROUP = sub("\\&", "_", csv$EXPORTER.GROUP)
  csv$EXPORTER.GROUP = sub(" ", "_", csv$EXPORTER.GROUP)
  csv$EXPORTER.GROUP = sub(",", "_", csv$EXPORTER.GROUP)
  csv$EXPORTER.GROUP = sub("\\(", "_", csv$EXPORTER.GROUP)
  csv$EXPORTER.GROUP = sub("\\)", "_", csv$EXPORTER.GROUP)
  
  return(csv)
}

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
  csv$COUNTRY = sub("BosniaAndHerzegovina", "BosniaHerzg", csv$COUNTRY)
  csv$COUNTRY = sub("Brunei", "BruneiDarsm", csv$COUNTRY)
  csv$COUNTRY = sub("China\\(HongKong\\)", "China", csv$COUNTRY)
  csv$COUNTRY = sub("China\\(Mainland\\)", "China", csv$COUNTRY)
  csv$COUNTRY = sub("CongoDemocraticRepublicOfThe", "CongoDemR", csv$COUNTRY)
  csv$COUNTRY = sub("Congo$", "CongoRep", csv$COUNTRY)
  csv$COUNTRY = sub("CoteD'ivoire", "CotedIvoire", csv$COUNTRY)
  csv$COUNTRY = sub("DominicanRepublic", "DominicanRp", csv$COUNTRY)
  csv$COUNTRY = sub("EquatorialGuinea", "EqGuinea", csv$COUNTRY)
  csv$COUNTRY = sub("FalklandIslands\\(Malvinas\\)", "FalklandIs", csv$COUNTRY)
  csv$COUNTRY = sub("FrenchGuyana", "FrGuiana", csv$COUNTRY)
  csv$COUNTRY = sub("FrenchPolynesia", "FrPolynesia", csv$COUNTRY)
  csv$COUNTRY = sub("Moldova", "MoldovaRep", csv$COUNTRY)
  csv$COUNTRY = sub("Serbia", "Serbia-Monte", csv$COUNTRY)
  csv$COUNTRY = sub("Montenegro", "Serbia-Monte", csv$COUNTRY)
  csv$COUNTRY = sub("NorthKorea", "KoreaDPRp", csv$COUNTRY)
  csv$COUNTRY = sub("PapuaNewGuinea", "PapuaNGuin", csv$COUNTRY)
  csv$COUNTRY = sub("RussianFederation", "RussianFed", csv$COUNTRY)
  csv$COUNTRY = sub("SouthKorea", "KoreaRep", csv$COUNTRY)
  csv$COUNTRY = sub("TrinidadAndTobago", "TrinidadTob", csv$COUNTRY)
  csv$COUNTRY = sub("UnitedArabEmirates", "UntdArabEm", csv$COUNTRY)
  csv$COUNTRY = sub("UnitedKingdom", "UK", csv$COUNTRY)
  csv$COUNTRY = sub("UnitedStates", "USA", csv$COUNTRY)
  csv$COUNTRY = sub("USAVirginIslands", "USA", csv$COUNTRY)
  csv$COUNTRY = sub("Vietnam", "VietNam", csv$COUNTRY)
  csv$COUNTRY = sub("VirginIslands\\(Uk\\)", "UK", csv$COUNTRY)
  csv$COUNTRY = sub("OccupiedPalestinianTerritory", "Palestin", csv$COUNTRY)

  csv <- cleanSpecialCharacters(csv)
  
  return(csv)
}

getStates <- function(){
  states <- list(
    ACRE = "AC", ALAGOAS = "AL", AMAPA = "AP", AMAZONAS = "AM", BAHIA = "BA", CEARA = "CE",
    `DISTRITO FEDERAL` = "DF",  `ESPIRITO SANTO` = "ES", GOIAS = "GO", MARANHAO = "MA",
    `MATO GROSSO` = "MT", `MATO GROSSO DO SUL` = "MS", `MINAS GERAIS` = "MG", PARA = "PA",
    PARAIBA = "PB", PARANA = "PR", PERNAMBUCO = "PE", PIAUI = "PI", `RIO DE JANEIRO` = "RJ",
    `RIO GRANDE DO NORTE` = "RN", `RIO GRANDE DO SUL` = "RS", 
    RONDONIA = "RO", RORAIMA = "RR",  `SAO PAULO` = "SP", `SANTA CATARINA` = "SC", SERGIPE = "SE",
    TOCANTINS = "TO", `UNKNOWN STATE` = "US"
  )
  
  return(states)
}

mapMUNICIPALITY = function(csv){
  #######################################################
  cat("Renaming municipalities\n")
  #######################################################
  
  mystates <- getStates()
  
  csv$Sigla = unlist(mystates[csv$STATE])
  
  # fix municipalities
  csv$code = tolower(paste0(csv$MUNICIPALITY, "-", csv$Sigla))
  csv$code = sub("'", "", csv$code)
  csv$code = sub("brasopolis-mg", "brazopolis-mg", csv$code)
  csv$code = sub("coronel vivid", "coronel vivida", csv$code)
  csv$code = sub("couto de magalhaes-to", "couto magalhaes-to", csv$code)
  csv$code = sub("eldorado do s", "eldorado do sul", csv$code)
  csv$code = sub("eldorado dos carajas-pa", "eldorado do carajas-pa", csv$code)
  csv$code = sub("faxinal dos g", "faxinal dos guedes", csv$code)
  csv$code = sub("florinia", "florinea", csv$code)
  csv$code = sub("limeira do oe", "limeira do oeste", csv$code)
  csv$code = sub("mogi-mirim", "mogi mirim", csv$code)
  csv$code = sub("muquem de sao francisco", "muquem do sao francisco", csv$code)
  csv$code = sub("passa-vinte-mg", "passa vinte-mg", csv$code)
  csv$code = sub("patos de mina", "patos de minas", csv$code)
  csv$code = sub("porto naciona", "porto nacional", csv$code)
  csv$code = sub("poxoreo", "poxoreu", csv$code)
  csv$code = sub("presidente castelo branco-sc", "presidente castello branco-sc", csv$code)
  csv$code = sub("santa isabel do para-pa", "santa izabel do para-pa", csv$code)
  csv$code = sub("sao luis do paraitinga", "sao luiz do paraitinga", csv$code)
  csv$code = sub("sao miguel d oeste", "sao miguel do oeste", csv$code)
  csv$code = sub("sao valerio da natividade-to", "sao valerio-to", csv$code)
  csv$code = sub("vila bela santissima", "vila bela da santissima", csv$code)
  
  return(csv)
}

getMunicipalities <- function(){
  #######################################################
  cat("Loading municipalities shapefile\n")
  #######################################################
  
  shp <- read_municipality(year = 2017) %>%
    as.data.frame() %>%
    dplyr::mutate(code = tolower(paste0(name_muni, "-", abbrev_state))) %>%
    dplyr::mutate(code = stringi::stri_trans_general(code, id="Latin-ASCII")) %>%
    dplyr::mutate(code = sub("'", "", code)) %>%
    dplyr::select(code_muni, code)
  
  return(shp)
}

# check whether all municipalities match their names and geocode
# using the csv data and the municipalities from ibge
checkMunicipalities <- function(csv, shp){
  #######################################################
  cat("Matching shapefile and CSV\n")
  #######################################################
  
  err = list()
  for(i in 1:dim(csv)[1]){
    munic <- csv$code[i]
    
    pos <- which(shp$code == munic)
    
    if(length(pos) != 1) {
      cat(paste0("Geocode does not match: ", munic, " at position ", i, "\n"))
      err[[munic]] = TRUE
    }
    
    if(munic != shp$code[pos]){
      cat(paste0("Munic names do not match: ", munic, " and ", shp$code[pos], "\n"))
      err[[munic]] = TRUE
      
    }
    
    
  }

  if(length(err) > 0) print(sort(names(err))) # municipalities with some problem to match Trase and IBGE
  assertthat::are_equal(length(err), 0)
}

buildGmsByPairs <- function(csv){
  muniToSimU <- read.csv(getFile("muni-to-simu.csv")) # read from file exported by createRelations.R
  years <- csv$YEAR %>% unique() %>% sort()
  result <- data.frame()
  
  for(year in years){
    cat(paste("Processing year", year, "\n"))
    csv_year <- csv %>%
      dplyr::filter(YEAR == !!year) %>%
      dplyr::select(YEAR, EXPORTER.GROUP, COUNTRY, TRASE_GEOCODE, Value, code)
    
    countries <- csv_year$COUNTRY %>% unique() %>% sort()
    
    result_simu <- data.frame()
    
    for(country in countries){
      csv_object <- csv_year %>%
        dplyr::filter(COUNTRY == country)
      
      csv_muni <- dplyr::left_join(csv_object, shp, "code") 
      
      assertthat::assert_that(all(csv_object$TRASE_GEOCODE == paste0("BR-", csv_muni$code_muni))) # check if everything is ok
      
      csv_muni <- csv_muni %>%
        dplyr::select(Value, code_muni, EXPORTER.GROUP)
      
      simu <- dplyr::inner_join(muniToSimU, csv_muni, "code_muni") %>%
        dplyr::mutate(value = area * Value) %>%
        dplyr::group_by(ID, EXPORTER.GROUP) %>%
        dplyr::summarise(value = sum(value), .groups = "drop") %>%
        dplyr::mutate(country = !!country) %>%
        dplyr::mutate(year = !!year)
      
      result_simu <- rbind(result_simu, simu)
      
      assertthat::are_equal(sum(simu$value), sum(csv_object$Value))
    }
    
    total <- result_simu %>%
      dplyr::group_by(ID, EXPORTER.GROUP) %>%
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
  
  res <- paste0("Brazil.", result$ID, ".\"", result$EXPORTER.GROUP, "\".\"", result$country, "\".", result$year, "\t", result$value) %>%
    c("/", ";") %>%
    data.frame()

  colnames(res) <- paste0("PARAMETER ",
                          name,
                          "_TRASE\n(COUNTRY,SimUID,ALLTRADER,ALLCOUNTRY,ALLSCENYEAR) ",
                          name,
                          " sourcing in kton per SimU\n/")
  
  write.table(res, getFile(paste0("result/trase-", name , ".gms")), row.names = FALSE, quote = FALSE)
}
