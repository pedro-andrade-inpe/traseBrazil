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
  csv$IMPORTER.GROUP = sub("\\&", "_", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("\\.", "_", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = gsub(" ", "_", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = gsub(",", "_", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = gsub("\\(", "_", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = gsub("\\)", "_", csv$IMPORTER.GROUP)

  return(csv)
}

# distribute UNKNOWN value proportionally to the other values
distributeUnknownValue <- function(csv){
  #######################################################
  cat("Distributing unknown\n")
  #######################################################

  total1 <- sum(csv$Value)

  total <- csv %>%
    dplyr::group_by(COUNTRY, YEAR) %>%
    dplyr::summarise(Total = sum(Value), .groups = "drop")

  unknownValue <- csv %>%
    dplyr::filter(str_detect(MUNICIPALITY, "^UNKNOWN")) %>%
    dplyr::group_by(COUNTRY, YEAR) %>%
    dplyr::summarise(Value = sum(Value), .groups = "drop") %>%
    dplyr::left_join(total, by = c("COUNTRY", "YEAR")) %>%
    dplyr::mutate(multiplier = 1 + Value / (Total - Value)) %>%
    dplyr::select(COUNTRY, YEAR, multiplier, Value)

  infCountries <- unknownValue %>%
    dplyr::filter(is.infinite(multiplier)) %>%
    dplyr::mutate(country_year = paste0(COUNTRY, "(", YEAR, ")")) %>%
    `$`("country_year") %>%
    unique() %>%
    sort()

  totalInf <- unknownValue %>%
    dplyr::filter(is.infinite(multiplier)) %>%
    `$`("Value") %>%
    sum() %>%
    round(2)

  cat(paste0("Production with unknown origin and no municipality to be used ", totalInf, " (", round(totalInf / total1 * 100, 4), "% of the total)\n"))

  if(dim(unknownValue)[1] == 0) return(csv) # nothing to be distributed

  unknownValue <- unknownValue %>%
    dplyr::select(-Value)

  csv <- csv %>%
    dplyr::filter(!str_detect(MUNICIPALITY, "^UNKNOWN")) %>%
    dplyr::left_join(unknownValue, by = c("COUNTRY", "YEAR")) %>%
    dplyr::filter(!is.infinite(multiplier)) %>%
    dplyr::mutate(multiplier = ifelse(is.na(multiplier), 1, multiplier)) %>%
    dplyr::mutate(Value = Value * multiplier) %>%
    dplyr::select(-multiplier)

  total2 <- sum(csv$Value)
  
  assertthat::are_equal(total1 - totalInf, total2, tol = 0.00001)
  
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

mergeSoyIMPORTER.GROUP <- function(csv, perc, groupedFile){
  #######################################################
  cat("Merging soy importers\n")
  #######################################################
  
  csv$IMPORTER.GROUP = sub("02\\.003\\.402/0046-77", "ADM DO BRASIL", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AMAGGI & COMMODITIES INTERNATIONAL LTD", "AMAGGI", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AMAGGI & COMMODITIES INTERNATIONAL LTDAMAGGI & LD COMMODITIES INTERNATIONAL LTD", "AMAGGI", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AMAGGI & LD COMMODITIES", "AMAGGI", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AMAGGI EUROPE B V ROTTERDAM", "AMAGGI", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AMAGGI LD COMMODITIES INTERNATIONAL LTD", "AMAGGI", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AMAGGIAMAGGI INTERNATIONAL LTD", "AMAGGI", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AMAGGI AND LD COMMODITIES INTERNATIONAL LTD", "AMAGGI", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("BUNGE ASIA PTE LTD", "BUNGE", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("BUNGE HANDELSGES", "BUNGE", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CARGIL BV ZOR", "CARGILL", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CARGILL DE HONDURAS S DE RL", "CARGILL", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CARGILL, INCORPORATED", "CARGILL", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CARGILL\t\t", "CARGILL", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CHS EUROPE S`RL", "CHS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CJ INTERNATIONAL ITALY SPA", "CJ CHEILJEDANG CORPORANT", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CJ INTERNATIONAL BRASIL COMERCIAL AGRICOLA LTDA\\.", "CJ CHEILJEDANG CORPORANT", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CJ INTERNATIONAL ASIA PTE", "CJ CHEILJEDANG CORPORANT", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CJ INTERNATIONAL ASIA LTD", "CJ CHEILJEDANG CORPORANT", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CJ EUROPE GMBH", "CJ CHEILJEDANG CORPORANT", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("COFCO AMERICAS RESOURCES CORP\\.", "COFCO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("COFCO BRASIL OVERSEAS LIMITED", "COFCO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("COFCO BRAZIL OVERSEAS LIMITED", "COFCO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("COFCO INERNATIONAL ITALY SRL", "COFCO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("COFCO INTERNATIONAL ITALY SRL", "COFCO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("COFCO RESOURCES PTE LTD", "COFCO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("COFCO RESOURCES S AAGROGRAIN LTD SUCURSAL URUGUAYCOFCO", "COFCO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("COFCO RESOURCES S AAGROGRAIN LTD SUCURSAL URUGUAYCOFCO BRAZIL OVERSEAS LIMITED", "COFCO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("COFCO RESOURCES SRL", "COFCO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CONCORDIA TRADING PTE LTD", "CONCORDIA TRDG", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CP VIETNAM CORPORATION", "CP GLOBAL TRADING LLP", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CP AQUACULTURE \\(INDIA\\) PRIVATE LIMITED", "CP GLOBAL TRADING LLP", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CUTRALE TRADING S A", "CUTRALE", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("GAVILON GRAIN LLC\\.", "GAVILON", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("GAVILON USA LLC", "GAVILON", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ENGELHART COMMODITIES PARTNERS", "ENGELHART", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ENGELHART CTP \\(SWITZERLAND\\) AS", "ENGELHART", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ENGELHART CTP SA", "ENGELHART", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ENGELHART CTP\\.\\(SWITERLAND\\.\\)SA", "ENGELHART", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ENGELHART CTP\\.\\(SWITZWERLAND\\.\\) S/A", "ENGELHART", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ENGELHART ECTP \\.\\(SWITZERLAND\\.\\) SA", "ENGELHART", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ENGELHART CTP\\(SWITERLAND\\)SA", "ENGELHART", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ENGELHART CTP\\(SWITZWERLAND\\) S/A", "ENGELHART", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ENGELHART ECTP \\(SWITZERLAND\\) SA", "ENGELHART", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ENGELHART PACTUAL COMMODITIES \\(SWITZERLAND\\) SA", "ENGELHART", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("INTERGRAIN COMAPANY LTD", "INTERGRAIN", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("INTERGRAIN SA", "INTERGRAIN", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("JBS URUGUAI - ZENDALEATHER S\\.A", "JBS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("KOSTER MARINE PROTEIN HOLDINGS GMBH E CO KG", "KOSTER MARINE PROTEINS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("KMP KOSTER MARINE PROTEINS", "KOSTER MARINE PROTEINS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("KMP KOSTER MARINE PROTEINS", "KOSTER MARINE PROTEINS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("KMP KMP KOSTER MARINE PROTEINS", "KOSTER MARINE PROTEINS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("KOSTER MARINE PROTEINS GMBH", "KOSTER MARINE PROTEINS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("KVSTER MARINE PROTEINS GMBH", "KOSTER MARINE PROTEINS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("LOUIS DRYFUS COMPANY SUISSE SA", "LOUIS DRYFUS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("LOUIS DREYFUS COMPANY S\\.A\\.", "LOUIS DREYFUS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("LOUIS DRYFUS", "LOUIS DREYFUS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("LOUIS DREYFUS COMPANY ASIA PTE \\(NORTH LATAM DIVISION\\)", "LOUIS DREYFUS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("MITSUI CO \\(U S A \\), INC , NEW YORK HEADQUARTERS", "MITSUI & CO.", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("MPH MARINE PROTEIN HOLDINGS GMBH & CO KG", "MPH MARINE PROTEIN HOLDINGS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("MPH MARINE PROTEIN HOLDINGS GMBH E CO", "MPH MARINE PROTEIN HOLDINGS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("MPH MARINE PROTEIN HOLDINGS GMBH E CO KG", "MPH MARINE PROTEIN HOLDINGS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("MPH MARINE PROTEIN HOLDINGS KG", "MPH MARINE PROTEIN HOLDINGS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("PERDUE AGRIBUSINESS LLC", "PERDUE AGRIBUSINESS GRAIN LLC", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("PT\\. CJ CHEILJEDANG FEED SEMARANG", "CJ CHEILJEDANG CORPORANT", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("PT CJ FEED JOMBANG", "CJ CHEILJEDANG CORPORANT", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("PT CJ CHEILJEDANG FEED LAMPUNG", "CJ CHEILJEDANG CORPORANT", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("SOJITZ CORPORATION GRAIN & FEED MATERIAL DEPT", "SOJITZ", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("YE CHERNG IND PRODUCTS CO , LTD", "YE CHENG INDUSTRIA PROD CO LTD", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("YE CHERNG INDUSTRIAL PRODUCTS CO , LTD", "YE CHENG INDUSTRIA PROD CO LTD", csv$IMPORTER.GROUP)

  csv <- cleanSpecialCharacters(csv)
  return(groupIMPORTER.GROUP(csv, perc, groupedFile))
}

mergeBeefIMPORTER.GROUP <- function(csv, perc, groupedFile){
  #######################################################
  cat("Merging beef importers\n")
  #######################################################
  
  csv$IMPORTER.GROUP = sub("ACP TRADING CO", "ACP INTERNATIONAL CO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AFANA BROTHERS COMPANY", "AFANA SONS COMPANY", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AFANA HOLDING CO", "AFANA SONS COMPANY", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AGRA CO LTD INN", "AGRA TRADING", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AGRAIMPORT CO", "AGRA TRADING", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AGRAKEPAK TRADING", "AGRAKEPAK INTERNATIONAL", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AJC TRADING CO", "AJC INTERNATIONAL", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AL AMAL NEW COLD STORES TRADING CO", "AL AMAL COMPANY FOR FOODS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AL FALOJA FOODS STUFF CO", "AL FALOJA INDUSTRY FOODS STUFF CO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AL KHALIL MEAT & SHEEP TRAD CO", "AL KHALIL TRADING CO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AL MANSOUR FOOD SUPPLY", "AL MANSOUR CO FOR TRADING & DISTRIBUTION", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("AL TAYEB COLD STORES", "AL TAYEB INTERNATIONAL GENERAL TRADING", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ALEIA GROUP COPR", "ALEIA GROUP CORP", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ANGLISS HONG FOOD SERVICE LTD", "ANGLISS SINGAPORE PTE LTD", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ANGLISS SHENZHEN FOOD SERVICE", "ANGLISS SINGAPORE PTE LTD", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ANHUI CERL OILS FOODSTUFFS IE G CORP", "ANHUI BBCA INT L FREIGHT CO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ANHUI CEREALS OILS AND FOODSTUFFS IE GROUP CORP", "ANHUI BBCA INT L FREIGHT CO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ARABIAN ARAMTEC TECHNOLOGY", "ARABIAN AMERICAN TECHNOLOGY ARAMTEC", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ARABIAN FOOD CORPORATION", "ARABIAN TRADE & FOOD INDUSTRIES", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ASHDALE HOLDING", "ASHDALE CAPITAL", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("BALTRYBTEX", "BALTRYBTECH", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("BEDAYA FOR PROC MEAT AND FOOD STUUF", "BEDAYA FOR MEAT PROCESSING", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("BRF FOODSTUFF TRADING", "BRF", csv$IMPORTER.GROUP)
  # CHONGQING was not agrregated as it is a city name in China
  csv$IMPORTER.GROUP = sub("CIPA FOODS IMPORT & EXPORT", "CIPA", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("CONTINENTAL COMMODITIES", "CONTINENTAL FOOD PROCESSING", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("COTIA CAYMAN", "COTIA ATLAS UK", csv$IMPORTER.GROUP)
  # DALIAN was not agrregated as it is a city name in China
  csv$IMPORTER.GROUP = sub("DAMACO FOODS PTE", "DAMACO FOODS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("DELTA-M", "DELTA TRDE", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("DELTA GROUP", "DELTA TRDE", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("DELTA MEAT FOODS", "DELTA TRDE", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("DELTA VIT	", "DELTA TRDE", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("FATHALLA COMP FOR COOL REF AND TRANS", "FATHALLA COMPOUND FOR COOLING REFREIGERATION AND COOLING", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("FATHALLA FOOD SOLUTIONS", "FATHALLA COMPOUND FOR COOLING REFREIGERATION AND COOLING", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("FOODEX GLOBAL FZE", "FOODEXCORE", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("FORTUNE INTERNATIONAL TRADING COMPANY", "FORTUNE GAIN INTERNATIONAL TRADING", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("FROSTMEAT", "FROSTMEAT FLEISCHHANDELSGESELLSCHA 1", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("FULLYTECH ASIA TRADING CO", "FULLYTECH INTL LOGISTICS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("GLORY FOOD INTERNATIONAL", "GLORY", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("GRACEKENNEDY FOODS USA LLCDBA LA FE", "GRACEKENNEDY", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("GREENLAND TOP TRADING CO", "GREENLAND ZHONGXUAN", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("GROENVELD VLEES BV", "GROENVELD", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("GVFI INTERNATIONAL", "GVFI EUROPE", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("HAKAN AGRO COMMODITIES", "HAKAN AGRO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("HALWANY CONSUMER PRODUCTS HCP", "HALWANI BROS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("HAMI KESHT COMPANY", "HAMI KESHT IRANIAN COMPANY", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("HAPI TRADING", "HAPI FRANCE", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("HEILONGJIANG TIANSHUNYUAN MUSLIM FOOD CO", "HEILONGJIANG T MUSLIM FOOD CO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("HENAN YISAI IMPORT & EXPORT TRADE CO", "HENAN YISAI BEEF CO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("HIGH HOPE INTL G J N P IMP & EX COR", "HIGH HOPE INTL GROUP", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("HISHAM TRADE FOR IMPORT & EXPORT", "HISHAM TRADE CO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("JAZEEL FOR TRADING AND CONTRACTING", "JAZEEL TRADING COMPANY", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("JBS AUSTRALIA PTY", "JBS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("JBS CHILE", "JBS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("JBS GLOBAL UK", "JBS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("JBS TOLEDO", "JBS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("JBS USA", "JBS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("ELITAFOODS JBS", "JBS", csv$IMPORTER.GROUP)
  # JIANGSU was not agrregated as it is a province name in China
  csv$IMPORTER.GROUP = sub("LAMEX FOODS EUROPE", "LAMEX FOODS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("MAGDY KHALIL MANDOUR", "MAGDY MANDOUR", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("MAKKAH FOR IMPORT & EXPORT HUSSEIN NASR ELDIN HUSSEIN", "MAKKAH COMPANY FOR IMPORT AND EXPORT", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("MERLO ERCOPOLE", "MERLO ERCOLE", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("MINERVA CHILE", "MINERVA", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("MINERVA MEATS USA", "MINERVA", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("MIRASCO URUGUAY", "MIRASCO INTERNATIONAL TRADING", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("MIRATORG AGRIBUSINESS HOLDING", "MIRATORG ZAPAD", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("MOHSEN HASSAN ALI ELKHAWAJA", "MOHSEN HASSAN EL KHAWAJA", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("OHANNESS FOODS", "OHANNES TRADING", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("PALLADA MEAT CO", "PALLADA", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("PMI HONG KONG FOOD SERVICE", "PMI FOOD SERVICES", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("PRINCES FOODS", "PRINCES", csv$IMPORTER.GROUP)
  # QINGDAO was not agrregated as it is a city name in China
  csv$IMPORTER.GROUP = sub("RAMAX LCC", "RAMAX", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("RXM TRADE INTERNATIONAL", "RXM ASIA", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("SAHAR FOOD INDUSTRY", "SAHAR ENTERPRISES", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("SCG SOCIETE DE COMMERCE GENERALE", "SCG", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("SEPEHRDAD JOUAN", "SEPEHR JOUAN", csv$IMPORTER.GROUP)
  # SHANDONG and SHANGHAI were not aggregated as they are a province and a metropolitan area
  csv$IMPORTER.GROUP = sub("SHANGHAI YJ FOOD GLOBAL SOURCING MANAGER", "SHANGHAI YJ FOOD INDUSTRIAL CO", csv$IMPORTER.GROUP)
  # SHENZHEN was not agrregated as it is a city name in China
  csv$IMPORTER.GROUP = sub("SKY REGAL TRADING", "SKY REGAL", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("SUZHOU INDUSTRIAL PARK GAOLI INTERNATIONAL TRADING CO", "SUZHOU INDL PARK GAOLI INTERNATIONAL", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("TARGETER UK", "TARGETER INTERNATIONAL", csv$IMPORTER.GROUP)
  # TIANJIN was not agrregated as it is a city name in China
  csv$IMPORTER.GROUP = sub("TOLEDO INTERNATIONAL", "TOLEDO IMPORMIT", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("UNIFOOD DHM INDUSTRIAL CO", "UNIFOOD INDUSTRIAL", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("VERDANT CHINA", "VERDANT INTERNATIONAL", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("VIKING FOOD", "VIKING", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("WELL BEST COMPANY", "WELL BEST TRADING CO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("WORLD MEAT SUPPLY", "WORLD MEAT SAL", csv$IMPORTER.GROUP)
  # XIAMEN was not agrregated as it is a city name in China
  csv$IMPORTER.GROUP = sub("YAU TAK INTERNATIONAL CO", "YAU TAK FOODSTUFF CO", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("YC HK FOODS", "YC HONGKONG FOODS", csv$IMPORTER.GROUP)
  csv$IMPORTER.GROUP = sub("YL FOODS", "YL TRADING INTERNATIONAL", csv$IMPORTER.GROUP)

  csv <- cleanSpecialCharacters(csv)

  return(groupIMPORTER.GROUP(csv, perc, groupedFile))
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
  csv$COUNTRY = sub("Vietnam", "VietNam", csv$COUNTRY)
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
  csv$code = sub("vila bela santissima", "vila bela da santissima", csv$code)
  csv$code = sub("patos de mina", "patos de minas", csv$code)
  csv$code = sub("coronel vivid", "coronel vivida", csv$code)
  csv$code = sub("porto naciona", "porto nacional", csv$code)
  csv$code = sub("limeira do oe", "limeira do oeste", csv$code)
  csv$code = sub("eldorado do s", "eldorado do sul", csv$code)
  csv$code = sub("sao miguel d oeste", "sao miguel do oeste", csv$code)
  csv$code = sub("mogi-mirim", "mogi mirim", csv$code)
  csv$code = sub("couto de magalhaes-to", "couto magalhaes-to", csv$code)
  csv$code = sub("faxinal dos g", "faxinal dos guedes", csv$code)
  csv$code = sub("presidente castelo branco-sc", "presidente castello branco-sc", csv$code)
  csv$code = sub("florinia", "florinea", csv$code)
  csv$code = sub("muquem de sao francisco", "muquem do sao francisco", csv$code)
  csv$code = sub("poxoreo", "poxoreu", csv$code)
  csv$code = sub("sao luis do paraitinga", "sao luiz do paraitinga", csv$code)
  csv$code = sub("sao valerio da natividade-to", "sao valerio-to", csv$code)
  csv$code = sub("passa-vinte-mg", "passa vinte-mg", csv$code)
  csv$code = sub("brasopolis-mg", "brazopolis-mg", csv$code)
  csv$code = sub("santa isabel do para-pa", "santa izabel do para-pa", csv$code)
  csv$code = sub("eldorado dos carajas-pa", "eldorado do carajas-pa", csv$code)
  
  # all the data with unknown municipality or aggregated (that have a state)
  # will be set as unknown
  # these data must be removed by functions distributeUnknownValue() and distributeAggregated()
  csv$code = str_replace(csv$code, "aggregated .*", "unknown-us")
  csv$code = str_replace(csv$code, "unknown municipality .*", "unknown-us")

  # do not do this to guarantee that these values will be removed
  # csv <- csv %>% dplyr::filter(code != "unknown-us")
  
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

checkMunicipalities <- function(csv, shp){
  #######################################################
  cat("Matching shapefile and CSV\n")
  #######################################################
  
  err = list()
  for(i in 1:dim(csv)[1]){
    munic <- csv$code[i]
    
    pos <- which(shp$code == munic)
    
    if(length(pos) == 0 & munic != "unknown-us") {
      #cat(paste0("Problem: ", munic, " at position ", i, "\n"))
      err[[munic]] = TRUE
    }
  }
  
  if(length(err) > 0) print(names(err)) # municipalities with some problem to match Trase and IBGE
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
      dplyr::select(YEAR, IMPORTER.GROUP, COUNTRY, TRASE_GEOCODE, Value, code)
    
    countries <- csv_year$COUNTRY %>% unique() %>% sort()
    
    result_simu <- data.frame()
    
    for(country in countries){
      csv_object <- csv_year %>%
        dplyr::filter(COUNTRY == country)
      
      csv_muni <- dplyr::left_join(csv_object, shp, "code") 
      
      assertthat::assert_that(all(csv_object$TRASE_GEOCODE == paste0("BR-", csv_muni$code_muni))) # check if everything is ok
      
      csv_muni <- csv_muni %>%
        dplyr::select(Value, code_muni, IMPORTER.GROUP)
      
      simu <- dplyr::inner_join(muniToSimU, csv_muni, "code_muni") %>%
        dplyr::mutate(value = area * Value) %>%
        dplyr::group_by(ID, IMPORTER.GROUP) %>%
        dplyr::summarise(value = sum(value), .groups = "drop") %>%
        dplyr::mutate(country = !!country) %>%
        dplyr::mutate(year = !!year)
      
      result_simu <- rbind(result_simu, simu)
      
      assertthat::are_equal(sum(simu$value), sum(csv_object$Value))
    }
    
    total <- result_simu %>%
      dplyr::group_by(ID, IMPORTER.GROUP) %>%
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
  
  res <- paste0("Brazil.", result$ID, ".", result$IMPORTER.GROUP, ".", result$country, ".", result$year, "\t", result$value) %>%
    c("/", ";") %>%
    data.frame()

  colnames(res) <- paste0("PARAMETER ",
                          name,
                          "_TRASE\n(COUNTRY,SimUID,ALLTRADER,ALLCOUNTRY,ALLSCENYEAR) ",
                          name,
                          " sourcing in kton per SimU\n/")
  
  write.table(res, getFile(paste0("result/trase-", name , ".gms")), row.names = FALSE, quote = FALSE)
}
