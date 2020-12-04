require(dplyr)
require(sf)
require(geobr)
require(stringr)
require(testthat)

CONVERSION = 1e3

#######################################################
cat("Reading CSV files\n")
#######################################################

beefFile1 <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2020\\rplus\\trase\\BRAZIL_BEEF_2.0.1_pc\\BRAZIL_BEEF_2.0.1_pc.2015.csv"
beefFile2 <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2020\\rplus\\trase\\BRAZIL_BEEF_2.0.1_pc\\BRAZIL_BEEF_2.0.1_pc.2016.csv"
beefFile3 <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2020\\rplus\\trase\\BRAZIL_BEEF_2.0.1_pc\\BRAZIL_BEEF_2.0.1_pc.2017.csv"

csv1 <- read.csv(beefFile1, as.is = TRUE)
csv2 <- read.csv(beefFile2, as.is = TRUE)
csv3 <- read.csv(beefFile3, as.is = TRUE)

csv <- rbind(csv1, csv2, csv3) %>%
  mutate(BEEF_EQUIVALENT_TONNES = BEEF_EQUIVALENT_TONNES / CONVERSION) %>%
  mutate(IMPORTER.GROUP = stringr::str_trim(IMPORTER.GROUP))

#######################################################
cat("Merging importers\n")
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

#######################################################
cat("Renaming out of 95% as OTHER\n")
#######################################################

ww <- csv %>% dplyr::select(BEEF_EQUIVALENT_TONNES, IMPORTER.GROUP) %>%
  group_by(IMPORTER.GROUP) %>%
  summarize(beef_k_tonnes = sum(BEEF_EQUIVALENT_TONNES), .groups = "drop") %>%
  dplyr::mutate(beef_k_tonnes = round(beef_k_tonnes, 1)) %>%
  dplyr::relocate(beef_k_tonnes, IMPORTER.GROUP) %>%
  dplyr::filter(IMPORTER.GROUP != "DOMESTIC CONSUMPTION") %>%
  dplyr::filter(IMPORTER.GROUP != "UNKNOWN")

total <- sum(ww$beef_k_tonnes)

#for(perc in c(1, 0.999, 0.99, 0.95, 0.8, 0.5)){
perc <- 0.95
ww2 <- ww %>% 
  arrange(desc(beef_k_tonnes)) %>%
  mutate(cumsum = cumsum(beef_k_tonnes)) %>%
  filter(cumsum <= total * perc)

#  cat(paste0(round(perc*100, 1), "% de todas importacoes de carne => ", dim(ww2)[1], " empresas\n"))
#}

# 100% de todas importacoes de carne => 2955 empresas
# 99.9% de todas importacoes de carne => 1990 empresas
# 99% de todas importacoes de carne => 1472 empresas
# 95% de todas importacoes de carne => 807 empresas
# 80% de todas importacoes de carne => 285 empresas
# 50% de todas importacoes de carne => 60 empresas

# for merging manually
#ww <- ww %>%
#  mutate(more_relevant = ifelse(IMPORTER.GROUP %in% ww2$IMPORTER.GROUP, "X", ""))
#View(ww)  

# merging the less relevant companies
csv <- csv %>%
  mutate(IMPORTER.GROUP = ifelse(IMPORTER.GROUP %in% ww2$IMPORTER.GROUP, IMPORTER.GROUP, "OTHER"))

#######################################################
cat("Renaming municipalities\n")
#######################################################

mystates = list(
  ACRE = "AC", ALAGOAS = "AL", AMAPA = "AP", AMAZONAS = "AM", BAHIA = "BA", CEARA = "CE",
  `DISTRITO FEDERAL` = "DF",  `ESPIRITO SANTO` = "ES", GOIAS = "GO", MARANHAO = "MA",
  `MATO GROSSO` = "MT", `MATO GROSSO DO SUL` = "MS", `MINAS GERAIS` = "MG", PARA = "PA",
  PARAIBA = "PB", PARANA = "PR", PERNAMBUCO = "PE", PIAUI = "PI", `RIO DE JANEIRO` = "RJ",
  `RIO GRANDE DO NORTE` = "RN", `RIO GRANDE DO SUL` = "RS", 
  RONDONIA = "RO", RORAIMA = "RR",  `SAO PAULO` = "SP", `SANTA CATARINA` = "SC", SERGIPE = "SE",
  TOCANTINS = "TO", `UNKNOWN STATE` = "US"
)

csv$Sigla = unlist(mystates[csv$STATE])

cat("Renaming municipalities\n")
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
csv$code = str_replace(csv$code, "aggregated .*", "unknown-us")
csv$code = str_replace(csv$code, "unknown municipality .*", "unknown-us")

#######################################################
cat("Loading municipalities shapefile\n")
#######################################################

munic_brazil <- read_municipality(year = 2017)
shp = as.data.frame(munic_brazil)

# fix names
shp$code = tolower(paste0(shp$name_muni, "-", shp$abbrev_state))
shp$code <- chartr("áéíóúâêôãõç", "aeiouaeoaoc", shp$code)
shp$code <- sub("'", "", shp$code)

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
testthat::expect_equal(length(err), 0)

####################################################
####################################################
# national production using pta joining with Trase by state

pta <- read.csv("ibge/pta_tabela1092_abate_bovino.csv", sep = ";", skip = 3)
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

ppm <- read.csv("ibge/ppm_tabela3939_efetivo_rebanho_bovino.csv", sep = ";", skip = 3)
ppm <- ppm[-1, ]
ppm
####################################################
####################################################

#######################################################
cat("Merging countries\n")
#######################################################

csv$COUNTRY = sub("CHINA \\(HONG KONG\\)", "CHINA", csv$COUNTRY)
csv$COUNTRY = sub("CHINA \\(MAINLAND\\)", "CHINA", csv$COUNTRY)

csv$ECONOMIC.BLOC = sub("CHINA \\(HONG KONG\\)", "CHINA", csv$ECONOMIC.BLOC)
csv$ECONOMIC.BLOC = sub("CHINA \\(MAINLAND\\)", "CHINA", csv$ECONOMIC.BLOC)

#######################################################
cat("Joining and exporting data\n")
#######################################################

shp <- shp %>% dplyr::select(code_muni, code)
muniToSimU <- read.csv("muni-to-simu.csv") # read from file exported by createRelations.R
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
