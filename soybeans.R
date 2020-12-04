require(dplyr)
require(sf)
require(geobr)
require(stringr)
require(testthat)

CONVERSION = 1e3 # thousand kg

#######################################################
cat("Reading CSV files\n")
#######################################################

soyFile <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2020\\rplus\\trase\\BRAZIL_SOY_2.5.1_pc\\BRAZIL_SOY_2.5.1_pc.csv"

csv <- read.csv(soyFile, as.is = TRUE) %>%
  dplyr::filter(YEAR >= 2015) %>%
  mutate(SOY_EQUIVALENT_TONNES = SOY_EQUIVALENT_TONNES / CONVERSION) %>%
  mutate(IMPORTER.GROUP = stringr::str_trim(IMPORTER.GROUP))

#######################################################
cat("Merging importers\n")
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

# auxiliary code to help merging companies

#######################################################
cat("Renaming out of 99.9% as OTHER\n")
#######################################################

ww <- csv %>% dplyr::select(SOY_EQUIVALENT_TONNES, IMPORTER.GROUP) %>%
  group_by(IMPORTER.GROUP) %>%
  summarize(soy_k_tonnes = sum(SOY_EQUIVALENT_TONNES)) %>%
  dplyr::mutate(soy_k_tonnes = round(soy_k_tonnes, 1)) %>%
  dplyr::relocate(soy_k_tonnes, IMPORTER.GROUP) %>%
  dplyr::filter(IMPORTER.GROUP != "DOMESTIC CONSUMPTION") %>%
  dplyr::filter(IMPORTER.GROUP != "UNKNOWN")

total <- sum(ww$soy_k_tonnes)

#for(perc in c(1, 0.999, 0.99, 0.95, 0.8, 0.5)){
perc <- 0.999
ww2 <- ww %>% 
  arrange(desc(soy_k_tonnes)) %>%
  mutate(cumsum = cumsum(soy_k_tonnes)) %>%
  filter(cumsum <= total * perc)

#  cat(paste0(round(perc*100, 1), "% de todas importacoes de soja => ", dim(ww2)[1], " empresas\n"))
#}

# 100% de todas importacoes de soja => 352 empresas
# 99.9% de todas importacoes de soja => 92 empresas
# 99% de todas importacoes de soja => 44 empresas
# 95% de todas importacoes de soja => 21 empresas
# 80% de todas importacoes de soja => 11 empresas
# 50% de todas importacoes de soja => 4 empresas

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
  RONDONIA = "RO", RORAIMA = "RR",  `SANTA CATARINA` = "SC", `SAO PAULO` = "SP", SERGIPE = "SE",
  TOCANTINS = "TO", `UNKNOWN STATE` = "US"
)

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
msum <- 0
for(i in 1:dim(csv)[1]){
  munic <- csv$code[i]
  soy   <- csv$SOY_EQUIVALENT_TONNES[i]
  year  <- csv$YEAR[i]
  
  pos <- which(shp$code == munic)
  
  if(length(pos) == 0 & munic != "unknown-us") {
    cat(paste0("Problem: ", munic, " at position ", i, "\n"))
    err[[munic]] = TRUE
  }
  else
  {
    msum <- msum + soy
    shp[pos, paste0("soy_", year)] <- soy
  }
}

if(length(err) > 0) print(names(err)) # municipalities with some problem to match Trase and IBGE
testthat::expect_equal(length(err), 0)

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
    dplyr::select(-CO2_EMISSIONS_TERRITORIAL_DEFORESTATION_RISK_TCO2.HA, -LAND_USE_HA,
                  -CO2_EMISSIONS_SOY_DEFORESTATION_5_YEAR_ANNUAL_RISK_TCO2, 
                  -SOY_DEFORESTATION_5_YEAR_ANNUAL_RISK_HA, -ZERO_DEFORESTATION_BRAZIL_SOY,
                  -TERRITORIAL_DEFORESTATION_RISK_HA, -FOB_USD, -LOGISTICS.HUB, -BIOME, -STATE,
                  -MUNICIPALITY, -Sigla, -TYPE, -ECONOMIC.BLOC, -COUNTRY.OF.PRODUCTION, -PORT,
                  -EXPORTER, -EXPORTER.GROUP, -IMPORTER) %>%
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
      dplyr::select(SOY_EQUIVALENT_TONNES, code_muni)
    
    simu <- dplyr::inner_join(muniToSimU, csv_muni, "code_muni") %>%
      dplyr::mutate(value = area * SOY_EQUIVALENT_TONNES) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::mutate(country = !!country)
    
    result <- rbind(result, simu)
    
    testthat::expect_equal(sum(simu$value), sum(csv_country$SOY_EQUIVALENT_TONNES))
  }
  
  testthat::expect_equal(sum(result$value), sum(csv_year$SOY_EQUIVALENT_TONNES))
  
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
  colnames(res) <- paste("Soybeans imported by country for", year, "in kton by SimU ID\n\\")
  write.table(res, paste0("result/soy-country-", year, ".gms"), row.names = FALSE, quote = FALSE)
  
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
      dplyr::select(SOY_EQUIVALENT_TONNES, code_muni)
    
    simu <- dplyr::inner_join(muniToSimU, csv_muni, "code_muni") %>%
      dplyr::mutate(value = area * SOY_EQUIVALENT_TONNES) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::mutate(importer = !!importer)
    
    result <- rbind(result, simu)
    
    testthat::expect_equal(sum(simu$value), sum(csv_importer$SOY_EQUIVALENT_TONNES))
  }
  
  testthat::expect_equal(sum(result$value), sum(csv_year$SOY_EQUIVALENT_TONNES))
  
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
  colnames(res) <- paste("Soybeans imported by country for", year, "in kton by SimU ID\n\\")
  write.table(res, paste0("result/soy-importer-", year, ".gms"), row.names = FALSE, quote = FALSE)
}
