DATA_DIR <- "C:\\Users\\pedro\\Dropbox\\pesquisa\\2020\\rplus\\trase\\"
getFile <- function(file) paste0(DATA_DIR, file)

require(dplyr)
require(progressr)

createRelations <- function(sf1, id1, sf2, id2){
  options(future.rng.onMisuse = "ignore")
  
  cores <- max(1, future::availableCores() - 1)
  message(paste('Using', cores, 'CPU cores'))
  
  oplan <- future::plan("multiprocess", workers = cores)
  on.exit(future::plan(oplan), add = TRUE)

  sf1 <- sf::st_transform(sf1, crs = 5880) %>%
    sf::st_buffer(dist = 0)
  
  sf2 <-  sf::st_transform(sf2, crs = 5880) %>%
    sf::st_buffer(dist = 0)

  sf::st_agr(sf1) = "constant"
  sf::st_agr(sf2) = "constant"
  
  total <- dim(sf2)[1]

  p <- progressr::progressor(steps = total)
  
  tryCorefun <- function(i){
    p()
    i1 <- sf::st_intersection(sf1, sf2[i, ])
    i1$area <- sf::st_area(i1)
    return(sf::st_drop_geometry(i1))
  }
  
  result <- furrr::future_map(.x = 1:total, .f = tryCorefun)  %>%
    dplyr::bind_rows() %>%
    dplyr::select(!!id1, !!id2, area)
  
  return(result)
}

normalizeArea <- function(relationsTable, by){
  relationsTable %>%
    group_by_at(by) %>%
    mutate(area = area / sum(area))
}


require(colrow)
dataDir <- "c:/Users/pedro/Dropbox/colrow/"

countries <- colrow::getCountries(dataDir)

country <- "Brazil"
mySimU <- colrow::getSimU(country, dataDir)

require(geobr)
munic_brazil <- read_municipality(year = 2017)

with_progress(result <- createRelations(mySimU, "ID",  munic_brazil, "code_muni"))
result

muniTosimu <- normalizeArea(result, "code_muni")

write.csv(muniTosimu, getFile("muni-to-simu.csv"), row.names = FALSE)
