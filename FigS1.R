library(tmap)
library(sf)
library(RColorBrewer)
require(colrow)

dataDir <- "c:/Users/pedro/Dropbox/colrow"
country <- "Brazil"
myCR <- colrow::getCR(country, dataDir)
sf::write_sf(myCR, paste0(country, "CR.gpkg"))

csvfile <- "figS1/output_fig_s1_SSP1.csv"
attributes <-colrow::attrs(Country,ID,CLASSE,VALUE)
result <- colrow::processFile(
  "BrazilCR.gpkg",
  csvfile,
  attributes,
  "data/output_fig_s1_SSP1.gpkg")

csvfile <- "figS1/output_fig_s1_SSP2.csv"
attributes <-colrow::attrs(Country,ID,CLASSE,VALUE)
result <- colrow::processFile(
  "BrazilCR.gpkg",
  csvfile,
  attributes,
  "data/output_fig_s1_SSP2.gpkg")

csvfile <- "figS1/output_fig_s1_SSP3.csv"
attributes <-colrow::attrs(Country,ID,CLASSE,VALUE)
result <- colrow::processFile(
  "BrazilCR.gpkg",
  csvfile,
  attributes,
  "data/output_fig_s1_SSP3.gpkg")

cuts <- c(0,0.0001, 5.92,16.76,37.33,71.11,130.38,211.54,260,309)

colors <- c(
  "#ffffff",
  "#fee8e6",
  "#fee0d2",
  "#fcbba1",
  "#fc9272",
  "#fb6a4a",
  "#ef3b2c",
  "#cb181d",
  "#67000d"
)

labels <- c(
  "0",
  "0.00–5.92",
  "5.92–16.76",
  "16.76–37.33",
  "37.33–71.11",
  "71.11–130.38",
  "130.38–211.54",
  "211.54–260.00",
  "260.00–309.00"
)

biomes <- sf::st_read("data/br_biomes.shp")

matopiba <- sf::st_read(
  "data/matopiba.shp",
  quiet = TRUE
)

make_map <- function(data, var, panel){
  tm_shape(data) +
    tm_fill(
      fill = var,
      fill.scale = tm_scale_intervals(
        breaks = cuts,
        values = colors,  
        labels = labels 
      ),
      fill.legend = tm_legend(
        title = "",
        position = c("left", "bottom"),
        text.size = 0.55,
        item.height = 0.7,
        item.width = 0.7,
        item.space = 0.3,
        margins = c(0.1, 0.1, 0.1, 0.1)
      )
    ) +
    tm_shape(biomes) +
    tm_borders(
      lwd = 0.2,
      col = "black"
    ) +
    tm_shape(matopiba) +
    tm_borders(
      lwd = 0.8,
      col = "blue"
    ) + 
    tm_credits(
      text = panel,
      position = c(0.0, 0.92),
      size = 0.9
    ) 
}

figs1_ssp1<- st_read("data/output_fig_s1_SSP1.gpkg")
figs1_ssp2<- st_read("data/output_fig_s1_SSP2.gpkg")
figs1_ssp3<- st_read("data/output_fig_s1_SSP3.gpkg")

FigS1a <- make_map(figs1_ssp1, "DBeef","a)")
FigS1b <- make_map(figs1_ssp1, "DSoy","b)")
FigS1c <- make_map(figs1_ssp1, "DOther","c)")
FigS1d <- make_map(figs1_ssp2, "DBeef","d)")
FigS1e <- make_map(figs1_ssp2, "DSoy","e)")
FigS1f <- make_map(figs1_ssp2, "DOther","f)")
FigS1g <- make_map(figs1_ssp3, "DBeef","g)")
FigS1h <- make_map(figs1_ssp3, "DSoy","h)")
FigS1i <- make_map(figs1_ssp3, "DOther","i)")

FigS1 <- tmap_arrange(
  FigS1a, FigS1b, FigS1c,
  FigS1d, FigS1e, FigS1f,
  FigS1g, FigS1h, FigS1i,
  ncol = 3,
  nrow = 3
)

tmap_save(
  FigS1,
  "FigS1.pdf",
  width = 8,
  height = 8,
)
