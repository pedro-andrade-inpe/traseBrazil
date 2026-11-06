library(sf)
library(tmap)

biomes <- sf::st_read("data/br_biomes.shp")

matopiba <- sf::st_read(
  "data/matopiba.shp",
  quiet = TRUE
)

cuts <- c(0.0, 0.1, 8.0)

map_info <- data.frame(
  variable = c(
    "BrazilSoyTH_0001",
    "SCFSoyTH_0001",
    "SCFSoyTH_75",
    "BrazilBeefTH_0001",
    "BIG3BeefTH_0001",
    "BIG3BeefTH_75"  
  ),
  color = c(
    "#ad7ed4",
    "#fda648",
    "#089948",
    "#ad7ed4",
    "#fda648",
    "#089948"
  ),
  title = c(
    "(a) Soy; Domestic market",
    "(b) Soy; SCF",
    "(c) Soy; >= 75% threshold for SCF",
    "(d) Beef; Domestic consumption",
    "(e) Beef; Big3",
    "(f) Beef; >= 75% threshold for Big3"
  ),
  stringsAsFactors = FALSE
)

make_map <- function(data, variable, color, suffix, title){
    tm_shape(data) +
    tm_fill(
      fill = variable,
      fill.scale = tm_scale_intervals(
        breaks = cuts,
        values = c("#fff1d3", color)
      ),
      fill.legend = tm_legend(show = FALSE)
    ) +
    tm_shape(biomes) +
    tm_borders(
      col = "black",
      lwd = 0.6
    ) +
    tm_shape(matopiba) +
    tm_borders(
      lwd = 0.8,
      col = "blue"
    )+ 
    tm_layout(
      frame = FALSE,
      legend.show = FALSE,
      meta.margins = c(0, 0, 0.12, 0)
    ) +
    tm_title_out(
      text = title,
      size = 1.4
    )
}

datasets <- list(
  "5y" = st_read("figS11/data_trase_marketshare_visualization_5y_with_pa.gpkg")
)

maps <- list()

for(suffix in names(datasets)){
  cat("Processing", suffix, "\n")
  
  data <- datasets[[suffix]]
  
  for(i in seq_len(nrow(map_info))){
    
    cat("   ", map_info$variable[i], "\n")
    
    maps[[i]] <- make_map(
      data     = data,
      variable = map_info$variable[i],
      color    = map_info$color[i],
      suffix   = suffix,
      title    = map_info$title[i]
    )
  }
}

tmap_mode("plot")

FigS11 <- tmap_arrange(
  maps,
  ncol = 3,
  nrow = 2
)

tmap_save(
  FigS11,
  "FigS11.pdf",
  width = 11,
  height = 7.5,
  units = "in"
)
