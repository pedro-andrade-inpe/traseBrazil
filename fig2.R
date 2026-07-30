library(ggplot2)
library(dplyr)
library(tidyr)

biomes <- sf::read_sf("data/br_biomes.dbf")
matopiba <- sf::read_sf("data/matopiba.shp")

dados <- data.frame(
  Bioma = c("Amazon", "Atlantic Forest", "Caatinga", "Cerrado", "Pampa", "Pantanal"),
  Forest = c(92, 88, 14, 30, 32, 25),
  OWL = c(1, 6, 79, 52, 0, 18),
  Other = c(7, 6, 7, 18, 68, 57),
  check.names = FALSE
)

dados_long <- dados %>%
  pivot_longer(
    cols = c("Forest", "OWL", "Other"),
    names_to = "Categoria",
    values_to = "Porcentagem"
  ) %>%
  mutate(
    Bioma = factor(
      Bioma,
      levels = rev(c(
        "Amazon",
        "Atlantic Forest",
        "Caatinga",
        "Cerrado",
        "Pampa",
        "Pantanal"
      ))
    ),
    Categoria = factor(
      Categoria,
      levels = c("Other", "OWL", "Forest")
    )
  )

cores <- c(
  Forest = "#488B39",
  OWL = "#ED6E2B",
  Other = "#EBEBEB"
)

plot_biomes <-
  ggplot(
    dados_long,
    aes(
      x = Bioma,
      y = Porcentagem,
      fill = Categoria
    )
  ) +
  geom_col(width = 0.7) +
  geom_text(
    data = subset(dados_long, Porcentagem > 0),
    aes(
      label = paste0(Porcentagem, "%"),
      color = ifelse(Categoria == "Other", "black", "white")
    ),
    position = position_stack(vjust = 0.5),
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_color_identity() +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = cores,
    breaks = c("Forest", "OWL", "Other")
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = paste0(seq(0, 100, 25), "%"),
    expand = c(0, 0)
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    
    axis.text.y = element_text(
      size = 12,
      colour = "black",
      face = "bold"
    ),
    axis.text.x = element_text(
      size = 11,
      colour = "black"
    ),
    
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(t = 15)
  )

############## LOWER

dados <- data.frame(
  Region = rep(c("EU", "China", "EU + China"), each = 6),
  Commodity = rep(c("Soy", "Beef"), times = 9),
  Policy = factor(
    rep(rep(c("EUDR", "EUDR-OWL", "ZD"), each = 2), times = 3),
    levels = c("EUDR", "EUDR-OWL", "ZD")
  ),
  Value = c(
    0.05, 0.05, 0.34, 0.09, 0.59, 0.18,
    0.22, 1.03, 1.72, 2.05, 2.52, 3.45,
    0.27, 1.08, 2.06, 2.14, 3.10, 3.63
  ),
  ymin = c(
    0.01, 0.04, 0.22, 0.07, 0.34, 0.12,
    0.04, 0.57, 1.09, 0.91, 1.50, 1.44,
    0.05, 0.63, 1.30, 1.00, 1.85, 1.61
  ),
  ymax = c(
    0.08, 0.10, 0.62, 0.16, 1.01, 0.33,
    0.51, 1.52, 2.04, 2.83, 2.97, 4.36,
    0.56, 1.58, 2.40, 2.92, 3.59, 4.55
  )
)

dados$Region <- factor(dados$Region,
                       levels = c("EU","China","EU + China"))

cores_commodity <- c(
  Soy="#0B3C85",
  Beef="#B3001B"
)

make_plot <- function(regiao){
  
  ggplot(
    subset(dados, Region == regiao),
    aes(y = Policy, x = Value, fill = Commodity)
  ) +
    geom_col(
      position = position_dodge(width = 0.7),
      width = 0.6
    ) +
    geom_linerange(
      aes(
        xmin = ymin,
        xmax = ymax,
        group = Commodity
      ),
      position = position_dodge(width = 0.7),
      linewidth = 0.4,
      color = "black"
    ) +
    geom_text(
      aes(
        x = ymax,
        label = sprintf("%.2f", Value),
        group = Commodity
      ),
      position = position_dodge(width = 0.7),
      hjust = -0.3,
      size = 3.5
    ) +
    scale_fill_manual(values = cores_commodity) +
    scale_x_continuous(
      limits = c(0, 6),
      breaks = 0:6,
      expand = c(0, 0)
    ) +
    labs(
      title = regiao,
      x = "Million hectares (Mha)",
      y = NULL
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 11
      ),
      axis.line.y = element_line(color = "black"),
      axis.line.x = element_line(color = "black"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.size = unit(0.4, "cm"),
      legend.margin = margin(t = 10)
    )
}
plot_eu <- make_plot("EU")
plot_china <- make_plot("China")
plot_eu_china <- make_plot("EU + China")

########################################################

dataDir <- "c:/Users/pedro/Dropbox/colrow"
cr <- colrow::getCR("Brazil", dataDir)
sf::write_sf(cr, "BrazilCR.gpkg")

dados <- colrow::processFile(
  "BrazilCR.gpkg",
  "data/output_fig2.csv",
  colrow::attrs(COUNTRY, ID, USE, VALUE)
)

cuts <- c(0,0.0001, 5.92,16.76,37.33,71.11,130.38,211.54,260,309)

cores <- c(
  "#ffffff",
  "#edf8e9",
  "#d9f0c7",
  "#bae4a0",
  "#8dd36f",
  "#5fbf58",
  "#37a148",
  "#1d8a38",
  "#006d2c"
)

labels <- c(
  "0",
  "< 5.92",
  "5.92–16.76",
  "16.76–37.33",
  "37.33–71.11",
  "71.11–130.38",
  "130.38–211.54",
  "211.54–260",
  "260–309"
)

map_forest <-
  tm_shape(dados) + 
  tm_fill(
    fill = "Forest",
    fill.scale = tm_scale_intervals(
      breaks = cuts,
      values = cores,
      labels = labels,
      value.na = "white"
    ),
    fill.legend = tm_legend(
      title = "",
      position = tm_pos_in("left", "bottom")
    )
  ) +
  tm_shape(biomes) +
  tm_borders(
    col = "grey40",
    lwd = 0.6,
    col.legend = tm_legend("Biomes")
  ) +
  tm_shape(matopiba) +
  tm_borders(
    col = "#3B5BDB",
    lwd = 1,
    col.legend = tm_legend("MATOPIBA")
  ) +
  tm_layout(
    legend.frame = TRUE,
    legend.bg.color = "white",
    inner.margins = c(0, 0, 0, 0),
    outer.margins = 0,
    frame = FALSE,
    legend.text.size = 0.4
  ) +
  tm_add_legend(
    type = "polygons",
    labels = c("Biomes", "Matopiba"),
    fill = c("white", "white"),
    col = c("grey40", "#3B5BDB"),
    lwd = c(0.6, 1),
    position = tm_pos_in("left", "bottom")
  )

map_forest

cores <- c(
  "#ffffff",
  "#fef2e6",
  "#fde0c5",
  "#fdc997",
  "#fdae61",
  "#fd8d3c",
  "#f16913",
  "#d95f02",
  "#b54a00"
)

map_owl <-
  tm_shape(dados) + 
  tm_fill(
    fill = "OWL",
    fill.scale = tm_scale_intervals(
      breaks = cuts,
      values = cores,
      labels = labels,
      value.na = "white"
    ),
    fill.legend = tm_legend(
      title = "",
      position = tm_pos_in("left", "bottom")
    )
  ) +
  tm_shape(biomes) +
  tm_borders(
    col = "grey40",
    lwd = 0.6,
    col.legend = tm_legend("Biomes")
  ) +
  tm_shape(matopiba) +
  tm_borders(
    col = "#3B5BDB",
    lwd = 1,
    col.legend = tm_legend("MATOPIBA")
  ) +
  tm_layout(
    legend.frame = TRUE,
    legend.bg.color = "white",
    inner.margins = c(0, 0, 0, 0),
    outer.margins = 0,
    frame = FALSE,
    legend.text.size = 0.4
  ) +
  tm_add_legend(
    type = "polygons",
    labels = c("Biomes", "Matopiba"),
    fill = c("white", "white"),
    col = c("grey40", "#3B5BDB"),
    lwd = c(0.6, 1),
    position = tm_pos_in("left", "bottom")
  )

tmap_mode("plot")

g_map_forest <- as.ggplot(map_forest, scale = 1.02)#, vjust = -0.08)
g_map_owl  <- as.ggplot(map_owl, scale = 1.02)#, vjust = -0.08)

top <-
  g_map_forest +
  g_map_owl +
  plot_biomes +
  plot_layout(widths = c(1.4, 1.4, 0.8))

bottom <-
  plot_eu +
  plot_china +
  plot_eu_china +
  plot_layout(widths = c(1,1,1))

final_plot <-
  top /
  bottom

final_plot <-
  (top / bottom) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) &
  theme(
    plot.tag = element_text(face = "bold", size = 15),
    plot.tag.position = c(0.01, 0.99)
  )

pdf("fig2.pdf", width = 12, height = 8)
final_plot
dev.off()
