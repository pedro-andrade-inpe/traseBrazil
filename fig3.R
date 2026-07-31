require(ggplot2)

dados <- data.frame(
  Policy = factor(
    c(
      "Brazil's Forest Code", "EUDR-OWL (SCF+Big3)", "EUDR (SCF+Big3)",
      "EUDR-OWL (EU and China)", "EUDR (EU and China)", "EUDR-OWL", "EUDR"
    ),
    levels = rev(c(
      "Brazil's Forest Code", "EUDR-OWL (SCF+Big3)", "EUDR (SCF+Big3)",
      "EUDR-OWL (EU and China)", "EUDR (EU and China)", "EUDR-OWL", "EUDR"
    ))
  ),
  Valor = c(
    31.4867, 10.0796, 6.07147, 4.2033908, 1.3542305, 0.4341508, 0.0997255
  ),
  Min = c(
    26.9916, 6.43504, 3.95068, 3.2891616, 1.0894789, 0.3093587, 0.06287781
  ),
  Max = c(
    31.4867, 12.9378, 7.46152, 4.455484, 1.62528941, 0.756877, 0.1594589
  ),
  Grupo = c(
    "Domestic policy", "Private sector", "Private sector",
    "EU + China markets", "EU + China markets",
    "EU market", "EU market"
  )
)

cores <- c(
  "Domestic policy"   = "#B40426",
  "Private sector"    = "#D24D57",
  "EU + China markets"= "#ECA19A",
  "EU market"         = "#F8DEDB"
)

right_panel <- ggplot(
  dados,
  aes(
    y = Policy,
    x = Valor,
    fill = Grupo
  )
) +
  geom_col(width = .58) +
  
  geom_errorbar(
    aes(
      xmin = Min,
      xmax = Max
    ),
    orientation = "y",
    width = 0,
    linewidth = .7,
    colour = "grey40"
  ) +
  
  geom_text(
    aes(
      x = pmax(Valor, Max) + 0.15,
      label = sprintf("%.2f", Valor)
    ),
    hjust = 0,
    size = 3.6
  ) +
  
  scale_fill_manual(values = cores) +
  
  scale_x_continuous(
    limits = c(0, 36),
    breaks = seq(0, 36, by = 6),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Avoided soy- and beef-driven deforestation (Mha)",
    y = NULL
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    legend.position = "none",
    
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    
    panel.grid.major.x = element_line(
      colour = "grey88",
      linewidth = 0.6
    ),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )


library(ggplot2)

brace_data <- data.frame(
  Grupo = c(
    "Domestic\npolicy",
    "Private sector\n(traders)",
    "EU + China\nmarkets",
    "EU\nmarket"
  ),
  y = c(7, 5.5, 3.5, 1.5),
  cor = c(
    "#000000",
    "#000000",
    "#000000",
    "#000000"
  )
)

x0 <- 0.92
x1 <- 1.00

left_panel <-
  ggplot() +
  
  # Domestic policy
  annotate("segment", x=x0, xend=x0, y=6.6, yend=7.4,
           colour="#B40426", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=7.4, yend=7.4,
           colour="#B40426", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=6.6, yend=6.6,
           colour="#B40426", linewidth=1) +
  
  # Private sector
  annotate("segment", x=x0, xend=x0, y=4.7, yend=6.4,
           colour="#D24D57", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=6.4, yend=6.4,
           colour="#D24D57", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=4.7, yend=4.7,
           colour="#D24D57", linewidth=1) +
  
  # EU + China
  annotate("segment", x=x0, xend=x0, y=2.7, yend=4.4,
           colour="#ECA19A", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=4.4, yend=4.4,
           colour="#ECA19A", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=2.7, yend=2.7,
           colour="#ECA19A", linewidth=1) +
  
  # EU market
  annotate("segment", x=x0, xend=x0, y=0.5, yend=2.4,
           colour="#F8DEDB", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=2.4, yend=2.4,
           colour="#F8DEDB", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=0.5, yend=0.5,
           colour="#F8DEDB", linewidth=1) +
  geom_text(
    data = brace_data,
    aes(x=.65, y=y, label=Grupo, colour=cor),
    hjust=1,
    size=5
  ) +
   annotate(
    "text",
    x = 0.03,
    y = 7.5,
    label = "(g)",
    fontface = "bold",
    size = 5
  ) + 
  scale_colour_identity() +
  scale_x_continuous(limits=c(0,1.1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0.5,7.5), expand=c(0,0)) +
  coord_cartesian(clip="off") +
  theme_void()


biomes <- sf::read_sf("data/br_biomes.dbf")
matopiba <- sf::read_sf("data/matopiba.shp")

dataDir <- "c:/Users/pedro/Dropbox/colrow"
cr <- colrow::getCR("Brazil", dataDir)
sf::write_sf(cr, "data/BrazilCR.gpkg")

dados <- colrow::processFile(
  "data/BrazilCR.gpkg",
  "data/output_fig_3_SSP2_dynamic.csv",
  colrow::attrs(COUNTRY, ID, USE, SCENARIO, TRADER, VALUE)
)

cuts <- c(-210, -71.11, -37.33, -16.76, -1e-10, 1e-10, 16.76, 37.33, 71.11, 210)

cores <- c(
  "#2166AC",
  "#67A9CF",
  "#B2D8E8",
  "#DCEEF7",
  "#FFFFFF",
  "#FDD0C4",
  "#FC9272",
  "#EF3B2C",
  "#99000D"
)

labels <- c(
  "-210–-71.11",
  "-71.11–-37.33",
  "-37.33–-16.76",
  "-16.76–<0",
  "0",
  ">0–16.76",
  "16.76–37.33",
  "37.33–71.11",
  "71.11–210"
)

require(tmap)

map_aa <- function(dados, mfill) {
  tm_shape(dados) +
    tm_fill(
      fill = mfill,
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
      lwd = 0.6
    ) +
    
    tm_shape(matopiba) +
    tm_borders(
      col = "#3B5BDB",
      lwd = 1
    ) +
    
    tm_layout(
      frame = FALSE,
      inner.margins = c(0,0,0,0),
      outer.margins = c(0,0,0,0)
    ) 
}


map_x <- function(dados, mfill) {
  tm_shape(dados) +
    tm_fill(
      fill = mfill,
      fill.scale = tm_scale_intervals(
        breaks = cuts,
        values = cores,
        labels = labels,
        value.na = "white"
      ),
      fill.legend = tm_legend_hide()
    ) +
    
    tm_shape(biomes) +
    tm_borders(
      col = "grey40",
      lwd = 0.6,
      col.legend = tm_legend_hide()
    ) +
    
    tm_shape(matopiba) +
    tm_borders(
      col = "#3B5BDB",
      lwd = 1,
      col.legend = tm_legend_hide()
    ) +
    
    tm_layout(
      frame = FALSE,
      inner.margins = c(0,0,0,0),
      outer.margins = c(0,0,0,0),
      legend.show = FALSE
    )
}

map_a <- map_aa(dados, "EUEUDR")
map_b <- map_x(dados, "EU+ChinaEUDR")
map_c <- map_x(dados, "SCF+BIG3EUDR")
map_d <- map_x(dados, "EUEUDR_OWL")
map_e <- map_x(dados, "EU+ChinaEUDR_OWL")
map_f <- map_x(dados, "SCF+BIG3EUDR_OWL")

map_a <- map_a + tm_credits("(a)", position = c("left", "top"), fontface = "bold", size = 1.2)
map_b <- map_b + tm_credits("(b)", position = c("left", "top"), fontface = "bold", size = 1.2)
map_c <- map_c + tm_credits("(c)", position = c("left", "top"), fontface = "bold", size = 1.2)
map_d <- map_d + tm_credits("(d)", position = c("left", "top"), fontface = "bold", size = 1.2)
map_e <- map_e + tm_credits("(e)", position = c("left", "top"), fontface = "bold", size = 1.2)
map_f <- map_f + tm_credits("(f)", position = c("left", "top"), fontface = "bold", size = 1.2)

library(ggplotify)

gmap_a <- as.ggplot(map_a)
gmap_b <- as.ggplot(map_b)
gmap_c <- as.ggplot(map_c)
gmap_d <- as.ggplot(map_d)
gmap_e <- as.ggplot(map_e)
gmap_f <- as.ggplot(map_f)

library(patchwork)

maps <-
  (gmap_a + gmap_b + gmap_c) /
  (gmap_d + gmap_e + gmap_f) 

pdf("maps3.pdf", width = 12, height = 8)
maps
dev.off()

bottom <- (left_panel | right_panel) +
  plot_layout(widths = c(0.20, 0.80))

pdf("chart3.pdf", width = 12, height = 3)
bottom
dev.off()

library(magick)

maps   <- image_read_pdf("maps3.pdf", density = 300)
bars   <- image_read_pdf("chart3.pdf", density = 300)

final <- image_append(c(maps, bars), stack = TRUE)

image_write(final, "figure3.pdf", format = "pdf")
