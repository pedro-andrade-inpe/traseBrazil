library(ggplot2)
library(dplyr)
library(tidyr)

dados <- data.frame(
  Bioma = c("Amazonia", "Atlantic Forest", "Caatinga", "Cerrado", "Pampa", "Pantanal"),
  Forest = c(92, 88, 14, 30, 32, 25),
  `OWL (other wooded land)` = c(1, 6, 79, 52, 0, 18),
  Other = c(7, 6, 7, 18, 68, 57),
  check.names = FALSE
)

dados_long <- dados %>%
  pivot_longer(
    cols = c("Forest", "OWL (other wooded land)", "Other"),
    names_to = "Categoria",
    values_to = "Porcentagem"
  ) %>%
  mutate(
    Bioma = factor(Bioma, levels = rev(c("Amazonia", "Atlantic Forest", "Caatinga", "Cerrado", "Pampa", "Pantanal"))),
    Categoria = factor(Categoria, levels = c("Other", "OWL (other wooded land)", "Forest"))
  )

cores <- c(
  "Forest" = "#488B39",
  "OWL (other wooded land)" = "#ED6E2B",
  "Other" = "#EBEBEB"
)

pos_empilhada <- position_stack(vjust = 0.5)

plot_biomes <- ggplot(dados_long, aes(x = Bioma, y = Porcentagem, fill = Categoria)) +
  geom_col(position = position_stack(), width = 0.7) +
  geom_text(
    data = subset(dados_long, Porcentagem > 0),
    aes(
      label = paste0(Porcentagem, "%"),
      color = ifelse(Categoria == "Other", "black", "white")
    ),
    position = pos_empilhada, 
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_color_identity() +
  coord_flip(clip = "off") +
  scale_fill_manual(values = cores, breaks = c("Forest", "OWL (other wooded land)", "Other")) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    labels = c("0%", "25%", "50%", "75%", "100%"),
    expand = c(0, 0)
  ) +
  labs(
    title = "Distribution of native vegetation types\nin Brazilian biomes",
    x = NULL,
    y = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.text.x = element_text(size = 11, color = "black"),
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
  Policy = factor(rep(rep(c("EUDR", "EUDR-OWL", "ZD (maximum)"), each = 2), times = 3),
                  levels = c("EUDR", "EUDR-OWL", "ZD (maximum)")), # Ordem de baixo para cima
  Value = c(
    # EU
    0.05, 0.05, 0.34, 0.09, 0.59, 0.18,
    # China
    0.22, 1.03, 1.72, 2.05, 2.52, 3.45,
    # EU + China
    0.27, 1.08, 2.06, 2.14, 3.10, 3.63
  ),
  sd_val = c(
    0.15, 0.15, 0.19, 0.31, 0.14, 0.20,
    0.35, 0.98, 0.70, 1.15, 1.10, 1.65,
    0.35, 0.98, 0.82, 1.15, 1.40, 1.55
  )
) %>%
  mutate(
    ymax = Value + sd_val,
    ymin = Value
  )

dados$Region <- factor(dados$Region, levels = c("EU", "China", "EU + China"))

region_labels <- c(
  "EU" = "EU\n(ZD total = 0.77 Mha)",
  "China" = "China\n(ZD total = 5.97 Mha)",
  "EU + China" = "EU + China\n(ZD total = 6.73 Mha)"
)

cores_commodity <- c("Soy" = "#0B3C85", "Beef" = "#B3001B")

soy_beef <- ggplot(dados, aes(x = Policy, y = Value, fill = Commodity)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = Value, ymax = ymax, group = Commodity),
    position = position_dodge(width = 0.7),
    width = 0.2,
    linewidth = 0.4,
    color = "black"
  ) +
  geom_text(
    aes(y = ymax, label = sprintf("%.2f", Value), group = Commodity),
    position = position_dodge(width = 0.7),
    hjust = -0.3,
    size = 3.5
  ) +
  coord_flip() +
  facet_wrap(~Region, scales = "free_x", labeller = labeller(Region = region_labels)) +
  scale_fill_manual(values = cores_commodity) +
  scale_y_continuous(
    limits = c(0, 6),        # Limite até 6.5 para dar margem de sobra aos textos
    breaks = 0:6,              # Marcas no eixo de 0, 1, 2, 3, 4, 5, 6
    expand = c(0, 0)
  ) +
  labs(
    title = expression(bold("Net avoided deforestation–risk exposure")),
    x = NULL,
    y = "Million hectares (Mha)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "plain", hjust = 0),
    strip.background = element_blank(), # Remove caixa do título das facets
    strip.text = element_text(face = "bold", size = 11),
    axis.line.y = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    panel.spacing = unit(2, "lines"),
    legend.position = "bottom",          # Move a legenda para a parte inferior
    legend.direction = "horizontal",     # Deixa os itens lado a lado
    legend.title = element_blank(),      # Remove o título da legenda
    legend.key.size = unit(0.4, "cm"),
    legend.margin = margin(t = 10),      # Dá um espaço em relação ao eixo X
  )

soy_beef


########################################################

dataDir <- "c:/Users/pedro/Dropbox/colrow"
cr <- colrow::getCR("Brazil", dataDir)
sf::write_sf(cr, "BrazilCR.gpkg")

dados <- colrow::processFile(
  "BrazilCR.gpkg",
  "c:/Users/pedro/Downloads/output_fig2.csv",
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

pdf("fig2.pdf", width = 12, height = 8)
#png("fig1.png", width = 1200, height = 800)

g_map_forest <- as.ggplot(map_forest)
g_map_owl  <- as.ggplot(map_owl)

(g_map_forest | g_map_owl | plot_biomes) /
  (soy_beef) 

dev.off()


