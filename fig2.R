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

p <- ggplot(dados_long, aes(x = Bioma, y = Porcentagem, fill = Categoria)) +
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

print(p)


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

p <- ggplot(dados, aes(x = Policy, y = Value, fill = Commodity)) +
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
  
    # Títulos e eixos
  labs(
    title = expression(bold("Net avoided deforestation–risk exposure")),
    x = NULL,
    y = "Million hectares (Mha)"
  ) +
  
  # Estilização visual básica
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

print(p)