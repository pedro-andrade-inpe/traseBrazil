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
    31.4867, 10.1552, 6.05779, 4.2287708, 1.3134046, 0.4153608, 0.0972146
  ),
  Min = c(
    26.9916, 6.39925, 3.95068, 3.3724075, 1.0834031, 0.3068748, 0.06472289
  ),
  Max = c(
    31.4867, 12.9722, 7.42134, 4.494013, 1.54813549, 0.790794, 0.1601891
  ),
  Grupo = c(
    "Domestic policy", "Private sector", "Private sector",
    "EU + China markets", "EU + China markets",
    "EU market", "EU market"
  )
)

cores <- c(
  "Domestic policy"="#1B7F2A",
  "Private sector"="#A02C9A",
  "EU + China markets"="#F0702A",
  "EU market"="#176A96"
)

right_panel <- ggplot(dados,
       aes(y=Policy,
           x=Valor,
           fill=Grupo)) +
  geom_col(width=.58) +
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
  scale_fill_manual(values=cores) +
  scale_x_continuous(
    limits=c(0,35),
    expand=c(0,0)
  ) +
  labs(
    x="Avoided soy- and beef-driven deforestation (Mha)",
    y=NULL
  ) +
  theme_classic(base_size=13) +
  theme(
    legend.position="none",
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_text(size=13),
    axis.text.x=element_text(size=13)
  ) +scale_x_continuous(
    limits = c(0, 35),
    expand = c(0,0)
  ) +
  coord_cartesian(clip = "off")


library(ggplot2)

brace_data <- data.frame(
  Grupo = c(
    "Domestic\npolicy",
    "Private\nsector\n(traders)",
    "EU + China\nmarkets",
    "EU\nmarket"
  ),
  y = c(7, 5.5, 3.5, 1.5),
  cor = c(
    "#1B7F2A",
    "#A02C9A",
    "#F0702A",
    "#176A96"
  )
)

x0 <- 0.92
x1 <- 1.00

left_panel <-
  ggplot() +
  
  # Domestic policy
  annotate("segment", x=x0, xend=x0, y=6.6, yend=7.4,
           colour="#1B7F2A", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=7.4, yend=7.4,
           colour="#1B7F2A", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=6.6, yend=6.6,
           colour="#1B7F2A", linewidth=1) +
  
  # Private sector
  annotate("segment", x=x0, xend=x0, y=4.7, yend=6.4,
           colour="#A02C9A", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=6.4, yend=6.4,
           colour="#A02C9A", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=4.7, yend=4.7,
           colour="#A02C9A", linewidth=1) +
  
  # EU + China
  annotate("segment", x=x0, xend=x0, y=2.7, yend=4.4,
           colour="#F0702A", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=4.4, yend=4.4,
           colour="#F0702A", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=2.7, yend=2.7,
           colour="#F0702A", linewidth=1) +
  
  # EU market
  annotate("segment", x=x0, xend=x0, y=0.5, yend=2.4,
           colour="#176A96", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=2.4, yend=2.4,
           colour="#176A96", linewidth=1) +
  annotate("segment", x=x0, xend=x1, y=0.5, yend=0.5,
           colour="#176A96", linewidth=1) +
  geom_text(
    data = brace_data,
    aes(x=.65, y=y, label=Grupo, colour=cor),
    hjust=1,
    size=5
  ) +
  scale_colour_identity() +
  scale_x_continuous(limits=c(0,1.1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0.5,7.5), expand=c(0,0)) +
  coord_cartesian(clip="off") +
  theme_void()


pdf("fig3.pdf", width = 12, height = 4)
(left_panel | right_panel) +
  plot_layout(widths = c(0.18, 0.82))
dev.off()
