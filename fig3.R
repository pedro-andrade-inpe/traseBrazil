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

pdf("fig3.pdf", width = 12, height = 8)

ggplot(dados,
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
    width = .18,
    linewidth = .7,
    colour = "grey40"
  )+
  scale_fill_manual(values=cores) +
  scale_x_continuous(
    limits=c(0,35),
    expand=c(0,0)
  ) +
  labs(
    x="Avoided soy- and beef-driven deforestation (Mha)",
    y=NULL
  ) +
  theme_classic(base_size=14) +
  theme(
    legend.position="none",
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_text(size=16),
    axis.text.x=element_text(size=13)
  ) +scale_x_continuous(
    limits = c(-15, 35),
    expand = c(0,0)
  ) +
  coord_cartesian(clip = "off")

annotate("segment", x=-8.8, xend=-8.8, y=5.5, yend=7.5,
         colour="#A02C9A", linewidth=1)

annotate("segment", x=-8.8, xend=-7.9, y=7.5, yend=7.5,
         colour="#A02C9A", linewidth=1)

annotate("segment", x=-8.8, xend=-7.9, y=5.5, yend=5.5,
         colour="#A02C9A", linewidth=1)

dev.off()
