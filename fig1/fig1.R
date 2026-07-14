

library(ggplot2)
library(patchwork)

beef <- data.frame(
  Region=factor(c("Brazil","Amazon","Caatinga","Cerrado","Pantanal","Pampa"),
                levels=c("Brazil","Amazon","Caatinga","Cerrado","Pantanal","Pampa")),
  SSP2=c(56.82,24.83,18.36,7.00,5.10,1.53),
  min=c(47.89,23.42,13.72,7.00,1.68,0.50),
  max=c(68.69,27.45,26.42,10.83,5.10,2.30)
)

soy <- data.frame(
  Region=factor(c("Brazil","Amazon","Caatinga","Cerrado","Pantanal","Pampa"),
                levels=c("Brazil","Amazon","Caatinga","Cerrado","Pantanal","Pampa")),
  SSP2=c(5.66,0.00,0.00,5.54,0.06,0.07),
  min=c(3.36,0.00,0.00,3.29,0.00,0.06),
  max=c(5.66,0.00,0.00,5.54,0.06,0.07)
)

theme_fig <- theme_bw(base_size=13) +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(colour="black"),
        plot.title=element_text(face="bold", hjust=.5),
        axis.title.x=element_blank(),
        legend.position="none")


p_beef <- ggplot(beef, aes(Region, SSP2)) +
  geom_col(width=.55, fill="#b3001b") +
  geom_linerange(aes(ymin=min, ymax=max), linewidth=.9) +
  geom_errorbar(aes(ymin=min, ymax=max), width=.16, linewidth=.8) +
  scale_y_continuous(limits=c(0,80), expand=c(0,0)) +
  labs(title="Projected deforestation by biome\n(Mha, 2020–2050)", y="Mha") +
  theme_fig


p_soy <- ggplot(soy, aes(Region, SSP2)) +
  geom_col(width=.55, fill="#b3001b") +
  geom_linerange(aes(ymin=min, ymax=max), linewidth=.9) +
  geom_errorbar(aes(ymin=min, ymax=max), width=.16, linewidth=.8) +
  scale_y_continuous(limits=c(0,7), expand=c(0,0)) +
  labs(y="Mha") +
  theme_fig


p_beef / p_soy
