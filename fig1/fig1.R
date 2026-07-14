

library(ggplot2)
library(patchwork)
library(dplyr)
library(colrow)
library(sf)
library(tmap)
library(scales)

beef <- data.frame(
  Region=factor(c("Brazil","Amazon","Caatinga","Cerrado","Pantanal"),
                levels=c("Brazil","Amazon","Caatinga","Cerrado","Pantanal")),
  SSP2=c(56.82,24.83,18.36,7.00,5.10),
  min=c(47.89,23.42,13.72,7.00,1.68),
  max=c(68.69,27.45,26.42,10.83,5.10)
)

soy <- data.frame(
  Region=factor(c("Brazil","Amazon","Caatinga","Cerrado","Pantanal"),
                levels=c("Brazil","Amazon","Caatinga","Cerrado","Pantanal")),
  SSP2=c(5.66,0.00,0.00,5.54,0.06),
  min=c(3.36,0.00,0.00,3.29,0.00),
  max=c(5.66,0.00,0.00,5.54,0.06)
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
  labs(title="Projected deforestation by biome (2020-2050)", y="Mha") +
  theme_fig

p_soy <- ggplot(soy, aes(Region, SSP2)) +
  geom_col(width=.55, fill="#b3001b") +
  geom_linerange(aes(ymin=min, ymax=max), linewidth=.9) +
  geom_errorbar(aes(ymin=min, ymax=max), width=.16, linewidth=.8) +
  scale_y_continuous(limits=c(0,7), expand=c(0,0)) +
  labs(y="Mha") +
  theme_fig

p_beef / p_soy


waffle_data <- function(values, labels){
  n <- round(values)
  # garante exatamente 100 quadrados
  n[length(n)] <- 100 - sum(n[-length(n)])
  
  data.frame(
    group = rep(labels, n),
    x = rep(1:10, each=10),
    y = rep(10:1, times=10)
  )
  
}

cores <- c(
  Brazil = "#b3001b",
  China  = "#FF8C00",
  EU     = "#FFD54F",
  RoW    = "#C7C7C7"
)

beef <- waffle_data(
  values = c(79,6,1,14),
  labels = c("Brazil","China","EU","RoW")
)

p_beef <- ggplot(beef, aes(x, y, fill=group)) +
  geom_tile(width=.82, height=.82, colour="white", linewidth=.8) +
  coord_equal() +
  scale_fill_manual(values=cores) +
  labs(title="Projected deforestation-risk exposure\nattributed to destination markets (%)") +
  theme_void() +
  theme(
    plot.title=element_text(face="bold", hjust=.5),
    legend.position="right"
  )

soy <- waffle_data(
  values = c(23,44,10,23),
  labels = c("Brazil","China","EU","RoW")
)

p_soy <- ggplot(soy, aes(x, y, fill=group)) +
  geom_tile(width=.82, height=.82, colour="white", linewidth=.8) +
  coord_equal() +
  scale_fill_manual(values=cores) +
  theme_void() +
  theme(
    legend.position="right"
  )

p_beef / p_soy


dataDir <- "c:/Users/pedro/Dropbox/colrow"

cr <- colrow::getCR("Brazil", dataDir)
sf::write_sf(cr, "BrazilCR.gpkg")

dados <- colrow::processFile(
  "BrazilCR.gpkg",
  "c:/Users/pedro/Downloads/fig1/output_fig_s1_SSP2.csv",
  colrow::attrs(COUNTRY, ID, DRIVER, VALUE)
)


theme_map <- theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    legend.position = c(.18, .15),
    legend.direction = "horizontal",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

fill_scale <- scale_fill_gradientn(
  colours = c(
    "#ffffff",
    "#fee5d9",
    "#fcbba1",
    "#fb6a4a",
    "#de2d26",
    "#a50f15"
  ),
  na.value = "white",
  name = "Deforestation (ha)",
  labels = label_number(big.mark = ",")
)

sf_use_s2(FALSE)

p_beef_map <-
  ggplot() +
  geom_sf(data = dados, aes(fill = DBeef), colour = NA) +
  geom_sf(data = st_union(dados), fill = NA, colour = "grey30", linewidth = .30) +
#  geom_sf(data = matopiba, fill = NA, colour = "#2E5EFF", linewidth = .55) +
  fill_scale +
  coord_sf(expand = FALSE) +
  labs(title = "Spatial distribution of\nprojected deforestation (2020-2050)") +
  theme_map

p_soy_map <-
  ggplot() +
  geom_sf(data = dados, aes(fill = DSoy), colour = NA) +
  geom_sf(data = st_union(dados), fill = NA, colour = "grey30", linewidth = .30) +
#  geom_sf(data = matopiba, fill = NA, colour = "#2E5EFF", linewidth = .55) +
  fill_scale +
  coord_sf(expand = FALSE) +
  theme_map

p_beef_map / p_soy_map

