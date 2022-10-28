library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(parallel)
library(scales)
library(ggbeeswarm)
library(ggpubr)
library(ggdark)

#"pp65" could be replaced with "pIKK",K63","M1" accordingly.

setwd("~")

pp65_analysis_normalized <- fread("~/pp65_analysis_normalized.csv")

pp65_analysis_normalized <- pp65_analysis_normalized %>%
  mutate(GROUP = factor(GROUP, levels = c("off_grid", "2.5um_grid", "1um_grid")))

# off grid integrated facetgrid --> off_grid only
ggplot(
  data = pp65_analysis_normalized %>% filter(GROUP == "off_grid"),
  aes(
    x = Normalized_Integrated_MyD88,
    y = Normalized_Integrated_pp65
  )
) +
  scale_x_continuous(limits = c(-0.06, 4), breaks = seq(0,4,1)) +
  scale_y_continuous(limits = c(-0.06, 4)) +
  geom_hex(
    aes(
      fill = ..count..
    ),
    binwidth = c(0.1,0.1)
  ) +
  geom_vline(
    aes(xintercept = 0.5),
    color = "black",
    linetype = "dashed"
  ) +
  geom_smooth(
    method = "lm",
    size = 0.5
  ) +
  stat_cor(
    method = "pearson"
  )+
  scale_fill_distiller(
    palette = "Greys",
    trans = "log",
    labels = number_format(
      accuracy = 1,
      big.mark = " "
    )
  ) +
  labs(
    x = "MyD88 Normalized Integrated Intensity (a.u.)",
    y = "pp65 Normalized Integrated Intensity (a.u.)",
    fill = "Particle Count\n(log scale)"
  ) +
  theme_classic() +
  theme(
    #legend.position = "none"
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank(),
    strip.background = element_blank()
  ) +
  ggsave(
    "pp65_Integrated_off grid.pdf",
    width = 5,
    height = 3.5
  )

# on grid integrated facetgrid --> on_grid only
ggplot(
  data = pp65_analysis_normalized %>% filter(GROUP != "off_grid"),
  aes(
    x = Normalized_Integrated_MyD88,
    y = Normalized_Integrated_pp65
  )
) +
  scale_x_continuous(limits = c(-0.06, 4), breaks = seq(0,4,1)) +
  scale_y_continuous(limits = c(-0.06, 4)) +
  geom_hex(
    aes(
      fill = ..count..
    ),
    binwidth = c(0.1,0.1)
  ) +
  geom_vline(
    aes(xintercept = 0.5),
    color = "black",
    linetype = "dashed"
  ) +
  geom_smooth(
    method = "lm",
    size = 0.5
  ) +
  stat_cor(
    method = "pearson"
  )+
  scale_fill_distiller(
    palette = "Greys",
    trans = "log",
    labels = number_format(
      accuracy = 1,
      big.mark = " "
    )
  ) +
  facet_wrap(
    ~GROUP, nrow = 1
  ) +
  labs(
    x = "MyD88 Normalized Integrated Intensity (a.u.)",
    y = "pp65 Normalized Integrated Intensity (a.u.)",
    fill = "Particle Count\n(log scale)"
  ) +
  theme_classic() +
  theme(
    #legend.position = "none"
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank(),
    strip.background = element_blank()
  ) +
  ggsave(
    "pp65_Integrated_on grid.pdf",
    width = 7,
    height = 3
  )
