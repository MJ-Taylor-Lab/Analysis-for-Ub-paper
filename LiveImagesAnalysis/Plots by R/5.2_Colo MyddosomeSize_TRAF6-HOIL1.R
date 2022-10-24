library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(parallel)
library(scales)

# Import table
ColocalizedIntensity <- fread("~")
MeanColocalizedIntensity <- fread("~")

setwd("~")

ColocalizedIntensity <-
  ColocalizedIntensity %>% 
  filter(PROTEIN == "MyD88") %>%
  mutate(GROUP = factor(GROUP, levels = c("MyD88 TRAF6", "MyD88 HOIL1")))

MeanColocalizedIntensity<-
  MeanColocalizedIntensity %>%
  mutate(GROUP = factor(GROUP, levels = c("MyD88 TRAF6", "MyD88 HOIL1")))

limits <- function(x) {x*100}

ggplot() +
  geom_histogram(
    data = ColocalizedIntensity %>% filter(GROUP == "MyD88 TRAF6"),
    binwidth = 0.25,
    aes(
      x = NORMALIZED_INTENSITY/4.5,
      y = ..count..
    ),
    color = "#b35806",
    fill = "#b35806",
    alpha = 0.7,
  ) +
  geom_density(
    data = ColocalizedIntensity %>% filter(GROUP == "MyD88 TRAF6"),
    aes(
      x = NORMALIZED_INTENSITY/4.5,
      y = ..count..*0.25
    )
  ) +
  geom_histogram(
    data = ColocalizedIntensity %>% filter(GROUP == "MyD88 HOIL1"),
    binwidth = 0.25,
    aes(
      x = NORMALIZED_INTENSITY/4.5,
      y = ..count..
    ),
    color = "#8073ac",
    fill = "#8073ac",
    alpha = 0.7,
  ) +
  geom_density(
    data = ColocalizedIntensity %>% filter(GROUP == "MyD88 HOIL1"),
    aes(
      x = NORMALIZED_INTENSITY/4.5,
      y = ..count..*0.25
    )
  ) +
  geom_vline(
    data = MeanColocalizedIntensity,
    aes(
      xintercept = NORMALIZED_INTENSITY/4.5
    ),
    color = "black",
    linetype = "dashed"
  ) +
  scale_x_continuous(limits = c(-0.13, 9), breaks = seq(0,12,3)) +
  facet_wrap(~GROUP, nrow = 2, strip.position = "right") +
  labs(
    x = "Number of Myddosomes at Landing",
    y = "Counts",
    color = "Group",
    fill = "Group"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank(),
    strip.background = element_blank()
  ) +
  ggsave(
    "ColoInt Counts_Myddosome.pdf",
    width = 5,
    height = 2.7
  )
