library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(parallel)
library(scales)

# Import table
ColocalizedIntensityHOIL1 <- fread("~")
MeanColocalizedIntensityHOIL1 <- fread("~")

setwd("~")

ColocalizedIntensityHOIL1 <-
  ColocalizedIntensityHOIL1 %>% 
  filter(PROTEIN == "MyD88", LIGAND_DENSITY_CAT == 10) %>%
  mutate(GROUP = factor(GROUP, levels = c("MyD88 HOIL1Goff", "MyD88 HOIL1G1on")))

MeanColocalizedIntensityHOIL1<-
  MeanColocalizedIntensityHOIL1 %>%
  filter(LIGAND_DENSITY_CAT == 10) %>%
  mutate(GROUP = factor(GROUP, levels = c("MyD88 HOIL1Goff", "MyD88 HOIL1G1on")))

limits <- function(x) {x*100}

ggplot() +
  geom_histogram(
    data = ColocalizedIntensityHOIL1 %>% filter(GROUP == "MyD88 HOIL1Goff"),
    binwidth = 0.25,
    aes(
      x = NORMALIZED_INTENSITY/4.5,
      y = ..count..
    ),
    color = "#0571b0",
    fill = "#0571b0",
    alpha = 0.7,
  ) +
  geom_density(
    data = ColocalizedIntensityHOIL1 %>% filter(GROUP == "MyD88 HOIL1Goff"),
    aes(
      x = NORMALIZED_INTENSITY/4.5,
      y = ..count..*0.25
    )
  ) +
  geom_histogram(
    data = ColocalizedIntensityHOIL1 %>% filter(GROUP == "MyD88 HOIL1G1on"),
    binwidth = 0.25,
    aes(
      x = NORMALIZED_INTENSITY/4.5,
      y = ..count..
    ),
    color = "#ca0020",
    fill = "#ca0020",
    alpha = 0.7,
  ) +
  geom_density(
    data = ColocalizedIntensityHOIL1 %>% filter(GROUP == "MyD88 HOIL1G1on"),
    aes(
      x = NORMALIZED_INTENSITY/4.5,
      y = ..count..*0.25
    )
  ) +
  geom_vline(
    data = MeanColocalizedIntensityHOIL1,
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
    "ColoInt Counts_Myddosome_HOIL1.pdf",
    width = 4,
    height = 2.7
  )
