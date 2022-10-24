library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(stringr)
library(parallel)
library(plotrix)
library(ggdark)

setwd("~")

GrandColoSummaryTRAF6HOIL1_MyD88_FA0_10mol_edit_with2p5data <- fread("~")

GrandColoSummaryTRAF6HOIL1_MyD88_FA0_10mol_edit_with2p5data <-
  GrandColoSummaryTRAF6HOIL1_MyD88_FA0_10mol_edit_with2p5data %>%
  mutate(
    GROUP = factor(GROUP, levels = c("MyD88 Goff", "MyD88 G2p5on", "MyD88 G1on"))
  )

MeanBar <-
  GrandColoSummaryTRAF6HOIL1_MyD88_FA0_10mol_edit_with2p5data %>%
  group_by(
    GROUP,
    IMAGE,
    CELL
  ) %>%
  summarize(
    MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY, na.rm = T)
  ) %>%
  group_by(
    GROUP,
    IMAGE
  ) %>%
  summarize(
    MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY, na.rm = T)
  ) %>%
  group_by(
    GROUP
  ) %>%
  summarize(
    SE_MAX_NORMALIZED_INTENSITY = std.error(MAX_NORMALIZED_INTENSITY),
    MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY, na.rm = T)
  )

write.csv(MeanBar, "MeanBar.csv", row.names = F, )

MeanBarByImage <-
  GrandColoSummaryTRAF6HOIL1_MyD88_FA0_10mol_edit_with2p5data %>%
  group_by(
    GROUP,
    IMAGE,
    CELL
  ) %>%
  summarize(
    MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY, na.rm = T)
  ) %>%
  group_by(
    GROUP,
    IMAGE
  ) %>%
  summarize(
    MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY, na.rm = T)
  )

write.csv(MeanBarByImage, "MeanBarByImage.csv", row.names = F, )

ggplot()+
  geom_violin(
    data = GrandColoSummaryTRAF6HOIL1_MyD88_FA0_10mol_edit_with2p5data,
    aes(
      x = GROUP,
      y = MAX_NORMALIZED_INTENSITY,
      color = GROUP,
      fill = GROUP
    ),
    alpha = 0.4
  ) +
  geom_jitter(
    data = MeanBarByImage,
    aes(
      x = GROUP,
      y = MAX_NORMALIZED_INTENSITY,
      color = GROUP,
      fill = GROUP
    )
  ) +
  geom_crossbar(
    data = MeanBar,
    aes(
      x = GROUP,
      y = MAX_NORMALIZED_INTENSITY,
      ymin = MAX_NORMALIZED_INTENSITY,
      ymax = MAX_NORMALIZED_INTENSITY
    ),
    color = "black",
    width = 0.3,
    fatten = 1
  ) +
  geom_errorbar(
    data = MeanBar,
    aes(
      x = GROUP,
      y = MAX_NORMALIZED_INTENSITY,
      ymin = MAX_NORMALIZED_INTENSITY - SE_MAX_NORMALIZED_INTENSITY,
      ymax = MAX_NORMALIZED_INTENSITY + SE_MAX_NORMALIZED_INTENSITY
    ),
    color = "black",
    width = 0.2
  ) +
  scale_y_continuous(limits = c(0, 25)) +
  scale_color_brewer(
    palette = "Set1"
  ) +
  scale_fill_brewer(
    palette = "Set1"
  ) +
  labs(
    y = "MyD88 Max Normalized Intensity (a.u.)",
    x = "",
    color = "Group",
    fill = "Group"
  ) +
  #dark_theme_classic() +
  theme_classic() +
  theme(
    legend.position = "none"
  ) +
  ggsave(
    "MyD88 MaxNormalizedIntensity Violin.pdf",
    # width = 12,
    # height = 9
    width = 4.5,
    height = 3.8
  )
