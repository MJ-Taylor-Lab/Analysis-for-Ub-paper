library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(stringr)
library(parallel)

setwd("~")

#Import TRAF6 data table
GrandColoSummaryOffTraf6 <- fread("")
GrandColoSummary1umGridTraf6 <- fread("~")
GrandColoSummaryTRAF6_All <- bind_rows(GrandColoSummaryOffTraf6, GrandColoSummary1umGridTraf6)

#Import HOIL1 data table
GrandColoSummaryOffHoil1 <- fread("~")
GrandColoSummary1umHoil1 <- fread("~")
GrandColoSummaryHOIL1_All <- bind_rows(GrandColoSummaryOffHoil1, GrandColoSummary1umHoil1)

#Filter the table to keep MyD88 only and FA=0 only
GrandColoSummaryTRAF6_All_MyD88_FA0 <-
  GrandColoSummaryTRAF6_All %>%
  filter(FRAMES_ADJUSTED == "0", PROTEIN == "MyD88")

GrandColoSummaryHOIL1_All_MyD88_FA0 <-
  GrandColoSummaryHOIL1_All %>%
  filter(FRAMES_ADJUSTED == "0", PROTEIN == "MyD88")

write.csv(GrandColoSummaryTRAF6_All_MyD88_FA0, "GrandColoSummaryTRAF6_All_MyD88_FA0.csv", row.names = F, )
write.csv(GrandColoSummaryHOIL1_All_MyD88_FA0, "GrandColoSummaryHOIL1_All_MyD88_FA0.csv", row.names = F, )

GrandColoSummaryTRAF6_MyD88_FA0_1mol <- GrandColoSummaryTRAF6_All_MyD88_FA0 %>% 
  filter(LIGAND_DENSITY_CAT == 1) %>% mutate(GROUP = factor(GROUP, levels = c("MyD88 TRAF6Goff", "MyD88 TRAF6G1on")))
GrandColoSummaryTRAF6_MyD88_FA0_10mol <- GrandColoSummaryTRAF6_All_MyD88_FA0 %>% 
  filter(LIGAND_DENSITY_CAT == 10) %>% mutate(GROUP = factor(GROUP, levels = c("MyD88 TRAF6Goff", "MyD88 TRAF6G1on")))

GrandColoSummaryHOIL1_MyD88_FA0_10mol <- GrandColoSummaryHOIL1_All_MyD88_FA0 %>% 
  filter(LIGAND_DENSITY_CAT == 10) %>% mutate(GROUP = factor(GROUP, levels = c("MyD88 HOIL1Goff", "MyD88 HOIL1G1on")))
GrandColoSummaryHOIL1_MyD88_FA0_32mol <- GrandColoSummaryHOIL1_All_MyD88_FA0 %>% 
  filter(LIGAND_DENSITY_CAT == 32) %>% mutate(GROUP = factor(GROUP, levels = c("MyD88 HOIL1Goff", "MyD88 HOIL1G1on")))

MeanBar_10mol_TRAF6 <- GrandColoSummaryTRAF6_MyD88_FA0_10mol %>%
  group_by(GROUP,IMAGE,CELL) %>% summarize(MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY)) %>%
  group_by(GROUP,IMAGE) %>% summarize(MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY)) %>%
  group_by(GROUP) %>% summarize(MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY))

MeanBar_1mol_TRAF6 <- GrandColoSummaryTRAF6_MyD88_FA0_1mol %>%
  group_by(GROUP,IMAGE,CELL) %>% summarize(MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY)) %>%
  group_by(GROUP,IMAGE) %>% summarize(MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY)) %>%
  group_by(GROUP) %>% summarize(MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY))

write.csv(MeanBar_10mol_TRAF6, "MeanBar_10mol_TRAF6.csv", row.names = F, )
write.csv(MeanBar_1mol_TRAF6, "MeanBar_1mol_TRAF6.csv", row.names = F, )

MeanBar_32mol_HOIL1 <- GrandColoSummaryHOIL1_MyD88_FA0_32mol %>%
  group_by(GROUP,IMAGE,CELL) %>% summarize(MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY)) %>%
  group_by(GROUP,IMAGE) %>% summarize(MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY)) %>%
  group_by(GROUP) %>% summarize(MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY))

MeanBar_10mol_HOIL1 <- GrandColoSummaryHOIL1_MyD88_FA0_10mol %>%
  group_by(GROUP,IMAGE,CELL) %>% summarize(MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY)) %>%
  group_by(GROUP,IMAGE) %>% summarize(MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY)) %>%
  group_by(GROUP) %>% summarize(MAX_NORMALIZED_INTENSITY = mean(MAX_NORMALIZED_INTENSITY))

write.csv(MeanBar_32mol_HOIL1, "MeanBar_32mol_HOIL1.csv", row.names = F, )
write.csv(MeanBar_10mol_HOIL1, "MeanBar_10mol_HOIL1.csv", row.names = F, )

#32mol_HOIL1
ggplot() +
  geom_density(
    data = GrandColoSummaryHOIL1_MyD88_FA0_32mol,
    aes(
      x = MAX_NORMALIZED_INTENSITY,
      y = ..scaled..,
      color = GROUP
    )
  ) +
  geom_vline(
    data = MeanBar_32mol_HOIL1,
    aes(
      xintercept = MAX_NORMALIZED_INTENSITY,
      color = GROUP
    ),
    linetype = "dashed"
  ) +
  scale_x_continuous(limits = c(0, 30)) +
  scale_color_manual(values = c("#0571b0","#ca0020")) +
  labs(
    x = "MyD88 Max Normalized Intensity (a.u.)",
    y = "Scaled Density",
    color = "Group",
    fill = "Group"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.7, 0.7)
  ) +
  ggsave(
    "32mol MaxInt Density Grid_scaled_HOIL1.pdf",
    width = 4,
    height = 2.7
  )

#10mol_TRAF6
ggplot() +
  geom_density(
    data = GrandColoSummaryTRAF6_MyD88_FA0_10mol,
    aes(
      x = MAX_NORMALIZED_INTENSITY,
      y = ..scaled..,
      color = GROUP
    )
  ) +
  geom_vline(
    data = MeanBar_10mol_TRAF6,
    aes(
      xintercept = MAX_NORMALIZED_INTENSITY,
      color = GROUP
    ),
    linetype = "dashed"
  ) +
  scale_x_continuous(limits = c(0, 30)) +
  scale_color_manual(values = c("#0571b0","#ca0020")) +
  labs(
    x = "MyD88 Max Normalized Intensity (a.u.)",
    y = "Scaled Density",
    color = "Group",
    fill = "Group"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.7, 0.7)
  ) +
  ggsave(
    "10mol MaxInt Density Grid_scaled_TRAF6.pdf",
    width = 4,
    height = 2.7
  )

#10mol_HOIL1
ggplot() +
  geom_density(
    data = GrandColoSummaryHOIL1_MyD88_FA0_10mol,
    aes(
      x = MAX_NORMALIZED_INTENSITY,
      y = ..scaled..,
      color = GROUP
    )
  ) +
  geom_vline(
    data = MeanBar_10mol_HOIL1,
    aes(
      xintercept = MAX_NORMALIZED_INTENSITY,
      color = GROUP
    ),
    linetype = "dashed"
  ) +
  scale_x_continuous(limits = c(0, 30)) +
  scale_color_manual(values = c("#0571b0","#ca0020")) +
  labs(
    x = "MyD88 Max Normalized Intensity (a.u.)",
    y = "Scaled Density",
    color = "Group",
    fill = "Group"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.7, 0.7)
  ) +
  ggsave(
    "10mol MaxInt Density Grid_scaled_HOIL1.pdf",
    width = 4,
    height = 2.7
  )

#1mol_TRAF6
ggplot() +
  geom_density(
    data = GrandColoSummaryTRAF6_MyD88_FA0_1mol,
    aes(
      x = MAX_NORMALIZED_INTENSITY,
      y = ..scaled..,
      color = GROUP
    )
  ) +
  geom_vline(
    data = MeanBar_1mol_TRAF6,
    aes(
      xintercept = MAX_NORMALIZED_INTENSITY,
      color = GROUP
    ),
    linetype = "dashed"
  ) +
  scale_x_continuous(limits = c(0, 30)) +
  scale_color_manual(values = c("#0571b0","#ca0020")) +
  labs(
    x = "MyD88 Max Normalized Intensity (a.u.)",
    y = "Scaled Density",
    color = "Group",
    fill = "Group"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.7, 0.7)
  ) +
  ggsave(
    "1mol MaxInt Density Grid_scaled_TRAF6.pdf",
    width = 4,
    height = 2.7
  )
