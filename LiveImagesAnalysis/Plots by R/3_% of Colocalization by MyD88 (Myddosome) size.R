library(data.table)
library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(plotrix)
library(ggforce)

setwd("~")

#Note: If want to show myddosome size instead of myd88 size, manually change x-axis in ai file.

TRAF6_Off_32mol_4.5 <- fread("~/MyD88 Mean_Colocalization Cat_MaxInt By_Cells_4.5 threshold.csv")
TRAF6_Off_32mol_9 <- fread("~/MyD88 Mean_Colocalization Cat_MaxInt By_Cells_9 threshold.csv")
TRAF6_Temp <- TRAF6_Off_32mol_9 %>% filter(MAX_INTENSITY_CAT == "≥9")  
TRAF6_Off <- bind_rows(TRAF6_Off_32mol_4.5,TRAF6_Temp) %>% 
  arrange(GROUP,PROTEIN,LIGAND_DENSITY_CAT,IMAGE,IMAGENUMBER,CELL,MAX_INTENSITY_CAT) %>%
  mutate(MAX_INTENSITY_CAT = factor(MAX_INTENSITY_CAT, levels = c("All","<4.5","≥4.5","≥9")))

HOIL1_Off_32mol_4.5 <- fread("~/MyD88 Mean_Colocalization Cat_MaxInt By_Cells_4.5 threshold.csv")
HOIL1_Off_32mol_9 <- fread("~/MyD88 Mean_Colocalization Cat_MaxInt By_Cells_9 threshold.csv")
HOIL1_Temp <- HOIL1_Off_32mol_9 %>% filter(MAX_INTENSITY_CAT == "≥9")  
HOIL1_Off <- bind_rows(HOIL1_Off_32mol_4.5,HOIL1_Temp) %>% 
  arrange(GROUP,PROTEIN,LIGAND_DENSITY_CAT,IMAGE,IMAGENUMBER,CELL,MAX_INTENSITY_CAT) %>%
  mutate(MAX_INTENSITY_CAT = factor(MAX_INTENSITY_CAT, levels = c("All","<4.5","≥4.5","≥9")))

write.csv(TRAF6_Off, "TRAF6_Off_threshold4.5and9.csv", row.names = F, )
write.csv(HOIL1_Off, "HOIL1_Off_threshold4.5and9.csv", row.names = F, )

MeanBarTRAF6_byReplicates <- 
  TRAF6_Off %>%
  group_by(IMAGE,IMAGENUMBER,MAX_INTENSITY_CAT) %>%
  mutate(N_TRACKS = sum(N_TRACKS),N_CELLS = NROW(MEAN)) %>%
  group_by(IMAGE,IMAGENUMBER,N_TRACKS,N_CELLS,MAX_INTENSITY_CAT) %>%
  summarise(SD_MEAN = sd(MEAN), SE_MEAN = std.error(MEAN), MEAN = mean(MEAN)) %>%
  mutate(MAX_INTENSITY_CAT = factor(MAX_INTENSITY_CAT, levels = c("All","<4.5","≥4.5","≥9")))

MeanBarHOIL1_byReplicates <- 
  HOIL1_Off %>%
  group_by(IMAGE,IMAGENUMBER,MAX_INTENSITY_CAT) %>%
  mutate(N_TRACKS = sum(N_TRACKS),N_CELLS = NROW(MEAN)) %>%
  group_by(IMAGE,IMAGENUMBER,N_TRACKS,N_CELLS,MAX_INTENSITY_CAT) %>%
  summarise(SD_MEAN = sd(MEAN), SE_MEAN = std.error(MEAN), MEAN = mean(MEAN)) %>%
  mutate(MAX_INTENSITY_CAT = factor(MAX_INTENSITY_CAT, levels = c("All","<4.5","≥4.5","≥9")))

write.csv(MeanBarTRAF6_byReplicates, "MeanBarTRAF6_byReplicates.csv", row.names = F, )
write.csv(MeanBarHOIL1_byReplicates, "MeanBarHOIL1_byReplicates.csv", row.names = F, )

MeanBarTRAF6 <- 
  MeanBarTRAF6_byReplicates %>%
  group_by(MAX_INTENSITY_CAT) %>%
  mutate(N_TRACKS = sum(N_TRACKS),N_CELLS = sum(N_CELLS),N_IMAGES = NROW(MEAN)) %>%
  group_by(N_TRACKS,N_CELLS,N_IMAGES,MAX_INTENSITY_CAT) %>%
  summarise(SD_MEAN = sd(MEAN), SE_MEAN = std.error(MEAN), MEAN = mean(MEAN)) %>%
  mutate(MAX_INTENSITY_CAT = factor(MAX_INTENSITY_CAT, levels = c("All","<4.5","≥4.5","≥9")))

MeanBarHOIL1 <- 
  MeanBarHOIL1_byReplicates %>%
  group_by(MAX_INTENSITY_CAT) %>%
  mutate(N_TRACKS = sum(N_TRACKS),N_CELLS = sum(N_CELLS),N_IMAGES = NROW(MEAN)) %>%
  group_by(N_TRACKS,N_CELLS,N_IMAGES,MAX_INTENSITY_CAT) %>%
  summarise(SD_MEAN = sd(MEAN), SE_MEAN = std.error(MEAN), MEAN = mean(MEAN)) %>%
  mutate(MAX_INTENSITY_CAT = factor(MAX_INTENSITY_CAT, levels = c("All","<4.5","≥4.5","≥9")))

write.csv(MeanBarTRAF6, "MeanBarTRAF6.csv", row.names = F, )
write.csv(MeanBarHOIL1, "MeanBarHOIL1.csv", row.names = F, )

limits <- function(x) {x*100}

#1_TRAF6
ggplot() +
  geom_violin(
    data = TRAF6_Off,
    aes(
      x = MAX_INTENSITY_CAT,
      y = MEAN
    ),
    color = "darkgrey",
    fill = "darkgrey",
    scale = "width"
  ) +
  geom_jitter(
    data = MeanBarTRAF6_byReplicates,
    aes(
      x = MAX_INTENSITY_CAT,
      y = MEAN
    ),
    size = 4,
    shape = 21,
    color = "black",
    fill = "#b35806"
  ) +  
  geom_crossbar(
    data = MeanBarTRAF6,
    aes(
      x = MAX_INTENSITY_CAT,
      y = MEAN,
      ymin = MEAN,
      ymax = MEAN
    ),
    width = 0.35,
    color = "black",
    fatten = 1
  ) +
  geom_errorbar(
    data = MeanBarTRAF6,
    aes(
      x = MAX_INTENSITY_CAT,
      y = MEAN,
      ymin = MEAN - SE_MEAN,
      ymax = MEAN + SE_MEAN
    ),
    width = 0.25,
    color = "black"
  ) +
  scale_y_continuous(limits = c(0, 1),label = limits) +
  labs(
    x = "MyD88 Puncta Size",
    y = "TRAF6-Positive MyD88 (% ± S.E.M)"
  ) +
  theme_classic() +
  theme(
    legend.position ="none"
  ) +
  ggsave(
    "TRAF6-Positive MyD88.svg",
    width = 4.5,
    height = 3
  ) +
  ggsave(
    "TRAF6-Positive MyD88.pdf",
    width = 4.5,
    height = 3
  )

#2_HOIL1
ggplot() +
  geom_violin(
    data = HOIL1_Off,
    aes(
      x = MAX_INTENSITY_CAT,
      y = MEAN
    ),
    color = "darkgrey",
    fill = "darkgrey",
    scale = "width"
  ) +
  geom_jitter(
    data = MeanBarHOIL1_byReplicates,
    aes(
      x = MAX_INTENSITY_CAT,
      y = MEAN
    ),
    size = 4,
    shape = 21,
    color = "black",
    fill = "#8073ac"
  ) +  
  geom_crossbar(
    data = MeanBarHOIL1,
    aes(
      x = MAX_INTENSITY_CAT,
      y = MEAN,
      ymin = MEAN,
      ymax = MEAN
    ),
    width = 0.35,
    color = "black",
    fatten = 1
  ) +
  geom_errorbar(
    data = MeanBarHOIL1,
    aes(
      x = MAX_INTENSITY_CAT,
      y = MEAN,
      ymin = MEAN - SE_MEAN,
      ymax = MEAN + SE_MEAN
    ),
    width = 0.25,
    color = "black"
  ) +
  scale_y_continuous(limits = c(0, 1),label = limits) +
  labs(
    x = "MyD88 Puncta Size",
    y = "HOIL1-Positive MyD88 (% ± S.E.M)"
  ) +
  theme_classic() +
  theme(
    legend.position ="none"
  ) +
  ggsave(
    "HOIL1-Positive MyD88.svg",
    width = 4.5,
    height = 3
  ) +
  ggsave(
    "HOIL1-Positive MyD88.pdf",
    width = 4.5,
    height = 3
  )

#3_TRAF6 by replicates
ggplot() +
  geom_violin(
    data = TRAF6_Off,
    aes(
      x = as.factor(IMAGENUMBER),
      y = MEAN
    ),
    color = "darkgrey",
    fill = "darkgrey",
    scale = "width"
  ) +
  geom_jitter(
    data = TRAF6_Off,
    aes(
      x = as.factor(IMAGENUMBER),
      y = MEAN
    ),
    size = 2,
    shape = 21,
    color = "black",
    fill = "#b35806"
  ) +  
  geom_crossbar(
    data = MeanBarTRAF6_byReplicates,
    aes(
      x = as.factor(IMAGENUMBER),
      y = MEAN,
      ymin = MEAN,
      ymax = MEAN
    ),
    width = 0.35,
    color = "black",
    fatten = 1
  ) +
  scale_y_continuous(limits = c(0, 1),label = limits) +
  labs(
    x = "Replicates",
    y = "TRAF6-Positive MyD88 (%)"
  ) +
  facet_grid(~MAX_INTENSITY_CAT) +
  theme_classic() +
  theme(
    legend.position ="none",
    strip.background = element_blank()
  ) +
  ggsave(
    "TRAF6-Positive MyD88_by replicates.svg",
    width = 5,
    height = 3
  ) +
  ggsave(
    "TRAF6-Positive MyD88_by replicates.pdf",
    width = 5,
    height = 3
  )

#4_HOIL1 by replicates
ggplot() +
  geom_violin(
    data = HOIL1_Off,
    aes(
      x = as.factor(IMAGENUMBER),
      y = MEAN
    ),
    color = "darkgrey",
    fill = "darkgrey",
    scale = "width"
  ) +
  geom_jitter(
    data = HOIL1_Off,
    aes(
      x = as.factor(IMAGENUMBER),
      y = MEAN
    ),
    size = 2,
    shape = 21,
    color = "black",
    fill = "#8073ac"
  ) +  
  geom_crossbar(
    data = MeanBarHOIL1_byReplicates,
    aes(
      x = as.factor(IMAGENUMBER),
      y = MEAN,
      ymin = MEAN,
      ymax = MEAN
    ),
    width = 0.35,
    color = "black",
    fatten = 1
  ) +
  scale_y_continuous(limits = c(0, 1),label = limits) +
  labs(
    x = "Replicates",
    y = "HOIL1-Positive MyD88 (%)"
  ) +
  facet_grid(~MAX_INTENSITY_CAT) +
  theme_classic() +
  theme(
    legend.position ="none",
    strip.background = element_blank()
  ) +
  ggsave(
    "HOIL1-Positive MyD88_by replicates.svg",
    width = 7.3,
    height = 3
  ) +
  ggsave(
    "HOIL1-Positive MyD88_by replicates.pdf",
    width = 7.5,
    height = 3
  )
