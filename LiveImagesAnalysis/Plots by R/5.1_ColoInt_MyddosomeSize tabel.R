library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(parallel)
library(ggdark)
library(scales)
library(plotrix)

GrandColoSummary1 <- fread("~")
GrandColoSummary2 <- fread("~")
GrandColoSummary <- bind_rows(GrandColoSummary1, GrandColoSummary2)

setwd("~")

# Rearrange row order
GrandColoSummary <-
  GrandColoSummary %>% 
  arrange(IMAGE,CELL,TRACK_ID,FRAME) %>%
  mutate(
    PROTEIN = factor(PROTEIN, levels = c("MyD88", "TRAF6", "HOIL1")),
    LIGAND_DENSITY_CAT = factor(LIGAND_DENSITY_CAT, levels = c("0.32", "1", "10", "32", "100")),
    GROUP = factor(GROUP, levels = c("MyD88 TRAF6", "MyD88 HOIL1",
                                     "MyD88 HOIL1Goff", "MyD88 HOIL1G2p5on", "MyD88 HOIL1G1on",
                                     "MyD88 TRAF6Goff", "MyD88 TRAF6G1on"))
  )

# Get colocalized intensity
ColocalizedIntensity <-
  GrandColoSummary %>% 
  group_by(
    IMAGE,
    CELL,
    COLOCALIZATION_GROUP,
    FRAME
  ) %>% 
  mutate(
    N = n()
  ) %>% 
  filter(
    N == 2
  ) %>% 
  group_by(
    GROUP,
    IMAGE,
    CELL,
    COLOCALIZATION_GROUP,
    PROTEIN
  ) %>% 
  filter(
    FRAME == min(FRAME),
    TRACK_ID == min(TRACK_ID)
  ) %>% 
  group_by(
    GROUP,
    IMAGE,
    CELL,
    COLOCALIZATION_GROUP,
  ) %>% 
  mutate(
    DISTANCE = sqrt((POSITION_X - lag(POSITION_X))^2 + (POSITION_Y - lag(POSITION_Y))^2)/.14667
  ) %>% 
  fill(
    DISTANCE, .direction = "updown"
  ) %>% 
  filter(
    DISTANCE <= 5
  )

# Save
write.csv(ColocalizedIntensity, "ColocalizedIntensity.csv", row.names = F, )

MeanColocalizedIntensity <-
  ColocalizedIntensity %>% filter(PROTEIN == "MyD88") %>% 
  group_by(LIGAND_DENSITY_CAT,GROUP,IMAGE,CELL) %>%
  summarise(NORMALIZED_INTENSITY = mean(NORMALIZED_INTENSITY)) %>%
  group_by(LIGAND_DENSITY_CAT,GROUP,IMAGE) %>%
  summarise(NORMALIZED_INTENSITY = mean(NORMALIZED_INTENSITY)) %>%
  group_by(LIGAND_DENSITY_CAT,GROUP) %>%
  summarise(NORMALIZED_INTENSITY = mean(NORMALIZED_INTENSITY))

write.csv(MeanColocalizedIntensity, "MeanColocalizedIntensity.csv", row.names = F, )

MeanColocalizedMyddosomeSize <-
  ColocalizedIntensity %>% filter(PROTEIN == "MyD88") %>% 
  mutate(MyddosomeSize = NORMALIZED_INTENSITY/4.5) %>%
  group_by(LIGAND_DENSITY_CAT,GROUP,IMAGE,CELL) %>%
  summarise(MyddosomeSize = mean(MyddosomeSize)) %>%
  group_by(LIGAND_DENSITY_CAT,GROUP,IMAGE) %>%
  summarise(MyddosomeSize = mean(MyddosomeSize)) %>%
  group_by(LIGAND_DENSITY_CAT,GROUP) %>%
  summarise(
    SE_MyddosomeSize = std.error(MyddosomeSize),
    MyddosomeSize = mean(MyddosomeSize))

write.csv(MeanColocalizedMyddosomeSize, "MeanColocalizedMyddosomeSize.csv", row.names = F, )
