library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(stringr)
library(parallel)
library(ggpubr)
library(ggforce)
library(plotrix)
library(lemon)

setwd("~")

ColocalizedInt <- fread("~.csv")
#Use mean of all, instead of cells-image-all. Because some images only have few even only one event, cannot represent the whole iamge

#Get colocalized table ----
#To compare HOIL1 or TRAF6 at given Myddosome only with cocalized events
ColocalizedTable <-
  ColocalizedInt %>%
  mutate(
    UNIVERSAL_GROUP_ID = paste(IMAGE, CELL, COLOCALIZATION_GROUP, sep = "...")
  ) %>% 
  arrange(
    UNIVERSAL_GROUP_ID
  ) %>% 
  group_by(
    UNIVERSAL_GROUP_ID
  ) %>% 
  mutate(
    MYD88_MAX_INT = ifelse(PROTEIN == "MyD88", MAX_NORMALIZED_INTENSITY, NA),
    QUERY_MAX_INT = ifelse(PROTEIN == "HOIL1", MAX_NORMALIZED_INTENSITY, NA),
    MYD88_LIFETIME = ifelse(PROTEIN == "MyD88", LIFETIME, NA),
    QUERY_LIFETIME = ifelse(PROTEIN == "HOIL1", LIFETIME, NA)
  ) %>% 
  fill(
    MYD88_MAX_INT,
    .direction = "updown"
  ) %>%
  fill(
    MYD88_LIFETIME,
    .direction = "updown"
  ) %>%
  fill(
    QUERY_MAX_INT,
    .direction = "updown"
  ) %>%
  fill(
    QUERY_LIFETIME,
    .direction = "updown"
  ) %>%
  filter(
    PROTEIN == "MyD88",
    LIGAND_DENSITY_CAT == 32 #For TRAF6 use 10
  ) %>%
  select(
    UNIVERSAL_GROUP_ID,UNIVERSAL_TRACK_ID, LIGAND_DENSITY_CAT, GROUP, IMAGE, CELL, 
    MYD88_MAX_INT, QUERY_MAX_INT, MYD88_LIFETIME, QUERY_LIFETIME
  ) %>%
  mutate(
    MAX_MyddosomeCluster = round(MYD88_MAX_INT/4.5),
    QUERY_MAX_INT = round(QUERY_MAX_INT),
    GROUP = factor(GROUP, levels = c("MyD88 HOIL1Goff","MyD88 HOIL1G1on"))
  ) %>%
  filter(
    MAX_MyddosomeCluster > 0
  )

write.csv(ColocalizedTable, "ColocalizedTable.csv", row.names = F, )

#Get Mean Query Lifetime MAX_MyddosomeCluster single & every 3 ----
MeanQueryLifetime_a <- ColocalizedTable %>%
  filter(MAX_MyddosomeCluster == 1) %>%
  group_by(IMAGE) %>%
  mutate(NTracks = NROW(IMAGE)) %>%
  filter(NTracks >= 2) %>%
  group_by(LIGAND_DENSITY_CAT,GROUP) %>%
  summarize(SD_QUERY_LIFETIME = sd(QUERY_LIFETIME),QUERY_LIFETIME = mean(QUERY_LIFETIME)) %>%
  mutate(MAX_MyddosomeCluster = 1)
MeanQueryLifetime_b <- ColocalizedTable %>%
  filter(MAX_MyddosomeCluster %in% c("2","3","4")) %>%
  group_by(IMAGE) %>%
  mutate(NTracks = NROW(IMAGE)) %>%
  filter(NTracks >= 2) %>%
  group_by(LIGAND_DENSITY_CAT,GROUP) %>%
  summarize(SD_QUERY_LIFETIME = sd(QUERY_LIFETIME),QUERY_LIFETIME = mean(QUERY_LIFETIME)) %>%
  mutate(MAX_MyddosomeCluster = 4)
MeanQueryLifetime_c <- ColocalizedTable %>%
  filter(MAX_MyddosomeCluster %in% c("5","6","7")) %>%
  group_by(IMAGE) %>%
  mutate(NTracks = NROW(IMAGE)) %>%
  filter(NTracks >= 2) %>%
  group_by(LIGAND_DENSITY_CAT,GROUP) %>%
  summarize(SD_QUERY_LIFETIME = sd(QUERY_LIFETIME),QUERY_LIFETIME = mean(QUERY_LIFETIME)) %>%
  mutate(MAX_MyddosomeCluster = 7)

MeanQueryLifetime <- bind_rows(MeanQueryLifetime_a,MeanQueryLifetime_b,MeanQueryLifetime_c)
write.csv(MeanQueryLifetime, "MeanQueryLifetime.csv", row.names = F, )

#Plot_MAX_MyddosomeCluster vs Mean QUERY_Lifetime - Histogram_Frequency ----
ggplot() +
  geom_histogram(
    data = QueryLifetime %>% filter(GROUP == "MyD88 HOIL1G1on"),
    binwidth = 12,
    aes(
      x = QUERY_LIFETIME,
      y = ..count..
    ),
    color = "#ca0020",
    fill = "#ca0020",
    #alpha = 0.7,
  ) +
  geom_histogram(
    data = QueryLifetime %>% filter(GROUP == "MyD88 HOIL1Goff"),
    binwidth = 12,
    aes(
      x = QUERY_LIFETIME,
      y = ..count..
    ),
    color = "#0571b0",
    fill = "#0571b0",
    #alpha = 0.7,
  ) +
  scale_x_continuous(limits = c(0, 800), breaks = seq(0,800,100)) +
  facet_rep_wrap(
    GROUP ~MAX_MyddosomeCluster, 
    repeat.tick.labels = "all",
    scales = "free_y") +
  labs(
    x = "HOIL1 Lifetime (s)",
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
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  ggsave(
    "HOIL1 lifetime_Histogram_Counts.pdf",
    width = 7.5,
    height = 4
  )
