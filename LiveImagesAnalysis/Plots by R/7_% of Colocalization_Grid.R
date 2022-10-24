# To get a merge table of TRAF6 and HOIL1 from colocalization analysis.

library(data.table)
library(dplyr)

setwd("/Users/u_cao/Downloads")

TRAF6_Off_1mol <- fread("~/MyD88_TRAF6 1/MyD88 Mean_Colocalization Cat_MaxInt By_Cells.csv.gz")
TRAF6_Off_10mol <- fread("~/MyD88_TRAF6 10/MyD88 Mean_Colocalization Cat_MaxInt By_Cells.csv.gz")
TRAF6_On_1mol <- fread("~/MyD88_TRAF6 1/MyD88 Mean_Colocalization Cat_MaxInt By_Cells.csv.gz")
TRAF6_On_10mol <- fread("~/MyD88_TRAF6 10/MyD88 Mean_Colocalization Cat_MaxInt By_Cells.csv.gz")

HOIL1_Off_10mol <- fread("~/MyD88_HOIL1 10/MyD88 Mean_Colocalization Cat_MaxInt By_Cells.csv.gz")
HOIL1_Off_32mol <- fread("~/MyD88_HOIL1 32/MyD88 Mean_Colocalization Cat_MaxInt By_Cells.csv.gz")
HOIL1_On_10mol <- fread("~/MyD88_HOIL1 10/MyD88 Mean_Colocalization Cat_MaxInt By_Cells.csv.gz")
HOIL1_On_32mol <- fread("~/MyD88_HOIL1 32/MyD88 Mean_Colocalization Cat_MaxInt By_Cells.csv.gz")

Total <- bind_rows(TRAF6_Off_1mol,TRAF6_Off_10mol,TRAF6_On_1mol,TRAF6_On_10mol,
                   HOIL1_Off_10mol,HOIL1_Off_32mol,HOIL1_On_10mol,HOIL1_On_32mol)

TotalTRAF6only <- bind_rows(TRAF6_Off_1mol,TRAF6_Off_10mol,TRAF6_On_1mol,TRAF6_On_10mol)
TotalHOIL1only <- bind_rows(HOIL1_Off_10mol,HOIL1_Off_32mol,HOIL1_On_10mol,HOIL1_On_32mol)

TotalMerge_byCells <-
  Total %>% filter(MAX_INTENSITY_CAT == "All") 

TotalTRAF6onlyMerge_byCells <-
  TotalTRAF6only %>% filter(MAX_INTENSITY_CAT == "All") %>%
  mutate(GROUP = factor(GROUP, levels = c("MyD88 TRAF6Goff","MyD88 TRAF6G1on")),
         LIGAND_DENSITY_CAT = factor(LIGAND_DENSITY_CAT, levels = c("1","10")))

TotalHOIL1onlyMerge_byCells <-
  TotalHOIL1only %>% filter(MAX_INTENSITY_CAT == "All") %>%
  mutate(GROUP = factor(GROUP, levels = c("MyD88 HOIL1Goff","MyD88 HOIL1G1on")),
         LIGAND_DENSITY_CAT = factor(LIGAND_DENSITY_CAT, levels = c("10","32")))

write.csv(TotalMerge_byCells, "TotalMerge_byCells.csv", row.names = F, )
write.csv(TotalTRAF6onlyMerge_byCells, "TotalTRAF6onlyMerge_byCells.csv", row.names = F, )
write.csv(TotalHOIL1onlyMerge_byCells, "TotalHOIL1onlyMerge_byCells.csv", row.names = F, )

MeanBarTRAF6_byReplicates <-
  TotalTRAF6onlyMerge_byCells %>%
  group_by(GROUP,LIGAND_DENSITY_CAT,IMAGENUMBER) %>%
  summarise(SD_MEAN = sd(MEAN), MEAN = mean(MEAN)) %>%
  mutate(GROUP = factor(GROUP, levels = c("MyD88 TRAF6Goff","MyD88 TRAF6G1on")))

MeanBarHOIL1_byReplicates <-
  TotalHOIL1onlyMerge_byCells %>%
  group_by(GROUP,LIGAND_DENSITY_CAT,IMAGENUMBER) %>%
  summarise(SD_MEAN = sd(MEAN), MEAN = mean(MEAN))%>%
  mutate(GROUP = factor(GROUP, levels = c("MyD88 HOIL1Goff","MyD88 HOIL1G1on")))

write.csv(MeanBarTRAF6_byReplicates, "MeanBarTRAF6_byReplicates.csv", row.names = F, )
write.csv(MeanBarHOIL1_byReplicates, "MeanBarHOIL1_byReplicates.csv", row.names = F, )

MeanBarTRAF6_byLigand <-
  TotalTRAF6onlyMerge_byCells %>%
  group_by(GROUP,LIGAND_DENSITY_CAT,IMAGENUMBER) %>%
  summarise(MEAN = mean(MEAN)) %>%
  group_by(GROUP,LIGAND_DENSITY_CAT) %>%
  summarise(SE_MEAN = std.error(MEAN), MEAN = mean(MEAN))%>%
  mutate(GROUP = factor(GROUP, levels = c("MyD88 TRAF6Goff","MyD88 TRAF6G1on")))

MeanBarHOIL1_byLigand <-
  TotalHOIL1onlyMerge_byCells %>%
  group_by(GROUP,LIGAND_DENSITY_CAT,IMAGENUMBER) %>%
  summarise(MEAN = mean(MEAN)) %>%
  group_by(GROUP,LIGAND_DENSITY_CAT) %>%
  summarise(SE_MEAN = std.error(MEAN), MEAN = mean(MEAN))%>%
  mutate(GROUP = factor(GROUP, levels = c("MyD88 HOIL1Goff","MyD88 HOIL1G1on")))

write.csv(MeanBarTRAF6_byLigand, "MeanBarTRAF6_byLigand.csv", row.names = F, )
write.csv(MeanBarHOIL1_byLigand, "MeanBarHOIL1_byLigand.csv", row.names = F, )

limits <- function(x) {x*100}
dodge <- position_dodge(width = 0.8)

#TRAF6_Grid_% of Colo
ggplot() +
  geom_violin(
    data = TotalTRAF6onlyMerge_byCells %>% filter(LIGAND_DENSITY_CAT == 1),
    aes(
      x = GROUP,
      y = MEAN,
      color = GROUP,
      fill = GROUP
    ),
    alpha = 0.5,
    position = dodge,
    color = NA,
    scale = "width"
  ) +
  geom_sina(
    data = MeanBarTRAF6_byReplicates %>% filter(LIGAND_DENSITY_CAT == 1),
    aes(
      x = GROUP,
      y = MEAN,
      fill = GROUP
    ),
    size = 3,
    shape = 21,
    color = "black"
  ) +  
  geom_crossbar(
    data = MeanBarTRAF6_byLigand %>% filter(LIGAND_DENSITY_CAT == 1),
    aes(
      x = GROUP,
      y = MEAN,
      ymin = MEAN,
      ymax = MEAN,
      group = GROUP
    ),
    position = dodge,
    width = 0.35,
    color = "black",
    fatten = 1
  ) +
  geom_errorbar(
    data = MeanBarTRAF6_byLigand %>% filter(LIGAND_DENSITY_CAT == 1),
    aes(
      x = GROUP,
      y = MEAN,
      ymin = MEAN - SE_MEAN,
      ymax = MEAN + SE_MEAN,
      group = GROUP
    ),
    position = dodge,
    width = 0.25,
    color = "black"
  ) +
  scale_color_manual(values = c("#0571b0","#ca0020")) +
  scale_fill_manual(values = c("#0571b0","#ca0020")) +
  scale_y_continuous(limits = c(0,0.6), label = limits) +
  labs(
    y = "TRAF6-Positive MyD88 (% ± S.E.M)",
    color = "Group",
    fill = "Group"
  ) +
  theme_classic() +
  theme(
    legend.position ="none",
    axis.title.x = element_blank()
  ) + 
  ggsave(
    "Grid percent Colo by ligand density Violin_TRAF6.pdf",
    width = 3,
    height = 4
  )

#HOIL1_Grid_% of Colo
ggplot() +
  geom_violin(
    data = TotalHOIL1onlyMerge_byCells %>% filter(LIGAND_DENSITY_CAT == 32),
    aes(
      x = GROUP,
      y = MEAN,
      color = GROUP,
      fill = GROUP
    ),
    alpha = 0.5,
    position = dodge,
    color = NA,
    scale = "width"
  ) +
  geom_sina(
    data = MeanBarHOIL1_byReplicates %>% filter(LIGAND_DENSITY_CAT == 32),
    aes(
      x = GROUP,
      y = MEAN,
      fill = GROUP
    ),
    size = 3,
    shape = 21,
    color = "black"
  ) +  
  geom_crossbar(
    data = MeanBarHOIL1_byLigand %>% filter(LIGAND_DENSITY_CAT == 32),
    aes(
      x = GROUP,
      y = MEAN,
      ymin = MEAN,
      ymax = MEAN,
      group = GROUP
    ),
    position = dodge,
    width = 0.35,
    color = "black",
    fatten = 1
  ) +
  geom_errorbar(
    data = MeanBarHOIL1_byLigand %>% filter(LIGAND_DENSITY_CAT == 32),
    aes(
      x = GROUP,
      y = MEAN,
      ymin = MEAN - SE_MEAN,
      ymax = MEAN + SE_MEAN,
      group = GROUP
    ),
    position = dodge,
    width = 0.25,
    color = "black"
  ) +
  scale_color_manual(values = c("#0571b0","#ca0020")) +
  scale_fill_manual(values = c("#0571b0","#ca0020")) +
  scale_y_continuous(limits = c(0,0.35), label = limits) +
  labs(
    y = "HOIL1-Positive MyD88 (% ± S.E.M)",
    color = "Group",
    fill = "Group"
  ) +
  theme_classic() +
  theme(
    legend.position ="none",
    axis.title.x = element_blank()
  ) + 
  ggsave(
    "Grid percent Colo by ligand density Violin_HOIL1.pdf",
    width = 3,
    height = 4
  )

#By replicates_TRAF6
ggplot() +
  geom_violin(
    data = TotalTRAF6onlyMerge_byCells,
    aes(
      x = as.factor(IMAGENUMBER),
      y = MEAN
    ),
    color = "grey",
    fill = "grey",
    scale = "width"
  ) +
  geom_jitter(
    data = TotalTRAF6onlyMerge_byCells,
    aes(
      x = as.factor(IMAGENUMBER),
      y = MEAN
    ),
    size = 2,
    shape = 21,
    #alpha = 0.7,
    color = "black",
    fill = "#74c476"
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
  scale_y_continuous(
    label = limits
  ) +
  labs(
    x = "Replicates",
    y = "TRAF6-Positive MyD88 (%)",
    color = "Replicates",
    fill = "Replicates"
  ) +
  facet_rep_grid(
    ~LIGAND_DENSITY_CAT~GROUP, 
    repeat.tick.labels = "all"
  ) +
  theme_classic() +
  theme(
    legend.position ="none",
    strip.background = element_blank()
  ) +
  ggsave(
    "Grid percent Colo by replicates & group_TRAF6.pdf",
    width = 6.5,
    height = 4
  )

#By replicates_HOIL1
ggplot() +
  geom_violin(
    data = TotalHOIL1onlyMerge_byCells,
    aes(
      x = as.factor(IMAGENUMBER),
      y = MEAN
    ),
    color = "grey",
    fill = "grey",
    scale = "width"
  ) +
  geom_jitter(
    data = TotalHOIL1onlyMerge_byCells,
    aes(
      x = as.factor(IMAGENUMBER),
      y = MEAN
    ),
    size = 2,
    shape = 21,
    #alpha = 0.7,
    color = "black",
    fill = "#74c476"
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
  scale_y_continuous(
    label = limits
  ) +
  labs(
    x = "Replicates",
    y = "HOIL1-Positive MyD88 (%)",
    color = "Replicates",
    fill = "Replicates"
  ) +
  facet_rep_grid(
    ~LIGAND_DENSITY_CAT~GROUP, 
    repeat.tick.labels = "all"
  ) +
  theme_classic() +
  theme(
    legend.position ="none",
    strip.background = element_blank()
  ) +
  ggsave(
    "Grid percent Colo by replicates & group_HOIL1.pdf",
    width = 6.5,
    height = 4
  )
