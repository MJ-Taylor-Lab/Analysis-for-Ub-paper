library(data.table)
library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(plotrix)
library(ggforce)

setwd("~")

TRAF6 <- fread("~GrandColoSummary.csv.gz") %>%
  filter(PROTEIN == "MyD88", FRAMES_ADJUSTED == 0) %>% mutate(MyddosomeCluster = MAX_NORMALIZED_INTENSITY/4.5)
HOIL1 <- fread("~GrandColoSummary.csv.gz") %>%
  filter(PROTEIN == "MyD88", FRAMES_ADJUSTED == 0) %>% mutate(MyddosomeCluster = MAX_NORMALIZED_INTENSITY/4.5)

#MeanBar_TRAF6 ----
TRAF6_All_MeanBar <- TRAF6 %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "All")

TRAF6_BelowEqualSingle_MeanBar <- TRAF6 %>%
  filter(MyddosomeCluster <= 1) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "≤1")

TRAF6_GreaterThanOne_MeanBar <- TRAF6 %>%
  filter(MyddosomeCluster > 1) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = ">1")

TRAF6_GreaterEqualThanTwo_MeanBar <- TRAF6 %>%
  filter(MyddosomeCluster >= 2) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "≥2")

MeanBar_TRAF6 <- bind_rows(TRAF6_All_MeanBar,TRAF6_BelowEqualSingle_MeanBar,TRAF6_GreaterThanOne_MeanBar,TRAF6_GreaterEqualThanTwo_MeanBar) %>%
  mutate(MyddosomeCluster = factor(MyddosomeCluster, levels = c("All", "≤1", ">1", "≥2")))

write.csv(MeanBar_TRAF6, "MeanBar_TRAF6.csv", row.names = F, )

#MeanBar_HOIL1 ----
HOIL1_All_MeanBar <- HOIL1 %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "All")

HOIL1_BelowEqualSingle_MeanBar <- HOIL1 %>%
  filter(MyddosomeCluster <= 1) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "≤1")

HOIL1_GreaterThanOne_MeanBar <- HOIL1 %>%
  filter(MyddosomeCluster > 1) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = ">1")

HOIL1_GreaterEqualThanTwo_MeanBar <- HOIL1 %>%
  filter(MyddosomeCluster >= 2) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "≥2")

MeanBar_HOIL1 <- bind_rows(HOIL1_All_MeanBar,HOIL1_BelowEqualSingle_MeanBar,HOIL1_GreaterThanOne_MeanBar,HOIL1_GreaterEqualThanTwo_MeanBar) %>%
  mutate(MyddosomeCluster = factor(MyddosomeCluster, levels = c("All", "≤1", ">1", "≥2")))

write.csv(MeanBar_HOIL1, "MeanBar_HOIL1.csv", row.names = F, )

#MeanByImage_TRAF6 ----
TRAF6_All_MeanByImage <- TRAF6 %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "All")

TRAF6_BelowEqualSingle_MeanByImage <- TRAF6 %>%
  filter(MyddosomeCluster <= 1) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "≤1")

TRAF6_GreaterThanOne_MeanByImage <- TRAF6 %>%
  filter(MyddosomeCluster > 1) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = ">1")

TRAF6_GreaterEqualThanTwo_MeanByImage <- TRAF6 %>%
  filter(MyddosomeCluster >= 2) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "≥2")

MeanByImage_TRAF6 <- bind_rows(TRAF6_All_MeanByImage,TRAF6_BelowEqualSingle_MeanByImage,TRAF6_GreaterThanOne_MeanByImage,TRAF6_GreaterEqualThanTwo_MeanByImage) %>%
  mutate(MyddosomeCluster = factor(MyddosomeCluster, levels = c("All", "≤1", ">1", "≥2")))

write.csv(MeanByImage_TRAF6, "MeanByImage_TRAF6.csv", row.names = F, )

#MeanByImage_HOIL1 ----
HOIL1_All_MeanByImage <- HOIL1 %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "All")

HOIL1_BelowEqualSingle_MeanByImage <- HOIL1 %>%
  filter(MyddosomeCluster <= 1) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "≤1")

HOIL1_GreaterThanOne_MeanByImage <- HOIL1 %>%
  filter(MyddosomeCluster > 1) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = ">1")

HOIL1_GreaterEqualThanTwo_MeanByImage <- HOIL1 %>%
  filter(MyddosomeCluster >= 2) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "≥2")

MeanByImage_HOIL1 <- bind_rows(HOIL1_All_MeanByImage,HOIL1_BelowEqualSingle_MeanByImage,HOIL1_GreaterThanOne_MeanByImage,HOIL1_GreaterEqualThanTwo_MeanByImage) %>%
  mutate(MyddosomeCluster = factor(MyddosomeCluster, levels = c("All", "≤1", ">1", "≥2")))

write.csv(MeanByImage_HOIL1, "MeanByImage_HOIL1.csv", row.names = F, )

#MeanByCell_TRAF6 ----
TRAF6_All_MeanByCell <- TRAF6 %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "All")

TRAF6_BelowEqualSingle_MeanByCell <- TRAF6 %>%
  filter(MyddosomeCluster <= 1) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "≤1")

TRAF6_GreaterThanOne_MeanByCell <- TRAF6 %>%
  filter(MyddosomeCluster > 1) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = ">1")

TRAF6_GreaterEqualThanTwo_MeanByCell <- TRAF6 %>%
  filter(MyddosomeCluster >= 2) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "≥2")

MeanByCell_TRAF6 <- bind_rows(TRAF6_All_MeanByCell,TRAF6_BelowEqualSingle_MeanByCell,TRAF6_GreaterThanOne_MeanByCell,TRAF6_GreaterEqualThanTwo_MeanByCell) %>%
  mutate(MyddosomeCluster = factor(MyddosomeCluster, levels = c("All", "≤1", ">1", "≥2")))

write.csv(MeanByCell_TRAF6, "MeanByCell_TRAF6.csv", row.names = F, )

#MeanByCell_HOIL1 ----
HOIL1_All_MeanByCell <- HOIL1 %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "All")

HOIL1_BelowEqualSingle_MeanByCell <- HOIL1 %>%
  filter(MyddosomeCluster <= 1) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "≤1")

HOIL1_GreaterThanOne_MeanByCell <- HOIL1 %>%
  filter(MyddosomeCluster > 1) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = ">1")

HOIL1_GreaterEqualThanTwo_MeanByCell <- HOIL1 %>%
  filter(MyddosomeCluster >= 2) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = "≥2")

MeanByCell_HOIL1 <- bind_rows(HOIL1_All_MeanByCell,HOIL1_BelowEqualSingle_MeanByCell,HOIL1_GreaterThanOne_MeanByCell,HOIL1_GreaterEqualThanTwo_MeanByCell) %>%
  mutate(MyddosomeCluster = factor(MyddosomeCluster, levels = c("All", "≤1", ">1", "≥2")))

write.csv(MeanByCell_HOIL1, "MeanByCell_HOIL1.csv", row.names = F, )

#TRAF6_Violin ----
limits <- function(x) {x*100}
ggplot() +
  geom_violin(
    data = MeanByCell_TRAF6,
    aes(
      x = MyddosomeCluster,
      y = COLOCALIZATION
    ),
    color = "darkgrey",
    fill = "darkgrey",
    scale = "width"
  ) +
  geom_jitter(
    data = MeanByImage_TRAF6,
    aes(
      x = MyddosomeCluster,
      y = COLOCALIZATION
    ),
    size = 4,
    shape = 21,
    color = "black",
    fill = "#b35806"
  ) +  
  geom_crossbar(
    data = MeanBar_TRAF6,
    aes(
      x = MyddosomeCluster,
      y = COLOCALIZATION,
      ymin = COLOCALIZATION,
      ymax = COLOCALIZATION
    ),
    width = 0.35,
    color = "black",
    fatten = 1
  ) +
  geom_errorbar(
    data = MeanBar_TRAF6,
    aes(
      x = MyddosomeCluster,
      y = COLOCALIZATION,
      ymin = COLOCALIZATION - SE_COLOCALIZATION,
      ymax = COLOCALIZATION + SE_COLOCALIZATION
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
  )

ggsave(
  "TRAF6-Positive MyD88_by replicates.pdf",
  width = 7.3,
  height = 3
)

#HOIL1_Violin ----
limits <- function(x) {x*100}
ggplot() +
  geom_violin(
    data = MeanByCell_HOIL1,
    aes(
      x = MyddosomeCluster,
      y = COLOCALIZATION
    ),
    color = "darkgrey",
    fill = "darkgrey",
    scale = "width"
  ) +
  geom_jitter(
    data = MeanByImage_HOIL1,
    aes(
      x = MyddosomeCluster,
      y = COLOCALIZATION
    ),
    size = 4,
    shape = 21,
    color = "black",
    fill = "#b35806"
  ) +  
  geom_crossbar(
    data = MeanBar_HOIL1,
    aes(
      x = MyddosomeCluster,
      y = COLOCALIZATION,
      ymin = COLOCALIZATION,
      ymax = COLOCALIZATION
    ),
    width = 0.35,
    color = "black",
    fatten = 1
  ) +
  geom_errorbar(
    data = MeanBar_HOIL1,
    aes(
      x = MyddosomeCluster,
      y = COLOCALIZATION,
      ymin = COLOCALIZATION - SE_COLOCALIZATION,
      ymax = COLOCALIZATION + SE_COLOCALIZATION
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
  )

ggsave(
  "HOIL1-Positive MyD88_by replicates.pdf",
  width = 7.3,
  height = 3
)
