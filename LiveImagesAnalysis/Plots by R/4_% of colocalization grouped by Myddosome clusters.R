library(data.table)
library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(plotrix)
library(ggforce)

setwd("/Users/u_cao/Downloads")

#MyddosomeCluster rounded to integer
TRAF6 <- fread("~") %>%
  filter(PROTEIN == "MyD88", FRAMES_ADJUSTED == 0) %>% mutate(MyddosomeCluster = round(MAX_NORMALIZED_INTENSITY/4.5))
HOIL1 <- fread("~") %>%
  filter(PROTEIN == "MyD88", FRAMES_ADJUSTED == 0) %>% mutate(MyddosomeCluster = round(MAX_NORMALIZED_INTENSITY/4.5))

#MeanBar_MyddosomeCluster_TRAF6 ----
TRAF6_Single_MeanBar <- TRAF6 %>%
  filter(MyddosomeCluster %in% c(0,1)) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = 1)

TRAF6_Four_MeanBar <- TRAF6 %>%
  filter(MyddosomeCluster %in% c(2,3,4)) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = 4)

TRAF6_Seven_MeanBar <- TRAF6 %>%
  filter(MyddosomeCluster %in% c(5,6,7)) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = 7)

TRAF6_Ten_MeanBar <- TRAF6 %>%
  filter(MyddosomeCluster %in% c(8,9,10)) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = 10)

TRAF6_Thirteen_MeanBar <- TRAF6 %>%
  filter(MyddosomeCluster %in% c(11,12,13)) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = 13)

TRAF6_Sixteen_MeanBar <- TRAF6 %>%
  filter(MyddosomeCluster %in% c(14,15,16)) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = 16)

MeanBar_MyddosomeCluster_TRAF6 <- bind_rows(TRAF6_Single_MeanBar,TRAF6_Four_MeanBar,TRAF6_Seven_MeanBar,
                                            TRAF6_Ten_MeanBar,TRAF6_Thirteen_MeanBar,TRAF6_Sixteen_MeanBar) 

write.csv(MeanBar_MyddosomeCluster_TRAF6, "MeanBar_MyddosomeCluster_TRAF6.csv", row.names = F, )

#MeanBar_MyddosomeCluster_HOIL1 ----
HOIL1_Single_MeanBar <- HOIL1 %>%
  filter(MyddosomeCluster %in% c(0,1)) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = 1)

HOIL1_Four_MeanBar <- HOIL1 %>%
  filter(MyddosomeCluster %in% c(2,3,4)) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = 4)

HOIL1_Seven_MeanBar <- HOIL1 %>%
  filter(MyddosomeCluster %in% c(5,6,7)) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = 7)

HOIL1_Ten_MeanBar <- HOIL1 %>%
  filter(MyddosomeCluster %in% c(8,9,10)) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = 10)

HOIL1_Thirteen_MeanBar <- HOIL1 %>%
  filter(MyddosomeCluster %in% c(11,12,13)) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = 13)

HOIL1_Sixteen_MeanBar <- HOIL1 %>%
  filter(MyddosomeCluster %in% c(14,15,16)) %>%
  group_by(GROUP, IMAGE, CELL) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP, IMAGE) %>%
  summarise(COLOCALIZATION = mean(COLOCALIZATION)) %>%
  group_by(GROUP) %>%
  summarise(SE_COLOCALIZATION = std.error(COLOCALIZATION),COLOCALIZATION = mean(COLOCALIZATION)) %>%
  mutate(MyddosomeCluster = 16)

MeanBar_MyddosomeCluster_HOIL1 <- bind_rows(HOIL1_Single_MeanBar,HOIL1_Four_MeanBar,HOIL1_Seven_MeanBar,
                                            HOIL1_Ten_MeanBar,HOIL1_Thirteen_MeanBar,HOIL1_Sixteen_MeanBar)

write.csv(MeanBar_MyddosomeCluster_HOIL1, "MeanBar_MyddosomeCluster_HOIL1.csv", row.names = F, )

#Merge data ----
MeanBar_MyddosomeCluster_TRAF6_HOIL1 <- bind_rows(MeanBar_MyddosomeCluster_TRAF6,MeanBar_MyddosomeCluster_HOIL1)

#Line plot ----
limits <- function(x) {x*100}
ggplot()+
  geom_line(
    data = MeanBar_MyddosomeCluster_TRAF6_HOIL1,
    aes(
      x = MyddosomeCluster,
      y = COLOCALIZATION,
      color = GROUP
    ),
    size = 1
  ) +
  geom_point(
    data = MeanBar_MyddosomeCluster_TRAF6_HOIL1,
    aes(
      x = MyddosomeCluster,
      y = COLOCALIZATION,
      color = GROUP
    )
  ) +
  geom_errorbar(
    data = MeanBar_MyddosomeCluster_TRAF6_HOIL1,
    aes(
      x = MyddosomeCluster,
      y = COLOCALIZATION,
      ymin = COLOCALIZATION - SE_COLOCALIZATION,
      ymax = COLOCALIZATION + SE_COLOCALIZATION,
      group = GROUP,
      color = GROUP
    ),
    width = 0.3,
    size = 0.8
  ) +
  scale_color_manual(values = c("#8073ac","#b35806")) +
  scale_x_continuous(limits = c(0, 17), breaks = seq(1,16,3)) +
  scale_y_continuous(label = limits) +
  labs(
    x = "Max Number of Myddosome Clusters (a.u.)",
    y = "TRAF6/HOIL1-Positive MyD88 (% ± S.E.M)",
    color = "Group",
    fill = "Group"
  ) +
  theme_classic() +
  theme(
    #legend.position = "bottom"
    legend.position="none" #remove legend
  )

ggsave("TRAF6 HOIL1-Positive MyD88 by Cluster Bins.pdf",
       units = "cm", height = 8, width = 14.5)
