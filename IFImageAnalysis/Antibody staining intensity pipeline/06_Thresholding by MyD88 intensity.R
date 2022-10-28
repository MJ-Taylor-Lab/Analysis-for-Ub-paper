library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(parallel)
library(scales)
library(ggbeeswarm)
library(ggpubr)
library(ggdark)
library(plotrix)

#"pp65" could be replaced with "pIKK",K63","M1" accordingly.

setwd("~")

pp65_analysis_normalized <- fread("~/pp65_analysis_normalized.csv")

MyD88_off_Integrated_above <- pp65_analysis_normalized %>%
  filter(GROUP == "off_grid", Normalized_Integrated_MyD88 >= 0.5) %>%
  mutate(New_Group = "off_grid_above")
MyD88_off_Integrated_below <- pp65_analysis_normalized %>%
  filter(GROUP == "off_grid", Normalized_Integrated_MyD88 < 0.5) %>%
  mutate(New_Group = "off_grid_below")
MyD88_2.5um <- pp65_analysis_normalized %>%
  filter(GROUP == "2.5um_grid") %>% mutate(New_Group = "2.5um_grid")
MyD88_1um <- pp65_analysis_normalized %>%
  filter(GROUP == "1um_grid") %>% mutate(New_Group = "1um_grid")

pp65_analysis_normalized <- bind_rows(MyD88_off_Integrated_above,MyD88_off_Integrated_below,MyD88_2.5um,MyD88_1um)

pp65_analysis_normalized <- pp65_analysis_normalized %>%
  mutate(New_Group = factor(New_Group, levels = c("off_grid_above","off_grid_below","2.5um_grid","1um_grid")),
         GROUP = factor(GROUP, levels = c("off_grid","2.5um_grid","1um_grid")))

#Calculate Normalized_Integrated_MyD88 >= 0.5
MyD88Integrated_nrowtotal<- pp65_analysis_normalized %>%
  group_by(GROUP,REP) %>%
  summarize(N_Tracks_Total = NROW(Normalized_Integrated_MyD88))

MyD88Integrated_nrow <- pp65_analysis_normalized %>%
  filter(Normalized_Integrated_MyD88 >= 0.5) %>%
  group_by(GROUP,REP) %>%
  summarize(N_Tracks = NROW(Normalized_Integrated_MyD88))

MyD88IntegratedPct <-
  merge(MyD88Integrated_nrowtotal,MyD88Integrated_nrow, all = TRUE) %>%
  mutate(Pct = N_Tracks/N_Tracks_Total*100)

MyD88IntegratedPct[is.na(MyD88IntegratedPct)] = 0

MyD88IntegratedPctMean <- 
  MyD88IntegratedPct %>%
  group_by(GROUP) %>%
  summarize(SE_Pct = std.error(Pct),MeanPct = mean(Pct))

write.csv(MyD88IntegratedPct, "MyD88Integrated >=0.5 Pct.csv", row.names = F, )
write.csv(MyD88IntegratedPctMean, "MyD88Integrated >=0.5 PctMean.csv", row.names = F, )

#pp65 mean int and SE
pp65MeanIntbyRep <-  
  pp65_analysis_normalized %>%
  group_by(New_Group,REP) %>%
  summarize(Mean_Normalized_Mean_pp65 = mean(Normalized_Mean_pp65)) 
pp65MeanIntMeanBar <-
  pp65MeanIntbyRep %>%
  group_by(New_Group) %>%
  summarize(SE_Normalized_Mean_pp65 = std.error(Mean_Normalized_Mean_pp65),
            Mean_Normalized_Mean_pp65 = mean(Mean_Normalized_Mean_pp65))

write.csv(pp65MeanIntbyRep, "pp65MeanIntbyRep.csv", row.names = F, )
write.csv(pp65MeanIntMeanBar, "pp65MeanIntMeanBar.csv", row.names = F, )

#1 Pct by cut off
ggplot() +
  geom_jitter(
    data = MyD88IntegratedPct,
    aes(
      x = GROUP,
      y = Pct,
      fill = GROUP
    ),
    color = "black",
    size = 4,
    shape = 21
  ) +
  geom_crossbar(
    data = MyD88IntegratedPctMean,
    aes(
      x = GROUP,
      y = MeanPct,
      ymin = MeanPct,
      ymax = MeanPct
    ),
    width = 0.35,
    color = "black",
    fatten = 1
  ) +
  geom_errorbar(
    data = MyD88IntegratedPctMean,
    aes(
      x = GROUP,
      y = MeanPct,
      ymin = MeanPct - SE_Pct,
      ymax = MeanPct + SE_Pct
    ),
    width = 0.25,
    color = "black"
  ) +
  scale_y_continuous(limits = c(-0.02,6),breaks = seq(0,6,1)) +
  labs(
    x = "Groups",
    y = "Percent of MyD88 greater than or equal to cutoff (%)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  ) +
  ggsave(
    "Percent of MyD88 greater than or equal to cutoff.pdf",
    width = 3,
    height = 4
  )

#2 off on grid integrated-mean (off above below cutoff, on all)
ggplot() +
  geom_violin(
    data = pp65_analysis_normalized,
    aes(
      x = New_Group,
      y = Normalized_Mean_pp65,
      color = New_Group,
      fill = New_Group
    ),
    size = 0.3,
    alpha = 0.4,
    scale = "width"
  ) +
  geom_jitter(
    data = pp65MeanIntbyRep,
    aes(
      x = New_Group,
      y = Mean_Normalized_Mean_pp65,
      fill = New_Group
    ),
    color = "black",
    size = 4,
    shape = 21
  ) +
  geom_crossbar(
    data = pp65MeanIntMeanBar,
    aes(
      x = New_Group,
      y = Mean_Normalized_Mean_pp65,
      ymin = Mean_Normalized_Mean_pp65,
      ymax = Mean_Normalized_Mean_pp65
    ),
    width = 0.35,
    color = "black",
    fatten = 1
  ) +
  geom_errorbar(
    data = pp65MeanIntMeanBar,
    aes(
      x = New_Group,
      y = Mean_Normalized_Mean_pp65,
      ymin = Mean_Normalized_Mean_pp65 - SE_Normalized_Mean_pp65,
      ymax = Mean_Normalized_Mean_pp65 + SE_Normalized_Mean_pp65
    ),
    width = 0.25,
    color = "black"
  ) +
  labs(
    x = "Groups",
    y = "pp65 Normalized Mean Intensity (a.u.)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_blank()
  ) +
  ggsave(
    "pp65Mean_myd88Integrated with cutoff.pdf",
    width = 5,
    height = 4
  )
