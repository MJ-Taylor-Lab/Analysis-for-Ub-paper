library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(parallel)
library(ggdark)
library(plotrix)

GrandColoSummary1 <- fread("~")
GrandColoSummary2 <- fread("~")
GrandColoSummary <- bind_rows(GrandColoSummary1, GrandColoSummary2)

setwd("~")

# Rearrange row order
GrandColoSummary <-
  GrandColoSummary %>% 
  arrange(
    IMAGE,
    CELL,
    TRACK_ID,
    FRAME
  ) %>% 
  mutate(
    PROTEIN = factor(PROTEIN, levels = c("MyD88", "TRAF6", "HOIL1")),
    GROUP = factor(GROUP, levels = c("MyD88 TRAF6", "MyD88 HOIL1", "MyD88 HOIL1G2p5on", "MyD88 HOIL1G1on", "MyD88 HOIL1Goff"))
  ) %>% 
  filter(
######  Remember to adjust ligand density ######
    #LIGAND_DENSITY_CAT == 32,
    !is.na(GROUP)
  )

ColocalizedSpots <-
  GrandColoSummary %>%
  filter(
    PROTEIN == "MyD88",
    FRAMES_ADJUSTED == 0,
    !is.na(GROUP)
  ) %>%
  arrange(
    MAX_NORMALIZED_INTENSITY
  ) %>%
  mutate(
    MAX_MyddosomeNumber = MAX_NORMALIZED_INTENSITY/4.5
  )  %>%
  mutate(
    MAX_MyddosomeNumber = round(MAX_MyddosomeNumber)
  )

write.csv(ColocalizedSpots, "ColocalizedSpots_MyddosomeSize.csv", row.names = F, )

#Calculate colocalization
ThresholdFx <- function(ThresholdX){
  TempColocalizedSpots <-
    ColocalizedSpots %>% 
    filter(
      MAX_MyddosomeNumber >= ThresholdX
    ) %>%
    group_by(
      GROUP,
      IMAGE,
      CELL
    ) %>% 
    summarize(
      COLOCALIZATION = mean(COLOCALIZATION),
    ) %>%
    group_by(
      GROUP,
      IMAGE
    ) %>%
    summarize(
      COLOCALIZATION = mean(COLOCALIZATION),
    ) %>%
    group_by(
      GROUP
    ) %>%
    summarize(
      SD_COLOCALIZATION = sd(COLOCALIZATION),
      SE_COLOCALIZATION = std.error(COLOCALIZATION),
      COLOCALIZATION = mean(COLOCALIZATION)
    )
  
  TempColocalizedSpots$MAX_MyddosomeNumber = ThresholdX
  
  return(TempColocalizedSpots)
}
ThresholdList <- unique(ColocalizedSpots$MAX_MyddosomeNumber)
Results <- mclapply(ThresholdList, ThresholdFx)
Results <- rbindlist(Results)

write.csv(Results, "ColocalizedAboveThresold_MyddosomeSize.csv", row.names = F, )

#Calculate colocalization by replicates 
ThresholdFx <- function(ThresholdX){
  TempColocalizedSpots <-
    ColocalizedSpots %>% 
    filter(
      MAX_MyddosomeNumber >= ThresholdX
    ) %>%
    group_by(
      GROUP,
      IMAGE,
      CELL
    ) %>% 
    summarize(
      COLOCALIZATION = mean(COLOCALIZATION),
    ) %>%
    group_by(
      GROUP,
      IMAGE
    ) %>%
    summarize(
      SD_COLOCALIZATION = sd(COLOCALIZATION),
      COLOCALIZATION = mean(COLOCALIZATION)
    )
  
  TempColocalizedSpots$MAX_MyddosomeNumber = ThresholdX
  
  return(TempColocalizedSpots)
}
ThresholdList <- unique(ColocalizedSpots$MAX_MyddosomeNumber)
Results2 <- mclapply(ThresholdList, ThresholdFx)
Results2 <- rbindlist(Results2)

# Save
write.csv(Results, "ColocalizedAboveThresold_MyddosomeSize_with replicates.csv", row.names = F, )

limits <- function(x) {
  x*100
}

AddSymbolFx <- function(x) {
  paste("≥", x)
}

#Plot
ggplot(
  Results,
  aes(
    x = MAX_MyddosomeNumber,
    y = COLOCALIZATION,
    color = GROUP
  )
) +
  geom_ribbon(
    aes(
      x = MAX_MyddosomeNumber,
      ymin = COLOCALIZATION - SE_COLOCALIZATION,
      ymax = COLOCALIZATION + SE_COLOCALIZATION,
      fill = GROUP
    ),
    alpha = 0.4,
    color =NA
  )+
  geom_line(size=0.75) +
  scale_x_continuous(
    limits = c(0, 18),
    breaks = seq(0,18,3),
    labels = AddSymbolFx
  ) +
  scale_y_continuous(
    limits = c(0, 1.04),
    label = limits,
    #labels = scales::percent
    breaks = seq(0,1.04,0.2)
  ) +
  scale_color_manual(values = c("#b35806", "#8073ac")) +
  scale_fill_manual(values = c("#b35806", "#8073ac")) +
  labs(
    x = "Number of Myddosomes",
    y = "% of MyD88 Colocalized \n with TRAF6/HOIL1",
    color = ""
  ) +
  theme_classic() +
  theme(
    #legend.position = "bottom"
    legend.position="none" #remove legend
  )  +
  ggsave(
    "ColocalizedAboveThreshold_MyddosomeSize_linearscale.svg",
    width = 6,
    height = 3
  ) +
  ggsave(
    "ColocalizedAboveThreshold_MyddosomeSize_linearscale.pdf",
    width = 6,
    height = 3
  )

#Plots by replicates
ggplot() +
  geom_line(
    data = Results2 %>% filter(GROUP == "MyD88 TRAF6"),
    aes(
      x = MAX_MyddosomeNumber,
      y = COLOCALIZATION,
      color = IMAGE
    ),
    size=0.75
  ) +
  scale_x_continuous(
    limits = c(0, 18), breaks = seq(0,18,3),
    labels = AddSymbolFx
  ) +
  scale_y_continuous(
    limits = c(0, 1.02),
    label = limits
    #labels = scales::percent
  ) +
  scale_color_brewer(palette = "YlOrBr") + #For TRAF6
  labs(
    x = "Number of Myddosomes",
    y = "% of MyD88 Colocalized with TRAF6",
    color = "Replicates"
  ) +
  theme_classic() +
  theme(
    #legend.position = c(0.7,0.3)
    legend.position="none" #remove legend
  )  +
  ggsave(
    "ColocalizedAboveThreshold_MyddosomeSize_TRAF6-replicates_linearscale.svg",
    width = 5,
    height = 3
  ) +
  ggsave(
    "ColocalizedAboveThreshold_MyddosomeSize_TRAF6-replicates_linearscale.pdf",
    width = 5,
    height = 3
  )

ggplot() +
  geom_line(
    data = Results2 %>% filter(GROUP == "MyD88 HOIL1"),
    aes(
      x = MAX_MyddosomeNumber,
      y = COLOCALIZATION,
      color = IMAGE
    ),
    size=0.75
  ) +
  scale_x_continuous(
    limits = c(0, 18), breaks = seq(0,18,3),
    labels = AddSymbolFx
  ) +
  scale_y_continuous(
    limits = c(0, 1.02),
    label = limits
    #labels = scales::percent
  ) +
  scale_color_brewer(palette = "Purples") + #For HOIL1
  labs(
    x = "Number of Myddosomes",
    y = "% of MyD88 Colocalized with HOIL1",
    color = "Replicates"
  ) +
  theme_classic() +
  theme(
    #legend.position = c(1, 0.2)
    legend.position="none" #remove legend
  )  +
  ggsave(
    "ColocalizedAboveThreshold_MyddosomeSize_HOIL1-replicates_linearscale.svg",
    width = 5,
    height = 3
  ) +
  ggsave(
    "ColocalizedAboveThreshold_MyddosomeSize_HOIL1-replicates_linearscale.pdf",
    width = 5,
    height = 3
  )
