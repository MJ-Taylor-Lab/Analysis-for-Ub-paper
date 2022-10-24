library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(parallel)
library(scales)
library(ggbeeswarm)
library(ggdark)
library(plotrix)

setwd("~")

Unstimulated <- fread("~/MyExpt_FilteredNuclei_0_unstimulated.csv")
On1um <- fread("~/MyExpt_FilteredNuclei_1_1µm grid.csv")
On2p5um <- fread("~/MyExpt_FilteredNuclei_3_2p5µm grid.csv")
Offgrid <- fread("~/MyExpt_FilteredNuclei_2_off grid.csv")

Offgrid_20220222 <- Offgrid %>% filter(Date == "20220222")
Math_IntensityRatio_5Pct <- quantile(Offgrid_20220222$Math_IntensityRatio, probs = 0.05)
Math_IntensityRatio_95Pct <- quantile(Offgrid_20220222$Math_IntensityRatio, probs = 0.95)

Unstimulated_20220222 <- Unstimulated %>% filter(Date == "20220222") %>%
  mutate(Normalized_Math_IntensityRatio = (Math_IntensityRatio - Math_IntensityRatio_5Pct)/(Math_IntensityRatio_95Pct - Math_IntensityRatio_5Pct))
On1um_20220222 <- On1um %>% filter(Date == "20220222") %>%
  mutate(Normalized_Math_IntensityRatio = (Math_IntensityRatio - Math_IntensityRatio_5Pct)/(Math_IntensityRatio_95Pct - Math_IntensityRatio_5Pct))
On2p5um_20220222 <- On2p5um %>% filter(Date == "20220222") %>%
  mutate(Normalized_Math_IntensityRatio = (Math_IntensityRatio - Math_IntensityRatio_5Pct)/(Math_IntensityRatio_95Pct - Math_IntensityRatio_5Pct))
Offgrid_20220222 <- Offgrid %>% filter(Date == "20220222") %>%
  mutate(Normalized_Math_IntensityRatio = (Math_IntensityRatio - Math_IntensityRatio_5Pct)/(Math_IntensityRatio_95Pct - Math_IntensityRatio_5Pct))

Offgrid_20220315 <- Offgrid %>% filter(Date == "20220315") %>% drop_na(Math_IntensityRatio)
Math_IntensityRatio_5Pct <- quantile(Offgrid_20220315$Math_IntensityRatio, probs = 0.05)
Math_IntensityRatio_95Pct <- quantile(Offgrid_20220315$Math_IntensityRatio, probs = 0.95)

Unstimulated_20220315 <- Unstimulated %>% filter(Date == "20220315") %>%
  mutate(Normalized_Math_IntensityRatio = (Math_IntensityRatio - Math_IntensityRatio_5Pct)/(Math_IntensityRatio_95Pct - Math_IntensityRatio_5Pct))
On1um_20220315 <- On1um %>% filter(Date == "20220315") %>%
  mutate(Normalized_Math_IntensityRatio = (Math_IntensityRatio - Math_IntensityRatio_5Pct)/(Math_IntensityRatio_95Pct - Math_IntensityRatio_5Pct))
On2p5um_20220315 <- On2p5um %>% filter(Date == "20220315") %>%
  mutate(Normalized_Math_IntensityRatio = (Math_IntensityRatio - Math_IntensityRatio_5Pct)/(Math_IntensityRatio_95Pct - Math_IntensityRatio_5Pct))
Offgrid_20220315 <- Offgrid %>% filter(Date == "20220315") %>%
  mutate(Normalized_Math_IntensityRatio = (Math_IntensityRatio - Math_IntensityRatio_5Pct)/(Math_IntensityRatio_95Pct - Math_IntensityRatio_5Pct))

FilteredNuclei1 <- bind_rows(Unstimulated_20220222,Unstimulated_20220315)
FilteredNuclei2 <- bind_rows(On1um_20220222,On1um_20220315)
FilteredNuclei3 <- bind_rows(On2p5um_20220222,On2p5um_20220315)
FilteredNuclei4 <- bind_rows(Offgrid_20220222,Offgrid_20220315)

write.csv(FilteredNuclei1, "Normalized_Unstimulated.csv", row.names = F, )
write.csv(FilteredNuclei2, "Normalized_On1um.csv", row.names = F, )
write.csv(FilteredNuclei3, "Normalized_On2p5um.csv", row.names = F, )
write.csv(FilteredNuclei4, "Normalized_Offgrid.csv", row.names = F, )

FilteredNuclei <- bind_rows(FilteredNuclei1, FilteredNuclei2, FilteredNuclei3,FilteredNuclei4) %>%
  mutate(
    ImageCategory = factor(ImageCategory, levels = c("Off Grid", "2.5um Grid", "1um Grid", "Unstimulated"))
  )

MeanBar1 <- 
  FilteredNuclei %>%
  group_by(
    ImageCategory,
    ImageReplicate
  ) %>%
  summarize(
    Mean1Normalized_Math_IntensityRatio = mean(Normalized_Math_IntensityRatio, na.rm = T)
  )

MeanBar2 <- 
  FilteredNuclei %>%
  group_by(
    ImageCategory,
    ImageReplicate
  ) %>%
  summarize(
    Normalized_Math_IntensityRatio = mean(Normalized_Math_IntensityRatio, na.rm = T)
  ) %>%
  group_by(
    ImageCategory
  ) %>%
  summarize(
    SE_Normalized_Math_IntensityRatio = std.error(Normalized_Math_IntensityRatio),
    Mean2Normalized_Math_IntensityRatio = mean(Normalized_Math_IntensityRatio)
  )

write.csv(MeanBar1, "MeanBar1.csv", row.names = F, )
write.csv(MeanBar2, "MeanBar2.csv", row.names = F, )

stat.test <- ggpubr::compare_means(
  Mean1Normalized_Math_IntensityRatio ~ ImageCategory, data = MeanBar1,
  method = "t.test" #for non-parametric, use wilcox.test
)

write.csv(stat.test, "stat.test.csv", row.names = F, )

ggplot() +
  geom_violin(
    data = FilteredNuclei,
    aes(
      x = ImageCategory,
      y = Normalized_Math_IntensityRatio,
      color = ImageCategory,
      fill = ImageCategory
    ),
    size = 0.3,
    alpha = 0.4,
    scale = "width"
  ) +
  geom_jitter(
    data = MeanBar1,
    aes(
      x = ImageCategory,
      y = Mean1Normalized_Math_IntensityRatio,
      color = ImageCategory,
      fill = ImageCategory
    )
  ) +
  geom_crossbar(
    data = MeanBar2,
    aes(
      x = ImageCategory,
      y = Mean2Normalized_Math_IntensityRatio,
      ymin = Mean2Normalized_Math_IntensityRatio,
      ymax = Mean2Normalized_Math_IntensityRatio
      # color = GROUP,
      # fill = GROUP
    ),
    color = "black",
    width = 0.3,
    fatten = 1
  ) +
  geom_errorbar(
    data = MeanBar2,
    aes(
      x = ImageCategory,
      y = Mean2Normalized_Math_IntensityRatio,
      ymin = Mean2Normalized_Math_IntensityRatio - SE_Normalized_Math_IntensityRatio,
      ymax = Mean2Normalized_Math_IntensityRatio + SE_Normalized_Math_IntensityRatio
      # color = GROUP,
      # fill = GROUP
    ),
    color = "black",
    width = 0.2
  ) +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0,1.2,0.2)) +
  scale_color_brewer(
    palette = "Set1"
  ) +
  scale_fill_brewer(
    palette = "Set1"
  ) +
  labs(
    y = "RelA Nucleus to Cytosol Ratio",
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
    "RelA Nucleus2Cytosol Ratio_Normalized.pdf",
    # width = 12,
    # height = 9
    width = 3.2,
    height = 3.5
  )
