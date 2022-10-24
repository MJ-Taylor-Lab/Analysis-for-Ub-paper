library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(parallel)
library(scales)
library(plotrix)

RecruitmentTimeTable <- fread("~")

RecruitmentTimeTable <-
  RecruitmentTimeTable %>%
  mutate(QUERY_PROTEIN = factor(QUERY_PROTEIN, levels = c("TRAF6", "HOIL1")))

setwd("~")

#Count means
RecruitmentTimeMean <- RecruitmentTimeTable %>%
  group_by(QUERY_PROTEIN, CELL) %>%
  summarize(RECRUITMENT_TIME = mean(RECRUITMENT_TIME)) %>%
  group_by(QUERY_PROTEIN) %>%
  summarize(
    SE_MeanRecruitmentTime = std.error(RECRUITMENT_TIME),
    SD_MeanRecruitmentTime = sd(RECRUITMENT_TIME),
    MeanRecruitmentTime = mean(RECRUITMENT_TIME)
    )

write.csv(RecruitmentTimeMean, "RecruitmentTimeMean.csv", row.names = F, )

limits <- function(x) {x*100}

ggplot() +
  geom_histogram(
    data = RecruitmentTimeTable %>% filter(QUERY_PROTEIN == "TRAF6"),
    binwidth = 20,
    aes(
      x = RECRUITMENT_TIME,
      y = ..count..
    ),
    color = "#b35806",
    fill = "#b35806",
    alpha = 0.7
  ) +
  geom_density(
    data = RecruitmentTimeTable %>% filter(QUERY_PROTEIN == "TRAF6"),
    aes(
      x = RECRUITMENT_TIME,
      y = ..count..*20,
    )
  ) +
  geom_histogram(
    data = RecruitmentTimeTable %>% filter(QUERY_PROTEIN == "HOIL1"),
    binwidth = 20,
    aes(
      x = RECRUITMENT_TIME,
      y = ..count..
    ),
    color = "#8073ac",
    fill = "#8073ac",
    alpha = 0.7
  ) +
  geom_density(
    data = RecruitmentTimeTable %>% filter(QUERY_PROTEIN == "HOIL1"),
    aes(
      x = RECRUITMENT_TIME,
      y = ..count..*20,
    )
  ) +
  geom_vline(
    data = RecruitmentTimeMean,
    aes(
      xintercept = MeanRecruitmentTime
    ),
    color = "black",
    linetype = "dashed"
  ) +
  scale_x_continuous(limits = c(-10, 400)) +
  facet_wrap(~QUERY_PROTEIN, nrow = 2, strip.position = "right") +
  labs(
    x = "Recruitment Time (s)",
    y = "Counts",
    color = "QUERY_PROTEIN",
    fill = "QUERY_PROTEIN"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank(),
    strip.background = element_blank()
  ) +
ggsave(
    "Recruitment Time by Counts.pdf",
    width = 5,
    height = 2.7
  )
