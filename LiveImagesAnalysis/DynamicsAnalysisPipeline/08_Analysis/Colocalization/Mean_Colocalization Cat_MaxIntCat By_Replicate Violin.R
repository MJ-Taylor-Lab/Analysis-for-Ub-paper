setwd(FOLDER)

#Supplementary figure
ggplot() +
  geom_violin(
    data = CellTracksSummary,
    aes(
      x = as.factor(IMAGENUMBER),
      y = MEAN
    ),
    color = "darkgrey",
    fill = "darkgrey",
    scale = "width"
  ) +
  geom_jitter(
    data = CellTracksSummary,
    aes(
      x = as.factor(IMAGENUMBER),
      y = MEAN
    ),
    #fill = "#d7191c", #TRAF6
    fill = "#2c7bb6", #HOIL1
    color = "black",
    size = 2,
    shape = 21
  ) +
  geom_crossbar(
    data = CellTracksImgSummary,
    aes(
      x = as.factor(IMAGENUMBER),
      ymin = MEAN,
      y = MEAN,
      ymax = MEAN
    ),
    width = 0.25,
    color = "black"
  ) +
  facet_wrap(~MAX_INTENSITY_CAT,nrow = 1) +
  scale_y_continuous( 
    limits = c(0, 1),
    label = limits
    #label = mult_format
  ) +
  labs(
    y = paste(Y_AXIS_TITLE, "(% ± S.E.M.)"),
    x = "Replicate",
    shape = "Replicate",
    color = "Replicate"
  ) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_blank()
  ) +
  ggsave(
    file = paste(lp.PROTEIN, " Mean_Colocalization Cat_MaxInt By_Replicate Violin ",DATE_TODAY,  ".svg", sep = ""),
    #width = 4*(44/62),
    width = 4,
    height = 4
  ) +
  ggsave(
    file = paste(lp.PROTEIN, " Mean_Colocalization Cat_MaxInt By_Replicate Violin ",DATE_TODAY,  ".pdf", sep = ""),
    #width = 4*(44/62),
    width = 4,
    height = 4
  )