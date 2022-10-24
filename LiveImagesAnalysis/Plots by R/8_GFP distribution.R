pacman::p_load(dplyr, data.table, ggplot2, viridis)

GFP <- fread("/Users/u_cao/Desktop/IL1 signaling project/Figure/z-Analysis/1_HOIL1/20220426/7_GFP distribution/Calibration Data GFP/20200821 GFP calibration_10pct_60ms 009_CalibrationData_GFP_HOIL1_TRAF6_using.csv.gz")

GFP_mean <- mean(GFP$TOTAL_INTENSITY)
GFP_sd <- sd(GFP$TOTAL_INTENSITY)

ggplot() +
  geom_rect(
    aes(
      xmin = 4.5 * GFP_mean,
      xmax = 9 * GFP_mean,
      ymin = 0,
      ymax = Inf),
    fill = "#377eb8",
    alpha = 0.25
  ) +
  geom_rect(
    aes(
      xmin = 9 * GFP_mean,
      xmax = Inf,
      ymin = 0,
      ymax = Inf),
    fill = "#377eb8",
    alpha = 0.5
  ) +
  geom_density(
    data = GFP,
    aes(
      x = TOTAL_INTENSITY,
      color = "GFP"
    )
  ) +
  geom_vline(
    xintercept = c(4.5, 9)*GFP_mean,
    linetype = "dashed"
  ) +
  stat_function(
    aes(color = "6x GFP"),
    fun = dnorm, n = 10^4, args = list(mean = GFP_mean*6, sd = sqrt(6*GFP_sd^2))
  ) +
  stat_function(
    aes(color = "12x GFP"),
    fun = dnorm, n = 10^4, args = list(mean = GFP_mean*12, sd = sqrt(12*GFP_sd^2))
  ) +
  scale_color_viridis(
    discrete = TRUE,
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(0, 1500)
  ) +
  labs(
    x = "Intensity (a.u.)",
    y = "Density"
  ) +
  theme_classic() +
  ggsave(
    "GFP distribution.pdf",
    width = 4,
    height = 3
  )
