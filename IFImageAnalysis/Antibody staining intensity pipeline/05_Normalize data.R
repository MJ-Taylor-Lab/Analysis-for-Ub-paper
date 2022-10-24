library(data.table)
library(dplyr)

#"pp65" could be replaced with "pIKK",K63","M1" accordingly.

setwd("~")

pp65_analysis_editted <- fread("~/pp65_analysis_editted.csv")

Off_grid_20220209 <- pp65_analysis_editted %>% filter(GROUP == "off_grid", DATE == "20220209")

IntegratedMyD88_20220209_1Pct <- quantile(Off_grid_20220209$Intensity_IntegratedIntensity_MyD88, probs = 0.01)
IntegratedMyD88_20220209_99Pct <-quantile(Off_grid_20220209$Intensity_IntegratedIntensity_MyD88, probs = 0.99)
Integratedpp65_20220209_1Pct <- quantile(Off_grid_20220209$Intensity_IntegratedIntensity_pp65, probs = 0.01)
Integratedpp65_20220209_99Pct <-quantile(Off_grid_20220209$Intensity_IntegratedIntensity_pp65, probs = 0.99)
MeanMyD88_20220209_1Pct <- quantile(Off_grid_20220209$Intensity_MeanIntensity_MyD88, probs = 0.01)
MeanMyD88_20220209_99Pct <-quantile(Off_grid_20220209$Intensity_MeanIntensity_MyD88, probs = 0.99)
Meanpp65_20220209_1Pct <- quantile(Off_grid_20220209$Intensity_MeanIntensity_pp65, probs = 0.01)
Meanpp65_20220209_99Pct <-quantile(Off_grid_20220209$Intensity_MeanIntensity_pp65, probs = 0.99)

pp65_20220209 <- pp65_analysis_editted %>% filter(DATE == "20220209") %>%
  mutate(
    Normalized_Integrated_MyD88 = (Intensity_IntegratedIntensity_MyD88-IntegratedMyD88_20220209_1Pct)/(IntegratedMyD88_20220209_99Pct-IntegratedMyD88_20220209_1Pct),
    Normalized_Integrated_pp65 = (Intensity_IntegratedIntensity_pp65-Integratedpp65_20220209_1Pct)/(Integratedpp65_20220209_99Pct-Integratedpp65_20220209_1Pct),
    Normalized_Mean_MyD88 = (Intensity_MeanIntensity_MyD88-MeanMyD88_20220209_1Pct)/(MeanMyD88_20220209_99Pct-MeanMyD88_20220209_1Pct),
    Normalized_Mean_pp65 = (Intensity_MeanIntensity_pp65-Meanpp65_20220209_1Pct)/(Meanpp65_20220209_99Pct-Meanpp65_20220209_1Pct)
    )

Off_grid_20220216 <- pp65_analysis_editted %>% filter(GROUP == "off_grid", DATE == "20220216")

IntegratedMyD88_20220216_1Pct <- quantile(Off_grid_20220216$Intensity_IntegratedIntensity_MyD88, probs = 0.01)
IntegratedMyD88_20220216_99Pct <-quantile(Off_grid_20220216$Intensity_IntegratedIntensity_MyD88, probs = 0.99)
Integratedpp65_20220216_1Pct <- quantile(Off_grid_20220216$Intensity_IntegratedIntensity_pp65, probs = 0.01)
Integratedpp65_20220216_99Pct <-quantile(Off_grid_20220216$Intensity_IntegratedIntensity_pp65, probs = 0.99)
MeanMyD88_20220216_1Pct <- quantile(Off_grid_20220216$Intensity_MeanIntensity_MyD88, probs = 0.01)
MeanMyD88_20220216_99Pct <-quantile(Off_grid_20220216$Intensity_MeanIntensity_MyD88, probs = 0.99)
Meanpp65_20220216_1Pct <- quantile(Off_grid_20220216$Intensity_MeanIntensity_pp65, probs = 0.01)
Meanpp65_20220216_99Pct <-quantile(Off_grid_20220216$Intensity_MeanIntensity_pp65, probs = 0.99)

pp65_20220216 <- pp65_analysis_editted %>% filter(DATE == "20220216") %>%
  mutate(
    Normalized_Integrated_MyD88 = (Intensity_IntegratedIntensity_MyD88-IntegratedMyD88_20220216_1Pct)/(IntegratedMyD88_20220216_99Pct-IntegratedMyD88_20220216_1Pct),
    Normalized_Integrated_pp65 = (Intensity_IntegratedIntensity_pp65-Integratedpp65_20220216_1Pct)/(Integratedpp65_20220216_99Pct-Integratedpp65_20220216_1Pct),
    Normalized_Mean_MyD88 = (Intensity_MeanIntensity_MyD88-MeanMyD88_20220216_1Pct)/(MeanMyD88_20220216_99Pct-MeanMyD88_20220216_1Pct),
    Normalized_Mean_pp65 = (Intensity_MeanIntensity_pp65-Meanpp65_20220216_1Pct)/(Meanpp65_20220216_99Pct-Meanpp65_20220216_1Pct)
  )

Off_grid_20220805 <- pp65_analysis_editted %>% filter(GROUP == "off_grid", DATE == "20220805")

IntegratedMyD88_20220805_1Pct <- quantile(Off_grid_20220805$Intensity_IntegratedIntensity_MyD88, probs = 0.01)
IntegratedMyD88_20220805_99Pct <-quantile(Off_grid_20220805$Intensity_IntegratedIntensity_MyD88, probs = 0.99)
Integratedpp65_20220805_1Pct <- quantile(Off_grid_20220805$Intensity_IntegratedIntensity_pp65, probs = 0.01)
Integratedpp65_20220805_99Pct <-quantile(Off_grid_20220805$Intensity_IntegratedIntensity_pp65, probs = 0.99)
MeanMyD88_20220805_1Pct <- quantile(Off_grid_20220805$Intensity_MeanIntensity_MyD88, probs = 0.01)
MeanMyD88_20220805_99Pct <-quantile(Off_grid_20220805$Intensity_MeanIntensity_MyD88, probs = 0.99)
Meanpp65_20220805_1Pct <- quantile(Off_grid_20220805$Intensity_MeanIntensity_pp65, probs = 0.01)
Meanpp65_20220805_99Pct <-quantile(Off_grid_20220805$Intensity_MeanIntensity_pp65, probs = 0.99)

pp65_20220805 <- pp65_analysis_editted %>% filter(DATE == "20220805") %>%
  mutate(
    Normalized_Integrated_MyD88 = (Intensity_IntegratedIntensity_MyD88-IntegratedMyD88_20220805_1Pct)/(IntegratedMyD88_20220805_99Pct-IntegratedMyD88_20220805_1Pct),
    Normalized_Integrated_pp65 = (Intensity_IntegratedIntensity_pp65-Integratedpp65_20220805_1Pct)/(Integratedpp65_20220805_99Pct-Integratedpp65_20220805_1Pct),
    Normalized_Mean_MyD88 = (Intensity_MeanIntensity_MyD88-MeanMyD88_20220805_1Pct)/(MeanMyD88_20220805_99Pct-MeanMyD88_20220805_1Pct),
    Normalized_Mean_pp65 = (Intensity_MeanIntensity_pp65-Meanpp65_20220805_1Pct)/(Meanpp65_20220805_99Pct-Meanpp65_20220805_1Pct)
  )

pp65_analysis_normalized <- bind_rows(pp65_20220209, pp65_20220216, pp65_20220805)

write.csv(pp65_analysis_normalized, "pp65_analysis_normalized.csv", row.names = F, )

