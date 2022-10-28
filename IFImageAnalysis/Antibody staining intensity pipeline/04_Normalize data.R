library(data.table)
library(dplyr)

#"pp65" could be replaced with "pIKK",K63","M1" accordingly.

setwd("~")

pp65_analysis <- fread("~/Analysis by CellProfiler/pp65_analysis.csv")

#Add additional columns "GROUP", "REP" and "DATE"
pp65_analysis_Off <- pp65_analysis %>% filter(grepl('off grid', PathName_Staining))
pp65_analysis_Off$GROUP <- c("off_grid")
pp65_analysis_1um <- pp65_analysis %>% filter(grepl('on 1um grid', PathName_Staining))
pp65_analysis_1um$GROUP <- c("1um_grid")
pp65_analysis_2.5um <- pp65_analysis %>% filter(grepl('on 2.5um grid', PathName_Staining))
pp65_analysis_2.5um$GROUP <- c("2.5um_grid")

pp65_analysis <- bind_rows(pp65_analysis_Off, pp65_analysis_1um, pp65_analysis_2.5um)

pp65_analysis_Rep1 <- pp65_analysis %>% filter(grepl('Rep1', PathName_Staining)) 
pp65_analysis_Rep1$REP <- c("Rep1")
pp65_analysis_Rep2 <- pp65_analysis %>% filter(grepl('Rep2', PathName_Staining)) 
pp65_analysis_Rep2$REP <- c("Rep2")
pp65_analysis_Rep3 <- pp65_analysis %>% filter(grepl('Rep3', PathName_Staining)) 
pp65_analysis_Rep3$REP <- c("Rep3")
pp65_analysis_Rep4 <- pp65_analysis %>% filter(grepl('Rep4', PathName_Staining)) 
pp65_analysis_Rep4$REP <- c("Rep4")

pp65_analysis <- bind_rows(pp65_analysis_Rep1, pp65_analysis_Rep2, pp65_analysis_Rep3, pp65_analysis_Rep4)

pp65_analysis_20220209 <- pp65_analysis %>% filter(grepl('20220209', PathName_Staining)) 
pp65_analysis_20220209$DATE <- c("20220209")
pp65_analysis_20220216 <- pp65_analysis %>% filter(grepl('20220216', PathName_Staining)) 
pp65_analysis_20220216$DATE <- c("20220216")
pp65_analysis_20220805 <- pp65_analysis %>% filter(grepl('20220805', PathName_Staining)) 
pp65_analysis_20220805$DATE <- c("20220805")

pp65_analysis <- bind_rows(pp65_analysis_20220209, pp65_analysis_20220216, pp65_analysis_20220805)

#Select columns needed
pp65_analysis_editted <- pp65_analysis %>%
  select(GROUP, DATE, REP,
         Intensity_IntegratedIntensity_MyD88,Intensity_IntegratedIntensity_pp65,
         Intensity_MeanIntensity_MyD88,Intensity_MeanIntensity_pp65,
         ImageNumber, ObjectNumber, FileName_Staining, PathName_Staining)

write.csv(pp65_analysis, "pp65_analysis_editted.csv", row.names = F, )

#Normalized data

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
