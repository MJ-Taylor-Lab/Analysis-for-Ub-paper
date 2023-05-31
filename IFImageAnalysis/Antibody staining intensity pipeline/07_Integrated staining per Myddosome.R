library(data.table)
library(dplyr)
library(ggplot2)
library(plotrix)

setwd("/Users/u_cao/Downloads")

#pIKK per Myddosome ----
pIKK <- fread("/Users/u_cao/Desktop/202302-05_update BioRxiv_EMBOR revision/EMBOR revision/Immunofluoresence/pIKK_2_analysis/5_Analysis and plots/pIKK_analysis_normalized.csv")

pIKK_Table <- pIKK %>%
  mutate(MyddosomeNumber = round(Normalized_Integrated_MyD88/0.5, digits = 3)) %>%
  filter(MyddosomeNumber > 0)

pIKK_Table_Single_Mean <- pIKK_Table %>%
  filter(MyddosomeNumber > 0, MyddosomeNumber <= 1) %>%
  mutate(pIKK_per_Myddosome = Normalized_Integrated_pIKK/1) %>%
  group_by(GROUP,REP) %>%
  summarise(pIKK_per_Myddosome = mean(pIKK_per_Myddosome)) %>%
  group_by(GROUP) %>%
  summarise(SD_pIKK_per_Myddosome = sd(pIKK_per_Myddosome),
            pIKK_per_Myddosome = mean(pIKK_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 1)

pIKK_Table_Three_Mean <- pIKK_Table %>%
  filter(MyddosomeNumber > 1, MyddosomeNumber <= 3) %>%
  mutate(pIKK_per_Myddosome = Normalized_Integrated_pIKK/3) %>%
  group_by(GROUP,REP) %>%
  summarise(pIKK_per_Myddosome = mean(pIKK_per_Myddosome)) %>%
  group_by(GROUP) %>%
  summarise(SD_pIKK_per_Myddosome = sd(pIKK_per_Myddosome),
            pIKK_per_Myddosome = mean(pIKK_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 3)

pIKK_Table_Five_Mean <- pIKK_Table %>%
  filter(MyddosomeNumber > 3, MyddosomeNumber <= 5) %>%
  mutate(pIKK_per_Myddosome = Normalized_Integrated_pIKK/5) %>%
  group_by(GROUP,REP) %>%
  summarise(pIKK_per_Myddosome = mean(pIKK_per_Myddosome)) %>%
  group_by(GROUP) %>%
  summarise(SD_pIKK_per_Myddosome = sd(pIKK_per_Myddosome),
            pIKK_per_Myddosome = mean(pIKK_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 5)

pIKK_Table_MydNumber_Mean <- bind_rows(pIKK_Table_Single_Mean,pIKK_Table_Three_Mean,pIKK_Table_Five_Mean) %>%
  arrange(GROUP)

write.csv(pIKK_Table_MydNumber_Mean, "pIKK_Table_MydNumber_Mean.csv", row.names = F, )

pIKK_Table_Single_Rep <- pIKK_Table %>%
  filter(MyddosomeNumber > 0, MyddosomeNumber <= 1) %>%
  mutate(pIKK_per_Myddosome = Normalized_Integrated_pIKK/1) %>%
  group_by(GROUP,REP) %>%
  summarise(pIKK_per_Myddosome = mean(pIKK_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 1)

pIKK_Table_Three_Rep <- pIKK_Table %>%
  filter(MyddosomeNumber > 1, MyddosomeNumber <= 3) %>%
  mutate(pIKK_per_Myddosome = Normalized_Integrated_pIKK/3) %>%
  group_by(GROUP,REP) %>%
  summarise(pIKK_per_Myddosome = mean(pIKK_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 3)

pIKK_Table_Five_Rep <- pIKK_Table %>%
  filter(MyddosomeNumber > 3, MyddosomeNumber <= 5) %>%
  mutate(pIKK_per_Myddosome = Normalized_Integrated_pIKK/5) %>%
  group_by(GROUP,REP) %>%
  summarise(pIKK_per_Myddosome = mean(pIKK_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 5)

pIKK_Table_MydNumber_Rep <- bind_rows(pIKK_Table_Single_Rep,pIKK_Table_Three_Rep,pIKK_Table_Five_Rep) %>%
  arrange(GROUP)

write.csv(pIKK_Table_MydNumber_Rep, "pIKK_Table_MydNumber_Rep.csv", row.names = F, )

pIKK_Table_Single <- pIKK_Table %>%
  filter(MyddosomeNumber > 0, MyddosomeNumber <= 1) %>%
  mutate(pIKK_per_Myddosome = Normalized_Integrated_pIKK/1) %>%
  mutate(MyddosomeNumber = 1)

pIKK_Table_Three <- pIKK_Table %>%
  filter(MyddosomeNumber > 1, MyddosomeNumber <= 3) %>%
  mutate(pIKK_per_Myddosome = Normalized_Integrated_pIKK/3) %>%
  mutate(MyddosomeNumber = 3)

pIKK_Table_Five <- pIKK_Table %>%
  filter(MyddosomeNumber > 3, MyddosomeNumber <= 5) %>%
  mutate(pIKK_per_Myddosome = Normalized_Integrated_pIKK/5) %>%
  mutate(MyddosomeNumber = 5)

pIKK_Table_MydNumber <- bind_rows(pIKK_Table_Single,pIKK_Table_Three,pIKK_Table_Five) %>%
  arrange(GROUP)

pIKK_Counts <- pIKK_Table_MydNumber %>%
  group_by(GROUP, REP, MyddosomeNumber) %>%
  summarise(Counts = NROW(GROUP)) %>%
  arrange(MyddosomeNumber)

write.csv(pIKK_Table_MydNumber, "pIKK_Table_MydNumber.csv", row.names = F, )
write.csv(pIKK_Counts, "pIKK_Counts.csv", row.names = F, )

#pp65 per Myddosome ----
pp65 <- fread("/Users/u_cao/Desktop/202302-05_update BioRxiv_EMBOR revision/EMBOR revision/Immunofluoresence/pp65_2_analysis/5_Analysis and plots/pp65_analysis_normalized.csv")

pp65_Table <- pp65 %>%
  mutate(MyddosomeNumber = round(Normalized_Integrated_MyD88/0.5, digits = 3)) %>%
  filter(MyddosomeNumber > 0)

pp65_Table_Single_Mean <- pp65_Table %>%
  filter(MyddosomeNumber > 0, MyddosomeNumber <= 1) %>%
  mutate(pp65_per_Myddosome = Normalized_Integrated_pp65/1) %>%
  group_by(GROUP,REP) %>%
  summarise(pp65_per_Myddosome = mean(pp65_per_Myddosome)) %>%
  group_by(GROUP) %>%
  summarise(SD_pp65_per_Myddosome = sd(pp65_per_Myddosome),
            pp65_per_Myddosome = mean(pp65_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 1)

pp65_Table_Three_Mean <- pp65_Table %>%
  filter(MyddosomeNumber > 1, MyddosomeNumber <= 3) %>%
  mutate(pp65_per_Myddosome = Normalized_Integrated_pp65/3) %>%
  group_by(GROUP,REP) %>%
  summarise(pp65_per_Myddosome = mean(pp65_per_Myddosome)) %>%
  group_by(GROUP) %>%
  summarise(SD_pp65_per_Myddosome = sd(pp65_per_Myddosome),
            pp65_per_Myddosome = mean(pp65_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 3)

pp65_Table_Five_Mean <- pp65_Table %>%
  filter(MyddosomeNumber > 3, MyddosomeNumber <= 5) %>%
  mutate(pp65_per_Myddosome = Normalized_Integrated_pp65/5) %>%
  group_by(GROUP,REP) %>%
  summarise(pp65_per_Myddosome = mean(pp65_per_Myddosome)) %>%
  group_by(GROUP) %>%
  summarise(SD_pp65_per_Myddosome = sd(pp65_per_Myddosome),
            pp65_per_Myddosome = mean(pp65_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 5)

pp65_Table_MydNumber_Mean <- bind_rows(pp65_Table_Single_Mean,pp65_Table_Three_Mean,pp65_Table_Five_Mean) %>%
  arrange(GROUP)

write.csv(pp65_Table_MydNumber_Mean, "pp65_Table_MydNumber_Mean.csv", row.names = F, )

pp65_Table_Single_Rep <- pp65_Table %>%
  filter(MyddosomeNumber > 0, MyddosomeNumber <= 1) %>%
  mutate(pp65_per_Myddosome = Normalized_Integrated_pp65/1) %>%
  group_by(GROUP,REP) %>%
  summarise(pp65_per_Myddosome = mean(pp65_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 1)

pp65_Table_Three_Rep <- pp65_Table %>%
  filter(MyddosomeNumber > 1, MyddosomeNumber <= 3) %>%
  mutate(pp65_per_Myddosome = Normalized_Integrated_pp65/3) %>%
  group_by(GROUP,REP) %>%
  summarise(pp65_per_Myddosome = mean(pp65_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 3)

pp65_Table_Five_Rep <- pp65_Table %>%
  filter(MyddosomeNumber > 3, MyddosomeNumber <= 5) %>%
  mutate(pp65_per_Myddosome = Normalized_Integrated_pp65/5) %>%
  group_by(GROUP,REP) %>%
  summarise(pp65_per_Myddosome = mean(pp65_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 5)

pp65_Table_MydNumber_Rep <- bind_rows(pp65_Table_Single_Rep,pp65_Table_Three_Rep,pp65_Table_Five_Rep) %>%
  arrange(GROUP)

write.csv(pp65_Table_MydNumber_Rep, "pp65_Table_MydNumber_Rep.csv", row.names = F, )

pp65_Table_Single <- pp65_Table %>%
  filter(MyddosomeNumber > 0, MyddosomeNumber <= 1) %>%
  mutate(pp65_per_Myddosome = Normalized_Integrated_pp65/1) %>%
  mutate(MyddosomeNumber = 1)

pp65_Table_Three <- pp65_Table %>%
  filter(MyddosomeNumber > 1, MyddosomeNumber <= 3) %>%
  mutate(pp65_per_Myddosome = Normalized_Integrated_pp65/3) %>%
  mutate(MyddosomeNumber = 3)

pp65_Table_Five <- pp65_Table %>%
  filter(MyddosomeNumber > 3, MyddosomeNumber <= 5) %>%
  mutate(pp65_per_Myddosome = Normalized_Integrated_pp65/5) %>%
  mutate(MyddosomeNumber = 5)

pp65_Table_MydNumber <- bind_rows(pp65_Table_Single,pp65_Table_Three,pp65_Table_Five) %>%
  arrange(GROUP)

pp65_Counts <- pp65_Table_MydNumber %>%
  group_by(GROUP, REP, MyddosomeNumber) %>%
  summarise(Counts = NROW(GROUP)) %>%
  arrange(MyddosomeNumber)

write.csv(pp65_Table_MydNumber, "pp65_Table_MydNumber.csv", row.names = F, )
write.csv(pp65_Counts, "pp65_Counts.csv", row.names = F, )

#M1 per Myddosome ----
M1 <- fread("/Users/u_cao/Desktop/202302-05_update BioRxiv_EMBOR revision/EMBOR revision/Immunofluoresence/M1_2_analysis/5_Analysis and plots/M1_analysis_normalized.csv")

M1_Table <- M1 %>%
  mutate(MyddosomeNumber = round(Normalized_Integrated_MyD88/0.5, digits = 3)) %>%
  filter(MyddosomeNumber > 0)

M1_Table_Single_Mean <- M1_Table %>%
  filter(MyddosomeNumber > 0, MyddosomeNumber <= 1) %>%
  mutate(M1_per_Myddosome = Normalized_Integrated_M1/1) %>%
  group_by(GROUP,REP) %>%
  summarise(M1_per_Myddosome = mean(M1_per_Myddosome)) %>%
  group_by(GROUP) %>%
  summarise(SD_M1_per_Myddosome = sd(M1_per_Myddosome),
            M1_per_Myddosome = mean(M1_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 1)

M1_Table_Three_Mean <- M1_Table %>%
  filter(MyddosomeNumber > 1, MyddosomeNumber <= 3) %>%
  mutate(M1_per_Myddosome = Normalized_Integrated_M1/3) %>%
  group_by(GROUP,REP) %>%
  summarise(M1_per_Myddosome = mean(M1_per_Myddosome)) %>%
  group_by(GROUP) %>%
  summarise(SD_M1_per_Myddosome = sd(M1_per_Myddosome),
            M1_per_Myddosome = mean(M1_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 3)

M1_Table_Five_Mean <- M1_Table %>%
  filter(MyddosomeNumber > 3, MyddosomeNumber <= 5) %>%
  mutate(M1_per_Myddosome = Normalized_Integrated_M1/5) %>%
  group_by(GROUP,REP) %>%
  summarise(M1_per_Myddosome = mean(M1_per_Myddosome)) %>%
  group_by(GROUP) %>%
  summarise(SD_M1_per_Myddosome = sd(M1_per_Myddosome),
            M1_per_Myddosome = mean(M1_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 5)

M1_Table_MydNumber_Mean <- bind_rows(M1_Table_Single_Mean,M1_Table_Three_Mean,M1_Table_Five_Mean) %>%
  arrange(GROUP)

write.csv(M1_Table_MydNumber_Mean, "M1_Table_MydNumber_Mean.csv", row.names = F, )

M1_Table_Single_Rep <- M1_Table %>%
  filter(MyddosomeNumber > 0, MyddosomeNumber <= 1) %>%
  mutate(M1_per_Myddosome = Normalized_Integrated_M1/1) %>%
  group_by(GROUP,REP) %>%
  summarise(M1_per_Myddosome = mean(M1_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 1)

M1_Table_Three_Rep <- M1_Table %>%
  filter(MyddosomeNumber > 1, MyddosomeNumber <= 3) %>%
  mutate(M1_per_Myddosome = Normalized_Integrated_M1/3) %>%
  group_by(GROUP,REP) %>%
  summarise(M1_per_Myddosome = mean(M1_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 3)

M1_Table_Five_Rep <- M1_Table %>%
  filter(MyddosomeNumber > 3, MyddosomeNumber <= 5) %>%
  mutate(M1_per_Myddosome = Normalized_Integrated_M1/5) %>%
  group_by(GROUP,REP) %>%
  summarise(M1_per_Myddosome = mean(M1_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 5)

M1_Table_MydNumber_Rep <- bind_rows(M1_Table_Single_Rep,M1_Table_Three_Rep,M1_Table_Five_Rep) %>%
  arrange(GROUP)

write.csv(M1_Table_MydNumber_Rep, "M1_Table_MydNumber_Rep.csv", row.names = F, )

M1_Table_Single <- M1_Table %>%
  filter(MyddosomeNumber > 0, MyddosomeNumber <= 1) %>%
  mutate(M1_per_Myddosome = Normalized_Integrated_M1/1) %>%
  mutate(MyddosomeNumber = 1)

M1_Table_Three <- M1_Table %>%
  filter(MyddosomeNumber > 1, MyddosomeNumber <= 3) %>%
  mutate(M1_per_Myddosome = Normalized_Integrated_M1/3) %>%
  mutate(MyddosomeNumber = 3)

M1_Table_Five <- M1_Table %>%
  filter(MyddosomeNumber > 3, MyddosomeNumber <= 5) %>%
  mutate(M1_per_Myddosome = Normalized_Integrated_M1/5) %>%
  mutate(MyddosomeNumber = 5)

M1_Table_MydNumber <- bind_rows(M1_Table_Single,M1_Table_Three,M1_Table_Five) %>%
  arrange(GROUP)

M1_Counts <- M1_Table_MydNumber %>%
  group_by(GROUP, REP, MyddosomeNumber) %>%
  summarise(Counts = NROW(GROUP)) %>%
  arrange(MyddosomeNumber)

write.csv(M1_Table_MydNumber, "M1_Table_MydNumber.csv", row.names = F, )
write.csv(M1_Counts, "M1_Counts.csv", row.names = F, )

#K63 per Myddosome ----
K63 <- fread("/Users/u_cao/Desktop/202302-05_update BioRxiv_EMBOR revision/EMBOR revision/Immunofluoresence/K63_2_analysis/5_Analysis and plots/K63_analysis_normalized.csv")

K63_Table <- K63 %>%
  mutate(MyddosomeNumber = round(Normalized_Integrated_MyD88/0.5, digits = 3)) %>%
  filter(MyddosomeNumber > 0)

K63_Table_Single_Mean <- K63_Table %>%
  filter(MyddosomeNumber > 0, MyddosomeNumber <= 1) %>%
  mutate(K63_per_Myddosome = Normalized_Integrated_K63/1) %>%
  group_by(GROUP,REP) %>%
  summarise(K63_per_Myddosome = mean(K63_per_Myddosome)) %>%
  group_by(GROUP) %>%
  summarise(SD_K63_per_Myddosome = sd(K63_per_Myddosome),
            K63_per_Myddosome = mean(K63_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 1)

K63_Table_Three_Mean <- K63_Table %>%
  filter(MyddosomeNumber > 1, MyddosomeNumber <= 3) %>%
  mutate(K63_per_Myddosome = Normalized_Integrated_K63/3) %>%
  group_by(GROUP,REP) %>%
  summarise(K63_per_Myddosome = mean(K63_per_Myddosome)) %>%
  group_by(GROUP) %>%
  summarise(SD_K63_per_Myddosome = sd(K63_per_Myddosome),
            K63_per_Myddosome = mean(K63_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 3)

K63_Table_Five_Mean <- K63_Table %>%
  filter(MyddosomeNumber > 3, MyddosomeNumber <= 5) %>%
  mutate(K63_per_Myddosome = Normalized_Integrated_K63/5) %>%
  group_by(GROUP,REP) %>%
  summarise(K63_per_Myddosome = mean(K63_per_Myddosome)) %>%
  group_by(GROUP) %>%
  summarise(SD_K63_per_Myddosome = sd(K63_per_Myddosome),
            K63_per_Myddosome = mean(K63_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 5)

K63_Table_MydNumber_Mean <- bind_rows(K63_Table_Single_Mean,K63_Table_Three_Mean,K63_Table_Five_Mean) %>%
  arrange(GROUP)

write.csv(K63_Table_MydNumber_Mean, "K63_Table_MydNumber_Mean.csv", row.names = F, )

K63_Table_Single_Rep <- K63_Table %>%
  filter(MyddosomeNumber > 0, MyddosomeNumber <= 1) %>%
  mutate(K63_per_Myddosome = Normalized_Integrated_K63/1) %>%
  group_by(GROUP,REP) %>%
  summarise(K63_per_Myddosome = mean(K63_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 1)

K63_Table_Three_Rep <- K63_Table %>%
  filter(MyddosomeNumber > 1, MyddosomeNumber <= 3) %>%
  mutate(K63_per_Myddosome = Normalized_Integrated_K63/3) %>%
  group_by(GROUP,REP) %>%
  summarise(K63_per_Myddosome = mean(K63_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 3)

K63_Table_Five_Rep <- K63_Table %>%
  filter(MyddosomeNumber > 3, MyddosomeNumber <= 5) %>%
  mutate(K63_per_Myddosome = Normalized_Integrated_K63/5) %>%
  group_by(GROUP,REP) %>%
  summarise(K63_per_Myddosome = mean(K63_per_Myddosome)) %>%
  mutate(MyddosomeNumber = 5)

K63_Table_MydNumber_Rep <- bind_rows(K63_Table_Single_Rep,K63_Table_Three_Rep,K63_Table_Five_Rep) %>%
  arrange(GROUP)

write.csv(K63_Table_MydNumber_Rep, "K63_Table_MydNumber_Rep.csv", row.names = F, )

K63_Table_Single <- K63_Table %>%
  filter(MyddosomeNumber > 0, MyddosomeNumber <= 1) %>%
  mutate(K63_per_Myddosome = Normalized_Integrated_K63/1) %>%
  mutate(MyddosomeNumber = 1)

K63_Table_Three <- K63_Table %>%
  filter(MyddosomeNumber > 1, MyddosomeNumber <= 3) %>%
  mutate(K63_per_Myddosome = Normalized_Integrated_K63/3) %>%
  mutate(MyddosomeNumber = 3)

K63_Table_Five <- K63_Table %>%
  filter(MyddosomeNumber > 3, MyddosomeNumber <= 5) %>%
  mutate(K63_per_Myddosome = Normalized_Integrated_K63/5) %>%
  mutate(MyddosomeNumber = 5)

K63_Table_MydNumber <- bind_rows(K63_Table_Single,K63_Table_Three,K63_Table_Five) %>%
  arrange(GROUP)

K63_Counts <- K63_Table_MydNumber %>%
  group_by(GROUP, REP, MyddosomeNumber) %>%
  summarise(Counts = NROW(GROUP)) %>%
  arrange(MyddosomeNumber)

write.csv(K63_Table_MydNumber, "K63_Table_MydNumber.csv", row.names = F, )
write.csv(K63_Counts, "K63_Counts.csv", row.names = F, )
