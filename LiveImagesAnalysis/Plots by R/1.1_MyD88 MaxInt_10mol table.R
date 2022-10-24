# To combine traf6 and hoil1 data together
# Note after running manually change GROUP and COHORT to remove "TRAF6" and "HOIL1" there.
# And save the file as "GrandColoSummaryTRAF6HOIL1_MyD88_FA0_10mol_edit".

library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(stringr)
library(parallel)

setwd("~")

#Import TRAF6 data table
GrandColoSummaryOffTraf6 <- fread("~")
GrandColoSummary1umGridTraf6 <- fread("~")

GrandColoSummaryTRAF6_All <- bind_rows(GrandColoSummaryOffTraf6, GrandColoSummary1umGridTraf6)

#Import HOIL1 data table
GrandColoSummaryOffHoil1 <- fread("~")
GrandColoSummary1umHoil1 <- fread("~")
GrandColoComp2p5Grid2 <- fread("~")

GrandColoSummaryHOIL1_All <- bind_rows(GrandColoSummaryOffHoil1, GrandColoSummary1umHoil1, GrandColoComp2p5Grid2)

#Filter the table to at 10mol and keep MyD88 only and FA=0 only
GrandColoSummaryTRAF6_MyD88_FA0_10mol <-
  GrandColoSummaryTRAF6_All %>%
  filter(
    FRAMES_ADJUSTED == "0",
    PROTEIN == "MyD88",
    LIGAND_DENSITY_CAT == "10"
  )

GrandColoSummaryHOIL1_MyD88_FA0_10mol <-
  GrandColoSummaryHOIL1_All %>%
  filter(
    FRAMES_ADJUSTED == "0",
    PROTEIN == "MyD88",
    LIGAND_DENSITY_CAT == "10"
  )

GrandColoSummaryTRAF6HOIL1_MyD88_FA0_10mol <- bind_rows(GrandColoSummaryTRAF6_MyD88_FA0_10mol,GrandColoSummaryHOIL1_MyD88_FA0_10mol)

write.csv(GrandColoSummaryTRAF6HOIL1_MyD88_FA0_10mol, "GrandColoSummaryTRAF6HOIL1_MyD88_FA0_10mol_with2p5data.csv", row.names = F, )

# Note then manually change GROUP and COHORT to remove "TRAF6" and "HOIL1" there.
# And save the file as "GrandColoSummaryTRAF6HOIL1_MyD88_FA0_10mol_edit_with2p5data".
