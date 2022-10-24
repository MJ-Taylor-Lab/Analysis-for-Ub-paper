cat(
  "To cite this work, please use:

  Deliz-Aguirre, Cao, et al. (2021) MyD88 oligomer size functions as a
       physical threshold to trigger IL1R Myddosome signaling.
       J. Cell Biol. https://doi.org/10.1083/jcb.202012071"
)

cat(
  "Copyright 2020-21 (c) Rafael Deliz-Aguirre

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the 'Software'), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."
)

# For initiating image analysis pipeline
#Clean everything
remove(list = ls())
gc(reset = TRUE)

###########################
###     USER INPUT      ###
###########################

#Folder containing all scripts
SCRIPTS_DIRECTORY = "~/image-analysis/SingleMolecule/08_Analysis"
#Folder containing all data
TOP_DIRECTORY = "~"
#Folder containing calibration and input data
SETUP_FOLDER = "00_Setup"
#Folder containing all images
ANALYSIS_FOLDER = "08_Analysis"
#List of images to analyze
INPUT_DATA = "~.csv"
#Calibration images
CALIBRATION_INPUT_DATA = "Calibrations.csv" 
#Order to sort proteins
PROTEIN_ORDER_INPUT = c("MyD88", "HOIL1", "TRAF6", "GFP", "mScarlet",
                        "HOIL1Goff", "HOIL1G2p5on", "HOIL1G1on","TRAF6Goff", "TRAF6G1on")
#Ligand
LIGAND = "IL-1"
#Pixel sizeS
PIXEL_SIZE = 0.1466667
#Reference protein for colocalization
REFERENCE_PROTEIN = "MyD88"
#Number of channels imaged
PROTEINS_IMAGED = 3
#Image properties
PIXEL_SIZE = 0.1467 #µm/px
IMAGE_SIZE = 88 #µm

#Scripts to run
#T is true, meaning run
#F is false, meaning do not run
RUN_CALIBRATION_SCRIPTS = T #May need to run twice if multiple files on the same date
RUN_CELLANALYSIS_SCRIPTS = T
RUN_FINALSUMMARY_SCRIPTS = T
RUN_COLOCALIZATION_SCRIPTS = T

###########################
###    CALL SCRIPTS     ###
###########################

SETUP_SCRIPTS <- file.path(SCRIPTS_DIRECTORY, "Setup")
setwd(SETUP_SCRIPTS)
print("Setup.R")
source("Setup.R", local = T)

tryCatch({
  if(RUN_CALIBRATION_SCRIPTS == T){
    print(":::::::::::::::::::: CalibrationSetup.r ::::::::::::::::::::")
    CALIBRATION_SCRIPTS <- file.path(SCRIPTS_DIRECTORY, "Calibration")
    setwd(CALIBRATION_SCRIPTS)
    print("Running script CalibrationSetup.R")
    source("CalibrationSetup.R", local = T)
  }
}, error = function(e) {print("Error with CALIBRATION_SCRIPTS")})

tryCatch({
  if(RUN_CELLANALYSIS_SCRIPTS == T){
    print(":::::::::::::::::::: CellAnalysisSetup.r ::::::::::::::::::::")
    CELLANALYSIS_SCRIPTS <- file.path(SCRIPTS_DIRECTORY, "CellAnalysis")
    setwd(CELLANALYSIS_SCRIPTS)
    print("Running script CellAnalysisSetup.R")
    source("CellAnalysisSetup.R", local = T)
  }
}, error = function(e) {print("Error with CELLANALYSIS_SCRIPTS")})

tryCatch({
  if(RUN_FINALSUMMARY_SCRIPTS == T){
    print(":::::::::::::::::::: FinalSummarySetup.r ::::::::::::::::::::")
    FINALSUMMARY_SCRIPTS <- file.path(SCRIPTS_DIRECTORY, "FinalSummary")
    setwd(FINALSUMMARY_SCRIPTS)
    print("Running script FinalSummarySetup.R")
    source("FinalSummarySetup.R", local = T)
  }
}, error = function(e) {print("Error with FINALSUMMARY_SCRIPTS")})

tryCatch({
  if(RUN_COLOCALIZATION_SCRIPTS == T){
    print(":::::::::::::::::::: ColocalizationSetup.R ::::::::::::::::::::")
    
    # If TRUE (T), it generates a refomratted table
    # based on the output of 07_Colocalization
    GENERATE_NEW_TABLE = T
    # Limit of the max intensity plot
    MAX_INT_LIMIT = 80
    # A manually curated table of the wait times between two colocalized puncta
    # Must be a csv and end in ".csv"
    
    COLOCALIZATION_SCRIPTS <- file.path(SCRIPTS_DIRECTORY, "Colocalization")
    setwd(COLOCALIZATION_SCRIPTS)
    print("Running script ColocalizationSetup.R")
    source("ColocalizationSetup.R", local = T)
  }
}, error = function(e) {print("Error with COLOCALIZATION_SCRIPTS")})