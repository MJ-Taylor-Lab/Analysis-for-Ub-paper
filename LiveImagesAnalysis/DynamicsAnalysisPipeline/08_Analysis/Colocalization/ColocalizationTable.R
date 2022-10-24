#Combines analysis table with colocalization table

#Number of images to run
ColocalizationImageFx <- function(ImageX) {
  tryCatch({
    #Folder to image set
    FOLDER = paste(ANALYSIS_DIRECTORY, ColocalizationInput$COHORT[ImageX], ColocalizationInput$IMAGE[ImageX], sep = "/")
    COLOCALIZATION = ColocalizationInput$COLOCALIZATION[ImageX]
    COHORT = ColocalizationInput$COHORT[ImageX]
    GROUP = ColocalizationInput$GROUP[ImageX]
    IMAGE = ColocalizationInput$IMAGE[ImageX]
    LIGAND_DENSITY = ColocalizationInput$LIGAND_DENSITY[ImageX]
    LIGAND_DENSITY_CAT = ColocalizationInput$LIGAND_DENSITY_CAT[ImageX]
    CELLS = ColocalizationInput$CELLS[ImageX]
    
    PROTEIN1 = ColocalizationInput$PROTEIN1[ImageX]
    PROTEIN2 = ColocalizationInput$PROTEIN2[ImageX]
    MAX_INTENSITY_THRESHOLD_1 = ColocalizationInput$MAX_INTENSITY_THRESHOLD_1[ImageX]
    MAX_INTENSITY_THRESHOLD_2 = ColocalizationInput$MAX_INTENSITY_THRESHOLD_2[ImageX]
    #Input feedback
    print(paste("   Colocalizing ImageX =", ImageX))
    
    #CELLS LOOP
    ColocalizationCellFx <- function (CellX) {
      tryCatch({
        #Change cells directory
        setwd(file.path(FOLDER, paste0("Cell_", CellX), COLOCALIZATION))
        #Display working directory
        print(paste("      Analyzing ImageX =", ImageX, "CellX =", CellX))
        
        tryCatch({
          setwd(paste(FOLDER, "/Cell_", CellX, sep = ""))
          filename = "ReformattedColocalization"
          ColocalizationData <-
            data.table::fread(paste(filename, ".csv.gz", sep = ""), integer64="character")
        },
        error = function(e) {print(paste("          ERROR ColocalizationCellFx Table missing at ImageX =", ImageX, "CellX =", CellX))})  
        
        ColocalizationData1 <-
          ColocalizationData %>%
          mutate(
            UNIVERSAL_TRACK_ID = UNIVERSAL_TRACK_ID_1
          ) %>%
          select(
            UNIVERSAL_TRACK_ID,
            COLOCALIZATION_GROUP
          )
        
        ColocalizationData2 <-
          ColocalizationData %>%
          mutate(
            UNIVERSAL_TRACK_ID = UNIVERSAL_TRACK_ID_2
          ) %>%
          select(
            UNIVERSAL_TRACK_ID,
            COLOCALIZATION_GROUP
          )
        
        ColocalizationData <- bind_rows(ColocalizationData1, ColocalizationData2)
        remove(ColocalizationData1, ColocalizationData2)
        
        ColocalizationData <-
          ColocalizationData %>%
          distinct()
        
        #Read csv
        setwd(paste(FOLDER, "/Cell_", CellX, sep = ""))
        filename = paste(PROTEIN1, "Analysis", sep = "_")
        ExpDataProt1 <-
          data.table::fread(paste(filename, ".csv.gz", sep = ""), integer64="character")
        
        setwd(paste(FOLDER, "/Cell_", CellX, sep = ""))
        filename = paste(PROTEIN2, "Analysis", sep = "_")
        ExpDataProt2 <-
          data.table::fread(paste(filename, ".csv.gz", sep = ""), integer64="character")
        
        ExpData <- bind_rows(ExpDataProt1, ExpDataProt2)
        remove(ExpDataProt1, ExpDataProt2)
        
        #Merge tables
        ColocalizationComparison = merge(ExpData, ColocalizationData, by = "UNIVERSAL_TRACK_ID", all = T, allow.cartesian=TRUE)
        remove(ExpDataProt)
        
        #Replace NA with 0
        ColocalizationComparison <-
          ColocalizationComparison %>%
          mutate(
            COLOCALIZATION = ifelse(is.na(COLOCALIZATION_GROUP), 0, 1)
          )
        
        #Save file
        filename = "ColocalizationAnalysis"
        file.remove(paste(filename, ".csv.gz", sep = ""))
        data.table::fwrite(ColocalizationComparison, paste(filename, ".csv.gz", sep = ""), row.names = F, na = "")
        
        ColocalizationComparison$CELL = CellX
        
        ColocalizationComparison
      },
      error = function(e) {print(paste("      ERROR with ColocalizationMaxCellFx. ImageX =", ImageX, " CellX =", CellX))})
    }
    #Loop
    nCells = 1:CELLS
    ColocalizationCells <- mclapply(nCells, ColocalizationCellFx)
    ColocalizationCells <- ColocalizationCells[(which(sapply(ColocalizationCells,is.list), arr.ind=TRUE))]
    ColocalizationCells <- do.call(bind_rows, ColocalizationCells)
    
    setwd(FOLDER)
    filename = "ColocalizationComparison"
    file.remove(paste(filename, ".csv.gz", sep = ""))
    data.table::fwrite(ColocalizationCells, paste(filename, ".csv.gz", sep = ""), row.names = F, na = "")
    
    ColocalizationCells$IMAGEX = ImageX
    ColocalizationCells
  }, error = function(e) {print(paste("   ERROR with ColocalizationImageFx. ImageX =", ImageX))})
}

#Run colocalization loop by image
nImages = 1:NROW(ColocalizationInput)
ColocalizationImgs <- mclapply(nImages, ColocalizationImageFx)
ColocalizationImgs <- ColocalizationImgs[(which(sapply(ColocalizationImgs,is.list), arr.ind=TRUE))]
ColocalizationImgs <- do.call(bind_rows, ColocalizationImgs)

#Assigns unique identifiers to images
ColocalizationImgs <-
  ColocalizationImgs %>%
  mutate(
    PROTEIN = factor(PROTEIN, levels = PROTEIN_ORDER),
    GROUP = factor(GROUP, levels = PROTEIN_ORDER),
    IMAGENUMBER = group_indices(., GROUP, LIGAND_DENSITY_CAT, IMAGE)
  ) %>%
  arrange(
    LIGAND_DENSITY_CAT,
    PROTEIN,
    GROUP,
  ) %>%
  group_by(
    GROUP,
    LIGAND_DENSITY_CAT
  ) %>%
  mutate(
    IMAGENUMBER = IMAGENUMBER - min(IMAGENUMBER) + 1,
    IMAGENUMBER = as.factor(IMAGENUMBER)
  ) %>%
  ungroup()

#Saves table
setwd(COLOCALIZATION_DIRECTORY)
filename = "GrandColocalizationComparison"
file.remove(paste(filename, ".csv.gz", sep = ""))
data.table::fwrite(ColocalizationImgs, paste(filename, ".csv.gz", sep = ""), row.names = F, na = "")


# GrandTableSpots path
TablePath <- file.path(TOP_DIRECTORY, ANALYSIS_FOLDER, "GrandTableSpots.csv.gz")
GrandTableSpots <- fread(TablePath)
remove(TablePath)
# Take out non-colocalized from GrandColocalizationComparision
ColocalizationImgs2 <- 
  ColocalizationImgs %>%
  filter(
    !is.na(COLOCALIZATION_GROUP),
    !is.na(TRACK_ID)
  )
# Get track ID and colocalization group
TempColocalizationImgs <-
  ColocalizationImgs2 %>%
  distinct(UNIVERSAL_TRACK_ID,.keep_all = TRUE) %>%
  select(
    UNIVERSAL_TRACK_ID,
    COLOCALIZATION_GROUP
  )
# Merge big table with colocalization group by track ID
ColocalizationCombined <- merge(TempColocalizationImgs, GrandTableSpots, by = "UNIVERSAL_TRACK_ID", all = T)
remove(TempColocalizationImgs)

ColocalizationCombined <-
  ColocalizationCombined %>%
  mutate(
    COLOCALIZATION =ifelse(is.na(COLOCALIZATION_GROUP), 0, 1)
  )

#Save table
setwd(COLOCALIZATION_DIRECTORY)
filename = "GrandColoSummary"
file.remove(paste(filename, ".csv.gz", sep = ""))
data.table::fwrite(ColocalizationCombined, paste(filename, ".csv.gz", sep = ""), row.names = F, na = "")
