# Antibody staining intensity pipeline

These scripts are for analysing fluorescence intensity from image processing to statistical analysis. 

Scripts used in Cao, et al. (2022) Myddosome clustering in IL-1 receptor signaling regulates the formation of a NF-kB activating signalosome.

# Analysis softwares
* FIJI is Just Image J
* CellProfiler 4.2.1
* RStudio 2022.07.1

# 01_TIFF-Subtract.ijm
This script was described previously (https://github.com/MJ-Taylor-Lab/myddosome-dynamics-pipeline)
This script converts nd2 files to 16-bit TIFF files with separated channels. This script removes background intensity (darkframe noise and non-specific cytosolic fluorescence). Note that radius for median background calculation may vary depending on structures to be analysed.

# 02_multiple_tiff_to_single.R
This script merge individual subtracted channels to one TIFF stack.

# 03_RelA transloaction pipeline.cppipe
This CellProfiler pipeline uses subtracted tiff images from 02_multiple_tiff_to_single.R. The pipeline identifies cell nucleus as primary objects, and cell body as secondary objects. Cytoplasm is identified as subtraction of cell nucleus by cell body. Then the pipeline measures RelA intensity ratio of nucleus to cytoplasm. Column name of the ratio in output table is "Math_IntensityRatio".

# 03_IF intensity at Myddosome pipeline.cppipe
This CellProfiler pipeline uses subtracted tiff images from 02_multiple_tiff_to_single.R. The pipeline identifies MyD88-GFP puncta with a diameter of 3-30 pixels as primary objects. Then the pipeline measures intensity of MyD88-GFP puncta and the associated intensity of K63/M1/pIKK/pp65. Integrated intensity and mean intensity are measured.

# 04_Normalize data and RelA plots.R
This script normalizes RelA nucleus to cytoplasm ratio from images acquired on 2.5 µm and 1 µm grids and unstimulated negative controls to the intensity of RelA nucleus to cytoplasm ratio from off grids data. The normalization uses the following equation: Norm. Int = (Intensity - quantile(0.05)off grid)/(quantile(0.95)off grid - quantile(0.05)off grid). This script also outputs data visualization in violin jitter combined plot.

# 04_Normalize data.R
This script normalizes fluorescence intensities of MyD88-GFP and immunoflourescence staining from images acquired on 2.5 µm and 1 µm grids to the intensity of those from off grids data. The normalization uses the following equation: Norm. Int = (Intensity - quantile(0.01)off grid)/(quantile(0.99)off grid - quantile(0.01)off grid).

# 05_Hex plot.R
This script outputs visualization of MyD88 normalized integrated intensity and associated K63/M1/pIKK/pp65 normalized integrated intensity in hex plot.

# 06_Thresholding by MyD88 intensity.R
This script calculates the percentage of Myddosome clusters (MyD88 Normalized Intensity >= 0.5) in each group, and outputs visualization in jitter plot. 
This script also calculates K63/M1/pIKK/pp65 normalized mean intensity in each group. Data from off grids are separated based on MyD88 normalized intensity (>=0.5 vs <0.5). Data are visulization in violin jitter combined plot.   

# Versioning
We use GitHub for versioning.

# Authors
Fakun Cao & Rafael Deliz-Aguirre
