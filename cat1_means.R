library(sf)
library(dplyr)
library(tidyr)
library(openxlsx)

#first change

# Define file paths
rsf_files <- c(
  "D:/FERIT_1/WSP_Churchill/Cat1_RSF/Output/Cat1PredShapefiles/RSF1_MAR_0_TL_0_T20.gpkg",
  "D:/FERIT_1/WSP_Churchill/Cat1_RSF/Output/Cat1PredShapefiles/RSF1_MAR_1_TL_1_T20.gpkg",
  "D:/FERIT_1/WSP_Churchill/Cat1_RSF/Output/Cat1PredShapefiles/RSF1_MAR_0_TL_0_T80.gpkg",
  "D:/FERIT_1/WSP_Churchill/Cat1_RSF/Output/Cat1PredShapefiles/RSF1_MAR_1_TL_1_T80.gpkg"
)

cat1_outlines_path <- "D:/FERIT_1/WSP_Churchill/Cat1_RSF/Output/Cat1PredShapefiles/MECP_Outlines.shp"

#project_centre_buffer_path <- "D:/FERIT_1/WSP_Churchill/Cat1_RSF/Output/Cat1PredShapefiles/ProjectCentre25KmBuffer.shp"
#project_centre_buffer_path <- "D:/FERIT_1/WSP_Churchill/Cat1_RSF/Output/Cat1PredShapefiles/LargeMammal_Caribou_LSA_CRS3161.shp"
project_LSA_path <- "D:/FERIT_1/WSP_Churchill/Cat1_RSF/Output/Cat1PredShapefiles/LargeMammal_Caribou_LSA_CRS3161.shp"
project_RSAHab_path <- "D:/FERIT_1/WSP_Churchill/Cat1_RSF/Output/Cat1PredShapefiles/Churchill_CRS3161.shp"



# Import shapefiles
rsf_list <- lapply(rsf_files, st_read)
# cat1_outlines <- st_read(cat1_outlines_path) %>%
#   mutate(MECP = 1)
cat1_outlines <- st_read(cat1_outlines_path)
#project_centre_buffer <- st_read(project_LSA_path)
project_LSA <- st_read(project_LSA_path)
project_RSAHab <- st_read(project_RSAHab_path)


# Ensure all shapefiles have the same CRS
target_crs <- st_crs(cat1_outlines)

rsf_list <- lapply(rsf_list, function(x) st_transform(x, target_crs))
project_LSA <- st_transform(project_LSA, target_crs)
project_RSAHab <- st_transform(project_RSAHab, target_crs)



# Function to perform the join and calculations
process_rsf <- function(rsf, cat1_outlines) {
  joined <- st_join(rsf, cat1_outlines["MECP"], left = TRUE)

  # Calculate the Cat1 variable
  joined <- joined %>%
    mutate(Cat1 = pmax(ifelse(is.na(bCat1_RSF), 0, bCat1_RSF), ifelse(is.na(MECP), 0, MECP)))

  # Calculate mean value of Cat1
  mean_cat1 <- mean(joined$Cat1, na.rm = TRUE)

  # Intersect with Project LSA
  intersectedLSA <- st_intersection(joined, project_LSA)
  intersectedRSAHab <- st_intersection(joined, project_RSAHab)

  # Calculate mean value of Cat1 for intersected areas
  mean_cat1_intersected <- mean(intersectedLSA$Cat1, na.rm = TRUE)
  mean_cat1_intersectedRSAHab <- mean(intersectedRSAHab$Cat1, na.rm = TRUE)

  return(list(mean_cat1 = mean_cat1, mean_cat1_intersected = mean_cat1_intersected, mean_cat1_intersectedRSAHab = mean_cat1_intersectedRSAHab))
}

# Process each RSF shapefile
results <- lapply(rsf_list, process_rsf, cat1_outlines)

# Create dataframes for results
mean_cat1_df <- data.frame(
  RSF_File = rsf_files,
  Mean_Cat1 = sapply(results, function(x) x$mean_cat1)
)

mean_cat1_intersected_df <- data.frame(
  RSF_File = rsf_files,
  Mean_Cat1_Intersected = sapply(results, function(x) x$mean_cat1_intersected)
)

mean_cat1_intersectedRSAHab_df <- data.frame(
  RSF_File = rsf_files,
  mean_cat1_intersectedRSAHab = sapply(results, function(x) x$mean_cat1_intersectedRSAHab)
)

# Extract scenario text and add as a new variable
mean_cat1_RSA <- mean_cat1_df %>%
  mutate(scenario = sub(".*RSF1_([^.]*)\\..*", "\\1", RSF_File))

mean_cat1_LSA <- mean_cat1_intersected_df %>%
  mutate(scenario = sub(".*RSF1_([^.]*)\\..*", "\\1", RSF_File))

mean_cat1_RSAHab <- mean_cat1_intersectedRSAHab_df %>%
  mutate(scenario = sub(".*RSF1_([^.]*)\\..*", "\\1", RSF_File))


# Convert mean_cat1_df to wide format
mean_cat1_RSA_wide <- mean_cat1_RSA %>%
  select(scenario, Mean_Cat1) %>%
  pivot_wider(names_from = scenario, values_from = Mean_Cat1)

mean_cat1_LSA_wide <- mean_cat1_LSA %>%
  select(scenario, Mean_Cat1_Intersected) %>%
  pivot_wider(names_from = scenario, values_from = Mean_Cat1_Intersected)

mean_cat1_RSAHab_wide <- mean_cat1_RSAHab %>%
  select(scenario, mean_cat1_intersectedRSAHab) %>%
  pivot_wider(names_from = scenario, values_from = mean_cat1_intersectedRSAHab)


mean_cat1_RSA_wide$StudyArea <- "RSA"
mean_cat1_LSA_wide$StudyArea <- "LSA"
mean_cat1_RSAHab_wide$StudyArea <- "RSAHab"
mean_cat1 <- rbind(mean_cat1_RSA_wide,mean_cat1_LSA_wide,mean_cat1_RSAHab_wide)

mean_cat1$pctChange1 <- ((mean_cat1$MAR_1_TL_1_T20-mean_cat1$MAR_0_TL_0_T20)/(mean_cat1$MAR_0_TL_0_T20))*100
mean_cat1$pctChange2 <- ((mean_cat1$MAR_0_TL_0_T80-mean_cat1$MAR_0_TL_0_T20)/(mean_cat1$MAR_0_TL_0_T20))*100
mean_cat1$pctChange3 <- ((mean_cat1$MAR_1_TL_1_T80-mean_cat1$MAR_1_TL_1_T20)/(mean_cat1$MAR_1_TL_1_T20))*100


# Print results
print(mean_cat1)

fName <- "D:/FERIT_1/WSP_Churchill/Cat1_RSF/Cat1Means_v2.xlsx"
write.xlsx(mean_cat1, fName)


