library(sf)
library(dplyr)
library(caret)
library(openxlsx)
# for models
library(gbm)
library(dismo) #gbm.fixed cv and select are masked
library(Epi) #for roc
library (PresenceAbsence) # for confusion matrix; masks sensitivity, specificity from caret
library(ggplot2)

#this is an addition

# 2nd addition

# 3rd additon

# 4th addition

# for creating initial empty subdirectories for plots, etc.
main_dir <- "D:\\FERIT_1\\WSP_Churchill\\Cat1_RSF"
sub_dir <- "Output"
success <- createDir1(main_dir, sub_dir)
success
setwd(file.path(main_dir))
getwd()

min_max_norm2 <- function(x) {
  (x) / (max(x))
}

setwd("D:/FERIT_1/WSP_Churchill/Cat1_RSF/QGIS")
shapefile_path <- "./pnts_biophys_random_ChBeKi_3161.shp"
samplePntsAvail_sf <- st_read(shapefile_path)

shapefile_pathUsed <- "pnts_usedCat1_LSLHab_BeChKi_V2_3161.shp"
samplePntsUsed_sf <- st_read(shapefile_pathUsed)

samplePntsAvail_sf$PolyID <- 0
samplePntsAvail_sf <- samplePntsAvail_sf %>%
  dplyr::select(status, everything())
geometryAvail <- st_geometry(samplePntsAvail_sf)
samplePntsAvail <- st_set_geometry(samplePntsAvail_sf, NULL)

samplePntsUsed_sf$status <- 1
samplePntsUsed_sf <- samplePntsUsed_sf %>%
  dplyr::select(status, everything())
geometryUsed <- st_geometry(samplePntsUsed_sf)
samplePntsUsed <- st_set_geometry(samplePntsUsed_sf, NULL)


samplePntsUsed_sf$sizeClass <- cut(samplePntsUsed_sf$Shape_Area,
                                   breaks = c(-Inf, 175000000, 1500000000, Inf),
                                   labels = c("Small", "Medium", "Large"),
                                   include.lowest = TRUE)

# Step 1: Aggregate by PolyID and choose a representative sizeClass
# (Assuming sizeClass does not vary within each PolyID. If it does, adjust accordingly, e.g., by taking the mode)
polyID_grouped <- samplePntsUsed_sf %>%
  group_by(PolyID) %>%
  summarise(sizeClass = first(sizeClass))  # Adjust this line if needed

# Step 2: Create indices for the training set using the aggregated data
set.seed(123)  # For reproducibility
trainIndexPolyID <- createDataPartition(polyID_grouped$sizeClass, p = 0.6,
                                        list = FALSE,
                                        times = 1)
# Extract training PolyIDs
trainPolyIDs <- polyID_grouped$PolyID[trainIndexPolyID]

# Step 3: Create the actual training and test datasets based on PolyID
trainDataUsed <- samplePntsUsed_sf %>%
  filter(PolyID %in% trainPolyIDs)
testDataUsed <- samplePntsUsed_sf %>%
  filter(!PolyID %in% trainPolyIDs)

# Check results
table(trainDataUsed$sizeClass) / nrow(trainDataUsed)
table(testDataUsed$sizeClass) / nrow(testDataUsed)

# plot(trainData[1])
# plot(testData[1])
crsInfo <- st_crs(trainDataUsed)
crsInfos <- st_crs(testDataUsed)


ggplot() +
  geom_sf(data = trainDataUsed, aes(color = 'Training Data'), size = 0.6) +
  geom_sf(data = testDataUsed, aes(color = 'Test Data'), size = 0.6) +
  coord_sf(crs=3161) +
  labs(title = "Train and Test Data Spatial Plot",
       color = "Dataset") +
  theme_minimal()


# Plot trainDataUsed
plot(st_geometry(trainDataUsed), main = "Train and Test Data Plot", col = "blue")
# Add testDataUsed to the plot
plot(st_geometry(testDataUsed), add = TRUE, col = "red")


# Ensure reproducibility
set.seed(123)
dataAvail <- samplePntsAvail_sf %>% dplyr::filter(status == 0)
# Assuming dataAvail is your full dataset
# Calculate the number of rows to include in the training set (75% of the dataset)
training_size <- floor(0.60 * nrow(dataAvail))
# Create a random sample of row indices for the training data
training_indices <- sample(seq_len(nrow(dataAvail)), size = training_size)
# Split the data into training and testing sets
trainDataAvail <- dataAvail[training_indices, ]
testDataAvail <- dataAvail[-training_indices, ]

testDataAvail1 <- testDataAvail[1:97]
trainDataAvail1 <- trainDataAvail[1:97]

# Now join used and available data for train and test data sets
testDataUsed1 <- testDataUsed[, c(1, 2, 4:98)]
trainDataUsed1 <- trainDataUsed[, c(1, 2, 4:98)]


#now join the data
(st_crs(testDataUsed1))
(st_crs(testDataAvail1))
testData_sf <- rbind(testDataUsed1,testDataAvail1)
trainData_sf <- rbind(trainDataUsed1,trainDataAvail1)



####################################################

{cNames <- colnames(trainData_sf %>% dplyr::select(ROADS:LGMD_S6, PSP_USE))
varList <- as.character(cNames)
varList <- varList[varList != "RAILWAY"] # Use TDen_LF
varList <- varList[varList != "UT_LINES" ] # Use TDen_LF (Density of roads, railways, and linear features Include with full data
varList <- varList[varList != "ROADS" ] # Use TDen_LF
varList <- varList[varList != "ROADS_S2" ] # Use TDen_LF
varList <- varList[varList != "ROADS_S3" ] # Use TDen_LF
varList <- varList[varList != "ROADS_S4" ] # Use TDen_LF
varList <- varList[varList != "ROADS_S5" ] # Use TDen_LF
varList <- varList[varList != "ROADS_S6" ] # Use TDen_LF
varList <- varList[varList != "TWAT_PLC" ] # Use LGW Deep and Shallow (turbid) Water
varList <- varList[varList != "OWAT_PLC" ] # Use LGW Deep and Shallow (turbid) Water
varList <- varList[varList != "CSWP_PLC" ] # Use LGTP
varList <- varList[varList != "LGMD_S6" ] # Use MIX
varList <- varList[varList != "SETT_PLC" ] # Use updated ANTHRO
varList <- varList[varList != "CUT_PLC" ]  # Use updated HARV
varList <- varList[varList != "BURN_PLC" ] # Use updated NATDIST
varList <- varList[varList != "TFEN_PLC" ] # Use LGTP Treed peatland (swamp, fen, and bog)
varList <- varList[varList != "TBOG_PLC" ] # Use LGTP Treed peatland (swamp, fen, and bog)
varList <- varList[varList != "OFEN_PLC" ] # Use LGOP Open Peatland (Fen and Bog)
varList <- varList[varList != "OBOG_PLC" ] # Use LGOP Open Peatland (Fen and Bog)
varList <- varList[varList != "RSF_1" ] # Use LGOP Open Peatland (Fen and Bog)

varList <- varList[varList != "ST_S6" ] # Use ST_S6
#varList <- varList[varList != "ST_S5" ] # Use ST_S6
varList <- varList[varList != "ST_S4" ] # Use ST_S6
varList <- varList[varList != "ST_S3" ] # Use ST_S6
varList <- varList[varList != "ST_S2" ] # Use ST_S6
varList <- varList[varList != "ST_PLC" ] # Use ST_S6

varList <- varList[varList != "LGW_S6" ] # Use ST_S5
varList <- varList[varList != "LGW_S4" ]
varList <- varList[varList != "LGW_S3" ]
varList <- varList[varList != "LGW_S2" ]
varList <- varList[varList != "LGW_PLC" ]

varList <- varList[varList != "CON_S6" ]
varList <- varList[varList != "CON_S4" ]
varList <- varList[varList != "CON_S3" ]
varList <- varList[varList != "CON_S2" ]
varList <- varList[varList != "CON_PLC" ]

varList <- varList[varList != "DEC_S6" ]
varList <- varList[varList != "DEC_S4" ]
varList <- varList[varList != "DEC_S3" ]
varList <- varList[varList != "DEC_S2" ]
varList <- varList[varList != "DEC_PLC" ]

varList <- varList[varList != "MIX_S6" ]
varList <- varList[varList != "MIX_S4" ]
varList <- varList[varList != "MIX_S3" ]
varList <- varList[varList != "MIX_S2" ]
varList <- varList[varList != "MIX_PLC" ]

#
varList <- varList[varList != "LGTP_S6" ]
varList <- varList[varList != "LGTP_S4" ]
varList <- varList[varList != "LGTP_S3" ]
varList <- varList[varList != "LGTP_S2" ]
varList <- varList[varList != "LGTP_PLC" ]

varList <- varList[varList != "LGOP_S6" ]
varList <- varList[varList != "LGOP_S4" ]
varList <- varList[varList != "LGOP_S3" ]
varList <- varList[varList != "LGOP_S2" ]
varList <- varList[varList != "LGOP_PLC" ]

varList <- varList[varList != "DTN_S6" ]
varList <- varList[varList != "DTN_S4" ]
varList <- varList[varList != "DTN_S3" ]
varList <- varList[varList != "DTN_S2" ]
varList <- varList[varList != "NATDIST" ]

varList <- varList[varList != "TDENLF_S6" ]
varList <- varList[varList != "TDENLF_S4" ]
varList <- varList[varList != "TDENLF_S3" ]
varList <- varList[varList != "TDENLF_S2" ]
varList <- varList[varList != "TDEN_LF" ]


varList <- varList[varList != "ESK_S6" ]
varList <- varList[varList != "ESK_S4" ]
varList <- varList[varList != "ESK_S3" ]
varList <- varList[varList != "ESK_S2" ]
varList <- varList[varList != "ESKERS" ]

varList <- varList[varList != "geometry" ]


}

dfVarList  <- as.data.frame(varList)

###############################################
# Create NewDat (full landscape for predictions)
# NOte: This data is 27 ha, not 3 ha, and HEXIDs are different than those in dfTT

#BioPhyAttrCh_BE_Ki_RasterAvg.shp
fnameShp <- "D:/FERIT_1/WSP_Churchill/Cat1_RSF/QGIS/"
NewDat <- st_read(paste(fnameShp,"NewData_LSL_Be_Ch_Ki.shp", sep=""))

#readin 27 ha shapefile output from LSL for the 3 ranges, then merge.
#time 20 (NewDat_20) and 80 (NewDat_80)

#save the geometry so it can be reattached later
geometry_NewDat <- st_geometry(NewDat)
# temporarily remove geometry
dfNewDat <- st_set_geometry(NewDat, NULL)
# Reassign the geometry that was previously removed
#dfNewDat_sf <- st_set_geometry(dfNewDat, geometry_NewDat)
dfNewDat <- dfNewDat %>% dplyr::select(ROADS:LGMD_S6, RSFAvg_mea, PSP_USE, RecordID)
dfNewDat <- dfNewDat %>%
  dplyr::rename(RSF_1 = RSFAvg_mea)

saveRDS(dfNewDat, "D:/FERIT_1/WSP_Churchill/Cat1_RSF/data/dfNewDat.RDS")
saveRDS(geometry_NewDat, "D:/FERIT_1/WSP_Churchill/Cat1_RSF/data/geometry_NewDat.RDS")
#dfNewDat_sf <- st_set_geometry(dfNewDat, geometry_NewDat)  #restore geometry
#############################################################

# ****************************************************************************
#  Start the boosted regression tree (BRT) model.  Presence/absence model (bernoulli)

# create formula for model using selected variables
setwd(file.path(main_dir)) # set wd to path where output folders are located
getwd()
#pathBase <- "D:/FERIT_1/WSP_Churchill/Cat1_RSF/"
pathBaseOutput <- "D:/FERIT_1/WSP_Churchill/Cat1_RSF/Output/"

pngName <- paste("Cat1RSF",sep = "_")
(b4Sq <- paste(varList, collapse="+"))
my_frmlaGBM <- as.formula (paste("status ~ ",b4Sq, sep = ""))

{datalistCM4 = list() # confusion matrix
  datalistVL = list() # variables from gbm
  while (!is.null(dev.list()))  dev.off()
  i <- 0
}


traindata <- st_set_geometry(trainData_sf, NULL)
testdata <- st_set_geometry(testData_sf, NULL)


SeasonList <- c("Cat1_RSF")
#  load("D:/FERIT_1/WSP_Churchill/Cat1_RSF/Output/saveMods/brt_noRSF1_Cat1_RSF.rda")
#   summary (brtMod)

# brtMod <- gbm(my_frmlaGBM, data = traindata,
#             distribution = "bernoulli",
#             cv.folds = 8,
#             n.trees = 250,
#             interaction.depth = 2,
#             shrinkage = .1,
#             n.cores = NULL,
#             train.fraction = 1,
#             bag.fraction = .5,
#             verbose = FALSE )


BRT_params <- list(
  BRT1 = list(ntrees = 100, interDepth = 1, shrink = 0.7, bagFract = 0.7, minObs = 1000),
  BRT2 = list(ntrees = 200, interDepth = 2, shrink = 0.5, bagFract = 0.6, minObs = 800),
  BRT3 = list(ntrees = 150, interDepth = 2, shrink = 0.6, bagFract = 0.5, minObs = 1200),
  BRT4 = list(ntrees = 250, interDepth = 2, shrink = 0.1, bagFract = 0.5, minObs = 1000), #original
  BRT5 = list(ntrees = 300, interDepth = 2, shrink = 0.3, bagFract = 0.9, minObs = 1100)
)

for (s in SeasonList) {
  s <- "Cat1_RSF"
  i <- i + 1
  # ntrees <- 100
  # interDepth <- 1
  # shrink <- .7  #.5
  # bagFract <- .7 #.5
  # minObs <- 1000
  
  current_params <- BRT_params$BRT4
  ntrees <- current_params$ntrees
  interDepth <- current_params$interDepth
  shrink <- current_params$shrink
  bagFract <- current_params$bagFract
  minObs <- current_params$minObs
  
  
  brtMod <- gbm(my_frmlaGBM, data = traindata,
                distribution = "bernoulli",
                cv.folds = 8,
                n.trees = ntrees,
                n.minobsinnode = minObs,
                interaction.depth = interDepth,
                shrinkage = shrink,
                n.cores = NULL,
                train.fraction = 1,
                bag.fraction = bagFract,
                verbose = FALSE )
  
  
  pdf(paste(pathBaseOutput, "SavePlots/CrossVal/",pngName, "_crossVal_250_trees.pdf", sep=""))
  #par(mar = c(5, 8, 1, 1))
  ntree_opt_cv <- gbm.perf(brtMod, method = "cv", plot.it = TRUE)
  title (main = paste(s))
  while (!is.null(dev.list()))  dev.off()
  
  pdf(paste(pathBaseOutput, "SavePlots/Influence/",pngName, "_influence.pdf", sep=""))
  par(mar = c(5, 8, 1, 1)) #par values must be set after opening the device
  (df_Summarygbm <- summary.gbm(
    brtMod,
    cBars = 14,
    method = relative.influence, # also can use permutation.test.gbm
    las = 2 ) )
  titleStr = paste("Caribou - (Variable Influence Diagram for ", s, ")", sep="")
  #title (main = paste(s))
  title (main =titleStr)
  while (!is.null(dev.list()))  dev.off()
  
  modName <- paste("brt_v3_",s,".rda", sep= "")
  save(brtMod, file = paste(pathBaseOutput, "saveMods/",  modName, sep =""))
  
  #Create list based on variables with the highest influence on the model
  gbmVarList <- subset(df_Summarygbm, rel.inf > 1)
  rownames(gbmVarList) <- NULL
  gbmVarList$Season <- s
  datalistVL[[i]] <- gbmVarList
  
  nc <- ncol(traindata)
  brtModP4 <- gbm.fixed(data=traindata, gbm.x = varList, n.trees = ntree_opt_cv, gbm.y = 1, family = "bernoulli",
                        tree.complexity = interDepth, learning.rate = shrink, bag.fraction = bagFract)
  
  pdf(paste(pathBaseOutput, "savePlots/Response/", pngName,"_response.pdf", sep=""),
      title = paste("BRT Variable Response for ",s, sep=""))
  gbm.plot(brtModP4,  smooth=TRUE, rug=TRUE, n.plots=8,
           common.scale=FALSE, write.title=TRUE, y.label="f(x variable)", x.label="Obs Presence",
           show.contrib=TRUE, plot.layout=c(4, 2))
  titleStr = paste(s, " (Variable Response Diagram",  ")", sep="")
  title (main =titleStr)
  while (!is.null(dev.list()))  dev.off()
  
  pdf(paste(pathBaseOutput, "savePlots/ROC/",pngName,"_brtROCTrain3.pdf", sep=""),
      title = paste("ROC Train Plots for ",s, sep=""))
  
  traindata$predbrt <- predict(brtMod, newdata = traindata, type= "response")
  dfROCTrain <- ROC(traindata$predbrt, traindata$status, plot ="ROC")
  title (main =s)
  while (!is.null(dev.list()))  dev.off()
  print ("AUC Train BGRT")
  print (dfROCTrain$AUC)
  
  testdata$predbrt <- predict(brtMod, newdata = testdata, type= "response")
  # create graph and stats for brt model
  pdf(paste(pathBaseOutput, "savePlots/ROC/",pngName,"_brtROCTest.pdf", sep=""))
  dfROCTest <- ROC(testdata$predbrt, testdata$status, plot ="ROC") # cutpoint not used for ROC curve
  titleStr = paste(s, " (ROC Diagram Test Data", ")", sep="")
  title (main =titleStr)
  while (!is.null(dev.list()))  dev.off()
  print ("AUC Test BGRT")
  print (dfROCTest$AUC)
  #}
  
  traindata$siteID <- seq.int(nrow(traindata))
  dfPredict<- traindata[, c('siteID', 'status', 'predbrt')]
  #dfPredict$TRAINCASE <- as.numeric(dfPredict$TRAINCASE) - 1
  
  # MaxSens+Spec maximizes (sensitivity+specificity)/2
  cp <- optimal.thresholds(dfPredict, threshold = 101, opt.methods=3)
  cutpointMaxSS <- cp$predbrt
  
  traindata$predStatus <- ifelse(traindata$predbrt > cutpointMaxSS,1,0)
  traindata$predStatus <- as.factor((traindata$predStatus))
  traindata$TRAINCASE <- as.factor((traindata$status))
  CMBrtTrainCPMaxSS <- confusionMatrix(traindata$predStatus, traindata$TRAINCASE)
  dfCM <- as.data.frame(CMBrtTrainCPMaxSS$byClass)
  cname <- paste("BRT","Train", sep = "_")
  colnames(dfCM) <- c(cname)
  dfCM1 <- dfCM
  
  testdata$predStatus <- ifelse(testdata$predbrt > cutpointMaxSS,1,0)
  testdata$predStatus <- as.factor((testdata$predStatus))
  testdata$status <- as.factor((testdata$status))
  CMBrtTestCPMaxSS <- confusionMatrix(testdata$predStatus, testdata$status)
  dfCM1 <- as.data.frame(CMBrtTestCPMaxSS$byClass[1], row.names = NULL, make.names = FALSE)
  rownames(dfCM1) <- NULL
  dfCM1$Season <- s
  
  dfCM1 <- dfCM1[-1]
  dfCM1$SensTrain <- CMBrtTrainCPMaxSS$byClass[1]#sensitivity
  dfCM1$SpecTrain <- CMBrtTrainCPMaxSS$byClass[2]#specificity
  dfCM1$SensTest <- CMBrtTestCPMaxSS$byClass[1]#sensitivity
  dfCM1$SpecTest <- CMBrtTestCPMaxSS$byClass[2]#specificity
  dfCM1$AUC_Train_BRT <-dfROCTrain$AUC
  dfCM1$AUC_Test_BRT <-dfROCTest$AUC
  datalistCM4[[i]] <- dfCM1  # use 1 instead of i because only one dataframe
  #}
  ##########################
  ##New data prediction
  varName <- paste("p1", s, sep="") #unscaled p
  varName4 <- paste("p2", s, sep="") #normalized p
  #varName5 <- paste("p3", s, sep="") #3 classes
  
  dfNewDat[varName] <- predict(brtMod, newdata = dfNewDat, type= "response") #specify column as variable name using [] rather than $
  #dfNewDat[varName4] <- min_max_norm2(dfNewDat[varName]) #specify column as variable name using [] rather than $
  dfNewDat[varName4] <- min_max_norm2(dfNewDat$p1Cat1_RSF)
  varName1 <- paste("cp", s, sep="")
  dfNewDat[varName1] <- cutpointMaxSS #cutpoint applies to unscaled variables
  varName2 <- paste("b", s, sep="")
  dfNewDat[varName2] <- ifelse(dfNewDat[varName] > cutpointMaxSS,1,0) # applies to unscaled pUse
  Threshold1 = 0.75
  # dfNewDat[varName5] <- ifelse(dfNewDat[varName] > cutpointMaxSS, 2,
  #                                ifelse(dfNewDat[varName] <= (cutpointMaxSS * Threshold1), 0, 1))
  dfNewDat<- dfNewDat %>% dplyr::mutate_at(c(varName, varName1,varName2), as.numeric)
  #dfNewDat<- dfNewDat %>% mutate_at(c(varName, varName1, varName2,varName5), as.numeric)
  
}
getwd()
dfCMStats4 <- dplyr::bind_rows(datalistCM4) #unlist the dataframe list from the loop
fName <- (paste(pathBaseOutput,"summaries/gbmPerform.xlsx", sep=""))
write.xlsx(dfCMStats4, fName)

datalistVL <- dplyr::bind_rows(datalistVL)
fName <- (paste(pathBaseOutput,"summaries/gbmVarList.xlsx", sep=""))
write.xlsx(datalistVL, fName)

# make dfNewDat_sf file for exporting
start_1 <- which(names(dfNewDat) == "RecordID") #grab last set of columns, including recordID (needed for linking)
end_1 <- which(names(dfNewDat) == "bCat1_RSF")
dfNewDat_sf <- dfNewDat %>%
  dplyr::select(c(start_1:end_1))
dfNewDat_sf <- st_set_geometry(dfNewDat_sf, geometry_NewDat)  #restore geometry

gpkg_pathRSF <- "D:/FERIT_1/WSP_Churchill/Cat1_RSF/QGIS/caribou_Cat1_RSF_v7.gpkg"
layer_name <- "Cat1_Pred"
st_write(dfNewDat_sf, dsn = gpkg_pathRSF, layer = layer_name, delete_layer = TRUE)




















polyID_grouped <- samplePntsUsed %>%
  group_by(PolyID) %>%
  summarise(sizeClass = first(sizeClass))

# Step 2: Create indices for the training set using the aggregated data
set.seed(123)  # For reproducibility
trainIndexPolyID <- createDataPartition(polyID_grouped$sizeClass, p = 0.6,
                                        list = FALSE,
                                        times = 1)
# Extract training PolyIDs
trainPolyIDs <- polyID_grouped$PolyID[trainIndexPolyID]

# Step 3: Create the actual training and test datasets based on PolyID
trainData <- samplePntsUsed %>%
  filter(PolyID %in% trainPolyIDs)
testData <- samplePntsUsed %>%
  filter(!PolyID %in% trainPolyIDs)
# Check results
table(trainData$sizeClass) / nrow(trainData)
table(testData$sizeClass) / nrow(testData)


#dataUsedTrain <- samplePnts %>% dplyr::filter(status == 1 & testPoly = NULL)
dataUsedTrain <- samplePnts %>%
  dplyr::filter(status == 1 & is.na(testPoly))


dataUsedTest <- samplePnts %>%
  dplyr::filter(status == 1 & testPoly ==1)

traindata <- rbind(traindata,dataUsedTrain)
testdata <- rbind(testdata,dataUsedTest)

