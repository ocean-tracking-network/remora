library(devtools)
library(httr2)
library(dplyr)
library(raster)
library(readr)
library(terra)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(stars)
library(utils)
library(geosphere)
library(rangeBuilder)
library(surimi)

remove.packages('glatos')
devtools::install_github("ocean-tracking-network/glatos", force=TRUE)
library(glatos)
remove.packages("remora")
devtools::install_github("ocean-tracking-network/remora@notebook_updates", force=TRUE)
library(remora)

#setwd('Work/remora')

#download.file("https://members.oceantrack.org/data/share/testdataotn.zip/@@download/file/testDataOTN.zip", "./testDataOTN.zip")
#unzip("testDataOTN.zip")

world_raster <- raster::raster("./testDataOTN/NE2_50M_SR.tif")

tests_vector <-  c("FDA_QC", #Is the detection likely to be false based on the Pincock/Remora algorithm?
                   "Velocity_QC", #Did the fish travel unrealistically fast?
                   "Distance_QC", #Did the fish travel unrealistically far?
                   "DetectionDistribution_QC", #Was the fish detected outside its known home range?
                   "DistanceRelease_QC", #Was the fish detected unrealistically far from the release?
                   "ReleaseDate_QC", #Was the fish detected before the release date?
                   "ReleaseLocation_QC", #Was the release location within the home range OR within 500km of the first detection? 
                   "Detection_QC") #Aggregation of all the above tests, returning a value between 1 and 4.

otn_files_ugacci <- list(det = "./testDataOTN/ugaaci_matched_detections_2017.csv")

scientific_name <- "Acipenser oxyrinchus"

#Mostly work by Steve Formel!
#Takes just under one minute to run
sturgeonOccurrence <- getOccurrence(scientific_name)

#Mostly work by Jessica Castellanos!
#Takes just under 90 seconds to run.
sturgeonList <- createPolygon(sturgeonOccurrence, fraction=1, partsCount=1, buff=100000, clipToCoast = "aquatic")

sturgeonVector <- sturgeonList$vector
sturgeonPolygon <- sturgeonList$polygon

plot(sturgeonPolygon)

#Parameters, which allows the user to pass parameters into the QC functions.
velocity_threshold <- 10
dist_threshold <- 1000
release_dist_threshold <- 500
pincock_threshold <- 3600
release_loc_threshold <- 500
transition_layer_res <- 5000

#Takes about 5.5m to run.
otn_test_tag_qc <- runQC(otn_files_ugacci, 
                         data_format = "otn", 
                         tests_vector = tests_vector, 
                         shapefile = sturgeonPolygon, 
                         fda_type = "pincock", 
                         rollup = TRUE,
                         world_raster = world_raster,
                         .parallel = FALSE, .progress = TRUE,
                         velocity_threshold,
                         dist_threshold,
                         release_dist_threshold,
                         pincock_threshold,
                         release_loc_threshold,
                         transition_layer_res)

#Mostly work by Ian Jonsen!
plotQC(otn_test_tag_qc, distribution_shp = sturgeonPolygon, data_format = "otn")


otn_files_fsugg <- list(det = "/Users/bruce/Downloads/fsugg_matched_detections_2017/fsugg_matched_detections_2017.csv")

scientific_name <- "Epinephelus itajara"

#Takes about 3.75m to run.
grouperOccurrence <- getOccurrence(scientific_name)

#OBIS is a data system populated by people observing biodiversity. So, it's very good about having data where there are people. - Some paper Jon read.
#grouperPolygon <- createPolygon(grouperOccurrence, fraction=1, partsCount=1, buff=10000, clipToCoast = "aquatic")
#Takes about 1.6m to run.
grouperPolygon <- createPolygon(grouperOccurrence, buff=2000, clipToCoast = "aquatic")

plot(grouperPolygon)

#Takes about 6m to run.
otn_test_tag_qc <- runQC(otn_files_fsugg, 
                         data_format = "otn", 
                         tests_vector = tests_vector, 
                         shapefile = grouperPolygon, 
                         col_spec = NULL, 
                         fda_type = "pincock", 
                         rollup = TRUE,
                         world_raster = world_raster,
                         .parallel = FALSE, .progress = TRUE)

plotQC(otn_test_tag_qc, distribution_shp = grouperPolygon, data_format = "otn")

otn_files_striper <- list(det = "/Users/bruce/Downloads/paxsb08_matched_detections_2007/paxsb08_matched_detections_2007.csv")

scientific_name <- "Morone saxatilis"

striperOccurrence <- getOccurrence(scientific_name)

striperPolygon <- createPolygon(striperOccurrence, fraction=1, partsCount = 1, buff=10000, clipToCoast = "aquatic")
plot(striperPolygon)

otn_test_tag_qc <- runQC(otn_files_striper, 
                         data_format = "otn", 
                         tests_vector = tests_vector, 
                         shapefile = striperPolygon, 
                         col_spec = NULL, 
                         fda_type = "pincock", 
                         rollup = TRUE,
                         world_raster = world_raster,
                         .parallel = FALSE, .progress = TRUE)

plotQC(otn_test_tag_qc, distribution_shp = striperPolygon, data_format = "otn")
