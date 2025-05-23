#If you need the installs.
#install.packages('readr')
#install.packages('tidyverse')
#install.packages('sf')
#install.packages('sp')
#install.packages('raster')
#install.packages('stars')
devtools::install_github('ocean-tracking-network/remora@R_cleanup', force=TRUE)
devtools::install_github('ocean-tracking-network/surimi', force=TRUE)

library(readr)
library(terra)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(stars)
library(glatos)
library(utils)
library(geosphere)
library(rangeBuilder)
library(remora)
library(surimi)

setwd('/Users/bruce/Work/remora')

install.packages('devtools')
library(devtools)
devtools::install_github('klutometis/roxygen')
library(roxygen2)

download.file("https://members.oceantrack.org/data/share/testdataotn.zip/@@download/file/testDataOTN.zip", "./testDataOTN.zip")
unzip("testDataOTN.zip")

#IMOS test data, as something to reference against if you want it.
imos_files <- list(det = system.file(file.path("test_data","IMOS_detections.csv"), package = "remora"),
                   rmeta = system.file(file.path("test_data","IMOS_receiver_deployment_metadata.csv"),
                                       package = "remora"),
                   tmeta = system.file(file.path("test_data","IMOS_transmitter_deployment_metadata.csv"),
                                       package = "remora"),
                   meas = system.file(file.path("test_data","IMOS_animal_measurements.csv"),
                                      package = "remora"))

#Hideous column type specification in compact string format (see the col_types help in the read_csv documentation for explanation)
string_spec = "ccccDccccddcccccccTcddciiiidcccc"
otn_test_data <- readr::read_csv("/Users/bruce/Downloads/animal_extract_2013_2.csv", col_types=string_spec) #Put your path to your test file here. 
otn_test_data <- readr::read_csv("testDataOTN/qc_princess.csv")
otn_test_data <- readr::read_csv("/Users/bruce/Downloads/cobcrp-all-years-matched-detections/cobcrp_matched_detections_2022/cobcrp_matched_detections_2022.csv")
otn_test_data <- readr::read_csv("cobia_subset_export.csv")
otn_test_data <- readr::read_csv("/Users/bruce/Downloads/cobcrp-all-years-matched-detections/cobcrp_matched_detections_2017/cobcrp_matched_detections_2017.csv")
otn_test_data <- readr::read_csv("testDataOTN/cobia/cobia_subset_export_TT1299_withBogus.csv")
otn_mapped_test <- otn_imos_column_map(otn_test_data)
#If you want to check your work. 
View(otn_mapped_test)

#The above code isn't meant to be run on its own just yet, the ideal is that you can pass it to QC without having to manually map it. 
#otn_files <- list(det = "/Users/bruce/Downloads/animal_extract_2013_2.csv") #Put your path to your files here
otn_files <- list(det = "testDataOTN/qc_princess.csv")
otn_files <- list(det = "/Users/bruce/Downloads/cobcrp-all-years-matched-detections/cobcrp_matched_detections_2022/cobcrp_matched_detections_2022.csv")
otn_files <- list(det = "/Users/bruce/Downloads/cobcrp-all-years-matched-detections/cobcrp_matched_detections_2015/cobcrp_matched_detections_2015.csv")
otn_files <- list(det = "/Users/bruce/Downloads/cobcrp-all-years-matched-detections/cobcrp_matched_detections_2017/cobcrp_matched_detections_2017.csv")
otn_files <- list(det = "testDataOTN/cobia/cobia_subset_export.csv")
otn_files <- list(det = "testDataOTN/cobia/cobia_subset_export_TT1299_withBogus.csv")

otn_files <- list(det = "/Users/bruce/Downloads/fsugg_matched_detections_2020/fsugg_matched_detections_2020.csv")

otn_files <- list(det = "/Users/bruce/Downloads/fsugg_matched_detections_2017/fsugg_matched_detections_2017.csv")


#UGAACI 2017 DATA
otn_files <- list(det = "/Users/bruce/Downloads/ugaaci_matched_detections_2017/ugaaci_matched_detections_2017.csv")

#Use this code with the appropriate files to generate a usable polygon for the species range.
grouper <- read_csv("/Users/bruce/Downloads/7cbe0bd8-0e19-4bcd-9f1f-f594637f0f38/Occurrence.csv", na = c("", "NA", "{}"))
grouperPoly <- createPolygon("/Users/bruce/Downloads/7cbe0bd8-0e19-4bcd-9f1f-f594637f0f38/Occurrence.csv", coordHeaders = c("decimallongitude", "decimallatitude"))
#grouper2 <- grouper %>% filter(is.na(flags))
#sps1Poly <- getDynamicAlphaHull(grouper, fraction = 0.70, buff = 1000, partCount = 20, coordHeaders = c("decimallongitude", "decimallatitude"), clipToCoast = "aquatic")

sturgeon <- read_csv("/Users/bruce/Downloads/4865ec7b-50ec-4f0a-a3dd-58806e19be6e 2/Occurrence.csv", na = c("", "NA", "{}"))
sturgeonPoly <- createPolygon("/Users/bruce/Downloads/4865ec7b-50ec-4f0a-a3dd-58806e19be6e/Occurrence.csv")

scientific

#The QC functions rely on having shapefiles for distributions and study areas to calculate distances. 
#We've got to get a shapefile for the Blue Shark test data, one is included here for sharks but for alternative data you will need your own appropriate one.
#We got ours from IUCN so maybe start there!
#shark_shp <- sf::st_read("./testDataOTN/SHARKS_RAYS_CHIMAERAS/SHARKS_RAYS_CHIMAERAS.shp")
#cobia_shp <- sf::st_read(sps1Poly[[1]])
#We're using the binomial name and bounding box that befits our species and area but feel free to sub in your own when you work with other datasets.
#blue_shark_shp <- shark_shp[shark_shp$binomial == 'Prionace glauca',]
#blue_shark_crop <- sf::st_crop(blue_shark_shp,  xmin=-68.4, ymin=42.82, xmax=-60.53, ymax=45.0)

#This is the format of the code we use to make a transition layer; you shouldn't need to run this here since it will run in 
#shark_transition <- glatos::make_transition2(shapefile_crop)
#shark_tr <- shark_transition$transition

#And also a spatial polygon that we can use later. 
#blue_shark_spatial <- sf::as_Spatial(blue_shark_crop)

#We also need a raster for the ocean. We'll load this from a mid-resolution tif file, for testing purposes. 
world_raster <- raster::raster("./testDataOTN/NE2_50M_SR.tif")
#And crop it based on our cropped blue shark extent. 
world_raster_sub <- raster::crop(world_raster, sf::as_Spatial(grouperPoly))
## set values to either 1 (ocean) or NA (land)
world_raster_sub[world_raster_sub < 251] <- NA
world_raster_sub[world_raster_sub == 251] <- 1

#These are the available tests at time of writing. A note: if you are running with an OTN detection extract, your data will already have QC'd for release date and release location. 
#At this time, however, the plotting function requires Release Location to run properly. You can run these tests if you want, but in an OTN data format
#they will not be counted towards final QC aggregation. 
tests_vector <-  c("FDA_QC",
                   "Velocity_QC",
                   "Distance_QC",
                   "DetectionDistribution_QC",
                   "DistanceRelease_QC",
                   "ReleaseDate_QC",
                   "ReleaseLocation_QC",
                   "Detection_QC")

#In a perfect world, when you run this code, you will get output with QC attached. 
#data$transmitter_codespace <- data$transmitter_id
#data$receiver_sn <- data$receiver_id
#data$detection_timestamp_utc <- data$detection_datetime

minLat = min(otn_test_data$latitude) - 5
minLon = min(otn_test_data$longitude) - 5
maxLat = max(otn_test_data$latitude) + 5
maxLon = max(otn_test_data$longitude) + 5

#Something about this doesn't work when I'm using the cobia 2017 data- shapefile_crop ends up being a list of 0. Gotta figure that out.
shapefile_crop <- sf::st_crop(sps1Poly[[1]],  xmin=minLon, ymin=minLat, xmax=maxLon, ymax=maxLat)

otn_test_tag_qc <- runQC(otn_files, 
                         data_format = "otn", 
                         tests_vector = tests_vector, 
                         shapefile = sturgeon_poly_weird, 
                         col_spec = NULL, 
                         fda_type = "pincock", 
                         rollup = TRUE,
                         world_raster = world_raster,
                         .parallel = FALSE, .progress = TRUE)

plotQC(otn_test_tag_qc, path = "cobiaOutput", species_range=sps1Poly[[1]])
writeQC(otn_test_tag_qc, path = "cobiaOutput/export", summary = FALSE, aggregate = TRUE)

cobia_filtered <- otn_test_tag_qc %>% filter(length(QC[[1]]) > 1)

View(otn_test_tag_qc)

latlon_test <- otn_test_data[c('latitude', 'longitude')]
coordinates(latlon_test) <- c("longitude", "latitude")
proj4string(latlon_test) <- CRS("+proj=longlat +datum=WGS84")
world_shapefile_crop <- rasterToPolygons(world_raster_sub)
sp::over(latlon_test, world_shapefile_crop)

cobia_subset_likely <- filter(otn_test_tag_qc, (QC$Detection_QC == 3))


#qc_shapes_test <- get_qc_shapes(otn_test_data, sps1Poly[1])

#All the below is just bits and scraps of stuff I've written here and there, no consistency or coherency to it.
#Keeping it around for posterity but you shouldn't have to run it. 
# rob_data_subset_glatos <- filter(otn_test_tag_qc, nrow(QC) > 10 & nrow(QC) < 500)
# 
#cobia_data_subset_distance <- filter(otn_test_tag_qc, (QC$FDA_QC == 1 & QC$DistanceRelease_QC == 2))
# 
filtered <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(filtered) <- colnames(otn_test_tag_qc)
filename <- otn_test_tag_qc[1, 'filename']
rowQC <- otn_test_tag_qc[1, 'QC']
View(rowQC)
for(row in 1:nrow(otn_test_tag_qc)) {
  filename <- otn_test_tag_qc[row, 'filename']
  rowQC <- otn_test_tag_qc[row, 'QC'][[1]][[1]]
  if(nrow(rowQC) > 1) {
    filteredQC <- filter(rowQC, Detection_QC == 2)
    if(nrow(filteredQC) > 0){
      message("Wahoo!")
      message(filename)
      message(filteredQC$receiver_deployment_latitude)
      message(filteredQC$receiver_deployment_longitude)
      write.csv(filteredQC, paste0("cobiaOutput/summary/summary_", filename, ".csv"), append=TRUE)
    }
  }
}
nrow(filtered)
View(filtered[,1])

# 
# test_detections$transmitter_codespace <- test_detections$transmitter_id
# test_detections$receiver_sn <- test_detections$receiver_id
# test_detections$detection_timestamp_utc <- test_detections$detection_datetime
# 
# 
# 
# temporal_outcome <- data.frame(matrix(ncol = length(tests_vector), nrow = nrow(x)))
# 
# all_data <- get_data_arbitrary(
#   det = otn_files$det,
#   rmeta = NULL,
#   tmeta = NULL,
#   meas = NULL,
#   logfile = "QC_logfile.txt",
#   data_format = "otn",
#   col_spec = NULL
# )
# 
# 
