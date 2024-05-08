devtools::install_github("lifewatch/mregions2")
install.packages("rfishbase")
install.packages("ggtext")
install.packages("voluModel")

library(dplyr)
library(mregions2)
library(sf)
library(wk)
library(worrms)
library(httr2)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(rfishbase)
library(robis)
library(rgbif)
library(stringr)
library(ggtext)
library(voluModel)

#Adapted from code by Steve Formel: https://www.gbif.us/post/2024/searching-with-aphiaids/

#Get the WoRMS AphiaID: 
AphiaID <- worrms::wm_name2id(name="Acipenser oxyrinchus")

#Important note: the below only worked after I dropped a second 'oxyrinchus' from the sciname. They returned two different AphiaIDs and Fishbase only has one of them.
fishbaseID <- request(base_url = 'https://www.marinespecies.org/rest/AphiaExternalIDByAphiaID/') %>% 
  req_url_path_append(AphiaID) %>%
  req_url_query(`type` = 'fishbase') %>%
  req_perform() %>%
  resp_body_json %>%
  unlist()

#This and the code above are two ways of getting the same thing. 
fishbaseID <- wm_external(id=AphiaID, type="fishbase")

sourceId <- paste0('urn:lsid:marinespecies.org:taxname:', AphiaID)

response <- request(base_url = 'https://api.gbif.org/v1/species') %>% 
  req_url_query(`datasetKey` = '2d59e5db-57ad-41ff-97d6-11f5fb264527', 
                `sourceId` = 'urn:lsid:marinespecies.org:taxname:159222') %>% 
  req_perform() %>%
  resp_body_json()

GBIF_backboneID <- response$results[[1]]$nubKey

#Formel's code uses mregions2 to get a polygon within which we'll search for occurrences, but I think we don't want that- however, i'm going to test with one: Atlantic Coast of Florida, MRGID 20827. 

FL_AC <- mregions2::gaz_geometry(x = 20827) %>%
  sf::st_convex_hull() %>%
  sf::st_as_text() %>%
  wk::wkt() %>%
  wk::wk_orient()

obis_results <- robis::occurrence(taxonid = AphiaID)

gbif_results <- rgbif::occ_data(taxonKey = GBIF_backboneID) %>%
  .[["data"]]

# Select needed columns
obis_select <- obis_results %>% 
  select(occurrenceID, 
         decimalLatitude, 
         decimalLongitude) %>% 
  mutate(Source = 'OBIS')

gbif_select <- gbif_results %>% 
  select(occurrenceID, 
         decimalLatitude, 
         decimalLongitude) %>% 
  mutate(Source = 'GBIF')

# Join Data from GBIF and OBIS
sturgeon_joined <- rbind(obis_select,
                     gbif_select)

createPolygon(sturgeon_joined)

sturgeon_subset = sturgeon_joined[1:15871,]
sturgeonPoly_formel_subset <- createPolygon(sturgeon_subset, coordHeaders = c("decimalLongitude", "decimalLatitude"), buff = 25000, partsCount = 1, clipToCoast = "no")
plot(sturgeonPoly_formel_subset)

sturgeon_subset = sturgeon_joined[1:15872,]
sturgeonPoly_formel_subset <- createPolygon(sturgeon_subset, coordHeaders = c("decimalLongitude", "decimalLatitude"), buff = 25000, partsCount = 1, clipToCoast = "no")
plot(sturgeonPoly_formel_subset)

volu_formel_sturgeon <- marineBackground(sturgeon_joined, fraction=1, partcount=1, buff=100000, clipToOcean = T, clipToCoast = "aquatic")
volu_formel_sturgeon_clipped <- marineBackground(sturgeon_joined, fraction = 1, partcount = 1, buff = 50000, clipToOcean = T)
#The above is on its way to working, I think, but it returns a SpatVector, rather than the list of multipolygons we seem to need. We'll probably need to do some after-the-fact fuckery to make
#it Do (st_as_sf seems promising????)

sturgeon_poly_noclip <- st_as_sf(volu_formel_sturgeon)


sturgeon_joined_sorted <- sturgeon_joined[order(sturgeon_joined$decimalLatitude, sturgeon_joined$decimalLongitude),]



map_plot <- ggplot(data = sturgeon_joined,
             inherit.aes = FALSE,
             aes(x = decimalLongitude, 
                 y = decimalLatitude,
                 color = Source)) +
  
  #everything below here only serves to stylize the plot
  
  scale_color_manual(values = c('orange', 'skyblue')) +
  theme_bw(base_size=14) +
  theme(plot.title = ggtext::element_markdown(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +

map_plot


