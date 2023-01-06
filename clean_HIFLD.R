# Description: This script will be used to clean HIFLD data. 
#   Specifically, duplicate locations across point and polygon files will be reconciled. 
#   On projection https://gis.stackexchange.com/questions/375725/appropriate-crs-for-close-distances-in-usa
# Author: Kate Nelson
# Last Modified: July 2021 (added unique processing for large oil and gas well data)

library(pacman)

p_load(tidyverse, sf, doParallel, stringr)

wd<-getwd()

#################################
### Read in HIFLD shapefiles ###
################################

#get file list
files<-list.files(paste0(wd,"/HIFLD/")) #get list of HIFLD files

files<- matrix(unlist(files), ncol=6, byrow=T) #break into a n_assets x 6 matrix (where 6 is the number of individual files ina  shapefile)

uassets<-files[ ,1] %>% str_remove(., "\\..*") #get each unique type of asset and remove the file ending

asset_shapefiles<-paste0(wd, "/HIFLD/" ,uassets, ".shp")


foreach (i=38:length(uassets)) %do% { #restarting at [11] electric power transmission lines after adding logic to not test distance on line geometery files, skip [37] Oil and NAtural Gas Wells as there are more than 1M points!
  
  dat<-st_read(asset_shapefiles[i]) #read in the shapefile

  crs<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" # Albers Equal Area

  dat<-st_transform(dat, crs) #transform projection
 
##########################
### Remove Duplicates ###
#########################
  
  # Remove any duplicate geometries 
  dat2 <- dat %>% distinct(., geometry, .keep_all=T) %>% drop_na() #remove empty geometries

  
  if (st_geometry_type(dat2, by_geometry = F) != "MULTILINESTRING" & 
      st_geometry_type(dat2, by_geometry = F) != "LINESTRING" & 
      st_geometry_type(dat2, by_geometry = F) != "GEOMETRYCOLLECTION") {
  
  
  # For point and polygon only. Remove locations that are really close to each other and likely represent the same piece of infrastructure (may have multiple companies associated with it)
  
  dis=100 #100 meters #do not run for public schools
  
  t<-st_is_within_distance(dat2,dist=dis) #check to see if any close locations (those that are close will have a list of index values instead of just an index for itself)
  # t2<-sapply(st_is_within_distance(dat2,dist=dis),"[[",1) #pull the first index value (second + values are for places neaby the first, for a single location the index of close locations are listed in numerical order)
  t2<-sapply(t,"[[",1) 
  t3<-unique(t2)
  
  distincts<-dat2[t3,]
  
  saveRDS(distincts, paste0(wd, "/cleaned_hifld/",uassets[i],".rds")) #saveRDS(dat2, paste0(wd, "/cleaned_hifld/",uassets[i],".rds")) #for public schools
  
  }else{
    
    saveRDS(dat2, paste0(wd, "/cleaned_hifld/",uassets[i],".rds"))
  }
  
}  

#For [37] Oil and Gas Wells

    dat<-st_read(asset_shapefiles[i]) #read in the shapefile
    
    
    # Remove any duplicate geometries 
    dat2 <- dat %>% distinct(., geometry, .keep_all=T) %>% drop_na() #remove empty geometries
    
    dat2 <-dat2 %>% mutate(lat = unlist(map(.$geometry,1)),
           long = unlist(map(.$geometry,2)))
    
    dat3 <- dat2 %>% mutate_at(c("lat","long"), ~round(., digits=3)) # three decimal places in coords is roughly 110 meters (https://gis.stackexchange.com/questions/8650/measuring-accuracy-of-latitude-and-longitude)
    
    distincts<-dat3 %>% distinct(lat,long, .keep_all=TRUE) #after rounding to ~100 meter resolution in coords remove duplicate corrdinate pairs
    
    #now project
    crs<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" # Albers Equal Area
    
    distincts<-st_transform(distincts, crs) #transform projection
    
      saveRDS(distincts, paste0(wd, "/cleaned_hifld/",uassets[i],".rds")) 
