# Description: This script will be used to compute measures of asset accessibility in block groups. 
  # Accessibility will be calculated as the shortest straight-line distance from the 
  # centroid of a block group to the nearest asset. Larger values  indicate that an asset is located far 
  # from a block group and hence is less convenient to reach and make use of.
# Author: Kate Nelson
# Last Modified: September 2021

library(pacman)

p_load(tidyverse, sf, doParallel, tidycensus)

wd<-getwd()

##################################################
### Read in Boundaries and Calculate Centroids ###
##################################################

bgs <- st_read("tl_2018_blockgroup.shp") %>% #load census block group geometries
        select(GEOID, STATEFP) #select only needed attributes

crs<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" # Albers Equal Area

bgs<-st_transform(bgs, crs)

ctr <- bgs %>% st_centroid(.) #centroids of blockgroups for distance calculations


################################################################
### Calculate Distance of OSM for each Block Group in Kansas ###
###############################################################


files <- list.files(paste0(wd,"/cleaned_osm/")) #get list of cleaned osm files


foreach (i=1:length(files)) %do% {
  
  dat <- readRDS(paste0(wd, "/cleaned_osm/",files[i])) #read in cleaned data file
    
    sub <- bgs %>% filter(STATEFP == 20) #subset blockgroups to state of KS
    ctr_s <- ctr %>% filter(STATEFP == 20) #subset blockgroup centroids to state of KS
    
    
    access_s<- ctr_s %>% mutate(nearest = try(st_nearest_feature(.,dat))) %>% 
      mutate(dist = st_distance(., dat[nearest,], by_element = TRUE)) %>% 
      st_drop_geometry(.) %>%
      left_join(sub, ., by="GEOID")
  
  
  
  saveRDS(access_s, paste0(wd, "/access/","ks", files[i]))
}

#find nearest points in list2 for each id in list1, and as a bonus, calculate the distance to this point
    # list1.sf %>% 
    #   dplyr::group_by( id ) %>%
    #   dplyr::mutate( np = sf::st_nearest_feature( geometry, list2.sf ),
    #                  dist_np = as.numeric( sf::st_distance( geometry, list2.sf[np,] ) ) )


ggplot() + geom_sf(data=access_s, aes(fill=as.numeric(dist)), color=NA)

#############################################################
### Calculate Distance of OSM for each Block Group For US ###  
#############################################################

files <- list.files(paste0(wd,"/cleaned_osm/")) #get list of cleaned osm files

foreach (i=1:length(files)) %do% {
  
  dat <- readRDS(paste0(wd, "/cleaned_osm/",files[i])) #read in cleaned data file
  
  access<- ctr %>% mutate(nearest = try(st_nearest_feature(.,dat))) %>% 
    mutate(dist = st_distance(., dat[nearest,], by_element = TRUE)) %>% 
    st_drop_geometry(.) %>%
    left_join(bgs, ., by="GEOID")
  
  saveRDS(access, paste0(wd, "/access/","US", files[i]))
}

ggplot() + geom_sf(data=access, aes(fill=as.numeric(dist)), color=NA)

############################
### Read in HIFLD Files ###
###########################


files <- list.files(paste0(wd,"/cleaned_hifld/")) #get list of cleaned hifld files


##################################################################
### Calculate Distance of HIFLD for each Block Group in Kansas###
#################################################################

foreach (i=1:length(files)) %do% {
  
  dat <- readRDS(paste0(wd, "/cleaned_hifld/",files[i])) #read in cleaned data file
  
  if (st_geometry_type(dat, by_geometry = F) != "MULTILINESTRING" & 
      st_geometry_type(dat, by_geometry = F) != "LINESTRING" & 
      st_geometry_type(dat, by_geometry = F) != "GEOMETRYCOLLECTION"){  #only run this if we have point or polygon data
  
   
      sub <- bgs %>% filter(STATEFP == 20) #subset blockgroups to state of KS
      ctr_s <- ctr %>% filter(STATEFP == 20) #subset blockgroup centroids to state of KS
           
      access_s<- ctr_s %>% mutate(nearest = try(st_nearest_feature(.,dat))) %>% 
        mutate(dist = st_distance(., dat[nearest,], by_element = TRUE)) %>% 
        st_drop_geometry(.) %>%
        left_join(sub, ., by="GEOID")
           
      
      saveRDS(access_s, paste0(wd, "/access/","ks", files[i]))
  }
}

#gut check
dat <- readRDS(paste0(wd, "/access/","ks",files[i]))
ggplot() + geom_sf(data=dat, aes(fill=as.numeric(dist)), color=NA)

################################################################
### Calculate Distance of HIFLD for each Block Group For US ###
###############################################################

foreach (i=1:length(files)) %do% { #restart at intermodal freight air to train #24, go back an rerun files that didn't process/save correctly (35,36,37,40,41,43)
  
  dat <- readRDS(paste0(wd, "/cleaned_hifld/",files[i])) #read in cleaned data file
  
  if (st_geometry_type(dat, by_geometry = F) != "MULTILINESTRING" & 
      st_geometry_type(dat, by_geometry = F) != "LINESTRING" & 
      st_geometry_type(dat, by_geometry = F) != "GEOMETRYCOLLECTION"){  #only run this if we have point or polygon data
  
  access<- ctr %>% mutate(nearest = try(st_nearest_feature(.,dat))) %>% 
    mutate(dist = st_distance(., dat[nearest,], by_element = TRUE)) %>% 
    st_drop_geometry(.) %>%
    left_join(bgs, ., by="GEOID")
  
    
  saveRDS(access, paste0(wd, "/access/","US", files[i]))
  }
  }

#gut check
dat <- readRDS(paste0(wd, "/access/","US",files[i]))
ggplot() + geom_sf(data=dat, aes(fill=as.numeric(dist)), color=NA)

####################################################################
### Calculate HIFLD Line Distance per Block Group  For KS and US ###
####################################################################

foreach (i=1:length(files)) %do% { 
  
  dat <- readRDS(paste0(wd, "/cleaned_hifld/",files[i])) #read in cleaned data file
  
  if (st_geometry_type(dat, by_geometry = F) == "MULTILINESTRING" | 
      st_geometry_type(dat, by_geometry = F) == "LINESTRING") {  #run on line geometries
    
    sub <- bgs %>% filter(STATEFP == 20) #subset to state of KS
    dat_s <- st_intersection(dat, sub)
    
    access<-  ctr %>% mutate(nearest = st_nearest_feature(.,dat_s)) %>% 
      mutate(dist = st_distance(., dat_s[nearest,], by_element = TRUE)) %>% 
      st_drop_geometry(.) %>%
      left_join(sub, ., by="GEOID")
    
    saveRDS(access, paste0(wd, "/access/","ks", files[i]))
  }
}

foreach (i=1:length(files)) %do% { 
  
  dat <- readRDS(paste0(wd, "/cleaned_hifld/",files[i])) #read in cleaned data file
  
  if (st_geometry_type(dat, by_geometry = F) == "MULTILINESTRING" | 
      st_geometry_type(dat, by_geometry = F) == "LINESTRING") {
    
  
    access<-  ctr %>% mutate(nearest = st_nearest_feature(.,dat)) %>% 
      mutate(dist = st_distance(., dat[nearest,], by_element = TRUE)) %>% 
      st_drop_geometry(.) %>%
      left_join(bgs, ., by="GEOID")
    
    saveRDS(access, paste0(wd, "/access/","US", files[i]))
    
  }
}


#gut check
dat <- readRDS(paste0(wd, "/access/","ks",files[i])) #i=11,19,21
ggplot()  + geom_sf(data=dat, aes(fill=as.numeric(dist)))

dat <- readRDS(paste0(wd, "/access/","US",files[i])) #i=11,19,21
ggplot()  + geom_sf(data=dat, aes(fill=as.numeric(dist)), color=NA)
