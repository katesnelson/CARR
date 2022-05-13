# This script will be used to compute measures of asset availability in block groups. 
  # For each type of unit-level service and amenity asset with a point or polygon geometry (e.g. post office, airport, grocery store) 
  # a measure of availability will be constructed by computing the density in counts per hundreds of square miles for each block group. 
  # For each type of unit-level asset with a line geometry (e.g. national freight system, electric transmission lines) availability will 
  # be assigned either a value of one or zero to indicate presence or absence, respectively, in a block group. 


library(pacman)

p_load(tidyverse, sf, doParallel, tidycensus)

wd<-getwd()

########################################
### Read in Boundaries and OSM Files ###
########################################

bgs <- st_read("tl_2018_blockgroup.shp") %>% #laod census block group geometries
        dplyr::select(GEOID, STATEFP) #select only needed attributes


crs<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" # Albers Equal Area

bgs<-st_transform(bgs, crs)

files <- list.files(paste0(wd,"/cleaned_osm/")) #get list of cleaned osm files


#####################################################
### Calculate Density per Block Group For Kansas ###
####################################################

foreach (i=1:length(files)) %do% {
  
  dat <- readRDS(paste0(wd, "/cleaned_osm/",files[i])) #read in cleaned data file
     
  
  bgs <- st_transform(bgs, st_crs(dat)) #make sure we are using the same CRS for both datasets, should use same as dat which was previously set to Albers equal area 
  
    sub <- bgs %>% filter(STATEFP == 20) #subset to state of KS
    bb <- st_bbox(sub) #create a bounding box around the state of KS
    dat_s <- st_crop(dat, bb) #crops dataset to a bounding box
  
    avail_s<-st_join(sub, dat_s, join=st_contains, left=TRUE) %>%
      group_by(GEOID)  %>%  add_tally(.) %>% ungroup() %>%
      mutate(cnt=ifelse(!is.na(!! rlang::sym(names(.)[3])),n, n-1)) %>%
      group_by(GEOID) %>% summarise(cnt=first(cnt))#report the count of assets in each blockgroup (by GEOID) 
  
  avail_s <- avail_s %>% mutate(area = st_area(.)) %>% mutate(density = cnt/area)
  
  saveRDS(avail_s, paste0(wd, "/avail/","ks", files[i]))
}


# ggplot() + geom_sf(data=avail_s, aes(fill=as.numeric(cnt)), color=NA)

#####################################################
### Calculate Density per Block Group  For US ###  Doing this in series takes more than 24 hours
####################################################

foreach (i=1:length(files)) %do% {
  
  dat <- readRDS(paste0(wd, "/cleaned_osm/",files[i])) #read in cleaned data file, these are all point data
  
  
  bgs <- st_transform(bgs, st_crs(dat)) #make sure we are using the same CRS for both datasets, should use same as dat which was previously set to Albers equal area 
  
  # sub <- bgs %>% filter(STATEFP == 20) #subset to state of KS
  # bb <- st_bbox(dat, sub) #create a bounding box around the state of KS
  # dat_s <- st_crop(dat, bb) #crops dataset to a bounding box
  
  
  avail<-st_join(bgs, dat, join=st_contains, left=TRUE) %>%
  group_by(GEOID)  %>%  add_tally(.) %>% ungroup() %>%
    mutate(cnt=ifelse(!is.na(!! rlang::sym(names(.)[3])),n, n-1)) %>%
    group_by(GEOID) %>% summarise(cnt=first(cnt))#report the count of assets in each blockgroup (by GEOID) 
  
  avail <- avail %>% mutate(area = st_area(.)) %>% mutate(density = cnt/area)
  
  saveRDS(avail, paste0(wd, "/avail/","US", files[i]))
}


############################
### Read in HIFLD Files ###
###########################


files <- list.files(paste0(wd,"/cleaned_hifld/")) #get list of cleaned hifld files


##########################################################
### Calculate HIFLD Density per Block Group For Kansas###
#########################################################


foreach (i=1:length(files)) %do% {
  
  dat <- readRDS(paste0(wd, "/cleaned_hifld/",files[i])) #read in cleaned data file
  
  if (st_geometry_type(dat, by_geometry = F) != "MULTILINESTRING" & 
      st_geometry_type(dat, by_geometry = F) != "LINESTRING" & 
      st_geometry_type(dat, by_geometry = F) != "GEOMETRYCOLLECTION"){  #only run this if we have point or polygon data
  
  bgs <- st_transform(bgs, st_crs(dat)) #make sure we are using the same CRS for both datasets, should use same as dat which was previously set to Albers equal area 
  
  sub <- bgs %>% filter(STATEFP == 20) #subset to state of KS
  bb <- st_bbox(sub) #create a bounding box around the state of KS
  dat_s <- st_crop(dat, bb) #crops dataset to a bounding box
  
  avail_s<-st_join(sub, dat_s, join=st_contains, left=TRUE) %>%
    group_by(GEOID)  %>%  add_tally(.) %>% ungroup() %>%
    mutate(cnt=ifelse(!is.na(!! rlang::sym(names(.)[3])),n, n-1)) %>%
    group_by(GEOID) %>% summarise(cnt=first(cnt))#report the count of assets in each blockgroup (by GEOID) 
  
  avail_s <- avail_s %>% mutate(area = st_area(.)) %>% mutate(density = cnt/area)
  
  saveRDS(avail_s, paste0(wd, "/avail/","ks", files[i]))
}
}

#gut check
dat <- readRDS(paste0(wd, "/avail/","ks",files[i]))
ggplot() + geom_sf(data=dat, aes(fill=cnt), color=NA)

#######################################################
### Calculate HIFLD Density per Block Group  For US ###
######################################################

foreach (i=1:length(files)) %do% { 
  
  dat <- readRDS(paste0(wd, "/cleaned_hifld/",files[i])) #read in cleaned data file
  
  
  if (st_geometry_type(dat, by_geometry = F) != "MULTILINESTRING" & 
      st_geometry_type(dat, by_geometry = F) != "LINESTRING") {  #don't run on lines
    
  
  avail<-st_join(bgs, dat, join=st_contains, left=TRUE) %>%
    group_by(GEOID)  %>%  add_tally(.) %>% ungroup() %>%
    mutate(cnt=ifelse(!is.na(!! rlang::sym(names(.)[3])),n, n-1)) %>%
    group_by(GEOID) %>% summarise(cnt=first(cnt))#report the count of assets in each blockgroup (by GEOID) 
  
  avail <- avail %>% mutate(area = st_area(.)) %>% mutate(density = cnt/area)
  
  saveRDS(avail, paste0(wd, "/avail/","US", files[i]))
}}

#gut check
dat <- readRDS(paste0(wd, "/avail/","US",files[i]))
ggplot() + geom_sf(data=dat, aes(fill=cnt), color=NA)

####################################################################
### Calculate HIFLD Line Presence per Block Group  For KS and US ###
####################################################################
# memory.limit(size = 2593804) #bump up memory limit for the big US files, if this doesn't work break into chunks, run, then merge

foreach (i=1:length(files)) %do% { 
  
  dat <- readRDS(paste0(wd, "/cleaned_hifld/",files[i])) #read in cleaned data file
  
  if (st_geometry_type(dat, by_geometry = F) == "MULTILINESTRING" | 
      st_geometry_type(dat, by_geometry = F) == "LINESTRING") {  #run on line geometries
  
   sub <- bgs %>% filter(STATEFP == 20) #subset to state of KS
   dat_s <- st_intersection(dat, sub) %>% st_union(.) #clip dataset to Kansas then merge the line network into a single feature

  avail<-  sub %>%
    mutate(exist = st_intersects(dat_s, ., sparse = FALSE)[1,])

  saveRDS(avail, paste0(wd, "/avail/","ks", files[i]))
  }
}
  
foreach (i=1:length(files)) %do% { 
  
  dat <- readRDS(paste0(wd, "/cleaned_hifld/",files[i])) #read in cleaned data file
  
  if (st_geometry_type(dat, by_geometry = F) == "MULTILINESTRING" | 
      st_geometry_type(dat, by_geometry = F) == "LINESTRING") {
  
  dat<-st_union(dat)
  
  avail<-  bgs %>%
    mutate(exist = st_intersects(dat,. , sparse = FALSE)[1,])
  
  saveRDS(avail, paste0(wd, "/avail/","US", files[i]))
  
  }
}


#gut check
dat <- readRDS(paste0(wd, "/avail/","ks",files[i])) #i=11,19,21
ggplot()  + geom_sf(data=dat, aes(fill=exist))
