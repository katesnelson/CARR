# Description: This script will be used to clean the extracted OpenStreetMap data. Specifically, duplicate locations across
#   point and polygon files will be reconciled and all locations will be converted to points. 
# Author: Kate Nelson
# Last Modified: June 2020


library(pacman)

p_load(tidyverse, sf, doParallel)

wd<-getwd()

#########################################################
### Read in OSM pt and poly files for one asset type ###
########################################################

#get file list
files<-list.files(paste0(wd,"/osm/")) #get list of osm files



#extract asset type strings and create a list of uniques
assets<-c()
foreach (i=1:length(files)) %do% {
if (!is.na(str_match(files[i], "\\s*_ply\\d"))){
  assets[i]<-str_remove(files[i], "\\s*_ply\\d\\s*.rds")
} else { 
    if (!is.na(str_match(files[i], "\\s*_pt\\d"))){
    assets[i]<-str_remove(files[i], "\\s*_pt\\d\\s*.rds")
} else { 
    if (!is.na(str_match(files[i], "\\s*_pt"))){
    assets[i]<-str_remove(files[i], "\\s*_pt.rds")
} else { 
    if (!is.na(str_match(files[i], "\\s*_ply"))){
    assets[i]<-str_remove(files[i], "\\s*_ply.rds")
      }
}}}
  
}

uassets<-unique(assets)


foreach (i=1:length(uassets)) %do% {

#identify asset files with pt or poly endings and read in
read_files<-list.files(paste0(wd,"/osm/"), pattern=paste0("^",uassets[i])) #files for asset
read_files<-paste0(wd,"/osm/",read_files, sep="") #full file path name

poly_files<-read_files[!is.na(str_match(read_files, "\\s*_ply"))] #separate out polygon files
pt_files<-read_files[!is.na(str_match(read_files, "\\s*_pt"))] #separate out point files

polys<-lapply(poly_files, readRDS) #read in the files
pts<-lapply(pt_files, readRDS)

#merge multiple polygons and multiple points

polys<-do.call(rbind, polys) #if there are multiple pt or poly files for a single asset merge them
pts<-do.call(rbind, pts)

crs<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" # Albers Equal Area

pt<-st_transform(pts, crs)
ply<-st_transform(polys, crs)


##########################
### Remove Duplicates ###
#########################

# Remove any duplicate geometries in pt and ply files
   pt2 <- pt %>% distinct(., geometry, .keep_all=T)
  
   ply2 <- st_make_valid(ply) %>% distinct(., geometry, .keep_all=TRUE) #added st_make_valid on 071521 to address problems with the Park data

# Remove polygons that intersect with points

    ply3<-ply2 %>% filter(lengths(st_intersects(., pt2, sparse=T)) == 0)
   
# Convert remaining polygons to points using centroids
    
    ply4<-st_centroid(ply3)

# Create a master pt file
    
    pt_full<-rbind(ply4,pt2)
    
# Remove points that are close to each other that have the same name or address
        #many locs have same name, but are not located near each other
   
    dis=100 #in meters
    
    pt_close<-pt_full %>% filter(lengths(st_is_within_distance(.,dist=dis)) > 1) #find pts within 100 meters of each other
    
    pt_distinct_addr<-pt_close %>% as.data.frame(.) %>% 
        distinct_at(., vars(addr.city, addr.housenumber, addr.postcode, addr.state, addr.street), .keep_all=T) #pts close to each but that have different addresses
                                                                                                              #note that this doesn't handle pts with NA address
    
    pt_distinct_name<-pt_close %>% as.data.frame(.) %>%
      distinct_at(., vars(name),.keep_all = T) %>%  #pts close to each other with different names
      filter_at(.,vars(starts_with("addr.")), all_vars(is.na(.))) #keep only the pts with distinct names that can't be distinguished using address due to NAs
    
     pt_distincts<- rbind(pt_distinct_addr, pt_distinct_name) %>% st_as_sf(.) #put the close but distinct by name or address pts together
     
     
  # Remove locations that are really close to each but may not have same name or address
     
     dis=10
     
     t<-st_is_within_distance(pt_distincts,dist=dis) #check if our close but distincts are REALLY close
     t2<-sapply(st_is_within_distance(pt_distincts,dist=dis),"[[",1) #pull the first index value (second + values are for places nearby the first)
     t3<-unique(t2) #get unique index values so we just retain the first of pairs of really close pts
     
     pt_final_distincts<-pt_distincts[t3,]
     
  # Put together the locations NOT close to each other with those that are close but distinct
     
     dis=100
     pt_far<-pt_full %>% filter(lengths(st_is_within_distance(.,dist=dis)) == 1) #get locations not within 100 meters of each other
     pt_final<-rbind(pt_far,pt_final_distincts) #combine pts that are close but distinct or far away
     
    plot(st_geometry(pt_final))
    
    saveRDS(pt_final, paste0(wd, "/cleaned_osm/",uassets[i],".rds"))
}
  
