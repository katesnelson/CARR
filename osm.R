# Description: This script queries the OpenStreetMap api to automatically extract locations of designated assets across all states in the U.S.
# Author: Kate Nelson
# Last Modified: August 2019


library(osmdata)
library(sf)
library(dplyr)
library(data.table)
#library(devtools, jsonlite)


wd<-getwd()
set_overpass_url('https://overpass.kumi.systems/api/interpreter')
#set_overpass_url('https://lz4.overpass-api.de/api/interpreter')

#####KEY_VALUE_PAIR for Data Extract####
#https://wiki.openstreetmap.org/wiki/Map_Features
  key<-"man_made"
  value<-"water_works"


#Loop through all states
    states<- c('Alabama','Arizona','Arkansas','California','Colorado','Connecticut','Delaware','Florida','Georgia','Idaho','Illinois','Indiana','Iowa','Kansas', 
               'Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire', 
               'New Jersey','New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania', 'Rhode Island','South Carolina', 
               'South Dakota','Tennessee','Utah','Vermont','Virginia','Washington state','West Virginia','Wisconsin','Wyoming','Texas')
    
    #rel[admin_level=4]["is_in:country_code"=US]  https://help.openstreetmap.org/questions/29485/how-to-get-state-list-for-a-country
 
n<-length(states)
  
  #initialize
fields<-c("name","osm_id","addr.street","addr.housenumber","addr.state","addr.city", "addr.postcode",paste0(key))
  q <- opq (states[1], timeout=900, memsize=1073741824) %>%
    add_osm_feature(key = key, value = value) %>% 
    osmdata_sf()
  y<-q$osm_points
  y<-y[!is.na(y$name),]
  y<-y[, colnames(y) %in% fields]
  y2<-q$osm_polygons
  y2<-y2[!is.na(y2$name),]
  y2<-y2[, colnames(y2) %in% fields]
  
  #loop through remaining states and add to sf objects
  for (i in 2:n) {
      q <- opq (states[i], timeout=900, memsize=1073741824) %>%
        add_osm_feature(key = key, value = value) %>% 
        osmdata_sf()
      z<-q$osm_points
      z<-z[!is.na(z$name),]
      z<-z[, colnames(z) %in% fields]
      z2<-q$osm_polygons
      z2<-z2[!is.na(z2$name),]
      z2<-z2[, colnames(z2) %in% fields]
       if (!is.null(z)){
         if ( nrow (z) > 0){ #equivalent to length(y[[1]]) 
          l<-list(y,z)
      yb<- sf::st_as_sf(data.table::rbindlist(l, fill=TRUE))
      y<-yb
       }}
       if(!is.null(z2)){
         if( nrow (z2) > 0){
      l2<-list(y2,z2)
      y2b<- sf::st_as_sf(data.table::rbindlist(l2, fill=TRUE))
      y2<-y2b
       }}
      #Sys.sleep(1) #currently turned off as sending queries to non-rate limited server, when using default rate-limited server could turn this on to pause between calls
      print(states[i]) #this tells me where the loop is getting hung up and throwing an error
  }
  
  saveRDS(y,paste0(value,"_pt.rds"))
  saveRDS(y2,paste0(value,"_ply.rds"))

  
  plot(y[1:length(st_geometry(y)),1])
  plot(y2[1:length(st_geometry(y2)),1])
  
  ##################################################
  ###Check the data pulls we want for local area####
  ##################################################

#Essentials

q <- opq ("kansas") %>%
  add_osm_feature(key = "shop", value~"supermarket") %>% 
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])



# q <- opq ("manhattan kansas") %>%
#   add_osm_feature(key = "shop", value="general") %>%
#   osmdata_sf()
# y<-q$osm_points
# y<-y[!is.na(y$name),]
# y2<-q$osm_polygons
# y2<-y2[!is.na(y2$name),]
# plot(y[,1])
# plot(y2[,1])


q <- opq ("manhattan kansas") %>%
  add_osm_feature(key = "amenity", value="fuel") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q <- opq ("manhattan kansas") %>%
  add_osm_feature(key = "amenity", value="dentist") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])


q <- opq ("manhattan kansas") %>%
  add_osm_feature(key = "amenity", value="clinic") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q <- opq ("manhattan kansas") %>%
  add_osm_feature(key = "amenity", value="post_office") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q <- opq ("manhattan kansas") %>%
  add_osm_feature(key = "shop", value="optician") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])





#Luxury


q <- opq ("manhattan kansas") %>%
  add_osm_feature(key = "shop", value="department_store") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q <- opq ("manhattan kansas") %>%
  add_osm_feature(key = "shop", value="hardware") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q <- opq ("manhattan kansas") %>%
  add_osm_feature(key = "shop", value="doityourself") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q <- opq ("manhattan kansas") %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])



#Parks & Recreation

q<-opq("New Hampshire")%>%
  add_osm_feature(key = "leisure", value="park") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q<-opq("kansas")%>%
  add_osm_feature(key = "tourism", value="hotel") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])


q<-opq(" kansas")%>%
  add_osm_feature(key = "tourism", value~"zoo") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q<-opq("utah")%>%
  add_osm_feature(key = "leisure", value~"nature_reserve") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1], add=T, col="red")
plot(y2[,1])

q<-opq(" kansas")%>%
  add_osm_feature(key = "tourism", value~"museum") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q<-opq(" kansas")%>%
  add_osm_feature(key = "amenity", value~"cinema") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q<-opq("kansas")%>%
  add_osm_feature(key = "amenity", value~"theatre") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q<-opq("kansas")%>%
  add_osm_feature(key = "amenity", value~"arts_centre") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q<-opq("kansas")%>%
  add_osm_feature(key = "amenity", value~"community_centre") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q<-opq("kansas")%>%
  add_osm_feature(key = "amenity", value~"library") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q<-opq("manhattan kansas")%>%
  add_osm_feature(key = "leisure", value~"swimming_pool") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q<-opq("manhattan kansas")%>%
  add_osm_feature(key = "leisure", value~"sports_centre") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q<-opq("manhattan kansas")%>%
  add_osm_feature(key = "leisure", value~"water_park") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q<-opq("kansas")%>%
  add_osm_feature(key = "leisure", value~"playground") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])

q<-opq("kansas")%>%
  add_osm_feature(key = "leisure", value~"fitness_centre") %>%
  osmdata_sf()
y<-q$osm_points
y<-y[!is.na(y$name),]
y2<-q$osm_polygons
y2<-y2[!is.na(y2$name),]
plot(y[,1])
plot(y2[,1])


head (available_features ())

#https://towardsdatascience.com/reverse-geocoding-in-r-f7fe4b908355
#https://rdrr.io/cran/tmaptools/man/geocode_OSM.html
#https://www.rdocumentation.org/packages/ggmap/versions/2.6.1/topics/geocode
#https://rdrr.io/cran/googleway/man/google_reverse_geocode.html

