# Description: This script will be used to clean data not obtained from OSM or HIFLD. 
#   Specifically, EPA drinking water data, FTC Broadband data, NLCD developed area data, metropolitan distance, and population data were cleaned and processed. 
# Author: Kate Nelson
# Last Modified: January 2022

library(pacman)

p_load(tidyverse, sf, doParallel, stringr, data.table, ff, readxl, fuzzyjoin, raster, rgdal, SpaDES, snow, ClusterR,ipumsr, units)

wd<-getwd()

##################################################
### Read in List of Files and Block Group Data ###
#################################################


files<-list.files(paste0(wd,"/other_data/")) #get list of files

bgs <- st_read("tl_2018_blockgroup.shp") %>% #load census block group geometries
  dplyr::select(GEOID, STATEFP) #select only needed attributes

crs<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" # Albers Equal Area

bgs<-st_transform(bgs, crs)

ctr <- bgs %>% st_centroid(.) #centroids of blockgroups for distance calculations

############################################################
### Clean Drinking Water Data and Calc Avail and Access  ###
###########################################################

    # This data comes from EPA and uses non-census locational standards for tribal areas. Locations are provided as service areas at multiple scales.
    #cities  obtained here (places) https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.2019.html
    #zipcodes from here https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2019.html
    #tribal areas from https://edg.epa.gov/metadata/catalog/main/home.page
    # counties from census

w <- read.csv(file=paste0(wd,"/other_data/",files[46])) #note that for Oklahoma many counties were listed as "b", manually replaced with the PWS community name in the dowloaded spreadsheet (these may or may not be counties)

#get count of unique PWS IDs for each geographic unit of each geographic area type (County, City, Zip Code, Tribal Area)
    
    zip_zcta <- read_excel(paste0(wd,"/other_data/",files[48])) #reading in postal zip code to zcta crosswalk from UDS mapper

    w_z <- w %>% filter(Zip.Code.Served != "-") %>% #get rid of the dashes
      mutate(Zip.Code.Served = ifelse(nchar(Zip.Code.Served) < 5, paste0("0",Zip.Code.Served), Zip.Code.Served)) %>%
      left_join(., zip_zcta, by=c("Zip.Code.Served"="ZIP_CODE")) %>% #join with postal zip code to ZCTA crosswalk
      group_by(ZCTA) %>%
      select(PWS.ID) %>% distinct() %>%
      summarise(count_z = n())  #get count of unique providers in each zipcode
      
    
    w_t <- w %>% filter(Tribal.Code != "-") %>% #get rid of the dashes
      group_by(Tribal.Code) %>%
      select(PWS.ID) %>% distinct() %>%
      summarise(count_t = n()) #get count of unique providers in each tribal code
       
    
    w_c <- w %>% filter(City.Served != "-") %>% #get rid of the dashes
      mutate(City.Served = str_remove(City.Served, "\\(.*")) %>% #get rid of parentheses and the text in them in city names, text after dashes, "boro", "twp" and following text
      mutate(City.Served = str_remove(City.Served, "\\-.*")) %>% 
      mutate(City.Served = str_remove(City.Served, "\\TWP.*")) %>% 
      mutate(City.Served = str_remove(City.Served, "\\ BORO.*")) %>% 
      mutate(State_City = paste0(Primacy.Agency, "_",City.Served)) %>% #create a State_city identifier since the same city name can be found in multiple states
      group_by(State_City) %>%
      select(PWS.ID) %>% distinct() %>%
      summarise(count_c = n()) #get count of unique providers in each unique state - city combination
      
    w_cnty <- w %>% filter(County.Served != "-") %>% #get rid of the dashes
      mutate(State_Cnty = paste0(Primacy.Agency, "_",County.Served)) %>% #create a State_cnty identifier since the same county name can be found in multiple states
      group_by(State_Cnty) %>%
      select(PWS.ID) %>% distinct() %>%
      summarise(count_cnty = n())  #get count of unique providers in each state - county combination
      

#now join these lists to the geographic area spatial reference datasets

    #Zip Codes
      z <- st_read(paste0(wd,"/other_data/",files[37])) #reading in zipcode shapefile
      
      w_z_sf <- w_z %>% inner_join(z,., by=c("ZCTA5CE10" = "ZCTA")) %>% select (ZCTA5CE10, count_z) 
    
    #Tribal Codes
      
      st_layers(paste0(wd,"/other_data/",files[44])) #see what layers we have in the tribal boundaries geodatabase
                
      t <- st_read(paste0(wd,"/other_data/",files[44]), layer = "LOWER48_TRIBES") #reading in lower 48 states tribal boundaries
          #none of the Alaska data in the other layers matches with the EPA data
      
      w_t_sf <- w_t %>% inner_join(t,., by=c("BIA_CODE"="Tribal.Code")) %>% select(BIA_CODE, count_t) #some Tribes (as designated by BIA_CODE) have multiple locations
       
    #Cities
      
      c <- st_read(paste0(wd,"/other_data/",files[4])) #reading in city (US census Place) shapefile
      
      st_fips <- read_excel(paste0(wd,"/other_data/",files[18]))
      
      c <- c %>% left_join(., st_fips, by = c("STATEFP"="FIPS")) %>% 
        mutate(State_City = paste0(Name,"_",NAME)) %>%
        mutate(State_City = tolower(State_City)) #get rid of case sensitivity issues with join
      
      w_c <- w_c %>% mutate(State_City = tolower(State_City)) #get rid of case sensitivity issues with join
      
      w_c_sf <- w_c %>% inner_join(c,., by="State_City") %>% select(State_City, count_c) 
      
      notin <- w_c %>% filter(!(State_City %in% c$State_City)) #some unincorporated"cities" not in the US Census Places shapefiles 
      #checked these census places with the tlgdb_2019_a_us_substategeo.gdb and it matches with census designated places and incorporated places
      #looks like the unincorporated places are what are missing, see https://www.census.gov/content/dam/Census/data/developers/understandingplace.pdf, not all places are included in census products
     #some do not match as EPA region is given instead of state
      # a few punctiation, abbreviation, and typo matching errors
    
    #Counties
      
      cnty <- st_read(paste0(wd,"/other_data/",files[22])) #reading in county shapefile
      
      st_fips <- read_excel(paste0(wd,"/other_data/",files[18]))
      
      cnty <- cnty %>% left_join(., st_fips, by = c("STATEFP"="FIPS")) %>% 
        mutate(State_Cnty = paste0(Name,"_",NAME)) %>%
        mutate(State_Cnty = tolower(State_Cnty)) %>% #get rid of case sensitivity issues with join
        mutate(State_Cnty = str_replace_all(State_Cnty, "ñ", "n")) %>%
        mutate(State_Cnty = str_replace_all(State_Cnty, " ", "")) #get rid of punctuation and spacing issues
      
      w_cnty <- w_cnty %>% mutate(State_Cnty = tolower(State_Cnty)) %>% #get rid of case sensitivity issues with join
        mutate(State_Cnty = str_remove(State_Cnty, "\\ borough.*")) %>% #ughh, get rid of stupid naming issues 
        mutate(State_Cnty = str_remove(State_Cnty, "\\ census.*")) %>%
        mutate(State_Cnty = str_remove(State_Cnty, "\\ municipality.*")) %>%
        mutate(State_Cnty = str_remove(State_Cnty, "\\ municipio.*")) %>% 
        mutate(State_Cnty = str_remove(State_Cnty, "\\ parish.*")) %>% 
        mutate(State_Cnty = str_remove(State_Cnty, "\\ city.*")) %>%
        mutate(State_Cnty = str_replace_all(State_Cnty, "ñ", "n")) %>%
        mutate(State_Cnty = str_replace_all(State_Cnty, " ", "")) %>%
        distinct(State_Cnty, .keep_all = T)
        
      w_cnty_sf <- w_cnty %>% inner_join(cnty,., by="State_Cnty") %>% select(State_Cnty, count_cnty) 
      
      notin <- w_cnty %>% filter(!(State_Cnty %in% cnty$State_Cnty)) #issues with naming conventions, some have spaces or extra words, etc... resolved, most of remaining few missing matches are not for contiguous US or are from Oklahoma
      
    #spatial join block group centroids to each geographic area dataset with unique PWSs

      #Zips  
      w_z_sf <- st_transform(w_z_sf, crs) 
      w_z_avail <- st_join(ctr, w_z_sf, join=st_intersects)
      
      #Tribes 
      w_t_sf <- st_transform(w_t_sf, crs) 
      w_t_avail <- st_join(ctr, w_t_sf, join=st_intersects) 
      w_t_avail_2 <- w_t_avail %>% group_by(GEOID) %>% 
        mutate(count_t= sum(count_t, na.rm=T))  %>% #some blockgroups may overlap with multiple tribal areas, combine them
        distinct(GEOID, .keep_all=T)
      
      #Cities  
      w_c_sf <- st_transform(w_c_sf, crs) 
      w_c_avail <- st_join(ctr, w_c_sf, join=st_intersects)
      
      #Counties  
      w_cnty_sf <- st_transform(w_cnty_sf, crs) 
      w_cnty_avail <- st_join(ctr, w_cnty_sf, join=st_intersects)
      
    #consolidate
      
      w_avail_full <- w_z_avail %>% left_join(., w_c_avail %>% st_set_geometry(., NULL), by="GEOID") %>%
                      left_join(., w_cnty_avail %>% st_set_geometry(., NULL), by="GEOID") %>%
                      left_join(., w_t_avail_2 %>% st_set_geometry(., NULL), by="GEOID") #gather all the different community water system geometries together
      
  ###Availability - get absence or presence of any PWS in each block group and reassociate with full blockgroup geometry

    w_avail <- w_avail_full %>% mutate(across(starts_with("count"), ~replace_na(.,0))) %>%
      mutate(providers = (count_z + count_c + count_cnty + count_t)) %>%
      mutate(density = ifelse(providers > 0, 1, 0)) %>%
      st_set_geometry(., NULL) %>%
      left_join(bgs, ., by="GEOID")
    
    saveRDS(w_avail, paste0(wd, "/avail/","US", "drinkingwater.rds"))
    
    ggplot() + geom_sf(data=w_avail %>% filter(!STATEFP.x %in% c("02","15","72","78","60","66","69")), aes(fill=as.numeric(density)), color=NA) +
      theme(legend.position = "none")
    
    ggplot() + geom_sf(data=w_avail %>% filter(!STATEFP.x %in% c("02","15","72","78","60","66","69")), aes(fill=as.numeric(providers)), color=NA) +
      theme(legend.position = "none")
    
  ###Accessability
    
    access <- ctr %>% mutate(nearest = try(st_nearest_feature(.,w_avail))) %>% 
      mutate(dist = st_distance(., w_avail[nearest,], by_element = TRUE)) %>% 
      st_drop_geometry(.) #get distance from all US blockgroup centroids to closest blockgroup with drinking water
    
    
    w_access <- access %>% left_join(bgs, ., by="GEOID")
    
    saveRDS(w_access, paste0(wd, "/access/","US", "drinkingwater.rds"))
    
    ggplot() + geom_sf(data=w_access %>% filter(!STATEFP.x %in% c("02","15","72","78","60","66","69")), aes(fill=log(as.numeric(dist))), color=NA) +
      theme(legend.position = "none")
    
#######################################################
### Clean Broadband Data and Calc Avail and Access ###
######################################################

    #This data is by provider and census BLOCK, so is huge!!!! https://opendata.fcc.gov/Wireline/Fixed-Broadband-Deployment-Data-June-2017-Status-V/9r8r-g7ut

        # bb<-fread(paste0(wd,"/other_data/",files[1]), header = T)
        # file_in    <- file(paste0(wd,"/other_data/",files[1]))
        # chunk_size <- 100000 # choose the best size for you
        # x          <- readLines(file_in, n=chunk_size)

bb <- read.csv.ffdf(file=paste0(wd,"/other_data/",files[8]), header=TRUE, VERBOSE=TRUE, 
                   first.rows=10000, next.rows=50000, colClasses=NA) #reading in data bit by bit


bb <- as.data.frame(bb)  %>% 
  transform(Census.Block.FIPS.Code = as.character(Census.Block.FIPS.Code)) %>%
  filter(Consumer == 1 | Business == 1) %>% #screen to only providers that offer general consumer and/or business broadband access in a block, stil 70M + records, lol
  filter(!(Technology.Code %in% c("30","60","70","90","0"))) #screen to only types of dsl, cable or fiber

      #got 2010 block boundaries from IPUMS (by state), but I think I can do avail without bothering with the shapefiles by using the GEOID
      #could also get those boundaries here https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-geodatabase-file.html
      #https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html, https://www.census.gov/programs-surveys/geography/about/glossary.html#:~:text=Census%20blocks%20nest%20within%20all,nest%20within%20state%20and%20county.
      #blocks are 15 characters, block-groups are the first 12 characters (from left to right)
      #so let's extract the census block group GEOID from the block GEOID

bb <- bb %>% mutate(Census.Block.FIPS.Code = ifelse(nchar(Census.Block.FIPS.Code) == 14, paste0("0",Census.Block.FIPS.Code),Census.Block.FIPS.Code)) %>% #becuase the FIPS was stored as numeric leading zeros were dropped, add them back
             mutate(GEOID_bg = substr(Census.Block.FIPS.Code,1,12)) #extract the first 12 characters

  ###Availability 
  
  bb_avail <- bb %>% group_by(GEOID_bg) %>% 
    dplyr::select(Provider.ID) %>% distinct() %>%
    summarise(count = n()) #get count of unique providers in each block group
  
  bb_avail_sf <- left_join(bgs, bb_avail, by = c("GEOID"="GEOID_bg"))
  
  bb_avail_sf <- bb_avail_sf %>% mutate(area = st_area(.)) %>% mutate(density = count/area)
  
   saveRDS(bb_avail_sf, paste0(wd, "/avail/","US", "broadband.rds"))
  
   
   #investigating missing values
   list1<-unique(bb_avail$GEOID_bg)
   list2<-unique(bgs$GEOID)
   list3<- unique(blks$GEOID) %>% substr(., 1,12) 
   list3<- unique(list3)
   
   
   test<-list1[which(!list1 %in% list2)] #69 blocks in the bb data for which there is no matching block group in the 2019 block group data
   test2<-list2[which(!list2 %in% list1)] #783 block groups for which there is no matching block in the bb data. These should be mostly becuase of complete lack of bb access here.
   test3<-list3[which(!list3 %in% list2)] #74 blockgroup from the blk data for which there is no match in bgs
   
   #The blks and bgs data are both based on 2010 census enumeration units (https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2019/2019_TIGER_GDB_Record_Layouts.pdf, 
   #https://www.census.gov/programs-surveys/acs/geography-acs/geography-boundaries-by-year.2018.html), however a few areas have had changes in boundaries. I have 2018 block groups and 2019 blocks so perhaps this explains the 74 non-matching.
  
  ###Accessability
  blks <- st_read("tlgdb_2019_a_us_block.gdb")
  
  blks <- st_transform(blks, crs)
  
  blks <- blks %>% mutate(State= substr(GEOID, 1,2)) #set things up to run distance calcs by state
  
  bb <- bb %>% mutate(State=substr(Census.Block.FIPS.Code,1,2))
  
  full<- data.frame() #initalize with empty dataframe
  
  n<-unique(blks$State) #vector of states to do distance calcs for
    
    foreach (i = 1:length(n)) %do% {
      
      df1 <- blks %>% filter(State == n[i])
    
      df2 <- bb %>% filter(State == n[i]) %>% select(State, GEOID_bg, Census.Block.FIPS.Code)
      
      dat <- left_join(df2, df1, by=c("Census.Block.FIPS.Code"="GEOID")) #get block geometries for places with broadband 
    
      full <- rbind(full,dat) #all blocks with a (hardline) broadband provider
    }
  
    saveRDS(full,"broadband_blocks.rds")
    full<-readRDS("broadband_blocks.rds")
  
    full<- full  %>% st_as_sf(.) %>% rename(geometry=SHAPE) %>% st_set_crs(crs)

  
  access <- ctr %>% mutate(nearest = try(st_nearest_feature(.,full))) %>% 
    mutate(dist = st_distance(., full[nearest,], by_element = TRUE)) %>% 
    st_drop_geometry(.) #get distance from all US blockgroup centroids to closest block with broadband
  
      # saveRDS(access, paste0(wd, "/access/","US", "broadband.rds"))
   

  bb_access <- access %>% left_join(bgs, ., by="GEOID")
  
  saveRDS(bb_access, paste0(wd, "/access/","US", "broadband.rds"))

  ggplot() + geom_sf(data=bb_access %>% filter(!STATEFP.x %in% c("02","15","72","78")), aes(fill=as.numeric(dist)), color=NA)
  
###############################################################################  
### Calculate Developed Area by Blockgroup for Relative Rurality Component ###
##############################################################################
  
  urban_cover <- raster(paste0(wd,"/other_data/",files[12])) #nlcd urban impervious developed area percent raster, https://www.mrlc.gov/data/nlcd-2019-percent-developed-imperviousness-conus
  
 
  bgs_1 <- st_transform(bgs, st_crs(urban_cover))
  
  bgs_1 <- bgs_1 %>% mutate(county_fips = substr(GEOID, 1,5))
  
  #chunk by state
  df2<-data.frame()
  
  n<-unique(bgs_1$county_fips) #vector of blockgroups to do  calcs for
  
  length(n) <- prod(dim(matrix(unlist(n), ncol = 10))) #pad the end of the list so we have an even matrix without recycling of values
  n<- matrix(unlist(n), ncol=10, byrow=TRUE) #break into a X x 20 matrix for beocat parallel runs
  
  detectCores()
  cl <- makeCluster(10) 
  registerDoParallel(cl)
  
  foreach(i=1:nrow(n), .combine='rbind') %do% {
    x <- foreach(j=1:ncol(n), .combine = 'rbind', .errorhandling="remove") %dopar% { 
      library(tidyverse)
      library(sf)
      library(raster)
      
      # foreach (i = 1:length(n)) %do% {
    
   df1 <- bgs_1 %>% filter(county_fips == n[i,j])
    
    r1 <- crop(urban_cover, df1)
    
    perc_developed <- raster::extract(r1, df1, fun=mean, na.rm=T, weights=F) #take average of pixel values in each block group, each pixel value is the estimated % impervious developed cover in that pixel
    
    df2 <- cbind(df1, perc_developed)
    
    # full <- rbind(full,df2)
   
    return(df2)
    }
    saveRDS(x, paste0(wd, "/relative_rurality/","US", "percent_developed",i,".rds"))
  }
  
  stopCluster(cl)
  
  
  
  int_files<-list.files(path= paste0(wd, "/relative_rurality/"), pattern= '^USpercent_developed[0-9]') #get list of files to integrate
  
  
  full_int<-readRDS(paste0(wd, "/relative_rurality/",int_files[1]))  %>%  st_transform(., crs)#initialize our full intersection file with the first file
  
  foreach (i=2:length(int_files)) %do% {
    
    f<-readRDS(paste0(wd, "/relative_rurality/",int_files[242])) %>% st_transform(., crs)#read in the next file
    
    full_int<-rbind(full_int,f)
    
  }
  
  # perc_developed <- raster::extract(urban_cover, bgs_1, fun=mean, na.rm=T, weights=F) #take average of pixel values in each block group, each pixel value is the estimated % impervious developed cover in that pixel
  # 
  # p_dev <- cbind(bgs, perc_developed) %>% mutate(percent = perc_developed*100) %>%
  #   st_transform(., crs)
  
  saveRDS(full_int, paste0(wd, "/relative_rurality/","US", "percent_developed_full.rds"))
  
  pd<-readRDS(paste0(wd, "/relative_rurality/","US", "percent_developed_full.rds")) # this is only for contiguous US
  
  ggplot() + geom_sf(data=pd , aes(fill=perc_developed), color=NA)
  
  
 ################################################################################## 
### Calculate Distance from Blockgroup to Metro for Relative Rurality Component ###
###################################################################################

  metro <- st_read(paste0(wd,"/other_data/",files[30])) #read in census metropolitan and micropolitan statistical areas (cbsa)
  
  metro <- metro %>% filter(str_detect(NAMELSAD, 'Metro')) #pull out just metropolitan areas, remove micropolitan areas
  
  metro <- metro %>% st_transform(., crs)
  
  d_metro <- ctr %>% mutate(nearest = try(st_nearest_feature(.,metro))) %>% 
    mutate(dist = st_distance(., metro[nearest,], by_element = TRUE)) %>% 
    st_drop_geometry(.) #get distance from all US blockgroup centroids to closest metro area
  
  d_metro <- d_metro %>% left_join(bgs, ., by="GEOID")
  
  saveRDS(d_metro, paste0(wd, "/relative_rurality/","US", "dist_metro.rds"))

  ggplot() + geom_sf(data=d_metro %>% filter(!STATEFP.x %in% c("02","15","72","78","60","66","69")), aes(fill=as.numeric(dist)), color=NA) +
    theme(legend.position = "none")

###################################################################################################
### Clean Time to Work, Population, and Population Density data for Relative Rurality Component ###
###################################################################################################
  
  #timetowork
  tw<-read_nhgis(paste0(wd,"/other_data/",files[9])) #read in data
  
  tw<- tw %>% dplyr::select(GISJOIN:BLKGRPA,ALU3E001:ALU3E013)
  
  cdbk.bg<-read_ipums_codebook(paste0(wd,"/other_data/",files[10])) #read in codebook
  
  var_names<-cdbk.bg$var_info$var_label #extract variable names from codebook
  
  var_names <- var_names[c(1:13,43:55)]
  
  colnames(tw)<-var_names #assign variable names to columns
  
  glimpse(tw)
  
  tw_wtav <- tw %>% mutate(wtavg = (`Less than 5 minutes`*2.5 + `5 to 9 minutes`*7 + `10 to 14 minutes`*12 + `15 to 19 minutes`*17 +
                                      `20 to 24 minutes`*22 + `25 to 29 minutes`*27 + `30 to 34 minutes`*32 + `35 to 39 minutes`*37 +
                                      `40 to 44 minutes`*42 + `45 to 59 minutes`*52 + `60 to 89 minutes`*74.5 + `90 or more minutes`*90)/Total) 
  
  # get rid of NAs where weighted average divides 0 by 0 (no popoulation)? or set to population average so it doesn't swing things one way or another?
  
  tw_wtav_sf <- tw_wtav %>% mutate(GEOID=paste0(`State Code`,`County Code`,`Census Tract Code`, `Block Group Code`)) %>%
            dplyr::select(`GEOID`,wtavg) %>% left_join(bgs, ., by="GEOID")
  
  saveRDS(tw_wtav_sf, paste0(wd, "/relative_rurality/","US", "timetowork.rds"))
  
  #population
  pop<-read_nhgis(paste0(wd,"/other_data/",files[12])) #read in data
  
  pop<- pop %>% dplyr::select(GISJOIN:BLKGRPA,ALUBE001)
  
  cdbk.bg<-read_ipums_codebook(paste0(wd,"/other_data/",files[13])) #read in codebook
  
  var_names<-cdbk.bg$var_info$var_label #extract variable names from codebook
  
  var_names <- var_names[c(1:13,43)]
  
  colnames(pop)<-var_names #assign variable names to columns
  
  glimpse(pop)
  
  bgs <- bgs %>% mutate(area=st_area(.))
  
  pop_sf <- pop %>% mutate(GEOID=paste0(`State Code`,`County Code`,`Census Tract Code`, `Block Group Code`)) %>%
    dplyr::select(`GEOID`,Total) %>% left_join(bgs, ., by="GEOID")
  
  pop_sf <- pop_sf %>% mutate(pop = Total, pop_density = Total/area) %>% mutate_at("pop_density", ~set_units(pop_density, 1/km^2)) #convert from count per sqm to count per sqkm
  
  saveRDS(pop_sf, paste0(wd, "/relative_rurality/","US", "pop.rds"))
  
  
