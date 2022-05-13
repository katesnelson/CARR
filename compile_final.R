#This script will be used to compile all of the metrics produced for the CAR index. 


library(pacman)

p_load(tidyverse, sf, doParallel, tidycensus, units)

wd<-getwd()





#####################################
### Building the Full US Dataset ###
####################################


### Read in and Combine Availability Files ###


files<-list.files(paste0(wd,"/avail/"), pattern = "^US")


  n<-detectCores()
  cl <- makeCluster(n/2) 
  registerDoParallel(cl)


  x<- foreach (i=1:length(files), .packages = c("tidyverse","sf","units"), .combine = 'cbind') %dopar% {
    
      df<-readRDS(paste0(wd,"/avail/", files[i]))
      
      df<- df %>% mutate(Feature = str_remove(files[i], "\\..*") %>% str_remove (., "US")) 
      
      if("density" %in% names(df)){
        df<-df %>% mutate_at("density", ~set_units(density, 1/km^2)) #convert from count per sqm to count per sqkm
      }else{
        df<-df %>% mutate(density = ifelse(exist==TRUE,1,0)) #for those features that are either present or not represent "density" using a 0 or 1
      }
      
      df<- df %>% select(density, Feature, GEOID) %>% st_drop_geometry() %>%  
        pivot_wider(., id_cols = GEOID, names_from = Feature, values_from = density) %>%
         rename_all(paste0, "_avail")
      
  }
  
  parallel::stopCluster(cl)
  
  df<- x %>% select(unique(colnames(.))) %>% rename(GEOID = GEOID_avail) #get rid of duplicate GEOID_avail columns and rename GEOID

  final_avail <- df %>% 
    select(-c("Crushed_Stone_Operations_avail","Intermodal_Freight_Facilities_avail")) #get rid of things we don't need
  
  

### Read in and combine Accessability Files ###
  
  
  files<-list.files(paste0(wd,"/access/"), pattern = "^US")
  
  
  n<-detectCores()
  cl <- makeCluster(n/2) 
  registerDoParallel(cl)
  
  
  x<- foreach (i=1:length(files), .packages = c("tidyverse","sf","units"), .combine = 'cbind') %dopar% {
    
    df<-readRDS(paste0(wd,"/access/", files[i]))
    
    df<- df %>% mutate(Feature = str_remove(files[i], "\\..*") %>% str_remove (., "US")) 
    
    df<-df %>% mutate_at("dist", ~set_units(dist, km)) #convert from meter to km
    
    df<- df %>% select(dist, Feature, GEOID) %>% st_drop_geometry() %>%  
      pivot_wider(., id_cols = GEOID, names_from = Feature, values_from = dist) %>%
      rename_all(paste0, "_access")
    
  }
  
  parallel::stopCluster(cl)
  
  df<- x %>% select(unique(colnames(.))) %>% rename(GEOID = GEOID_access) #get rid of duplicate GEOID_access columns and rename GEOID
  
  final_access <- df %>% 
    select(-c("Crushed_Stone_Operations_access","Intermodal_Freight_Facilities_access", "Cellular_Service_Areas_access")) #get rid of things we don't need
  
  
### Now put Availability and Accessability Together ###
  
  
  full <- left_join(final_avail, final_access, by="GEOID")
  
  saveRDS(full, "US_avail_access_09032021.rds")
  write.csv(full, "US_avail_access_09032021.csv")
  
  glimpse(full)
  
### Add Relative Rurality Variables ###
  
  
  files<-list.files(paste0(wd,"/relative_rurality/"), pattern = "^US")
  
  #distance to metro is [1], percent developed area is [3], population and popualtion density are [328], and time to work is [329]
 
   dm<-readRDS(paste0(wd,"/relative_rurality/", files[1]))

   dm <- dm %>% select(GEOID, dist) %>% st_drop_geometry() %>% 
     mutate_at("dist", ~set_units(dist, km)) %>% #convert from meter to km
     rename(Dist_to_metro = dist)
   
  
   pda <- readRDS(paste0(wd,"/relative_rurality/", files[3]))
   
   pda <- pda %>% select(GEOID, perc_developed) %>% st_drop_geometry() 
   
   
   pop <- readRDS(paste0(wd,"/relative_rurality/", files[328]))
  
   pop_dens <- pop %>% select(GEOID, pop_density) %>% st_drop_geometry()
   pop <- pop %>% select(GEOID, pop) %>% st_drop_geometry()
   
   
   tw <- readRDS(paste0(wd,"/relative_rurality/", files[329]))
   
   tw <- tw %>% dplyr::select(GEOID, wtavg) %>% st_drop_geometry() %>%
     rename(time_to_work = wtavg) #time in minutes
   
 
   ##### SAVE ########  
   
   full_final <- left_join(full, pop, by ="GEOID") %>% left_join(., pop_dens, by ="GEOID") %>% 
                left_join(., tw, by ="GEOID") %>% left_join(., dm, by ="GEOID") %>% left_join(., pda, by ="GEOID")
  
   glimpse(full_final)
   
   
   full_final_sf <- left_join(bgs, full_final, by ="GEOID")
   
   saveRDS(full_final_sf, "US_full_09032021.rds")
   
   full_final <- full_final_sf %>% st_set_geometry(NULL) %>% transform(GEOID = as.factor(GEOID))
   
   write.csv(full_final, "US_full_09202021.csv")
   

   #### Quick NA Check ####
   
   na_check <- full_final %>% filter(!STATEFP %in% c("02","15","72","78","60","66","69")) %>% summarise_all(funs(sum(is.na(.))))
   
   

   