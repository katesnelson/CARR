# Description: Script used to create figures used in published paper (https://doi.org/10.1016/j.jrurstud.2022.12.025).
# Author: Kate Nelson

library(tidyverse)
library(ggstance)
library(sf)


#########################
### Main Text Figures ###
#########################

#Figure 1: Map of CARR

ggplot() +  
  geom_sf(data = carr_ru_ia, aes(fill= carr3_rural), color= NA, alpha = 0.7)  +
  scale_fill_distiller(palette = "BrBG", values = c(0,.25,.4, 0.5, 0.6, 0.75, 1)) + 
  ggtitle("CARR Index") +
  theme_minimal() +
  theme(legend.title = element_blank())

ggsave("carr_map.jpeg", device="jpeg", width = 6, height = 6, units = "in") 

# ggplot() +  
#   geom_sf(data = carr_ru_ia, aes(fill= carr3_rural), color= NA, alpha = 0.7)  +
#   scale_fill_gradient2(low = "#5ab4ac", mid = "#d8b365", high = "#f5f5f5", midpoint = 0.5 ) + 
#   ggtitle("CARR Index") +
#   theme_minimal() +
#   theme(legend.title = element_blank())

library(viridis)
p <- ggplot() +  
  geom_sf(data = carr_ru_ia, aes(fill= carr3_rural), color= NA)  +
  scale_fill_viridis_c(option = "mako") + 
  ggtitle("CARR Index") +
  theme_minimal() +
  theme(legend.title = element_blank())

ggsave(plot = p, "carr_mapb.jpeg", device="jpeg", width = 6, height = 6, units = "in") 

#Figure 2: Density with boxplot and N labels for CARR v. census

  options(scipen = 999) #lose the scientific notation
  
  my_xlab <- paste(levels(as.factor(carr_ru$census_ua)),"\nn=",table(carr_ru$census_ua),sep="") 
  
  ggplot() + geom_density(data=carr_ru, aes(x=carr3_rural, fill=fct_rev(census_ua),   
                                            alpha = 0.5,  color = "white"),  color = "white") +
    scale_fill_manual(values = c("#d8b365","#5ab4ac"), labels=fct_rev(my_xlab)) +
    guides(alpha="none") +
    theme_minimal() +
    xlab("CARR") + ylab("Density") +
    theme(legend.title=element_blank(), legend.position = "bottom", axis.text.x = element_text(size = 8))+
    geom_boxploth(data=carr_ru, aes(y=1, x=carr3_rural, fill=fct_rev(census_ua)), 
                  width=1.5, outlier.alpha = 0)
  
  ggsave("carr_by_ua_density_boxplot.jpeg", device="jpeg", width = 6, height = 6, units = "in") 
  
  
#Figure 3: Violin plot with N labels for CARR v. RUCC
  
  my_xlab <- paste(levels(as.factor(carr_ru$RUCC_2013)),"\nn=",table(carr_ru$RUCC_2013),sep="") 
  
  ggplot() + geom_violin(data=carr_ru %>% mutate(RUCC_2013 = as.factor(RUCC_2013)), 
                         aes(x=RUCC_2013, y=carr3_rural, fill=RUCC_2013), alpha = 0.5) +
    scale_fill_brewer(palette = "BrBG") + 
    theme_minimal() +
    xlab("RUCC Class") + ylab("CARR") +
    theme(legend.title=element_blank(), legend.position = "bottom", axis.text.x = element_text(size = 8))+
    scale_x_discrete(labels=my_xlab) 
  
  ggsave("carr_by_rucc_violin.jpeg", device="jpeg", width = 6, height = 6, units = "in")
  

#Figure 4: Violin plot with N labels for CARR v. RUCA
  my_xlab <- paste(levels(as.factor(carr_ru$RUCA_2010)),"\nn=",table(carr_ru$RUCA_2010),sep="") 
  
  ggplot() + geom_violin(data=carr_ru %>% mutate(RUCA_2010 = as.factor(RUCA_2010)), 
                         aes(x=RUCA_2010, y=carr3_rural, fill=RUCA_2010), alpha = 0.5) +
    scale_fill_brewer(palette = "BrBG") + 
    theme_minimal() +
    xlab("RUCA Class") + ylab("CARR") +
    theme(legend.title=element_blank(), legend.position = "bottom", axis.text.x = element_text(size = 8))+
    scale_x_discrete(labels=my_xlab) 
  
  ggsave("carr_by_ruca_violin.jpeg", device="jpeg", width = 6, height = 6, units = "in")
  
  

  
  carr_ru %>% 
    filter(as.numeric(substr(GEOID,1,5)) == 36061) %>%
    filter(pop != 0) %>% 
    ggplot() +
    annotation_map_tile(type = "osm", zoomin = 0) +
    geom_sf(aes(fill = carr3_rural), alpha = 0.6, color = "white", lwd = 0.5) +
    scale_fill_viridis_c(option = "mako") +
    theme_minimal() +
    theme(axis.text = element_blank(), legend.title = element_blank()) +
    annotation_scale()
  
  ggsave("newyork_carr_final.png", width = 6, height = 6, units = "in")
  
  
  carr_ru %>% 
    filter(as.numeric(substr(GEOID,1,5)) == 32023) %>%
    filter(pop != 0) %>% 
    ggplot() +
    annotation_map_tile(type = "osm", zoomin = 0) +
    geom_sf(aes(fill = carr3_rural), alpha = 0.6, color = "white", lwd = 0.5) +
    scale_fill_viridis_c(option = "mako") +
    theme_minimal() +
    theme(axis.text = element_blank(), legend.title = element_blank()) +
    annotation_scale()
  
  ggsave("vegas_carr_final.png", width = 6, height = 6, units = "in")
  
  carr_ru %>% 
    filter(as.numeric(substr(GEOID,1,5)) == 06111) %>%
    filter(pop != 0) %>% 
    ggplot() +
    annotation_map_tile(type = "osm", zoomin = 0) +
    geom_sf(aes(fill = carr3_rural), alpha = 0.6, color = "white", lwd = 0.75) +
    scale_fill_viridis_c(option = "mako") +
    theme_minimal() +
    theme(axis.text = element_blank(), legend.title = element_blank()) +
    annotation_scale()
  
  ggsave("santabarbara_carr_final.png", width = 6, height = 6, units = "in")
  
  ##################
  ### SI Figures ###
  ##################
  

  
  #Figure S1
  library(ggstance)
  options(scipen = 999) #lose the scientific notation
  
  
  ggplot() + geom_density(data=carr_ru, aes(x=carr3_rural, fill= "CARR",   
                                            alpha = 0.5,  color = "white"),  color = "white") +
    scale_fill_manual(values = c("#d8b365")) +
    guides(alpha="none", fill = "none") +
    theme_minimal() +
    xlab("CARR") + ylab("Density") +
    theme(legend.title=element_blank(), legend.position = "bottom")+
    geom_boxploth(data=carr_ru, aes(y=1, x=carr3_rural, fill="CARR"), width = 1.5, outlier.alpha = 0)
  
  ggsave("carr_density_final.jpeg", device="jpeg", width = 6, height = 6, units = "in") 
  
    bump <- min(carr_ru_ia$avail1Z)
  
 p <- ggplot() +  geom_sf(data = carr_ru_ia, aes(fill= log(avail1Z - bump)),  color=NA )  +
    scale_fill_viridis_c(option = "mako", na.value = NA) + 
    ggtitle("Availability factor") +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  
  ggsave(plot = p, "carr_avail_map.jpeg", device="jpeg", width = 6, height = 6, units = "in") 
  
  p <- ggplot() +  geom_sf(data = carr_ru_ia, aes(fill= access1Z),  color=NA )  +
    scale_fill_viridis_c(option = "mako", na.value = NA) + 
    ggtitle("First accessibility factor") +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  
  ggsave(plot = p, "carr_access1_map.jpeg", device="jpeg", width = 6, height = 6, units = "in") 
  
  
  
  p <- ggplot() +  geom_sf(data = carr_ru_ia, aes(fill= access2Z),  color=NA )  +
    scale_fill_viridis_c(option = "mako", na.value = NA) + 
    ggtitle("Second accessibility factor") +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  
  ggsave(plot = p, "carr_access2_map.jpeg", device="jpeg", width = 6, height = 6, units = "in")
  
  
  p <- ggplot() +  geom_sf(data = carr_ru_ia, aes(fill= access3Z),  color=NA )  +
    scale_fill_viridis_c(option = "mako", na.value = NA) + 
    ggtitle("Third accessibility factor") +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  
  ggsave(plot = p, "carr_access3_map.jpeg", device="jpeg", width = 6, height = 6, units = "in")
  
  

  
  
  
  ggplot() + geom_density(data=carr_ru, aes(x=carr1_rural , fill= "CARR",   
                                            alpha = 0.5,  color = "white"),  color = "white") +
    scale_fill_manual(values = c("#d8b365")) +
    guides(alpha="none", fill = "none") +
    theme_minimal() +
    xlab("CARR Version 2") + ylab("Density") +
    theme(legend.title=element_blank(), legend.position = "bottom")+
    geom_boxploth(data=carr_ru, aes(y=1, x=carr1_rural, fill="CARR"), width = 1.5, outlier.alpha = 0)
  
  ggsave("carrv2_density.jpeg", device="jpeg", width = 6, height = 6, units = "in") 
  
