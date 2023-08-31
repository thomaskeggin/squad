# set --------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(tidyterra)
library(smoothr)
library(rayshader)

# load -------------------------------------------------------------------------
# list all files
map_files <-
  list.files("./data/maps")

# pick out height files
map_files <-
  map_files[grep("height",map_files)]

# load in height files
maps    <- list()
maps_df <- list()

for(map in map_files){
  
  maps[[map]] <-
    rast(paste0("./data/maps/",map))[[1]]
  
  maps_df[[map]] <-
    maps[[map]] %>% 
    as.data.frame(xy = TRUE)
  
  print(map)
}

# simplify names
names(maps) <-
  gsub("\\_.*","",names(maps))
names(maps_df) <-
  names(maps)

# plot -------------------------------------------------------------------------
# tidyterra
bin_width <- 5

plots <- list()

for(map in names(maps)){
  
  plots[[map]] <-
    ggplot() +
    
    # heatmap
    geom_spatraster(data = maps[[map]]) +
    
    # contours
    #geom_spatraster_contour_filled(data = maps[[map]],
    #                               binwidth = bin_width,
    #                               show.legend = FALSE) +
    geom_spatraster_contour(data = maps[[map]],
                            binwidth = bin_width,
                            colour = "black") +
    
    # settings
    coord_fixed() +
    #scale_fill_viridis_c() +
    scale_fill_gradient(low = "#7A5D3A",
                        high = "white") +
    
    # annotations
    ggtitle(map) +
    theme_void()
  
  ggsave(paste0("./plots/maps/",map,".png"),
         plots[[map]])
}
