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

# put maps here
maps <-
  list()

# load heights -----------------------------------------------------------------
# pick out height files
height_files <-
  map_files[grep("height",map_files)]

for(map in height_files){
  
  # load in height map
  maps[[map]] <-
    rast(paste0("./data/maps/",map))[[1]]
  
  print(map)
}

# simplify names
names(maps) <-
  gsub("\\_.*","",names(maps))

# plot -------------------------------------------------------------------------
# tidyterra
bin_width <- 5

plots <- list()

for(map in names(maps)){
  
  plots[[map]] <-
    ggplot() +
    
    # contours
    geom_spatraster_contour(data = maps[[map]],
                            binwidth = bin_width,
                            colour = "black",
                            alpha = 0.9) +
    
    # settings
    coord_fixed() +
    
    # annotations
    theme_void() +
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
  ggsave(paste0("./plots/maps/contour_overlay/",map,".png"),
         plots[[map]])
}
