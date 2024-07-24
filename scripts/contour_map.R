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
  list(height = list(),
       minimap = list())

# load minimaps ----------------------------------------------------------------
# pick out minimaps
mini_files <-
  map_files[grep("minimap",map_files)]

for(map in mini_files[1:2]){
  
  # load in height map
  maps$minimap[[map]] <-
    rast(paste0("./data/maps/",map))
  
  crs(maps$minimap[[map]]) <- "WGS84"
  
  print(map)
}

# simplify names
names(maps$minimap) <-
  gsub("\\_.*","",names(maps$minimap))

# load heights -----------------------------------------------------------------
# pick out height files
height_files <-
  map_files[grep("height",map_files)]

# load in height files
maps$height$terra <- list()
maps$height$df    <- list()

for(map in height_files[1:2]){
  
  # load in height map
  maps$height$terra[[map]] <-
    rast(paste0("./data/maps/",map))[[1]]
  
  # convert to data frame
  #maps$height$df[[map]] <-
  #  maps$height$terra[[map]] %>% 
  #  as.data.frame(xy = TRUE) %>% 
  #  tibble()
  
  # rename z column to z
  #colnames(maps$height$df[[map]])[3] <- "z"
  
  print(map)
}

# simplify names
names(maps$height$terra) <-
  gsub("\\_.*","",names(maps$height$terra))
#names(maps$height$df) <-
#  names(maps$height$terra)

# match extents ----------------------------------------------------------------
for(map in names(maps$minimap)){

  maps$height$terra[[map]] <-
    resample(maps$height$terra[[map]],
           maps$minimap[[map]])
}

# plot -------------------------------------------------------------------------
# tidyterra
bin_width <- 5

plots <- list()

for(map in names(maps$minimap)){
  
  plots[[map]] <-
    ggplot() +
    
    # minimap
    geom_spatraster(data = maps$minimap[[map]],
                    alpha = 0.9) +
    
    # contours
    #geom_spatraster_contour_filled(data = maps[[map]],
    #                               binwidth = bin_width,
    #                               show.legend = FALSE) +
    geom_spatraster_contour(data = maps$height$terra[[map]],
                            binwidth = bin_width,
                            colour = "black",
                            alpha = 0.9) +
    
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






























