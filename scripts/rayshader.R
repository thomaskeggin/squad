# set --------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(tidyterra)
library(rayshader)

plots <- list()
contour_levels <- seq(0,300,by = 10)

# set load ---------------------------------------------------------------------
# list all files
map_files <-
  list.files("./data/maps")

# put maps here
maps <-
  list(height = list(),
       minimap = list())

# load and wrangle heights -----------------------------------------------------
# pick out height files
height_files <-
  map_files[grep("height",map_files)]

# load in height files
maps$height$terra <- list()
maps$height$mat   <- list()

for(map in height_files){
  
  # load in height map
  maps$height$terra[[map]] <-
    rast(paste0("./data/maps/",map))[[1]]
  
  # convert to matrix for rayshader
  maps$height$mat[[map]] <-
    maps$height$terra[[map]] %>% 
    terra::aggregate(fact = 1) %>% 
    raster_to_matrix()
  
  print(map)
}

# simplify names
names(maps$height$terra) <-
  gsub("\\_.*","",names(maps$height$terra))
names(maps$height$mat) <-
  gsub("\\_.*","",names(maps$height$terra))

# load and wrangle minimaps ----------------------------------------------------
# pick out height files
mini_files <-
  map_files[grep("mini",map_files)]

# load in height files
for(map in mini_files){
  
  # load in height map
  maps$minimap[[map]] <-
    rast(paste0("./data/maps/",map))
  
  print(map)
}

# simplify names
names(maps$minimap) <-
  gsub("\\_.*","",names(maps$minimap))

# albasrah ---------------------------------------------------------------------
target_map <- maps$height$mat$albasrah
bathy_hs   <- height_shade(target_map, texture = "#04B2D9")

target_map %>%
  
  # main part
  sphere_shade(texture = "desert") %>%
  
  # contours
  add_overlay(generate_contour_overlay(target_map),
              alphalayer = 0.7)  %>%
  
  # shadows
  add_shadow(ray_shade(target_map, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(target_map), 0) %>%
  
  # water
  add_overlay(generate_altitude_overlay(bathy_hs, target_map, 0, 70)) %>%
  
  # save
  save_png("./plots/maps/albasrah_ray.png")

# anvil ------------------------------------------------------------------------
target_map <- maps$height$mat$anvil

target_map %>%
  
  # basemap
  sphere_shade(texture = "desert") %>%
  
  # contours
  add_overlay(generate_contour_overlay(target_map),
              alphalayer = 0.7)  %>%
  
  # shadows
  add_shadow(ray_shade(target_map, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(target_map), 0) %>%
  
  # export
  save_png("./plots/maps/anvil_ray.png")

# belaya -----------------------------------------------------------------------
target_map <- maps$height$mat$belaya

target_map %>%
  
  # basemap
  #sphere_shade(texture = "desert") %>%
  height_shade() %>% 
  
  # contours
  add_overlay(generate_contour_overlay(target_map),
              alphalayer = 0.7)  %>%
  
  # shadows
  add_shadow(ray_shade(target_map, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(target_map), 0) %>%
  plot_map()
  
  # export
  save_png("./plots/maps/anvil_ray.png")
  
  # mutaha -----------------------------------------------------------------------
  target_map <- maps$height$mat$mutaha
  
  target_map %>%
    
    # basemap
    sphere_shade(texture = "desert") %>%
    
    # contours
    add_overlay(generate_contour_overlay(target_map),
                alphalayer = 0.7)  %>%
    
    # shadows
    add_shadow(ray_shade(target_map, zscale = 3), 0.5) %>%
    add_shadow(ambient_shade(target_map), 0) %>% 
  
  # export
  save_png("./plots/maps/mutaha_ray.png")


