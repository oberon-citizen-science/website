library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(terra)
library(terrainr)
library(raster)
library(osmdata)
library(sf)

terraOptions(progress=0, tempdir="/Volumes/TimChurchesSamsungT5_1TB/Pines/")

# paths to data files
wip_prefix <- "_wip/pines/"
assets_prefix <- "assets/"
downloaded_data_prefix <- "~/NSW_Government_Spatial_Services/"

if (!file.exists(paste0(wip_prefix, "oberon_and_environs_dem_5m.tif"))) {

  # NSW Elevation Data Service
  # https://portal.spatial.nsw.gov.au/portal/apps/webappviewer/index.html?id=437c0697e6524d8ebf10ad0d915bc219

  wallerawang_rast <- terra::rast(paste0(downloaded_data_prefix,
                                    "Wallerawang-DEM-AHD_56_5m/Wallerawang-DEM-AHD_56_5m.asc"))
  orange_rast <- terra::rast(paste0(downloaded_data_prefix,
                                    "Orange-DEM-AHD_55_5m/Orange-DEM-AHD_55_5m.asc"))
  bathurst_rast <- terra::rast(paste0(downloaded_data_prefix,
                                      "Bathurst-DEM-AHD_55_5m/Bathurst-DEM-AHD_55_5m.asc"))
  blayney_rast <- terra::rast(paste0(downloaded_data_prefix,
                                     "Blayney-DEM-AHD_55_5m/Blayney-DEM-AHD_55_5m.asc"))
  oberon_rast <- terra::rast(paste0(downloaded_data_prefix,
                                    "Oberon-DEM-AHD_55_5m/Oberon-DEM-AHD_55_5m.asc"))
  crookwell_rast <- terra::rast(paste0(downloaded_data_prefix,
                                       "Crookwell-DEM-AHD_55_5m/Crookwell-DEM-AHD_55_5m.asc"))
  katoomba_rast <- terra::rast(paste0(downloaded_data_prefix,
                                      "Katoomba-DEM-AHD_56_5m/Katoomba-DEM-AHD_56_5m.asc"))
  taralga_rast <- terra::rast(paste0(downloaded_data_prefix,
                                     "Taralga-DEM-AHD_55_5m/Taralga-DEM-AHD_55_5m.asc"))
  burragorang_rast <- terra::rast(paste0(downloaded_data_prefix,
                                   "Burragorang-DEM-AHD_56_5m/Burragorang-DEM-AHD_56_5m.asc"))

  terra::writeRaster(wallerawang_rast,
                     paste0(wip_prefix, "wallerawang_dem.tif"),
                     overwrite=TRUE)
  terra::writeRaster(orange_rast,
                     paste0(wip_prefix, "orange_dem.tif"),
                     overwrite=TRUE)
  terra::writeRaster(bathurst_rast,
                     paste0(wip_prefix, "bathurst_dem.tif"),
                     overwrite=TRUE)
  terra::writeRaster(blayney_rast,
                     paste0(wip_prefix, "blayney_dem.tif"),
                     overwrite=TRUE)
  terra::writeRaster(oberon_rast,
                     paste0(wip_prefix, "oberon_dem.tif"),
                     overwrite=TRUE)
  terra::writeRaster(crookwell_rast,
                     paste0(wip_prefix, "crookwell_dem.tif"),
                     overwrite=TRUE)
  terra::writeRaster(katoomba_rast,
                     paste0(wip_prefix, "katoomba_dem.tif"),
                     overwrite=TRUE)
  terra::writeRaster(taralga_rast,
                     paste0(wip_prefix, "taralga_dem.tif"),
                     overwrite=TRUE)
  terra::writeRaster(burragorang_rast,
                     paste0(wip_prefix, "burragorang_dem.tif"),
                     overwrite=TRUE)

  rm(wallerawang_rast)
  rm(orange_rast)
  rm(bathurst_rast)
  rm(blayney_rast)
  rm(oberon_rast)
  rm(crookwell_rast)
  rm(katoomba_rast)
  rm(taralga_rast)
  rm(burragorang_rast)

  terrainr::merge_rasters(
    c(paste0(wip_prefix, "wallerawang_dem.tif"),
      paste0(wip_prefix, "orange_dem.tif"),
      paste0(wip_prefix, "bathurst_dem.tif"),
      paste0(wip_prefix, "blayney_dem.tif"),
      paste0(wip_prefix, "oberon_dem.tif"),
      paste0(wip_prefix, "crookwell_dem.tif"),
      paste0(wip_prefix, "katoomba_dem.tif"),
      paste0(wip_prefix, "taralga_dem.tif"),
      paste0(wip_prefix, "burragorang_dem.tif")),
     output_raster = paste0(wip_prefix, "oberon_and_environs_dem_5m.tif"),
    overwrite=TRUE)
}

localtif <- terra::rast(paste0(wip_prefix, "oberon_and_environs_dem_5m.tif"))


# Load turbine points
palings_yard_turbines_untransformed_sf <- sf::read_sf(paste0(assets_prefix,
                                                            "paling_yards_turbines.geojson"))
palings_yard_turbines_sf <- st_transform(palings_yard_turbines_untransformed_sf, crs=crs(localtif))
palings_yard_turbines_df <- as.data.frame(st_coordinates(palings_yard_turbines_sf))

palings_yard_turbines_untransformed_df <- as.data.frame(st_coordinates(palings_yard_turbines_untransformed_sf)) %>%
  rename(long=X, lat = Y)

pines_turbines_untransformed_sf <- sf::read_sf(paste0(assets_prefix,"Pines_Windfarm_Tower_Locations_Oct_2024.kml"))
pines_turbines_sf <- st_transform(pines_turbines_untransformed_sf, crs=crs(localtif))
pines_turbines_df <- as.data.frame(st_coordinates(pines_turbines_sf))
pines_turbines_untransformed_df <- as.data.frame(st_coordinates(pines_turbines_untransformed_sf))

n_turbines <- nrow(pines_turbines_untransformed_sf)
turbine_range <- 160:199
turbine_range_char <- "_160_199"

if (TRUE) {
  for (i in turbine_range) {
    current_turbine <- vect(rbind(st_coordinates(pines_turbines_sf[i,])[1,1:2]),
                                crs=crs(localtif))
    # loop over the three exposure values
    for (j in 1:3) {
      print(i)
      print(j)
      observer_ht <- turbine_visible_exposure_heights[j]
      vshed_raster <- viewshed(x=localtif,
                             loc=c(st_coordinates(pines_turbines_sf[20,])[1,1:2]),
                             observer=observer_ht,
                             target=1.5,
                             curvcoef=0.85714,
                             output="yes/no")
      # reclassify binary (0,1) visibility to exposure scores
      vshed_raster <- classify(vshed_raster, cbind(1, exposure_scores[j]))
      # if first exposure level, first create a categorised distance layer
      if (j == 1) {
        distance_layer <- distance(vshed_raster, current_turbine)
        m <-  c(0, 800, 1,
                800, 3200, 2,
                3200, 8000, 3,
                8000, 16100, 4,
                16100, 32201, 5)
        rclmat <- matrix(m, ncol=3, byrow=TRUE)
        dist_for_turbine <- classify(distance_layer, rclmat,
                        others = NA,
                        include.lowest=TRUE,
                        right=TRUE)
        dist_for_turbine <- crop(dist_for_turbine, distance_extent)
        vshed_for_turbine <- crop(vshed_raster, distance_extent)
        # name the layer
        set.names(vshed_for_turbine, j, nlyr(vshed_for_turbine))
        rm(vshed_raster)
        gc(verbose=TRUE, full=TRUE)
      } else {
        vshed_for_turbine <- c(vshed_for_turbine, crop(vshed_raster, dist_for_turbine))
        # name the layer
        set.names(vshed_for_turbine, j, nlyr(vshed_for_turbine))
        rm(vshed_raster)
        gc(verbose=TRUE, full=TRUE)

      }
    }
    # Now get maximum exposure value at each point in raster
    vshed_max_exp <- max(vshed_for_turbine)
    rm(vshed_for_turbine)
    gc(verbose=TRUE, full=TRUE)
    # now reclassify to visual exposure
    print("reclassify to visual_exp")
    vshed_visual_exp <-        ifel(dist_for_turbine == 1 & vshed_max_exp == 10,
                               4,
                               ifel(dist_for_turbine == 1 & vshed_max_exp == 20,
                               7,
                               ifel(dist_for_turbine == 1 & vshed_max_exp == 30,
                               10,
                               ifel(dist_for_turbine == 1 & vshed_max_exp == 10,
                               2,
                               ifel(dist_for_turbine == 2 & vshed_max_exp == 20,
                               4,
                               ifel(dist_for_turbine == 2 & vshed_max_exp == 30,
                               6,
                               ifel(dist_for_turbine == 3 & vshed_max_exp == 10,
                               1,
                               ifel(dist_for_turbine == 3 & vshed_max_exp == 20,
                               2,
                               ifel(dist_for_turbine == 3 & vshed_max_exp == 30,
                               4,
                               ifel(dist_for_turbine == 4 & vshed_max_exp == 10,
                               0,
                               ifel(dist_for_turbine == 4 & vshed_max_exp == 20,
                               1,
                               ifel(dist_for_turbine == 4 & vshed_max_exp == 30,
                               2,
                               ifel(dist_for_turbine == 5 & vshed_max_exp == 10,
                               0,
                               ifel(dist_for_turbine == 5 & vshed_max_exp == 20,
                               0,
                               ifel(dist_for_turbine == 5 & vshed_max_exp == 30,
                               1,
                               NA)))))))))))))))
    writeRaster(vshed_visual_exp,
                paste0(wip_prefix, "pines_vshed_visual_exp_", i, ".tif"),
                overwrite=TRUE)
    rm(vshed_max_exp)
    rm(dist_for_turbine)
    rm(vshed_visual_exp)
    gc(verbose=TRUE, full=TRUE)
  }
}
