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
turbine_range <- 80:159
turbine_range_char <- "_80_159"

if (TRUE) {
  # first get the extents of 32,201m buffers around each turbine and get union of them all
  for (i in turbine_range) {
    current_turbine <- terra::vect(rbind(sf::st_coordinates(pines_turbines_sf[i,])[1,1:2]),
                                crs=terra::crs(localtif))
    turbine_distance <- terra::distance(localtif, current_turbine)
    # set distance to NA if beyond 32,200m and trim to that
    turbine_distance <- terra::trim(terra::ifel(turbine_distance <= 32201, 1, NA))
    turbine_extent <- terra::ext(turbine_distance)
    rm(turbine_distance)
    if (i == 80) {
      distance_extent <- turbine_extent
    } else {
      distance_extent <- terra::union(distance_extent, turbine_extent)
    }
    gc(verbose=TRUE, full=TRUE)
    print(paste("i=", i))
    print(distance_extent)
  }
  saveRDS(as.vector(distance_extent), paste0(wip_prefix,
                                             "pines_distance_extent",
                                             turbine_range_char, ".rds"))
}
