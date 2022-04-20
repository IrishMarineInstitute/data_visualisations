library(tidyverse)
library(osmdata)
library(sf)
library(rayshader)
library(raster)
library(elevatr)


# Killarney co-ordinates 
# xmin = -9.806521, xmax = -9.320271, 
# ymin = 51.934163, ymax = 52.116657)

# West Kerry co-ordinates
# (xmin = -10.331577, xmax = -9.904794, 
#   ymin = 52.100955, ymax = 52.323784)

# Create Bounding Box
med_bbox <- st_bbox(c(xmin = -9.806521, xmax = -9.320271, 
                      ymin = 51.934163, ymax = 52.116657),
                    crs = 4326)

med_bbox_df <- data.frame(x = c(-9.806521, -9.320271),
                          y = c(51.934163, 52.116657))


extent_zoomed <- raster::extent(med_bbox)

# Get elevation data
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

elev_med <- get_elev_raster(med_bbox_df, prj =  prj_dd, z = 12, clip = "bbox")

elev_med_mat <- raster_to_matrix(elev_med)

# Get roads data
med_roads <- med_bbox %>%
  opq() %>%
  add_osm_feature("highway",
                  c("motorway",
                    "trunk",
                    "primary", 
                    "secondary")) %>%
  osmdata_sf()

med_road_lines <- med_roads$osm_lines

# Get river data
med_rivers <- med_bbox %>%
  opq() %>%
  add_osm_feature("waterway",
                  c("river")) %>%
  osmdata_sf()

med_river_lines <- med_rivers$osm_lines

# Get lake data
med_lakes <- med_bbox %>%
  opq() %>%
  add_osm_feature("natural",
                  c("water")) %>%
  osmdata_sf()

med_lakes_polygons <- med_lakes$osm_polygons
med_lake_lines <- med_lakes$osm_lines
lakes_multi = med_lakes$osm_multipolygons

# Get buildings data
med_buildings <- med_bbox %>%
  opq() %>%
  add_osm_feature("building") %>%
  osmdata_sf()

med_building_polygons <- med_buildings$osm_polygons

# Create basemap
base_map <- elev_med_mat %>%
  height_shade() %>%
  add_overlay(
    sphere_shade(elev_med_mat,
                 texture = rayshader::create_texture(
                   lightcolor = "#b8ff78",
                   shadowcolor = "#193600",
                   leftcolor = "#80d453",
                   rightcolor = "#80d453",
                   centercolor = "#568a27"),
                 sunangle = 0, 
                 colorintensity = 5)
  )

# Add roads, rivers and buildings
final_map <- base_map %>%
  add_overlay(
    generate_line_overlay(
      med_road_lines, extent = extent_zoomed,
      linewidth = 3, color = "white",
      heightmap = elev_med_mat
    )) %>%
  add_overlay(
    generate_line_overlay(
      med_river_lines, extent = extent_zoomed,
      linewidth = 10, color = "lightblue",
      heightmap = elev_med_mat
    )) %>%
  add_overlay(
    generate_line_overlay(
      med_lake_lines, extent = extent_zoomed,
      linewidth = 10, color = "lightblue",
      heightmap = elev_med_mat
    )) %>%
  add_overlay(
    generate_polygon_overlay(
      med_lakes_polygons, extent = extent_zoomed,
      linewidth = 0, palette = "lightblue", 
      heightmap = elev_med_mat
    )) %>%
  add_overlay(
    generate_polygon_overlay(
      lakes_multi, extent = extent_zoomed,
      linewidth = 0, palette = "lightblue", 
      heightmap = elev_med_mat
    )) %>%
  add_overlay(
    generate_polygon_overlay(
      med_building_polygons, extent = extent_zoomed,
      linewidth = 0, palette = "red",
      heightmap = elev_med_mat
    )) %>%
  plot_3d(elev_med_mat, zscale = 10, fov = 0, 
          theta = -45, zoom = .5, phi = 30, 
          windowsize = c(1000,800))

render_snapshot(
  filename = 'killarney.png',
  title_text = "Killarney, Ireland",
  title_offset = c(20, 20),
  title_color = "black",
  title_size = 25,
  title_font = "Assistant",
  title_position = "north"
)
