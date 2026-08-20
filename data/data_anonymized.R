
# Library
library(sf)
library(terra)

# Load all rasters and shapefiles to anonymize
setwd("C:/Users/corradinia/R/Elements_on_the_move/data")

r_winter  <- rast("winter_NDVI.asc")
r_spring  <- rast("spring_NDVI.asc")
r_summer  <- rast("latesum_NDVI.asc")
r_landuse <- rast("landuse.asc")
r_aspect  <- rast("aspect.tif")

mr_300  <- readRDS("migrate_and_resident_nitrogen.rds")
m_150   <- readRDS("migration_150_nitrogen.rds")
r_150   <- readRDS("resident_150_nitrogen.rds")
r_300   <- readRDS("resident_300_nitrogen.rds")
sp_covar <- readRDS("spatial_covariates_R1.rds")

all_deer <- st_read("All_Deer.shp")
res_deer <- st_read("Resident_Deer.shp")
migr_deer <- st_read("Migratory_Deer.shp")
roads     <- st_read("roads.shp")

# extract global minimum
x_min <- ext(r_winter)$xmin
y_min <- ext(r_winter)$ymin

# Shift rasters to local origin (0,0) and strip CRS
shift_raster <- function(r, dx, dy) {
  r_shifted <- shift(r, dx = -dx, dy = -dy)
  crs(r_shifted) <- "" # Remove real geographic projection metadata
  return(r_shifted)
}

r_winter_anon  <- shift_raster(r_winter,  x_min, y_min)
r_spring_anon  <- shift_raster(r_spring,  x_min, y_min)
r_summer_anon  <- shift_raster(r_summer,  x_min, y_min)
r_landuse_anon <- shift_raster(r_landuse, x_min, y_min)
r_aspect_anon  <- shift_raster(r_aspect,  x_min, y_min)

mr_300_anon   <- shift_raster(mr_300, x_min, y_min)
m_150_anon    <- shift_raster(m_150, x_min, y_min)
r_150_anon    <- shift_raster(r_150, x_min, y_min)
r_300_anon    <- shift_raster(r_300, x_min, y_min)
sp_covar_anon <- shift_raster(sp_covar, x_min, y_min)

# Shift vectors by the exact same dx, dy offset and strip CRS
shift_vector <- function(v, dx, dy) {
  v_geom <- st_geometry(v) - c(dx, dy)
  st_geometry(v) <- v_geom
  st_crs(v) <- NA # Remove projection info
  return(v)
}

all_deer_anon  <- shift_vector(all_deer, x_min, y_min)
res_deer_anon  <- shift_vector(res_deer, x_min, y_min)
migr_deer_anon <- shift_vector(migr_deer, x_min, y_min)
roads_anon     <- shift_vector(roads,     x_min, y_min)

# Export anonymized files for NetLogo
setwd("C:/Users/corradinia/R/Elements_on_the_move/data_anonymized")

# Save ASCII and GeoTIFF rasters
writeRaster(r_winter_anon,  "winter_NDVI.asc", filetype = "AAIGrid", overwrite = TRUE)
writeRaster(r_spring_anon,  "spring_NDVI.asc", filetype = "AAIGrid", overwrite = TRUE)
writeRaster(r_summer_anon,  "latesum_NDVI.asc", filetype = "AAIGrid", overwrite = TRUE)
writeRaster(r_landuse_anon, "landuse.asc",    filetype = "AAIGrid", overwrite = TRUE)
writeRaster(r_aspect_anon,  "aspect.tif",     overwrite = TRUE)

# Save RDS objects (FIXED: Using saveRDS instead of writeRaster)
saveRDS(mr_300_anon,   "migrate_and_resident_nitrogen.rds")
saveRDS(m_150_anon,    "migration_150_nitrogen.rds")
saveRDS(r_150_anon,    "resident_150_nitrogen.rds")
saveRDS(r_300_anon,    "resident_300_nitrogen.rds")
saveRDS(sp_covar_anon, "spatial_covariates.rds")

# Save Shapefiles (FIXED: Using delete_layer = TRUE for clean layer overwrites)
st_write(all_deer_anon,  "All_Deer.shp",       delete_layer = TRUE)
st_write(res_deer_anon,  "Resident_Deer.shp",  delete_layer = TRUE)
st_write(migr_deer_anon, "Migratory_Deer.shp", delete_layer = TRUE)
st_write(roads_anon,     "roads.shp",          delete_layer = TRUE)

# end anonymization
setwd("C:/Users/corradinia/R/Elements_on_the_move")
