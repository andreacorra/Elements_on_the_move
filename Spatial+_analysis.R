
# ==============================================================================
# Element on the move
# Author: Ferraro, Corradini, et al.
# Publication year: 2025
# Description: Analysis of nitrogen deposition patterns across different 
#              ungulate migration scenarios
# ==============================================================================

# Set working space -------------------------------------------------------
rm(list = ls())
gc()

library(sf)
library(ggplot2)
library(dplyr)
library(terra)
library(tidyr)
library(tidyterra)
library(scales)
library(purrr)
library(ggpubr)
library(mgcv)
library(performance)
library(gratia)
library(DHARMa)

# dir <- "path/to/your/data/directory/"  # Update this path
dir <- "C:/Users/corradinia/R/Elements_on_the_move"
utm_crs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"

## load raster stack with net nitrogen values as derived from NetLogo
## Each stack has the results from 10 simulation
migrate_and_resident_nitrogen <- readRDS("data/migrate_and_resident_nitrogen.rds")
resident_150_nitrogen <- readRDS("data/resident_150_nitrogen.rds")
resident_300_nitrogen <- readRDS("data/resident_300_nitrogen.rds")
migration_150_nitrogen <- readRDS("data/migration_150_nitrogen.rds")

## load environmental rasters
spatial_covariates <- readRDS("data/spatial_covariates.rds")


# Calculate area with net nitrogen deposition -----------------------------
## M+R_300
areas_migr_res_300 <- lapply(1:10, function(i) {
  r <- migrate_and_resident_nitrogen[[i]]
  (sum(!is.na(values(r))) * 100) / 1e6
})

areas_migr_res_300 <- unlist(areas_migr_res_300)

cat(sprintf("mean %.2f km² (SD = %.2f)", 
            mean(areas_migr_res_300), 
            sd(areas_migr_res_300)))

## R_300
areas_res_300 <- lapply(1:10, function(i) {
  r <- resident_300_nitrogen[[i]]
  (sum(!is.na(values(r))) * 100) / 1e6
})

areas_res_300 <- unlist(areas_res_300)

cat(sprintf("mean %.2f km² (SD = %.2f)", 
            mean(areas_res_300), 
            sd(areas_res_300)))

## R_150
areas_res_150 <- lapply(1:10, function(i) {
  r <- resident_150_nitrogen[[i]]
  (sum(!is.na(values(r))) * 100) / 1e6
})

areas_res_150 <- unlist(areas_res_150)

cat(sprintf("mean %.2f km² (SD = %.2f)", 
            mean(areas_res_150), 
            sd(areas_res_150)))

## M_150
areas_migr_150 <- lapply(1:10, function(i) {
  r <- migration_150_nitrogen[[i]]
  (sum(!is.na(values(r))) * 100) / 1e6
})

areas_migr_150 <- unlist(areas_migr_150)

cat(sprintf("mean %.2f km² (SD = %.2f)", 
            mean(areas_migr_150), 
            sd(areas_migr_150)))


# Average Net Change ------------------------------------------------------
## M+R_300
abs_net_MR_300 <- abs(mean(values(migrate_and_resident_nitrogen[[11]]), na.rm = T))
cat(sprintf("%.3f g N/patch", abs_net_MR_300))

## R_300
abs_net_R_300 <- abs(mean(values(resident_300_nitrogen[[11]]), na.rm = T))
cat(sprintf("%.3f g N/patch", abs_net_R_300))

## R_150
abs_net_R_150 <- abs(mean(values(resident_150_nitrogen[[11]]), na.rm = T))
cat(sprintf("%.3f g N/patch", abs_net_R_150))

## M_150
abs_net_M_150 <- abs(mean(values(migration_150_nitrogen[[11]]), na.rm = T))
cat(sprintf("%.3f g N/patch", abs_net_M_150))


# M_R_300 spatial modelling -----------------------------------------------

# Create empty list to store sampled points from each raster layer
pts_migr_res_300 <- vector("list", nlyr(migrate_and_resident_nitrogen))

# Sample points regularly from each nitrogen deposition layer
n_points <- 21000 # this returns a regular grid with values of about 3000 points

for (i in 1:nlyr(migrate_and_resident_nitrogen)) {
  set.seed(123)  # Set seed for reproducible sampling
  pts_migr_res_300[[i]] <- spatSample(migrate_and_resident_nitrogen[[i]], 
                                      size = n_points,
                                      method = "regular", 
                                      as.points = TRUE, 
                                      na.rm = TRUE)
}

# Standardize column names and assign layer names
pts_migr_res_300 <- map(pts_migr_res_300, ~ {
  if(!is.null(.x)) names(.x)[1] <- "net_nitrogen"
  .x
})
names(pts_migr_res_300) <- names(migrate_and_resident_nitrogen)

# Extract coordinates and covariate values for each point
df_migr_res_300 <- map2(pts_migr_res_300, names(migrate_and_resident_nitrogen), ~{
  pts <- .x
  layer_name <- .y
  if(is.null(pts)) return(NULL)
  
  # Get coordinates and net nitrogen values
  coords <- terra::crds(pts)
  df <- data.frame(layer = layer_name, 
                   x = coords[,1], 
                   y = coords[,2], 
                   net = terra::values(pts)[,1])
  
  # Extract covariate values
  cov_vals <- terra::extract(spatial_covariates, pts)[,-1, drop = FALSE]
  df <- cbind(df, cov_vals)
  
  # Remove rows with missing values
  df %>% drop_na(net, tcd, elevation, slope, distance_roads, 
                 distance_big_roads, distance_small_roads)
})

# Combine all layers into single data frame and preprocess variables
df_migr_res_300 <- bind_rows(df_migr_res_300[!sapply(df_migr_res_300, is.null)]) %>%
  mutate(
    landuse = factor(landuse, levels = c(0,1), labels = c("Open", "Forest")),
    net_abs = abs(net),                    # Absolute net nitrogen
    net_trans = asinh(net)                 # Arcsinh transformation
  ) %>%
  # Standardize continuous covariates
  mutate(across(c(tcd, elevation, slope, distance_roads, 
                  distance_big_roads, distance_small_roads), 
                ~ as.numeric(scale(.))))

## Spatial Residual Calculation --------------------------------------------

# Filter to mean layer for final analysis
res_migr_res_300 <- df_migr_res_300 %>% 
  filter(layer == "migrate_and_resident_patches_mean")

# Calculate spatial residuals for each covariate
res_migr_res_300 <- res_migr_res_300 %>%
  mutate(
    tcd_res_mr300 = residuals(gam(tcd ~ s(x, y, bs = "tp", k = 150), data = .)),
    elev_res_mr300 = residuals(gam(elevation ~ s(x, y, bs = "tp", k = 150), data = .)),
    slope_res_mr300 = residuals(gam(slope ~ s(x, y, bs = "tp", k = 150), data = .)),
    dist_rd_res_mr300 = residuals(gam(distance_roads ~ s(x, y, bs = "tp", k = 150), data = .))
  )

## Generalized Additive Model (GAM) Fitting --------------------------------
fit_migr_res_300 <- gam(
  net_trans ~
    s(tcd_res_mr300, k = 3) +          # Tree cover density effect
    s(elev_res_mr300, k = 8) +         # Elevation effect  
    s(slope_res_mr300, k = 5) +        # Slope effect
    s(dist_rd_res_mr300, k = 4) +      # Distance to roads effect
    s(x, y, bs = "tp", k = 150),       # Spatial smooth
  data = res_migr_res_300, 
  family = gaussian(), 
  method = "REML"
)

summary(fit_migr_res_300) # Model summary
performance::r2(fit_migr_res_300) # R-squared

# Model checks and diagnostics
concurvity(fit_migr_res_300, full = TRUE)
gam.check(fit_migr_res_300)

# Optional additional diagnostics:
# appraise(fit_migr_res_300)    # DHARMa-style residuals
# draw(fit_migr_res_300)        # Smooth term plots

## Plot --------------------------------------------------------------------
sm_migr_res_300 <- smooth_estimates(fit_migr_res_300) |> add_confint()

plots_mr300 <- list(
  tcd = ggplot(sm_migr_res_300, aes(x=tcd_res_mr300, y=.estimate)) +
    geom_rug(data=res_migr_res_300, aes(x=tcd_res_mr300), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#CC79A7") +
    geom_line(col="#995C7F", linewidth=.8) + labs(x="Tree cover density", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  slope = ggplot(sm_migr_res_300, aes(x=slope_res_mr300, y=.estimate)) +
    geom_rug(data=res_migr_res_300, aes(x=slope_res_mr300), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#CC79A7") +
    geom_line(col="#995C7F", linewidth=.8) + labs(x="Slope", y=NULL) +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  elev = ggplot(sm_migr_res_300, aes(x=elev_res_mr300, y=.estimate)) +
    geom_rug(data=res_migr_res_300, aes(x=elev_res_mr300), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#CC79A7") +
    geom_line(col="#995C7F", linewidth=.8) + labs(x="Elevation", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  dist_rd = ggplot(sm_migr_res_300, aes(x=dist_rd_res_mr300, y=.estimate)) +
    geom_rug(data=res_migr_res_300, aes(x=dist_rd_res_mr300), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#CC79A7") +
    geom_line(col="#995C7F", linewidth=.8) + labs(x="Distance from roads", y=NULL) +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank())
)


# R_300 spatial modelling -------------------------------------------------

# Create empty list to store sampled points from each raster layer
pts_res_300 <- vector("list", nlyr(resident_300_nitrogen))

# Sample points regularly from each nitrogen deposition layer
n_points <- 21000 # this returns a regular grid with values of about 3000 points

for (i in 1:nlyr(resident_300_nitrogen)) {
  set.seed(123)  # Set seed for reproducible sampling
  pts_res_300[[i]] <- spatSample(resident_300_nitrogen[[i]], 
                                 size = n_points,
                                 method = "regular", 
                                 as.points = TRUE, 
                                 na.rm = TRUE)
}

# Standardize column names and assign layer names
pts_res_300 <- map(pts_res_300, ~ {
  if(!is.null(.x)) names(.x)[1] <- "net_nitrogen"
  .x
})
names(pts_res_300) <- names(resident_300_nitrogen)

# Extract coordinates and covariate values for each point
df_res_300 <- map2(pts_res_300, names(resident_300_nitrogen), ~{
  pts <- .x
  layer_name <- .y
  if(is.null(pts)) return(NULL)
  
  # Get coordinates and net nitrogen values
  coords <- terra::crds(pts)
  df <- data.frame(layer = layer_name, 
                   x = coords[,1], 
                   y = coords[,2], 
                   net = terra::values(pts)[,1])
  
  # Extract covariate values
  cov_vals <- terra::extract(spatial_covariates, pts)[,-1, drop = FALSE]
  df <- cbind(df, cov_vals)
  
  # Remove rows with missing values
  df %>% drop_na(net, tcd, elevation, slope, distance_roads)
})

# Combine all layers into single data frame and preprocess variables
df_res_300 <- bind_rows(df_res_300[!sapply(df_res_300, is.null)]) %>%
  mutate(
    landuse = factor(landuse, levels = c(0,1), labels = c("Open", "Forest")),
    net_abs = abs(net),                    # Absolute net nitrogen
    net_trans = asinh(net)                 # Arcsinh transformation
  ) %>%
  # Standardize continuous covariates
  mutate(across(c(tcd, elevation, slope, distance_roads), 
                ~ as.numeric(scale(.))))

## Spatial Residual Calculation --------------------------------------------

# Filter to mean layer for final analysis
res_res_300 <- df_res_300 %>% 
  filter(layer == "resident_300_patches_mean")

# Calculate spatial residuals for each covariate
res_res_300 <- res_res_300 %>%
  mutate(
    tcd_res_r300 = residuals(gam(tcd ~ s(x, y, bs = "tp", k = 150), data = .)),
    elev_res_r300 = residuals(gam(elevation ~ s(x, y, bs = "tp", k = 150), data = .)),
    slope_res_r300 = residuals(gam(slope ~ s(x, y, bs = "tp", k = 150), data = .)),
    dist_rd_res_r300 = residuals(gam(distance_roads ~ s(x, y, bs = "tp", k = 150), data = .))
  )

## Generalized Additive Model (GAM) Fitting --------------------------------
fit_res_300 <- gam(
  net_trans ~
    s(tcd_res_r300, k = 3) +          # Tree cover density effect
    s(elev_res_r300, k = 8) +         # Elevation effect  
    s(slope_res_r300, k = 5) +        # Slope effect
    s(dist_rd_res_r300, k = 4) +      # Distance to roads effect
    s(x, y, bs = "tp", k = 150),      # Spatial smooth
  data = res_res_300, 
  family = gaussian(), 
  method = "REML"
)

summary(fit_res_300) # Model summary
performance::r2(fit_res_300) # R-squared

# Model checks and diagnostics
concurvity(fit_res_300, full = TRUE)
gam.check(fit_res_300)

# Optional additional diagnostics:
# appraise(fit_res_300)    # DHARMa-style residuals
# draw(fit_res_300)        # Smooth term plots

## Plot --------------------------------------------------------------------
sm_res_300 <- smooth_estimates(fit_res_300) |> add_confint()

plots_r300 <- list(
  tcd = ggplot(sm_res_300, aes(x = tcd_res_r300, y = .estimate)) +
    geom_rug(data = res_res_300, aes(x = tcd_res_r300), sides = "b", alpha = 0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "#009E73") +
    geom_line(col = "#006D51", linewidth = .8) + 
    labs(x = "Tree cover density", y = "Partial effect") +
    coord_cartesian(ylim = c(-2.6, 2.6)) + 
    theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  slope = ggplot(sm_res_300, aes(x = slope_res_r300, y = .estimate)) +
    geom_rug(data = res_res_300, aes(x = slope_res_r300), sides = "b", alpha = 0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "#009E73") +
    geom_line(col = "#006D51", linewidth = .8) + 
    labs(x = "Slope", y = NULL) +
    coord_cartesian(ylim = c(-2.6, 2.6)) + 
    theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  elev = ggplot(sm_res_300, aes(x = elev_res_r300, y = .estimate)) +
    geom_rug(data = res_res_300, aes(x = elev_res_r300), sides = "b", alpha = 0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "#009E73") +
    geom_line(col = "#006D51", linewidth = .8) + 
    labs(x = "Elevation", y = "Partial effect") +
    coord_cartesian(ylim = c(-2.6, 2.6)) + 
    theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  dist_rd = ggplot(sm_res_300, aes(x = dist_rd_res_r300, y = .estimate)) +
    geom_rug(data = res_res_300, aes(x = dist_rd_res_r300), sides = "b", alpha = 0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "#009E73") +
    geom_line(col = "#006D51", linewidth = .8) + 
    labs(x = "Distance from roads", y = NULL) +
    coord_cartesian(ylim = c(-2.6, 2.6)) + 
    theme_minimal() +
    theme(axis.title.x = element_blank())
)


# R_150 spatial modelling -------------------------------------------------

# Create empty list to store sampled points from each raster layer
pts_res_150 <- vector("list", nlyr(resident_150_nitrogen))

# Sample points regularly from each nitrogen deposition layer
n_points <- 21000 # this returns a regular grid with values of about 3000 points

for (i in 1:nlyr(resident_150_nitrogen)) {
  set.seed(123)  # Set seed for reproducible sampling
  pts_res_150[[i]] <- spatSample(resident_150_nitrogen[[i]], 
                                 size = n_points,
                                 method = "regular", 
                                 as.points = TRUE, 
                                 na.rm = TRUE)
}

# Standardize column names and assign layer names
pts_res_150 <- map(pts_res_150, ~ {
  if(!is.null(.x)) names(.x)[1] <- "net_nitrogen"
  .x
})
names(pts_res_150) <- names(resident_150_nitrogen)

# Extract coordinates and covariate values for each point
df_res_150 <- map2(pts_res_150, names(resident_150_nitrogen), ~{
  pts <- .x
  layer_name <- .y
  if(is.null(pts)) return(NULL)
  
  # Get coordinates and net nitrogen values
  coords <- terra::crds(pts)
  df <- data.frame(layer = layer_name, 
                   x = coords[,1], 
                   y = coords[,2], 
                   net = terra::values(pts)[,1])
  
  # Extract covariate values
  cov_vals <- terra::extract(spatial_covariates, pts)[,-1, drop = FALSE]
  df <- cbind(df, cov_vals)
  
  # Remove rows with missing values
  df %>% drop_na(net, tcd, elevation, slope, distance_roads)
})

# Combine all layers into single data frame and preprocess variables
df_res_150 <- bind_rows(df_res_150[!sapply(df_res_150, is.null)]) %>%
  mutate(
    landuse = factor(landuse, levels = c(0,1), labels = c("Open", "Forest")),
    net_abs = abs(net),                    # Absolute net nitrogen
    net_trans = asinh(net)                 # Arcsinh transformation
  ) %>%
  # Standardize continuous covariates
  mutate(across(c(tcd, elevation, slope, distance_roads), 
                ~ as.numeric(scale(.))))

## Spatial Residual Calculation --------------------------------------------

# Filter to mean layer for final analysis
res_res_150 <- df_res_150 %>% 
  filter(layer == "resident_150_patches_mean")

# Calculate spatial residuals for each covariate
res_res_150 <- res_res_150 %>%
  mutate(
    tcd_res_r150 = residuals(gam(tcd ~ s(x, y, bs = "tp", k = 150), data = .)),
    elev_res_r150 = residuals(gam(elevation ~ s(x, y, bs = "tp", k = 150), data = .)),
    slope_res_r150 = residuals(gam(slope ~ s(x, y, bs = "tp", k = 150), data = .)),
    dist_rd_res_r150 = residuals(gam(distance_roads ~ s(x, y, bs = "tp", k = 150), data = .))
  )

## Generalized Additive Model (GAM) Fitting --------------------------------
fit_res_150 <- gam(
  net_trans ~
    s(tcd_res_r150, k = 3) +          # Tree cover density effect
    s(elev_res_r150, k = 8) +         # Elevation effect  
    s(slope_res_r150, k = 5) +        # Slope effect
    s(dist_rd_res_r150, k = 4) +      # Distance to roads effect
    s(x, y, bs = "tp", k = 150),      # Spatial smooth
  data = res_res_150, 
  family = gaussian(), 
  method = "REML"
)

summary(fit_res_150) # Model summary
performance::r2(fit_res_150) # R-squared

# Model checks and diagnostics
concurvity(fit_res_150, full = TRUE)
gam.check(fit_res_150)

# Optional additional diagnostics:
# appraise(fit_res_150)    # DHARMa-style residuals
# draw(fit_res_150)        # Smooth term plots

## Plot --------------------------------------------------------------------
sm_res_150 <- smooth_estimates(fit_res_150) |> add_confint()

plots_r150 <- list(
  tcd = ggplot(sm_res_150, aes(x = tcd_res_r150, y = .estimate)) +
    geom_rug(data = res_res_150, aes(x = tcd_res_r150), sides = "b", alpha = 0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "#D55E00") +
    geom_line(col = "#A04800", linewidth = .8) + 
    labs(x = "Tree cover density", y = "Partial effect") +
    coord_cartesian(ylim = c(-2.6, 2.6)) + 
    theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  slope = ggplot(sm_res_150, aes(x = slope_res_r150, y = .estimate)) +
    geom_rug(data = res_res_150, aes(x = slope_res_r150), sides = "b", alpha = 0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "#D55E00") +
    geom_line(col = "#A04800", linewidth = .8) + 
    labs(x = "Slope", y = NULL) +
    coord_cartesian(ylim = c(-2.6, 2.6)) + 
    theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  elev = ggplot(sm_res_150, aes(x = elev_res_r150, y = .estimate)) +
    geom_rug(data = res_res_150, aes(x = elev_res_r150), sides = "b", alpha = 0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "#D55E00") +
    geom_line(col = "#A04800", linewidth = .8) + 
    labs(x = "Elevation", y = "Partial effect") +
    coord_cartesian(ylim = c(-2.6, 2.6)) + 
    theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  dist_rd = ggplot(sm_res_150, aes(x = dist_rd_res_r150, y = .estimate)) +
    geom_rug(data = res_res_150, aes(x = dist_rd_res_r150), sides = "b", alpha = 0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "#D55E00") +
    geom_line(col = "#A04800", linewidth = .8) + 
    labs(x = "Distance from roads", y = NULL) +
    coord_cartesian(ylim = c(-2.6, 2.6)) + 
    theme_minimal() +
    theme(axis.title.x = element_blank())
)


# M_150 spatial modelling -------------------------------------------------

# Create empty list to store sampled points from each raster layer
pts_migr_150 <- vector("list", nlyr(migration_150_nitrogen))

# Sample points regularly from each nitrogen deposition layer
n_points <- 21000 # this returns a regular grid with values of about 3000 points

for (i in 1:nlyr(migration_150_nitrogen)) {
  set.seed(123)  # Set seed for reproducible sampling
  pts_migr_150[[i]] <- spatSample(migration_150_nitrogen[[i]], 
                                  size = n_points,
                                  method = "regular", 
                                  as.points = TRUE, 
                                  na.rm = TRUE)
}

# Standardize column names and assign layer names
pts_migr_150 <- map(pts_migr_150, ~ {
  if(!is.null(.x)) names(.x)[1] <- "net_nitrogen"
  .x
})
names(pts_migr_150) <- names(migration_150_nitrogen)

# Extract coordinates and covariate values for each point
df_migr_150 <- map2(pts_migr_150, names(migration_150_nitrogen), ~{
  pts <- .x
  layer_name <- .y
  if(is.null(pts)) return(NULL)
  
  # Get coordinates and net nitrogen values
  coords <- terra::crds(pts)
  df <- data.frame(layer = layer_name, 
                   x = coords[,1], 
                   y = coords[,2], 
                   net = terra::values(pts)[,1])
  
  # Extract covariate values
  cov_vals <- terra::extract(spatial_covariates, pts)[,-1, drop = FALSE]
  df <- cbind(df, cov_vals)
  
  # Remove rows with missing values
  df %>% drop_na(net, tcd, elevation, slope, distance_roads)
})

# Combine all layers into single data frame and preprocess variables
df_migr_150 <- bind_rows(df_migr_150[!sapply(df_migr_150, is.null)]) %>%
  mutate(
    landuse = factor(landuse, levels = c(0,1), labels = c("Open", "Forest")),
    net_abs = abs(net),                    # Absolute net nitrogen
    net_trans = asinh(net)                 # Arcsinh transformation
  ) %>%
  # Standardize continuous covariates
  mutate(across(c(tcd, elevation, slope, distance_roads), 
                ~ as.numeric(scale(.))))

## Spatial Residual Calculation --------------------------------------------

# Filter to mean layer for final analysis
res_migr_150 <- df_migr_150 %>% 
  filter(layer == "migration_150_patches_mean")

# Calculate spatial residuals for each covariate
res_migr_150 <- res_migr_150 %>%
  mutate(
    tcd_res_m150 = residuals(gam(tcd ~ s(x, y, bs = "tp", k = 150), data = .)),
    elev_res_m150 = residuals(gam(elevation ~ s(x, y, bs = "tp", k = 150), data = .)),
    slope_res_m150 = residuals(gam(slope ~ s(x, y, bs = "tp", k = 150), data = .)),
    dist_rd_res_m150 = residuals(gam(distance_roads ~ s(x, y, bs = "tp", k = 150), data = .))
  )

## Generalized Additive Model (GAM) Fitting --------------------------------
fit_migr_150 <- gam(
  net_trans ~
    s(tcd_res_m150, k = 3) +          # Tree cover density effect
    s(elev_res_m150, k = 8) +         # Elevation effect  
    s(slope_res_m150, k = 5) +        # Slope effect
    s(dist_rd_res_m150, k = 4) +      # Distance to roads effect
    s(x, y, bs = "tp", k = 150),      # Spatial smooth
  data = res_migr_150, 
  family = gaussian(), 
  method = "REML"
)

summary(fit_migr_150) # Model summary
performance::r2(fit_migr_150) # R-squared

# Model checks and diagnostics
concurvity(fit_migr_150, full = TRUE)
gam.check(fit_migr_150)

# Optional additional diagnostics:
# appraise(fit_migr_150)    # DHARMa-style residuals
# draw(fit_migr_150)        # Smooth term plots

## Plot --------------------------------------------------------------------
sm_migr_150 <- smooth_estimates(fit_migr_150) |> add_confint()

plots_m150 <- list(
  tcd = ggplot(sm_migr_150, aes(x = tcd_res_m150, y = .estimate)) +
    geom_rug(data = res_migr_150, aes(x = tcd_res_m150), sides = "b", alpha = 0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "#0072B2") +
    geom_line(col = "#005280", linewidth = .8) + 
    labs(x = "Tree cover density", y = "Partial effect") +
    coord_cartesian(ylim = c(-2.6, 2.6)) + 
    theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  slope = ggplot(sm_migr_150, aes(x = slope_res_m150, y = .estimate)) +
    geom_rug(data = res_migr_150, aes(x = slope_res_m150), sides = "b", alpha = 0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "#0072B2") +
    geom_line(col = "#005280", linewidth = .8) + 
    labs(x = "Slope", y = NULL) +
    coord_cartesian(ylim = c(-2.6, 2.6)) + 
    theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  elev = ggplot(sm_migr_150, aes(x = elev_res_m150, y = .estimate)) +
    geom_rug(data = res_migr_150, aes(x = elev_res_m150), sides = "b", alpha = 0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "#0072B2") +
    geom_line(col = "#005280", linewidth = .8) + 
    labs(x = "Elevation", y = "Partial effect") +
    coord_cartesian(ylim = c(-2.6, 2.6)) + 
    theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  dist_rd = ggplot(sm_migr_150, aes(x = dist_rd_res_m150, y = .estimate)) +
    geom_rug(data = res_migr_150, aes(x = dist_rd_res_m150), sides = "b", alpha = 0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "#0072B2") +
    geom_line(col = "#005280", linewidth = .8) + 
    labs(x = "Distance from roads", y = NULL) +
    coord_cartesian(ylim = c(-2.6, 2.6)) + 
    theme_minimal() +
    theme(axis.title.x = element_blank())
)


# Graphical output --------------------------------------------------------

## Legend creation --------------------------------------------------------
# Create dummy plots to extract legends for consistent formatting

scenario_colors <- c(
  "M_150"   = "#0072B2",    # Blue: Migrants 150
  "R_150"   = "#D55E00",    # Orange: Residents 150
  "M+R_300" = "#009E73",    # Green: Migrants + Residents 300
  "R_300"   = "#CC79A7"     # Pink: Residents 300
)

scenario_colors_subset <- c(
  "M+R_300" = "#009E73",
  "R_300"   = "#CC79A7"
)

# Full legend (all four scenarios)
legend_df <- data.frame(
  Simulation = factor(names(scenario_colors), levels = names(scenario_colors)),
  x = 1, y = 1
)
legend_plot <- ggplot(legend_df, aes(x = x, y = y, color = Simulation)) +
  geom_point() +
  scale_color_manual(values = scenario_colors, limits = names(scenario_colors)) +
  theme_void() +
  theme(legend.position = "bottom")

legend <- get_legend(legend_plot)

# Subset legend (M+R_300 vs R_300 only)
legend_df_subset <- data.frame(
  Simulation = factor(names(scenario_colors_subset), levels = names(scenario_colors_subset)),
  x = 1, y = 1
)

legend_plot_subset <- ggplot(legend_df_subset, aes(x = x, y = y, color = Simulation)) +
  geom_point() +
  scale_color_manual(values = scenario_colors_subset, limits = names(scenario_colors_subset)) +
  theme_void() +
  theme(legend.position = "bottom")

legend_subset <- get_legend(legend_plot_subset)

## Multi-panel figure ------------------------------------------------------

# Elevation effects across all scenarios
elev_plots <- ggarrange(
  plots_m150$elev,   # A: Migrants 150
  plots_r150$elev,   # B: Residents 150
  plots_mr300$elev,  # C: Migrants + Residents 300
  plots_r300$elev,   # D: Residents 300
  ncol = 2, nrow = 2, 
  labels = c("A", "B", "C", "D")
)
elev_plots <- annotate_figure(
  elev_plots, 
  top = text_grob("Elevation", face = "bold", size = 14),
  bottom = legend
)

# Tree cover density effects across all scenarios
tcd_plots <- ggarrange(
  plots_m150$tcd,   # A: Migrants 150
  plots_r150$tcd,   # B: Residents 150
  plots_mr300$tcd,  # C: Migrants + Residents 300
  plots_r300$tcd,   # D: Residents 300
  ncol = 2, nrow = 2, 
  labels = c("A", "B", "C", "D")
)
tcd_plots <- annotate_figure(
  tcd_plots, 
  top = text_grob("Tree Cover Density", face = "bold", size = 14),
  bottom = legend
)

# Slope effects across all scenarios
slope_plots <- ggarrange(
  plots_m150$slope,   # A: Migrants 150
  plots_r150$slope,   # B: Residents 150
  plots_mr300$slope,  # C: Migrants + Residents 300
  plots_r300$slope,   # D: Residents 300
  ncol = 2, nrow = 2, 
  labels = c("A", "B", "C", "D")
)
slope_plots <- annotate_figure(
  slope_plots, 
  top = text_grob("Slope", face = "bold", size = 14),
  bottom = legend
)

# Distance to roads effects (subset comparison)
roads_plots <- ggarrange(
  plots_mr300$dist_rd,  # A: Migrants + Residents 300
  plots_r300$dist_rd,   # B: Residents 300
  ncol = 2, nrow = 1, 
  labels = c("A", "B")
)
roads_plots <- annotate_figure(
  roads_plots, 
  top = text_grob("Distance from Roads", face = "bold", size = 14),
  bottom = legend_subset
)

# ==============================================================================
# END SCRIPT
# ==============================================================================
