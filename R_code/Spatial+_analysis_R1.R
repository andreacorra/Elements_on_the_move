# ==============================================================================
# Element on the move
# Description: Spatial+ analysis of nitrogen deposition patterns across 
#              different red deer migration scenarios
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

# Anonymized global variables
dir <- "data/"
utm_crs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"

movement_colors <- c("Resident and migrant" = "#ed6b5b", 
                     "Migrant 150" = "#7e549e",
                     "Resident 150" = "#ece6ce",
                     "Resident 300" = "#f9ac68" )


## load raster stack with net nitrogen values as derived from NetLogo
## Each stack has the results from 10 simulation
migrate_and_resident_nitrogen <- readRDS("data/migrate_and_resident_nitrogen.rds")
resident_150_nitrogen <- readRDS("data/resident_150_nitrogen.rds")
resident_300_nitrogen <- readRDS("data/resident_300_nitrogen.rds")
migration_150_nitrogen <- readRDS("data/migration_150_nitrogen.rds")

add_sd_layer <- function(r, sd_name) {
  sd_layer <- app(r, fun = sd, na.rm = TRUE) 
  names(sd_layer) <- sd_name                 
  r <- c(r, sd_layer)                        
  return(r)
}

migrate_and_resident_nitrogen <- add_sd_layer(migrate_and_resident_nitrogen, "migrate_and_resident_patches_sd")
resident_150_nitrogen <- add_sd_layer(resident_150_nitrogen, "resident_150_patches_sd")
resident_300_nitrogen <- add_sd_layer(resident_300_nitrogen, "resident_300_patches_sd")
migration_150_nitrogen <- add_sd_layer(migration_150_nitrogen, "migration_150_patches_sd")

## load environmental rasters
spatial_covariates <- readRDS("data/spatial_covariates_R1.rds")
covs <- spatial_covariates

dem                <- covs[["elevation"]]
slope              <- covs[["slope"]]
tcd                <- covs[["tcd"]]
distance_roads     <- covs[["distance_roads"]]


## Area with net nitrogen ----
areas_migr_res_300 <- lapply(1:10, function(i) {
  r <- migrate_and_resident_nitrogen[[i]]
  (sum(!is.na(values(r))) * 100) / 1e6
})
areas_migr_res_300 <- unlist(areas_migr_res_300)
cat(sprintf("mean %.2f km² (SD = %.2f)", 
            mean(areas_migr_res_300), 
            sd(areas_migr_res_300)))


areas_res_300 <- lapply(1:10, function(i) {
  r <- resident_300_nitrogen[[i]]
  (sum(!is.na(values(r))) * 100) / 1e6
})
areas_res_300 <- unlist(areas_res_300)
cat(sprintf("mean %.2f km² (SD = %.2f)", 
            mean(areas_res_300), 
            sd(areas_res_300)))


areas_res_150 <- lapply(1:10, function(i) {
  r <- resident_150_nitrogen[[i]]
  (sum(!is.na(values(r))) * 100) / 1e6
})
areas_res_150 <- unlist(areas_res_150)
cat(sprintf("mean %.2f km² (SD = %.2f)", 
            mean(areas_res_150), 
            sd(areas_res_150)))


areas_migr_150 <- lapply(1:10, function(i) {
  r <- migration_150_nitrogen[[i]]
  (sum(!is.na(values(r))) * 100) / 1e6
})
areas_migr_150 <- unlist(areas_migr_150)
cat(sprintf("mean %.2f km² (SD = %.2f)", 
            mean(areas_migr_150), 
            sd(areas_migr_150)))

round((mean(areas_migr_150) - mean(areas_res_150))/mean(areas_res_150)*100, 1)

### Total number of patches
(sum(!is.na(values(migrate_and_resident_nitrogen[[11]]))) * 100) / 1e6
(sum(!is.na(values(resident_300_nitrogen[[11]]))) * 100) / 1e6
(sum(!is.na(values(resident_150_nitrogen[[11]]))) * 100) / 1e6
(sum(!is.na(values(migration_150_nitrogen[[11]]))) * 100) / 1e6


## Total nitrogen moved ----
total_N_migr_res_300 <- lapply(1:10, function(i) {
  r <- migrate_and_resident_nitrogen[[i]]
  sum(values(r), na.rm = TRUE)
})
total_N_migr_res_300 <- unlist(total_N_migr_res_300)
mean(total_N_migr_res_300); sd(total_N_migr_res_300)

total_N_res_300 <- lapply(1:10, function(i) {
  r <- resident_300_nitrogen[[i]]
  sum(values(r), na.rm = TRUE)
})
total_N_res_300 <- unlist(total_N_res_300)
mean(total_N_res_300); sd(total_N_res_300)

total_N_res_150 <- lapply(1:10, function(i) {
  r <- resident_150_nitrogen[[i]]
  sum(values(r), na.rm = TRUE)
})
total_N_res_150 <- unlist(total_N_res_150)
mean(total_N_res_150); sd(total_N_res_150)

total_N_migr_150 <- lapply(1:10, function(i) {
  r <- migration_150_nitrogen[[i]]
  sum(values(r), na.rm = TRUE)
})
total_N_migr_150 <- unlist(total_N_migr_150)
mean(total_N_migr_150); sd(total_N_migr_150)

### Total nitrogen moved on mean raster
sum(values(migrate_and_resident_nitrogen[[11]]), na.rm = TRUE)
sum(values(resident_300_nitrogen[[11]]), na.rm = TRUE)
sum(values(resident_150_nitrogen[[11]]), na.rm = TRUE)
sum(values(migration_150_nitrogen[[11]]), na.rm = TRUE)


## Average Net Change ----
calculate_N_stats <- function(raster_obj, scenario_name) {
  
  # Initialize vectors to store per-simulation results
  mean_net <- numeric(10)
  min_net <- numeric(10)
  max_net <- numeric(10)
  
  mean_gain <- numeric(10)
  mean_loss <- numeric(10)
  
  # Loop through the 10 simulation layers
  for (i in 1:10) {
    # Extract values and remove NAs (visited patches only)
    vals <- values(raster_obj[[i]])
    visited_vals <- vals[!is.na(vals)]
    
    # --- OVERALL NET CHANGE ---
    mean_net[i] <- mean(visited_vals)
    min_net[i] <- min(visited_vals)
    max_net[i] <- max(visited_vals)
    
    # --- GAINS (Deposition) ---
    gains <- visited_vals[visited_vals > 0]
    mean_gain[i] <- ifelse(length(gains) > 0, mean(gains), NA)    
    
    # --- LOSSES (Removal) ---
    losses <- visited_vals[visited_vals < 0]
    mean_loss[i] <- ifelse(length(losses) > 0, mean(losses), NA)
  }
  
  # Summarize across all 10 simulations and return as a data frame row
  data.frame(
    Scenario = scenario_name,
    
    # Net overall metrics (g/100m²)
    Net_Mean = mean(mean_net, na.rm = TRUE),
    Net_SD = sd(mean_net, na.rm = TRUE),
    Avg_Net_Min = mean(min_net, na.rm = TRUE),
    Avg_Net_Max = mean(max_net, na.rm = TRUE),
    
    # Gain metrics (g/100m²)
    Gain_Mean = mean(mean_gain, na.rm = TRUE),
    Gain_SD = sd(mean_gain, na.rm = TRUE),
    
    # Loss metrics (g/100m²)
    Loss_Mean = mean(mean_loss, na.rm = TRUE),
    Loss_SD = sd(mean_loss, na.rm = TRUE)
  )
}

(results_mig_res <- calculate_N_stats(migrate_and_resident_nitrogen, "Migrate & Resident"))
(results_res_150 <- calculate_N_stats(resident_150_nitrogen, "Resident 150"))
(results_res_300 <- calculate_N_stats(resident_300_nitrogen, "Resident 300"))
(results_mig_150 <- calculate_N_stats(migration_150_nitrogen, "Migration 150"))

all_scenario_results <- rbind(results_mig_res, results_res_150, results_res_300, results_mig_150)
print(all_scenario_results)


# 1. Migrants + Residents, 300 deer ----
n_points <- 21000

# Sample points
pts_migr_res_300 <- vector("list", nlyr(migrate_and_resident_nitrogen))
for (i in 1:nlyr(migrate_and_resident_nitrogen)) {
  set.seed(123)
  pts_migr_res_300[[i]] <- spatSample(migrate_and_resident_nitrogen[[i]], size=n_points,
                                      method="regular", as.points=TRUE, na.rm=TRUE)
}
pts_migr_res_300 <- map(pts_migr_res_300, ~ { if(!is.null(.x)) names(.x)[1]<-"net_nitrogen"; .x })
names(pts_migr_res_300) <- names(migrate_and_resident_nitrogen)

# Build data frame
df_migr_res_300 <- map2(pts_migr_res_300, names(migrate_and_resident_nitrogen), ~{
  pts <- .x; layer_name <- .y
  if(is.null(pts)) return(NULL)
  coords <- terra::crds(pts)
  df <- data.frame(layer=layer_name, x=coords[,1], y=coords[,2], net=terra::values(pts)[,1])
  cov_vals <- terra::extract(covs, pts)[,-1, drop=FALSE]
  df <- cbind(df, cov_vals)
  df %>% drop_na(net, tcd, elevation, slope, distance_roads)
})
df_migr_res_300 <- bind_rows(df_migr_res_300[!sapply(df_migr_res_300, is.null)]) %>%
  mutate(net_abs=abs(net), net_trans=asinh(net)) %>%
  mutate(across(c(tcd,elevation,slope,distance_roads), ~as.numeric(scale(.))))

# GAM residuals
res_migr_res_300 <- df_migr_res_300 %>% filter(layer=="migrate_and_resident_patches_mean")
res_migr_res_300 <- res_migr_res_300 %>%
  mutate(
    tcd_res_mr300 = residuals(gam(tcd ~ s(x,y,bs="tp",k=150), data=.)),
    elev_res_mr300 = residuals(gam(elevation ~ s(x,y,bs="tp",k=150), data=.)),
    slope_res_mr300 = residuals(gam(slope ~ s(x,y,bs="tp",k=150), data=.)),
    dist_rd_res_mr300 = residuals(gam(distance_roads ~ s(x,y,bs="tp",k=150), data=.))
  )

# Full GAM
fit_migr_res_300 <- gam(net_trans ~
                          s(tcd_res_mr300,k=3) + s(elev_res_mr300,k=8) +
                          s(slope_res_mr300,k=5) + s(dist_rd_res_mr300,k=4) +
                          s(x,y,bs="tp",k=150),
                        data=res_migr_res_300, family=gaussian(), method="REML")

summary(fit_migr_res_300)
performance::r2(fit_migr_res_300)
concurvity(fit_migr_res_300, full = T) # alternative of collinearity for GAM

gam.check(fit_migr_res_300)

# Plot
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
    geom_line(col="#995C7F", linewidth=.8) + labs(x="Slope", y="Partial effect") +
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
    geom_line(col="#995C7F", linewidth=.8) + labs(x="Distance from roads", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank())
)

migr_res_300_plots <- ggarrange(plots_mr300$tcd, plots_mr300$slope,
                                plots_mr300$elev, plots_mr300$dist_rd,
                                ncol=2, nrow=2, labels=c("A","B","C","D"))
migr_res_300_plots <- annotate_figure(migr_res_300_plots,
                                      top=text_grob("Migrants+Residents, 300 deer", face="bold", size=14))


# 2. Residents, 300 deer ----
pts_res_300 <- vector("list", nlyr(resident_300_nitrogen))
for(i in 1:nlyr(resident_300_nitrogen)) {
  set.seed(123)
  pts_res_300[[i]] <- spatSample(resident_300_nitrogen[[i]], size=n_points,
                                 method="regular", as.points=TRUE, na.rm=TRUE)
}
pts_res_300 <- map(pts_res_300, ~ { if(!is.null(.x)) names(.x)[1]<-"net_nitrogen"; .x })
names(pts_res_300) <- names(resident_300_nitrogen)

df_res_300 <- map2(pts_res_300, names(resident_300_nitrogen), ~{
  pts <- .x; layer_name <- .y
  if(is.null(pts)) return(NULL)
  coords <- terra::crds(pts)
  df <- data.frame(layer=layer_name, x=coords[,1], y=coords[,2], net=terra::values(pts)[,1])
  cov_vals <- terra::extract(covs, pts)[,-1, drop=FALSE]
  df <- cbind(df, cov_vals)
  df %>% drop_na(net, tcd, elevation, slope, distance_roads)
})
df_res_300 <- bind_rows(df_res_300[!sapply(df_res_300, is.null)]) %>%
  mutate(net_abs=abs(net), net_trans=asinh(net)) %>%
  mutate(across(c(tcd,elevation,slope,distance_roads), ~as.numeric(scale(.))))

res_res_300 <- df_res_300 %>% filter(layer=="resident_300_patches_mean")
res_res_300 <- res_res_300 %>%
  mutate(
    tcd_res_r300 = residuals(gam(tcd ~ s(x,y,bs="tp",k=150), data=.)),
    elev_res_r300 = residuals(gam(elevation ~ s(x,y,bs="tp",k=150), data=.)),
    slope_res_r300 = residuals(gam(slope ~ s(x,y,bs="tp",k=150), data=.)),
    dist_rd_res_r300 = residuals(gam(distance_roads ~ s(x,y,bs="tp",k=150), data=.))
  )

fit_res_300 <- gam(net_trans ~
                     s(tcd_res_r300,k=3) + s(elev_res_r300,k=8) +
                     s(slope_res_r300,k=5) + s(dist_rd_res_r300,k=4) +
                     s(x,y,bs="tp",k=150),
                   data=res_res_300, family=gaussian(), method="REML")

summary(fit_res_300)
performance::r2(fit_res_300)
concurvity(fit_res_300, full = T) # alternative of collinearity for GAM

gam.check(fit_res_300)

# Plot
sm_res_300 <- smooth_estimates(fit_res_300) |> add_confint()

plots_r300 <- list(
  tcd = ggplot(sm_res_300, aes(x=tcd_res_r300, y=.estimate)) +
    geom_rug(data=res_res_300, aes(x=tcd_res_r300), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#009E73") +
    geom_line(col="#006D51", linewidth=.8) + labs(x="Tree cover density", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  slope = ggplot(sm_res_300, aes(x=slope_res_r300, y=.estimate)) +
    geom_rug(data=res_res_300, aes(x=slope_res_r300), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#009E73") +
    geom_line(col="#006D51", linewidth=.8) + labs(x="Slope", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  elev = ggplot(sm_res_300, aes(x=elev_res_r300, y=.estimate)) +
    geom_rug(data=res_res_300, aes(x=elev_res_r300), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#009E73") +
    geom_line(col="#006D51", linewidth=.8) + labs(x="Elevation", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  dist_rd = ggplot(sm_res_300, aes(x=dist_rd_res_r300, y=.estimate)) +
    geom_rug(data=res_res_300, aes(x=dist_rd_res_r300), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#009E73") +
    geom_line(col="#006D51", linewidth=.8) + labs(x="Distance from roads", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank())
)

res_300_plots <- ggarrange(plots_r300$tcd, plots_r300$slope,
                           plots_r300$elev, plots_r300$dist_rd,
                           ncol=2, nrow=2, labels=c("A","B","C","D"))
res_300_plots <- annotate_figure(res_300_plots,
                                 top=text_grob("Residents, 300 deer", face="bold", size=14))


# 3. Residents, 150 deer ----
pts_res_150 <- vector("list", nlyr(resident_150_nitrogen))
for(i in 1:nlyr(resident_150_nitrogen)) {
  set.seed(123)
  pts_res_150[[i]] <- spatSample(resident_150_nitrogen[[i]], size=n_points,
                                 method="regular", as.points=TRUE, na.rm=TRUE)
}
pts_res_150 <- map(pts_res_150, ~ { if(!is.null(.x)) names(.x)[1]<-"net_nitrogen"; .x })
names(pts_res_150) <- names(resident_150_nitrogen)

df_res_150 <- map2(pts_res_150, names(resident_150_nitrogen), ~{
  pts <- .x; layer_name <- .y
  if(is.null(pts)) return(NULL)
  coords <- terra::crds(pts)
  df <- data.frame(layer=layer_name, x=coords[,1], y=coords[,2], net=terra::values(pts)[,1])
  cov_vals <- terra::extract(covs, pts)[,-1, drop=FALSE]
  df <- cbind(df, cov_vals)
  df %>% drop_na(net, tcd, elevation, slope, distance_roads)
})
df_res_150 <- bind_rows(df_res_150[!sapply(df_res_150, is.null)]) %>%
  mutate(net_abs=abs(net), net_trans=asinh(net)) %>%
  mutate(across(c(tcd,elevation,slope,distance_roads), ~as.numeric(scale(.))))

res_res_150 <- df_res_150 %>% filter(layer=="resident_150_patches_mean")
res_res_150 <- res_res_150 %>%
  mutate(
    tcd_res_r150 = residuals(gam(tcd ~ s(x,y,bs="tp",k=150), data=.)),
    elev_res_r150 = residuals(gam(elevation ~ s(x,y,bs="tp",k=150), data=.)),
    slope_res_r150 = residuals(gam(slope ~ s(x,y,bs="tp",k=150), data=.)),
    dist_rd_res_r150 = residuals(gam(distance_roads ~ s(x,y,bs="tp",k=150), data=.))
  )

fit_res_150 <- gam(net_trans ~
                     s(tcd_res_r150,k=3) + s(elev_res_r150,k=8) +
                     s(slope_res_r150,k=5) + s(dist_rd_res_r150,k=4) +
                     s(x,y,bs="tp",k=150),
                   data=res_res_150, family=gaussian(), method="REML")

summary(fit_res_150)
performance::r2(fit_res_150)
concurvity(fit_res_150, full = T) # alternative of collinearity for GAM

gam.check(fit_res_150)

# Plot
sm_res_150 <- smooth_estimates(fit_res_150) |> add_confint()

plots_r150 <- list(
  tcd = ggplot(sm_res_150, aes(x=tcd_res_r150, y=.estimate)) +
    geom_rug(data=res_res_150, aes(x=tcd_res_r150), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#D55E00") +
    geom_line(col="#A04800", linewidth=.8) + labs(x="Tree cover density", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  slope = ggplot(sm_res_150, aes(x=slope_res_r150, y=.estimate)) +
    geom_rug(data=res_res_150, aes(x=slope_res_r150), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#D55E00") +
    geom_line(col="#A04800", linewidth=.8) + labs(x="Slope", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  elev = ggplot(sm_res_150, aes(x=elev_res_r150, y=.estimate)) +
    geom_rug(data=res_res_150, aes(x=elev_res_r150), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#D55E00") +
    geom_line(col="#A04800", linewidth=.8) + labs(x="Elevation", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  dist_rd = ggplot(sm_res_150, aes(x=dist_rd_res_r150, y=.estimate)) +
    geom_rug(data=res_res_150, aes(x=dist_rd_res_r150), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#D55E00") +
    geom_line(col="#A04800", linewidth=.8) + labs(x="Distance from roads", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank())
)

res_150_plots <- ggarrange(plots_r150$tcd, plots_r150$slope,
                           plots_r150$elev, plots_r150$dist_rd,
                           ncol=2, nrow=2, labels=c("A","B","C","D"))
res_150_plots <- annotate_figure(res_150_plots,
                                 top=text_grob("Residents, 150 deer", face="bold", size=14))


# 4. Migrants, 150 deer ----
pts_migr_150 <- vector("list", nlyr(migration_150_nitrogen))
for(i in 1:nlyr(migration_150_nitrogen)) {
  set.seed(123)
  pts_migr_150[[i]] <- spatSample(migration_150_nitrogen[[i]], size=n_points,
                                  method="regular", as.points=TRUE, na.rm=TRUE)
}
pts_migr_150 <- map(pts_migr_150, ~ { if(!is.null(.x)) names(.x)[1]<-"net_nitrogen"; .x })
names(pts_migr_150) <- names(migration_150_nitrogen)

df_migr_150 <- map2(pts_migr_150, names(migration_150_nitrogen), ~{
  pts <- .x; layer_name <- .y
  if(is.null(pts)) return(NULL)
  coords <- terra::crds(pts)
  df <- data.frame(layer=layer_name, x=coords[,1], y=coords[,2], net=terra::values(pts)[,1])
  cov_vals <- terra::extract(covs, pts)[,-1, drop=FALSE]
  df <- cbind(df, cov_vals)
  df %>% drop_na(net, tcd, elevation, slope, distance_roads)
})
df_migr_150 <- bind_rows(df_migr_150[!sapply(df_migr_150, is.null)]) %>%
  mutate(net_abs=abs(net), net_trans=asinh(net)) %>%
  mutate(across(c(tcd,elevation,slope,distance_roads), ~as.numeric(scale(.))))

res_migr_150 <- df_migr_150 %>% filter(layer=="migration_150_patches_mean")

res_migr_150 <- res_migr_150 %>%
  mutate(
    tcd_res_m150 = residuals(gam(tcd ~ s(x,y,bs="tp",k=150), data=.)),
    elev_res_m150 = residuals(gam(elevation ~ s(x,y,bs="tp",k=150), data=.)),
    slope_res_m150 = residuals(gam(slope ~ s(x,y,bs="tp",k=150), data=.)),
    dist_rd_res_m150 = residuals(gam(distance_roads ~ s(x,y,bs="tp",k=150), data=.))
  )

fit_migr_150 <- gam(net_trans ~
                      s(tcd_res_m150,k=3) + s(elev_res_m150,k=8) +
                      s(slope_res_m150,k=5) + s(dist_rd_res_m150,k=4) +
                      s(x,y,bs="tp",k=150),
                    data=res_migr_150, family=gaussian(), method="REML")

summary(fit_migr_150)
performance::r2(fit_migr_150)
concurvity(fit_migr_150, full = T) # alternative of collinearity for GAM

gam.check(fit_migr_150)

# Plot
sm_migr_150 <- smooth_estimates(fit_migr_150) |> add_confint()

plots_m150 <- list(
  tcd = ggplot(sm_migr_150, aes(x=tcd_res_m150, y=.estimate)) +
    geom_rug(data=res_migr_150, aes(x=tcd_res_m150), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#0072B2") +
    geom_line(col="#005280", linewidth=.8) + labs(x="Tree cover density", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  slope = ggplot(sm_migr_150, aes(x=slope_res_m150, y=.estimate)) +
    geom_rug(data=res_migr_150, aes(x=slope_res_m150), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#0072B2") +
    geom_line(col="#005280", linewidth=.8) + labs(x="Slope", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  elev = ggplot(sm_migr_150, aes(x=elev_res_m150, y=.estimate)) +
    geom_rug(data=res_migr_150, aes(x=elev_res_m150), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#0072B2") +
    geom_line(col="#005280", linewidth=.8) + labs(x="Elevation", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank()),
  
  dist_rd = ggplot(sm_migr_150, aes(x=dist_rd_res_m150, y=.estimate)) +
    geom_rug(data=res_migr_150, aes(x=dist_rd_res_m150), sides="b", alpha=0.3,
             inherit.aes = FALSE) +
    geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), alpha=0.2, fill="#0072B2") +
    geom_line(col="#005280", linewidth=.8) + labs(x="Distance from roads", y="Partial effect") +
    coord_cartesian(ylim=c(-2.6,2.6)) + theme_minimal() +
    theme(axis.title.x = element_blank())
)

migr_150_plots <- ggarrange(plots_m150$tcd, plots_m150$slope,
                            plots_m150$elev, plots_m150$dist_rd,
                            ncol=2, nrow=2, labels=c("A","B","C","D"))
migr_150_plots <- annotate_figure(migr_150_plots,
                                  top=text_grob("Migrants, 150 deer", face="bold", size=14))


## Net nitrogen per elevation ----
calc_zonal_binary_elev <- function(raster_stack, dem) {
  
  # Create elevation zones: below and above 1200 m
  elev_zones <- dem
  elev_zones[] <- ifelse(values(dem) < 1200, 0, 1)  # 0 = below, 1 = above
  
  # Run zonal sum for each layer
  results <- bind_rows(lapply(seq_len(nlyr(raster_stack)), function(i) {
    z <- zonal(raster_stack[[i]], elev_zones, fun = "sum", na.rm = TRUE)
    names(z) <- c("zone", "net_nitrogen")
    z$elev_class <- ifelse(z$zone == 0, "below_1200", "above_1200")
    z$layer <- names(raster_stack[[i]])
    z
  }))
}

calc_zonal_binary_elev(
  raster_stack = migrate_and_resident_nitrogen, dem = dem)  %>%
  filter(!layer %in% c("migrate_and_resident_patches_mean", "migrate_and_resident_patches_sd")) %>%
  group_by(elev_class) %>%
  summarise(
    mean_net_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd_net_nitrogen   = sd(net_nitrogen, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

calc_zonal_binary_elev(
  raster_stack = resident_300_nitrogen, dem = dem)  %>%
  filter(!layer %in% c("resident_300_patches_mean", "resident_300_patches_sd")) %>%
  group_by(elev_class) %>%
  summarise(
    mean_net_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd_net_nitrogen   = sd(net_nitrogen, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

calc_zonal_binary_elev(
  raster_stack = resident_150_nitrogen, dem = dem)  %>%
  filter(!layer %in% c("resident_150_patches_mean", "resident_150_patches_sd")) %>%
  group_by(elev_class) %>%
  summarise(
    mean_net_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd_net_nitrogen   = sd(net_nitrogen, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

calc_zonal_binary_elev(
  raster_stack = migration_150_nitrogen, dem = dem)  %>%
  filter(!layer %in% c("migration_150_patches_mean", "migration_150_patches_sd")) %>%
  group_by(elev_class) %>%
  summarise(
    mean_net_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd_net_nitrogen   = sd(net_nitrogen, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()


values(dem) <- round(values(dem), digits = -1)

calculate_elevation_zonal_sum <- function(raster_stack, dem, type_label) {
  sim_layers <- 1:(nlyr(raster_stack) - 2)
  
  bind_rows(lapply(sim_layers, function(i) {
    z <- zonal(raster_stack[[i]], dem, fun = "sum", na.rm = TRUE)
    names(z)[2] <- "net_nitrogen"
    z
  })) %>% mutate(type = type_label)
}

# Calculate zonal sums for each nitrogen raster
elev_migrate_resident_df <- calculate_elevation_zonal_sum(migrate_and_resident_nitrogen, dem, "Resident and migrant")
elev_resident_150_df <- calculate_elevation_zonal_sum(resident_150_nitrogen, dem, "Resident 150")
elev_resident_300_df <- calculate_elevation_zonal_sum(resident_300_nitrogen, dem, "Resident 300")
elev_migration_150_df <- calculate_elevation_zonal_sum(migration_150_nitrogen, dem, "Migrant 150")

# Combine datasets
elevation_df <- bind_rows(
  elev_migrate_resident_df,
  elev_resident_150_df,
  elev_resident_300_df,
  elev_migration_150_df
)

# Compute summary statistics
elevation_summary_df <- elevation_df %>%
  group_by(elevation, type) %>%
  summarise(
    mean_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd = sd(net_nitrogen, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci_lower = mean_nitrogen - qt(0.975, df = n - 1) * se,
    ci_upper = mean_nitrogen + qt(0.975, df = n - 1) * se,
    .groups = "drop")

elevation_summary_df$type <- factor(elevation_summary_df$type, 
                                    levels = c("Migrant 150", 
                                               "Resident 150", 
                                               "Resident and migrant", 
                                               "Resident 300"))

# Plot
elevation_plot <- ggplot(elevation_summary_df, aes(x = elevation, y = mean_nitrogen, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = NULL)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = NULL)) +
  labs(x = "Elevation (m)", y = "Net nitrogen (gN)", fill = "Type") +
  scale_fill_manual(values = movement_colors) +
  scale_x_continuous(breaks = seq(700, 2500, by = 300)) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(700, 2250)) +
  facet_wrap(~ type, nrow = 2, scales = "fixed") +
  theme_minimal() +
  theme(strip.text = element_blank(),
        legend.position = "top")


## Net nitrogen per slope ----
calc_zonal_binary_slope <- function(raster_stack, slope) {
  
  # Create elevation zones: below and above 1200 m
  slope_zones <- slope
  slope_zones[] <- ifelse(values(slope) < 25, 0, 1)  # 0 = below, 1 = above
  
  # Run zonal sum for each layer
  results <- bind_rows(lapply(seq_len(nlyr(raster_stack)), function(i) {
    z <- zonal(raster_stack[[i]], slope_zones, fun = "sum", na.rm = TRUE)
    names(z) <- c("zone", "net_nitrogen")
    z$slope_zones <- ifelse(z$zone == 0, "below_25", "above_25")
    z$layer <- names(raster_stack[[i]])
    z
  }))
}

calc_zonal_binary_slope(
  raster_stack = migrate_and_resident_nitrogen, slope = slope)  %>%
  filter(!layer %in% c("migrate_and_resident_patches_mean", "migrate_and_resident_patches_sd")) %>%
  group_by(slope_zones) %>%
  summarise(
    mean_net_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd_net_nitrogen   = sd(net_nitrogen, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

calc_zonal_binary_slope(
  raster_stack = resident_300_nitrogen, slope = slope)  %>%
  filter(!layer %in% c("resident_300_patches_mean", "resident_300_patches_sd")) %>%
  group_by(slope_zones) %>%
  summarise(
    mean_net_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd_net_nitrogen   = sd(net_nitrogen, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

calc_zonal_binary_slope(
  raster_stack = resident_150_nitrogen, slope = slope)  %>%
  filter(!layer %in% c("resident_150_patches_mean", "resident_150_patches_sd")) %>%
  group_by(slope_zones) %>%
  summarise(
    mean_net_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd_net_nitrogen   = sd(net_nitrogen, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

calc_zonal_binary_slope(
  raster_stack = migration_150_nitrogen, slope = slope)  %>%
  filter(!layer %in% c("migration_150_patches_mean", "migration_150_patches_sd")) %>%
  group_by(slope_zones) %>%
  summarise(
    mean_net_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd_net_nitrogen   = sd(net_nitrogen, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()


values(slope) <- round(values(slope), digits = 0)

# Function for zonal sum by slope
calculate_slope_zonal_sum <- function(raster_stack, slope_raster, type_label) {
  sim_layers <- 1:(nlyr(raster_stack) - 2)
  
  bind_rows(lapply(sim_layers, function(i) {
    z <- zonal(raster_stack[[i]], slope_raster, fun = "sum", na.rm = TRUE)
    names(z)[2] <- "net_nitrogen"
    z
  })) %>% mutate(type = type_label)
}

# Calculate zonal sums for each nitrogen raster using slope
slope_migrate_resident_df <- calculate_slope_zonal_sum(migrate_and_resident_nitrogen, slope, "Resident and migrant")
slope_resident_150_df <- calculate_slope_zonal_sum(resident_150_nitrogen, slope, "Resident 150")
slope_resident_300_df <- calculate_slope_zonal_sum(resident_300_nitrogen, slope, "Resident 300")
slope_migration_150_df <- calculate_slope_zonal_sum(migration_150_nitrogen, slope, "Migrant 150")

# Combine datasets
slope_df <- bind_rows(
  slope_migrate_resident_df,
  slope_resident_150_df,
  slope_resident_300_df,
  slope_migration_150_df
)

# Compute summary statistics
slope_summary_df <- slope_df %>%
  group_by(slope, type) %>%
  summarise(
    mean_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd = sd(net_nitrogen, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci_lower = mean_nitrogen - qt(0.975, df = n - 1) * se,
    ci_upper = mean_nitrogen + qt(0.975, df = n - 1) * se,
    .groups = "drop") 

slope_summary_df$type <- factor(slope_summary_df$type, 
                                levels = c("Migrant 150", 
                                           "Resident 150", 
                                           "Resident and migrant", 
                                           "Resident 300"))

# Plot
slope_plot <- ggplot(slope_summary_df, aes(x = slope, y = mean_nitrogen, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = NULL)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = NULL)) +
  labs(x = "Slope (°)", y = "Net nitrogen (gN)", fill = "Type") +
  scale_fill_manual(values = movement_colors) +
  scale_x_continuous(breaks = seq(min(slope_summary_df$slope), max(slope_summary_df$slope), by = 5)) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 60)) +
  facet_wrap(~ type, nrow = 2, scales = "fixed") +
  theme_minimal() +
  theme(strip.text = element_blank(),
        legend.position = "top") 


## Net nitrogen per tree cover density ----
calc_zonal_binary_tcd <- function(raster_stack, tcd) {
  
  # Create elevation zones: below and above 1200 m
  habitat_zones <- tcd
  habitat_zones[] <- ifelse(values(tcd) < 65, 0, 1)  # 0 = below, 1 = above
  
  # Run zonal sum for each layer
  results <- bind_rows(lapply(seq_len(nlyr(raster_stack)), function(i) {
    z <- zonal(raster_stack[[i]], habitat_zones, fun = "sum", na.rm = TRUE)
    names(z) <- c("zone", "net_nitrogen")
    z$habitat_zones <- ifelse(z$zone == 0, "below_65", "above_65")
    z$layer <- names(raster_stack[[i]])
    z
  }))
}

calc_zonal_binary_tcd(
  raster_stack = migrate_and_resident_nitrogen, tcd = tcd)  %>%
  filter(!layer %in% c("migrate_and_resident_patches_mean", "migrate_and_resident_patches_sd")) %>%
  group_by(habitat_zones) %>%
  summarise(
    mean_net_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd_net_nitrogen   = sd(net_nitrogen, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

calc_zonal_binary_tcd(
  raster_stack = resident_300_nitrogen, tcd = tcd)  %>%
  filter(!layer %in% c("migrate_and_resident_patches_mean", "migrate_and_resident_patches_sd")) %>%
  group_by(habitat_zones) %>%
  summarise(
    mean_net_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd_net_nitrogen   = sd(net_nitrogen, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

calc_zonal_binary_tcd(
  raster_stack = resident_150_nitrogen, tcd = tcd)  %>%
  filter(!layer %in% c("resident_150_patches_mean", "resident_150_patches_sd")) %>%
  group_by(habitat_zones) %>%
  summarise(
    mean_net_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd_net_nitrogen   = sd(net_nitrogen, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

calc_zonal_binary_tcd(
  raster_stack = migration_150_nitrogen, tcd = tcd)  %>%
  filter(!layer %in% c("migration_150_patches_mean", "migration_150_patches_sd")) %>%
  group_by(habitat_zones) %>%
  summarise(
    mean_net_nitrogen = mean(net_nitrogen, na.rm = TRUE),
    sd_net_nitrogen   = sd(net_nitrogen, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()


values(tcd) <- round(values(tcd), digits = 0)
tcd[tcd < 0] <- 0
tcd[tcd > 100] <- 100

# Zonal sum calculation function
calculate_tcd_zonal_sum <- function(raster_stack, tcd_raster, type_label) {
  sim_layers <- 1:(nlyr(raster_stack) - 2)
  
  bind_rows(lapply(sim_layers, function(i) {
    z <- zonal(raster_stack[[i]], tcd_raster, fun = "sum", na.rm = TRUE)
    names(z)[2] <- "net_nitrogen"
    z
  })) %>% mutate(type = type_label)
}

# Calculate zonal sums
tcd_migrate_resident_df <- calculate_tcd_zonal_sum(migrate_and_resident_nitrogen, tcd, "Resident and migrant")
tcd_resident_150_df     <- calculate_tcd_zonal_sum(resident_150_nitrogen, tcd, "Resident 150")
tcd_resident_300_df     <- calculate_tcd_zonal_sum(resident_300_nitrogen, tcd, "Resident 300")
tcd_migration_150_df    <- calculate_tcd_zonal_sum(migration_150_nitrogen, tcd, "Migrant 150")

# Combine all into a single DF
tcd_df <- bind_rows(
  tcd_migrate_resident_df,
  tcd_resident_150_df,
  tcd_resident_300_df,
  tcd_migration_150_df
)

# Summary stats
tcd_summary_df <- tcd_df %>%
  group_by(tcd, type) %>%
  summarise(mean_nitrogen = mean(net_nitrogen, na.rm = TRUE),
            sd = sd(net_nitrogen, na.rm = TRUE),
            n = n(),
            se = sd / sqrt(n),
            ci_lower = mean_nitrogen - qt(0.975, df = n - 1) * se,
            ci_upper = mean_nitrogen + qt(0.975, df = n - 1) * se,
            .groups = "drop") 

tcd_summary_df$type <- factor(tcd_summary_df$type, 
                              levels = c("Migrant 150", 
                                         "Resident 150", 
                                         "Resident and migrant", 
                                         "Resident 300"))

# Plot
tcd_plot <- ggplot(tcd_summary_df, aes(x = tcd, y = mean_nitrogen, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = NULL)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = NULL)) +
  labs(x = "Tree cover density (%)", y = "Net nitrogen (gN)", fill = "Type") +
  scale_fill_manual(values = movement_colors) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 100)) +
  facet_wrap(~ type, nrow = 2, scales = "fixed") +
  theme_minimal() +
  theme(strip.text = element_blank(),
        legend.position = "top") 


## Figure plots ----
scenario_colors <- c(
  "M_150"   = "#0072B2",
  "R_150"   = "#D55E00",
  "M+R_300" = "#CC79A7",
  "R_300"   = "#009E73"
)

legend_df <- data.frame(
  Simulation = factor(names(scenario_colors), levels = names(scenario_colors)),
  x = 1, y = 1
)
legend_plot <- ggplot(legend_df, aes(x=x, y=y, color=Simulation)) +
  geom_point() +
  scale_color_manual(values=scenario_colors, limits = names(scenario_colors)) +
  theme_void() +
  theme(legend.position="bottom")

legend <- get_legend(legend_plot)

movement_colors2 <- c("Resident and migrant" = "#CC79A7", 
                      "Migrant 150" = "#0072B2",
                      "Resident 150" = "#D55E00",
                      "Resident 300" = "#009E73" )

create_single_barplot <- function(df, x_col, scenario_name, x_label, x_limits, x_breaks, v_line = NULL) {
  
  # Subset to only the specific scenario
  df_sub <- df %>% filter(type == scenario_name)
  color_val <- movement_colors2[[scenario_name]]
  
  p <- ggplot(df_sub, aes(x = .data[[x_col]], y = mean_nitrogen)) +
    geom_bar(stat = "identity", fill = color_val) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2)
  
  # Add vertical dashed line if provided
  if (!is.null(v_line)) {
    p <- p + geom_vline(xintercept = v_line, linetype = "dashed", color = "black", linewidth = 0.5)
  }
  
  p <- p + 
    labs(x = x_label, y = "Net nitrogen (gN)") +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(labels = comma) +
    coord_cartesian(xlim = x_limits) +
    theme_minimal() +
    theme(legend.position = "none") # Legend is added globally later
  
  return(p)
}

# Combine Plots for ELEVATION
elev_combined <- ggarrange(
  
  # A: Migrants 150
  ggarrange(plots_m150$elev + xlab("Residualized elevation") + theme(axis.title.x = element_text()), 
            create_single_barplot(elevation_summary_df, "elevation", "Migrant 150", 
                                  "Elevation (m)", c(700, 2250), seq(700, 2500, by = 300), v_line = 1200), 
            nrow = 2),
  
  # B: Residents 150
  ggarrange(plots_r150$elev + xlab("Residualized elevation") + theme(axis.title.x = element_text()), 
            create_single_barplot(elevation_summary_df, "elevation", "Resident 150", 
                                  "Elevation (m)", c(700, 2250), seq(700, 2500, by = 300), v_line = 1200), 
            nrow = 2),
  
  # C: Migrants+Residents 300
  ggarrange(plots_mr300$elev + xlab("Residualized elevation") + theme(axis.title.x = element_text()), 
            create_single_barplot(elevation_summary_df, "elevation", "Resident and migrant", 
                                  "Elevation (m)", c(700, 2250), seq(700, 2500, by = 300), v_line = 1200), 
            nrow = 2),
  
  # D: Residents 300
  ggarrange(plots_r300$elev + xlab("Residualized elevation") + theme(axis.title.x = element_text()), 
            create_single_barplot(elevation_summary_df, "elevation", "Resident 300", 
                                  "Elevation (m)", c(700, 2250), seq(700, 2500, by = 300), v_line = 1200), 
            nrow = 2),
  
  labels = c("A", "B", "C", "D"),
  ncol = 2, nrow = 2 
)

elev_combined <- annotate_figure(elev_combined, bottom = legend)

# Combine Plots for SLOPE
slope_combined <- ggarrange(
  
  # A: Migrants 150
  ggarrange(plots_m150$slope + xlab("Residualized slope") + theme(axis.title.x = element_text()),
            create_single_barplot(slope_summary_df, "slope", "Migrant 150", 
                                  "Slope (°)", c(0, 60), seq(0, 60, by = 10), v_line = 25), 
            nrow = 2),
  
  # B: Residents 150
  ggarrange(plots_r150$slope + xlab("Residualized slope") + theme(axis.title.x = element_text()), 
            create_single_barplot(slope_summary_df, "slope", "Resident 150", 
                                  "Slope (°)", c(0, 60), seq(0, 60, by = 10), v_line = 25), 
            nrow = 2),
  
  # C: Migrants+Residents 300
  ggarrange(plots_mr300$slope + xlab("Residualized slope") + theme(axis.title.x = element_text()), 
            create_single_barplot(slope_summary_df, "slope", "Resident and migrant", 
                                  "Slope (°)", c(0, 60), seq(0, 60, by = 10), v_line = 25), 
            nrow = 2),
  
  # D: Residents 300
  ggarrange(plots_r300$slope + xlab("Residualized slope") + theme(axis.title.x = element_text()), 
            create_single_barplot(slope_summary_df, "slope", "Resident 300", 
                                  "Slope (°)", c(0, 60), seq(0, 60, by = 10), v_line = 25), 
            nrow = 2),
  labels = c("A", "B", "C", "D"),
  ncol = 2, nrow = 2 
)

slope_combined <- annotate_figure(slope_combined, bottom = legend)

# Combine Plots for TCD
tcd_combined <- ggarrange(
  
  # A: Migrants 150
  ggarrange(plots_m150$tcd + xlab("Residualized tree cover density") + theme(axis.title.x = element_text()), 
            create_single_barplot(tcd_summary_df, "tcd", "Migrant 150", 
                                  "Tree cover density (%)", c(0, 100), seq(0, 100, by = 10), v_line = 65), 
            nrow = 2),
  
  # B: Residents 150
  ggarrange(plots_r150$tcd + xlab("Residualized tree cover density") + theme(axis.title.x = element_text()), 
            create_single_barplot(tcd_summary_df, "tcd", "Resident 150", 
                                  "Tree cover density (%)", c(0, 100), seq(0, 100, by = 10), v_line = 65), 
            nrow = 2),
  
  # C: Migrants+Residents 300
  ggarrange(plots_mr300$tcd + xlab("Residualized tree cover density") + theme(axis.title.x = element_text()), 
            create_single_barplot(tcd_summary_df, "tcd", "Resident and migrant", 
                                  "Tree cover density (%)", c(0, 100), seq(0, 100, by = 10), v_line = 65), 
            nrow = 2),
  
  # D: Residents 300
  ggarrange(plots_r300$tcd + xlab("Residualized tree cover density") + theme(axis.title.x = element_text()), 
            create_single_barplot(tcd_summary_df, "tcd", "Resident 300", 
                                  "Tree cover density (%)", c(0, 100), seq(0, 100, by = 10), v_line = 65), 
            nrow = 2),
  labels = c("A", "B", "C", "D"),
  ncol = 2, nrow = 2
)

tcd_combined <- annotate_figure(tcd_combined, bottom = legend)

# Plots for distance from roads/trails
roads_plots <- ggarrange(
  plots_m150$dist_rd + xlab("Residualized distance from infrastructure") + theme(axis.title.x = element_text()),
  plots_r150$dist_rd + xlab("Residualized distance from infrastructure") + theme(axis.title.x = element_text()),
  plots_mr300$dist_rd + xlab("Residualized distance from infrastructure") + theme(axis.title.x = element_text()),
  plots_r300$dist_rd + xlab("Residualized distance from infrastructure") + theme(axis.title.x = element_text()),
  ncol=2, nrow=2, labels=c("A","B", "C", "D")
)

roads_plots <- annotate_figure(roads_plots, bottom =legend)


# Mapping study area ----
library(ggfx)
library(ggnewscale)
library(scico)
library(ggspatial)

# Convert raster extent to polygon (respects rotation / tilt)
aspect <- rast("data/aspect.tif")
roads <- st_read("data/roads.shp")  
  
big_roads <- roads %>% filter(highway %in% c("primary", "secondary", "tertiary"))
small_roads <- roads %>% filter(!highway %in% c("primary", "secondary", "tertiary"))

aspect_coarse <- aggregate(aspect, fact = 5)
raster_poly <- st_as_sf(as.polygons(aspect_coarse))
raster_poly <- st_union(raster_poly)
st_crs(raster_poly) <- st_crs(roads) 

big_roads <- st_intersection(big_roads, raster_poly)
small_roads <- st_intersection(small_roads, raster_poly)

aspect_df <- as.data.frame(aspect, xy = TRUE)
names(aspect_df)[3] <- "aspect"

map1 <-
  ggplot() +
  as_reference(
    geom_raster(data = aspect_df, aes(x = x, y = y, fill = aspect), show.legend = FALSE),
    id = "aspect_bg") +
  scale_fill_gradient(low = "grey95", high = "grey70") +
  new_scale_fill() +
  with_blend(
    geom_point(data = df_migr_res_300 %>% filter(layer=="migrate_and_resident_patches_1"),
               aes(x = x, y = y, fill = net),
               shape = 21, size = 1, stroke = 0.1, alpha = 1,
               color = "grey30"),
    bg_layer = "aspect_bg", blend = "multiply") +
  scale_fill_scico(palette = "roma", direction = -1, 
                   name = expression(Net~nitrogen~(gN / 100~m^2))) +
  geom_sf(data = small_roads, color = "grey35", size = 0.25, alpha = 0.5) +
  geom_sf(data = big_roads, color = "grey20", size = 1.5) +
  geom_sf(data = big_roads, color = "white", size = 1) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(title = "M+R_300") +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

map2 <-
  ggplot() +
  as_reference(
    geom_raster(data = aspect_df, aes(x = x, y = y, fill = aspect), show.legend = FALSE),
    id = "aspect_bg") +
  scale_fill_gradient(low = "grey95", high = "grey70") +
  new_scale_fill() +
  with_blend(
    geom_point(data = df_res_300 %>% filter(layer=="resident_300_patches_1"),
               aes(x = x, y = y, fill = net),
               shape = 21, size = 1, stroke = 0.1, alpha = 1,
               color = "grey30"),
    bg_layer = "aspect_bg", blend = "multiply") +
  scale_fill_scico(palette = "roma", direction = -1, 
                   name = expression(Net~nitrogen~(gN / 100~m^2))) +
  geom_sf(data = small_roads, color = "grey35", size = 0.25, alpha = 0.5) +
  geom_sf(data = big_roads, color = "grey20", size = 1.5) +
  geom_sf(data = big_roads, color = "white", size = 1) +
  annotation_scale(location = "bl", width_hint = 0.2, unit_category = "metric", 
                   text_face = "bold", text_cex = 1.2) +
  annotation_north_arrow(
    location = "tr",            
    which_north = "true",      
    height = unit(0.4, "in"),
    width = unit(0.4, "in"),
    pad_x = unit(0.15, "in"),   
    pad_y = unit(0.15, "in"),   
    style = north_arrow_orienteering(
      fill = c("grey35", "white"),
      line_col = "grey20"
    )
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(title = "R_300") +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

map3 <-
  ggplot() +
  as_reference(
    geom_raster(data = aspect_df, aes(x = x, y = y, fill = aspect), show.legend = FALSE),
    id = "aspect_bg") +
  scale_fill_gradient(low = "grey95", high = "grey70") +
  new_scale_fill() +
  with_blend(
    geom_point(data = df_res_150 %>% filter(layer=="resident_150_patches_1"),
               aes(x = x, y = y, fill = net),
               shape = 21, size = 1, stroke = 0.1, alpha = 1,
               color = "grey30"),
    bg_layer = "aspect_bg", blend = "multiply") +
  scale_fill_scico(palette = "roma", direction = -1, 
                   name = expression(Net~nitrogen~(gN / 100~m^2))) +
  geom_sf(data = small_roads, color = "grey35", size = 0.25, alpha = 0.5) +
  geom_sf(data = big_roads, color = "grey20", size = 1.5) +
  geom_sf(data = big_roads, color = "white", size = 1) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(title = "R_150") +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

map4 <-
  ggplot() +
  as_reference(
    geom_raster(data = aspect_df, aes(x = x, y = y, fill = aspect), show.legend = FALSE),
    id = "aspect_bg") +
  scale_fill_gradient(low = "grey95", high = "grey70") +
  new_scale_fill() +
  with_blend(
    geom_point(data = df_migr_150 %>% filter(layer=="migration_150_patches_1"),
               aes(x = x, y = y, fill = net),
               shape = 21, size = 1, stroke = 0.1, alpha = 1,
               color = "grey30"),
    bg_layer = "aspect_bg", blend = "multiply") +
  scale_fill_scico(palette = "roma", direction = -1, 
                   name = expression(Net~nitrogen~(gN / 100~m^2))) +
  geom_sf(data = small_roads, color = "grey35", size = 0.25, alpha = 0.5) +
  geom_sf(data = big_roads, color = "grey20", size = 1.5) +
  geom_sf(data = big_roads, color = "white", size = 1) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(title = "M_150") +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

mapp <- ggarrange(
  map4, map3, map1, map2,
  ncol = 2, nrow = 2,
  common.legend = TRUE, legend = "bottom"
)

# ==============================================================================
# END SCRIPT
# ==============================================================================
