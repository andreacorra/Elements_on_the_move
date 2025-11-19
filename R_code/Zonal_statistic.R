# ==============================================================================
# Element on the move
# Author: Ferraro, Corradini, et al.
# Publication year: 2025
# Description: Zonal statistic analysis of nitrogen deposition patterns across 
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
library(mapview)
library(spdep)
library(gstat)
library(tmap)
library(scales)
library(geosphere)
library(rlang) # for !!sym()

# dir <- "path/to/your/data/directory/"  # Update this path
dir <- "C:/Users/corradinia/R/Elements_on_the_move"
utm_crs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"

## Nitrogen rasters
rasters <- list(
  "M+R_300" = readRDS(file.path(dir, "data/migrate_and_resident_nitrogen.rds")),
  "M_150"   = readRDS(file.path(dir, "data/migration_150_nitrogen.rds")),
  "R_150"   = readRDS(file.path(dir, "data/resident_150_nitrogen.rds")),
  "R_300"   = readRDS(file.path(dir, "data/resident_300_nitrogen.rds"))
)

## Environmental rasters
spatial_covariates <- readRDS(file.path(dir, "data/spatial_covariates.rds"))
dem   <- spatial_covariates$elevation
slope <- spatial_covariates$slope
tcd   <- spatial_covariates$tcd

### Round environmental rasters
values(dem)   <- round(values(dem), digits = -1)
values(slope) <- round(values(slope), digits = 0)
values(tcd)   <- round(values(tcd), digits = 0)
tcd[tcd < 0]  <- 0
tcd[tcd > 100] <- 100


# Function to calculate zonal summaries -----------------------------------
calculate_zonal_summary <- function(raster_stack, env_raster, env_name, type_label) {
  bind_rows(lapply(seq_len(nlyr(raster_stack)), function(i) {
    z <- zonal(raster_stack[[i]], env_raster, fun = "sum", na.rm = TRUE)
    names(z)[2] <- "net_nitrogen"
    z
  })) %>%
    mutate(type = type_label) %>%
    group_by(!!sym(env_name), type) %>%
    summarise(
      mean_nitrogen = mean(net_nitrogen, na.rm = TRUE),
      sd            = sd(net_nitrogen, na.rm = TRUE),
      n             = n(),
      se            = sd / sqrt(n),
      ci_lower      = mean_nitrogen - qt(0.975, df = n - 1) * se,
      ci_upper      = mean_nitrogen + qt(0.975, df = n - 1) * se,
      .groups = "drop"
    ) %>%
    mutate(facet_row = case_when(
      type %in% c("M_150", "R_150") ~ "Top",
      type %in% c("M+R_300", "R_300") ~ "Bottom"
    ))
}


# Zonal summaries for all environmental variables -------------------------
summary_list <- list(
  elevation = list(env = dem,   env_name = "elevation"),
  slope     = list(env = slope, env_name = "slope"),
  tcd       = list(env = tcd,   env_name = "tcd")
)

zonal_summaries <- list()
for (var in names(summary_list)) {
  env <- summary_list[[var]]$env
  env_name <- summary_list[[var]]$env_name
  
  zonal_summaries[[var]] <- bind_rows(lapply(names(rasters), function(type_label) {
    calculate_zonal_summary(rasters[[type_label]], env, env_name, type_label)
  }))
  
  ### Ensure correct factor order for plotting -------
  zonal_summaries[[var]] <- zonal_summaries[[var]] %>%
    mutate(type = factor(type, levels = c("M_150", "R_150", "M+R_300", "R_300")))
}


# Plotting function -------------------------------------------------------
movement_colors <- c(
  "M_150"   = "#7e549e",
  "R_150"   = "#ece6ce",
  "M+R_300" = "#ed6b5b",
  "R_300"   = "#f9ac68"
)

plot_zonal <- function(summary_df, x_var, y_var = "mean_nitrogen", fill_var = "type",
                       x_label = NULL, y_label = "Net nitrogen (gN)",
                       x_breaks = NULL, x_limits = NULL) {
  
  p <- ggplot(summary_df, aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = 0.9)) +
    labs(x = x_label, y = y_label, fill = "Type") +
    scale_fill_manual(values = movement_colors) +
    theme_minimal() +
    theme(strip.text = element_blank(), legend.position = "top") +
    facet_wrap(~ type, nrow = 2, scales = "fixed")
  
  if (!is.null(x_breaks)) p <- p + scale_x_continuous(breaks = x_breaks)
  if (!is.null(x_limits)) p <- p + coord_cartesian(xlim = x_limits)
  
  return(p)
}


# Generate plots  ---------------------------------------------------------
elev_plot <- plot_zonal( # Figure S2
  zonal_summaries$elevation, x_var = "elevation",
  x_label = "Elevation (m)", x_breaks = seq(700, 2500, 300),
  x_limits = c(700, 2250)
)

slope_plot <- plot_zonal( # Figure S1
  zonal_summaries$slope, x_var = "slope",
  x_label = "Slope (°)", x_breaks = seq(0, 60, 5),
  x_limits = c(0, 60)
)

tcd_plot <- plot_zonal( # Figure S3
  zonal_summaries$tcd, x_var = "tcd",
  x_label = "Tree cover density (%)", x_breaks = seq(0, 100, 10),
  x_limits = c(0, 100)
)

# Plots are returned as objects: elev_plot, slope_plot, tcd_plot
# Use ggsave() to save manually if needed


# Zonal statistics --------------------------------------------------------
tcd_thresh <- 65    # % tree cover for open to forest
elev_thresh <- 1200 # m a.s.l.
slope_thresh <- 25  # degrees


# Function to compute mean ± SD for a threshold ----------------
compute_sum_threshold <- function(raster_stack, env_raster, threshold) {
  sums <- sapply(seq_len(nlyr(raster_stack)), function(i) {
    vals <- raster_stack[[i]][env_raster >= threshold]
    sum(vals, na.rm = TRUE)
  })
  c(mean = mean(sums, na.rm = TRUE), sd = sd(sums, na.rm = TRUE))
}

# Compute Table 1
table1 <- lapply(names(rasters), function(type_label) {
  
  # Open to forest: tcd >= 65
  open_forest <- compute_sum_threshold(rasters[[type_label]], tcd, tcd_thresh)
  
  # Elevation > 1200 m
  elev_high <- compute_sum_threshold(rasters[[type_label]], dem, elev_thresh)
  
  # Slope > 25°
  slope_steep <- compute_sum_threshold(rasters[[type_label]], slope, slope_thresh)
  
  data.frame(
    Scenario = type_label,
    NetN_Open_Forest = paste0(format(round(open_forest["mean"], 0), big.mark = ","), 
                              " ± ", format(round(open_forest["sd"], 0), big.mark = ",")),
    NetN_Above_1200  = paste0(format(round(elev_high["mean"], 0), big.mark = ","), 
                              " ± ", format(round(elev_high["sd"], 0), big.mark = ",")),
    NetN_Above_25Slope = paste0(format(round(slope_steep["mean"], 0), big.mark = ","), 
                                " ± ", format(round(slope_steep["sd"], 0), big.mark = ","))
  )
}) %>% bind_rows()

# Data tidying
table1$Scenario <- factor(table1$Scenario, levels = c("M_150", "R_150", "M+R_300", "R_300"))
table1 <- table1 %>% arrange(Scenario)

table1

# ==============================================================================
# END SCRIPT
# ==============================================================================
