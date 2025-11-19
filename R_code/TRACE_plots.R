##Model Output Verification Code
#Ferraro and Corradini et al., 2026


library(ggplot2)
library(dplyr)
library(cowplot)


patagonia_palette <- c(
  "#1B264F", "#2A628F", "#1E7A8A", "#468FAF",
  "#8AB17D", "#E9C46A", "#F4A261", "#E76F51",
  "#8E7DBE", "#5A3E85"
)

setwd("/Users/kristyferraro/My Drive/Scholarship/Yale/Projects/Alps Nutrient Translocation/Data")


#-----Mean Body N -----


#-----.Migrant and resident runs -----
migrant_and_resident_deer_files <- list.files(pattern = "Migrate_and_Resident_Deer.*\\.csv$", full.names = TRUE)

# Create new names just for internal use
new_names <- paste0("run_", seq_along(migrant_and_resident_deer_files))

# Read in the files and assign their new "run" names
migrant_and_resident_deer_data <- lapply(seq_along(migrant_and_resident_deer_files), function(i) {
  df <- read.csv(migrant_and_resident_deer_files[i], skip = 15)
  df$source_file <- new_names[i]  # assign new logical name inside data
  return(df)
})

# Combine all into one data frame
migrant_and_resident_deer_data <- dplyr::bind_rows(migrant_and_resident_deer_data)

#Main plot 
migrant_and_resident_deer_main_plot <- ggplot(
  migrant_and_resident_deer_data,
  aes(x = X.step., y = mean..body.n..of.turtles, color = source_file)
) +
  geom_line(size = 1.1) +
  scale_color_manual(values = patagonia_palette) +
  geom_rect(
    xmin = 8270, xmax = 8290, ymin = -Inf, ymax = Inf,
    inherit.aes = FALSE, fill = "grey85", alpha = 0.25, color = NA
  ) +
  labs(
    title = "Migrant and Resident 300 Deer",
    x = "Simulation Time Step",
    y = "Mean Body Nitrogen",
    color = "Model Run"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  ) +
  expand_limits(y = c(
    max(migrant_and_resident_deer_data$mean..body.n..of.turtles, na.rm = TRUE) * 1.10
  ))


# Zoomed-in subset around timestep 5250
migrant_and_resident_deer_zoom_data <- migrant_and_resident_deer_data %>%
  filter(X.step. >= 5250 & X.step. <= 5251)

migrant_and_resident_deer_zoom_plot <- ggplot(
  migrant_and_resident_deer_zoom_data,
  aes(x = X.step., y = mean..body.n..of.turtles, color = source_file)
) +
  geom_line(size = 1.1) +
  scale_color_manual(values = patagonia_palette) +
  scale_x_continuous(
    breaks = range(migrant_and_resident_deer_zoom_data$X.step.)
  ) + 
  labs(
    title = "Time Step 5250",
    x = NULL,
    y = NULL,
    color = "Model Run"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85")
  )

# Combine plots
migrant_and_resident_deer_final_plot <- ggdraw() +
  draw_plot(migrant_and_resident_deer_main_plot) +
  draw_plot(migrant_and_resident_deer_zoom_plot, x = 0.44, y = 0.61, width = 0.4, height = 0.30)

# Display the final figure 
print(migrant_and_resident_deer_final_plot)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#-----.Migrant 150 runs -----

migrant_150_files <- list.files(pattern = "Migration_150_Deer_.*\\.csv$", full.names = TRUE)

# Create new names just for internal use
new_names <- paste0("run_", seq_along(migrant_150_files))

# Read in the files and assign their new "run" names
migrant_150_data <- lapply(seq_along(migrant_150_files), function(i) {
  df <- read.csv(migrant_150_files[i], skip = 15)
  df$source_file <- new_names[i]  # assign new logical name inside data
  return(df)
})

# Combine all into one data frame
migrant_150_data <- dplyr::bind_rows(migrant_150_data)

#Main plot 
migrant_150_main_plot <- ggplot(
  migrant_150_data,
  aes(x = X.step., y = mean..body.n..of.turtles, color = source_file)
) +
  geom_line(size = 1.1) +
  scale_color_manual(values = patagonia_palette) +
  geom_rect(
    xmin = 8270, xmax = 8290, ymin = -Inf, ymax = Inf,
    inherit.aes = FALSE, fill = "grey85", alpha = 0.25, color = NA
  ) +
  labs(
    title = "Migrant 150 Deer",
    x = "Simulation Time Step",
    y = "Mean Body Nitrogen",
    color = "Model Run"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  ) +
  expand_limits(y = c(
    max(migrant_150_data$mean..body.n..of.turtles, na.rm = TRUE) * 1.10
  ))



# Zoomed-in subset around timestep 5250
migrant_150_zoom_data <- migrant_150_data %>%
  filter(X.step. >= 5250 & X.step. <= 5251)

migrant_150_zoom_plot <- ggplot(
  migrant_150_zoom_data,
  aes(x = X.step., y = mean..body.n..of.turtles, color = source_file)
) +
  geom_line(size = 1.1) +
    scale_x_continuous(
    breaks = range(migrant_and_resident_deer_zoom_data$X.step.)
  ) + 
  scale_color_manual(values = patagonia_palette) +
  labs(
    title = "Time Step 5250",
    x = NULL,
    y = NULL,
    color = "Model Run"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85")
  )

# Combine plots
migrant_150_final_plot <- ggdraw() +
  draw_plot(migrant_150_main_plot) +
  draw_plot(migrant_150_zoom_plot, x = 0.44, y = 0.61, width = 0.4, height = 0.30)

# Display the final figure 
print(migrant_150_final_plot)



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#-----.Resident 150 runs -----

resident_150_files <- list.files(pattern = "Resident_150_Deer_.*\\.csv$", full.names = TRUE)

# Create new names just for internal use
new_names <- paste0("run_", seq_along(resident_150_files))

# Read in the files and assign their new "run" names
resident_150_data <- lapply(seq_along(resident_150_files), function(i) {
  df <- read.csv(resident_150_files[i], skip = 15)
  df$source_file <- new_names[i]  # assign new logical name inside data
  return(df)
})

# Combine all into one data frame
resident_150_data <- dplyr::bind_rows(resident_150_data)

#Main plot 
resident_150_main_plot <- ggplot(
  resident_150_data,
  aes(x = X.step., y = mean..body.n..of.turtles, color = source_file)
) +
  geom_line(size = 1.1) +
  scale_color_manual(values = patagonia_palette) +
  geom_rect(
    xmin = 8270, xmax = 8290, ymin = -Inf, ymax = Inf,
    inherit.aes = FALSE, fill = "grey85", alpha = 0.25, color = NA
  ) +
  labs(
    title = "Resident 150 Deer",
    x = "Simulation Time Step",
    y = "Mean Body Nitrogen",
    color = "Model Run"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  ) +
  expand_limits(y = c(
     max(resident_150_data$mean..body.n..of.turtles, na.rm = TRUE) * 1.10
  ))


# Zoomed-in subset around timestep 5250
resident_150_zoom_data <- resident_150_data %>%
  filter(X.step. >= 5250 & X.step. <= 5251)

resident_150_zoom_plot <- ggplot(
  resident_150_zoom_data,
  aes(x = X.step., y = mean..body.n..of.turtles, color = source_file)
) +
  geom_line(size = 1.1) +
  scale_color_manual(values = patagonia_palette) +
    scale_x_continuous(
    breaks = range(migrant_and_resident_deer_zoom_data$X.step.)
  ) + 
  labs(
    title = "Time Step 5250",
    x = NULL,
    y = NULL,
    color = "Model Run"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85")
  )

# Combine plots
resident_150_final_plot <- ggdraw() +
  draw_plot(resident_150_main_plot) +
  draw_plot(resident_150_zoom_plot, x = 0.44, y = 0.61, width = 0.4, height = 0.30)

# Display the final figure 
print(resident_150_final_plot)



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#-----.Resident 300 runs -----

resident_300_files <- list.files(pattern = "Resident_300_Deer_.*\\.csv$", full.names = TRUE)

# Create new names just for internal use
new_names <- paste0("run_", seq_along(resident_300_files))

# Read in the files and assign their new "run" names
resident_300_data <- lapply(seq_along(resident_300_files), function(i) {
  df <- read.csv(resident_300_files[i], skip = 15)
  df$source_file <- new_names[i]  # assign new logical name inside data
  return(df)
})

# Combine all into one data frame
resident_300_data <- dplyr::bind_rows(resident_300_data)

#Main plot 
resident_300_main_plot <- ggplot(
  resident_300_data,
  aes(x = X.step., y = mean..body.n..of.turtles, color = source_file)
) +
  geom_line(size = 1.1) +
  scale_color_manual(values = patagonia_palette) +
  geom_rect(
    xmin = 8270, xmax = 8290, ymin = -Inf, ymax = Inf,
    inherit.aes = FALSE, fill = "grey85", alpha = 0.25, color = NA
  ) +
  labs(
    title = "Resident 300 Deer",
    x = "Simulation Time Step",
    y = "Mean Body Nitrogen",
    color = "Model Run"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  ) +
  expand_limits(y = c(
  max(resident_300_data$mean..body.n..of.turtles, na.rm = TRUE) * 1.10
   
  ))

# Zoomed-in subset around timestep 5250
resident_300_zoom_data <- resident_300_data %>%
  filter(X.step. >= 5250 & X.step. <= 5251)

resident_300_zoom_plot <- ggplot(
  resident_300_zoom_data,
  aes(x = X.step., y = mean..body.n..of.turtles, color = source_file)
) +
  geom_line(size = 1.1) +
  scale_color_manual(values = patagonia_palette) +
    scale_x_continuous(
    breaks = range(migrant_and_resident_deer_zoom_data$X.step.)
  ) + 
  labs(
    title = "Time Step 5250",
    x = NULL,
    y = NULL,
    color = "Model Run"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85")
  )

# Combine plots
resident_300_final_plot <- ggdraw() +
  draw_plot(resident_150_main_plot) +
  draw_plot(resident_150_zoom_plot, x = 0.44, y = 0.61, width = 0.4, height = 0.30)

# Display the final figure 
print(resident_300_final_plot)




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

#----------.Create a shared legend from one main plot ----------
legend_plot <- migrant_and_resident_deer_main_plot + theme(legend.position = "bottom")
shared_legend <- cowplot::get_legend(legend_plot)

## ---------- Remove legends from each main plot used inside the finals
# We need legend-free versions of the *main* plots to recompose legend-free finals
migrant_and_resident_deer_main_noleg <- migrant_and_resident_deer_main_plot + theme(legend.position = "none")
migrant_150_main_noleg             <- migrant_150_main_plot             + theme(legend.position = "none")
resident_150_main_noleg              <- resident_150_main_plot              + theme(legend.position = "none")
resident_300_main_noleg              <- resident_300_main_plot              + theme(legend.position = "none")

## ---------- Rebuild the four final plots with no legends 
migrant_and_resident_deer_final_noleg <- ggdraw() +
  draw_plot(migrant_and_resident_deer_main_noleg) +
  draw_plot(migrant_and_resident_deer_zoom_plot, x = 0.55, y = 0.55, width = 0.4, height = 0.35)

migrant_150_final_noleg <- ggdraw() +
  draw_plot(migrant_150_main_noleg) +
  draw_plot(migrant_150_zoom_plot, x = 0.55, y = 0.55, width = 0.4, height = 0.35)

resident_150_final_noleg <- ggdraw() +
  draw_plot(resident_150_main_noleg) +
  draw_plot(resident_150_zoom_plot, x = 0.55, y = 0.55, width = 0.4, height = 0.35)

resident_300_final_noleg <- ggdraw() +
  draw_plot(resident_300_main_noleg) +
  draw_plot(resident_300_zoom_plot, x = 0.55, y = 0.55, width = 0.4, height = 0.35)

## ---------- Arrange the four finals in a 2x2 grid 
grid_2x2 <- plot_grid(
  migrant_and_resident_deer_final_noleg, migrant_150_final_noleg,
  resident_150_final_noleg,              resident_300_final_noleg,
  labels = c("A", "B", "C", "D"),
  label_size = 12,
  ncol = 2, align = "hv"
)

## ---------- Add the shared legend below the grid 
final_4up_with_legend <- plot_grid(
  grid_2x2,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.10)
)

## ---------- Show it 
print(final_4up_with_legend)

quartz()
final_4up_with_legend









#-----Daily N Consumed -----

#-----.Migrant and resident runs -----
migrant_and_resident_deer_data_24 <- migrant_and_resident_deer_data %>%
  filter(Hour == 23)

migrant_and_resident_deer_mean_daily_n <- ggplot(
  migrant_and_resident_deer_data_24,
  aes(x = X.step., y =  mean..daily.n..of.turtles, color = source_file)
) +
  geom_line(size = 1.1) +
  scale_color_manual(values = patagonia_palette) +
  geom_rect(
    xmin = 8270, xmax = 8290, ymin = -Inf, ymax = Inf,
    inherit.aes = FALSE, fill = "grey85", alpha = 0.25, color = NA
  ) +
  labs(
    title = "Migrate and Resident 300 Deer",
    x = "Simulation Time Step",
    y = "Daily Nitrogen Consumed",
    color = "Model Run"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  ) 
 

#-----.Migrant 150 runs -----
migrant_150_data_24 <- migrant_150_data %>%
  filter(Hour == 23)

migrant_150_data_mean_daily_n <- ggplot(
  migrant_150_data_24,
  aes(x = X.step., y =  mean..daily.n..of.turtles, color = source_file)
) +
  geom_line(size = 1.1) +
  scale_color_manual(values = patagonia_palette) +
  geom_rect(
    xmin = 8270, xmax = 8290, ymin = -Inf, ymax = Inf,
    inherit.aes = FALSE, fill = "grey85", alpha = 0.25, color = NA
  ) +
  labs(
    title = "Migrant 150 Deer",
    x = "Simulation Time Step",
    y = "Daily Nitrogen Consumed",
    color = "Model Run"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  ) 
 


#-----.resident 150 runs -----
resident_150_data_24 <- resident_150_data %>%
  filter(Hour == 23)

resident_150_data_mean_daily_n <- ggplot(
  resident_150_data_24,
  aes(x = X.step., y =  mean..daily.n..of.turtles, color = source_file)
) +
  geom_line(size = 1.1) +
  scale_color_manual(values = patagonia_palette) +
  geom_rect(
    xmin = 8270, xmax = 8290, ymin = -Inf, ymax = Inf,
    inherit.aes = FALSE, fill = "grey85", alpha = 0.25, color = NA
  ) +
  labs(
    title = "Resident 150 Deer",
    x = "Simulation Time Step",
    y = "Daily Nitrogen Consumed",
    color = "Model Run"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  ) 
 

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#-----.resident 300 runs ----
resident_300_data_24 <- resident_300_data %>%
  filter(Hour == 23)

resident_300_data_mean_daily_n <- ggplot(
  resident_300_data_24,
  aes(x = X.step., y =  mean..daily.n..of.turtles, color = source_file)
) +
  geom_line(size = 1.1) +
  scale_color_manual(values = patagonia_palette) +
  geom_rect(
    xmin = 8270, xmax = 8290, ymin = -Inf, ymax = Inf,
    inherit.aes = FALSE, fill = "grey85", alpha = 0.25, color = NA
  ) +
  labs(
    title = "Resident 300 Deer",
    x = "Simulation Time Step",
    y = "Daily Nitrogen Consumed",
    color = "Model Run"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  ) 


#----------.Create a shared legend from one main plot ----------
dailyn_legend_plot <- resident_300_data_mean_daily_n + theme(legend.position = "bottom")
dailyn_shared_legend <- cowplot::get_legend(dailyn_legend_plot)

## ---------- Remove legends from each main plot used inside the finals
# We need legend-free versions of the *main* plots to recompose legend-free finals
migrant_and_resident_deer_mean_daily_n <- migrant_and_resident_deer_mean_daily_n + theme(legend.position = "none")
migrant_150_data_mean_daily_n       <- migrant_150_data_mean_daily_n             + theme(legend.position = "none")
resident_150_data_mean_daily_n        <- resident_150_data_mean_daily_n              + theme(legend.position = "none")
resident_300_data_mean_daily_n        <- resident_300_data_mean_daily_n              + theme(legend.position = "none")

## ---------- Arrange the four finals in a 2x2 grid 
grid_2x2_dailyn <- plot_grid(
  migrant_and_resident_deer_mean_daily_n, migrant_150_data_mean_daily_n,
  resident_150_data_mean_daily_n,         resident_300_data_mean_daily_n,
  labels = c("A", "B", "C", "D"),
  label_size = 12,
  ncol = 2, align = "hv"
)

## ---------- Add the shared legend below the grid 
dailyn_with_legend <- plot_grid(
  grid_2x2_dailyn,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.10)
)

## ---------- Show it 
print(dailyn_with_legend)

quartz()
dailyn_with_legend
