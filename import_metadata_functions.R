# script to integrate self-defined metadata into the merged_table utilizing the plater package
# used after process_ilastik_output_function and before final data visualization

library(tidyverse)
library(plater)

# Define functions ----

# function to preprocess data for each plate
preprocess_plate <- function(path, pattern_path, cell_type, identifier = NULL, predicted_class = NULL) {
  # Check Plater format
  check_plater_format(pattern_path)
  
  # Read metadata
  metadata <- read_plate(file = pattern_path, well_ids_column = "Plater_well_IDs", sep = ",")
  
  # Read merged table
  merged_tables <- read_csv(glue('{path}/merged_tables.csv'))
  
  # Select necessary columns
  df <- merged_tables %>%
    select(file, well, Row, Col, time_stamp, `Predicted Class`, 
           `Probability of Spheroid`, Size_mm2, Diameter_mm, Size_in_µm2, 
           Diameter_in_µm, object_id) 
  
  # Filter by predicted class if provided
  if (!is.null(predicted_class)) {
    df <- df %>% filter(`Predicted Class` == predicted_class)
  }
  
  # Add cell type and identifier columns
  df <- df %>%
    mutate(cell_type = as.factor(cell_type),
           plate_identifier = as.factor(identifier))
  
  # Merge with metadata
  df <- left_join(df, metadata, by = "well")
  
  return(df)
}

# Function to convert the timestamp of the images into hours
convert_time_stamp_to_hours <- function(df, interval) {
  # Extract the last 3 characters from time_stamp
  df$hours <- as.numeric(substr(df$time_stamp, nchar(df$time_stamp) - 2, nchar(df$time_stamp)))
  
  # Calculate hours since the start of the experiment
  df$hours <- ifelse(final_df$hours == 1, 1, (final_df$hours - 1) * interval)
  
  return(df)
}

# I. import the data using the functions ----

# a) EXAMPLE how to use the function with 4 plates: ----
# a.1. Define individual paths and plate information, example for 4 different plates that belong to the same experiment
plate_info <- list(
  list(
    path = "/Volumes/FC spheroid/Kristin/23_9/selected_z_001-007_p1/",
    pattern_path = "/Volumes/FC spheroid/Kristin/23_9/selected_z_001-007_p1/Experiment_design_pattern.csv",
    cell_type = "HepG2",
    identifier = "LPG_1"
  ),
  list(
    path = "/Volumes/FC spheroid/Kristin/23_9/selected_z_001-007_p2/",
    pattern_path = "/Volumes/FC spheroid/Kristin/23_9/selected_z_001-007_p2/Experiment_design_pattern.csv",
    cell_type = "HepG2",
    identifier = "LPG_1"
  ),
  list(
    path = "/Volumes/FC spheroid/Kristin/23_9/selected_z_001-007_p3/",
    pattern_path = "/Volumes/FC spheroid/Kristin/23_9/selected_z_001-007_p3/Experiment_design_pattern_p3.csv",
    cell_type = "HepG2",
    identifier = "LPG_2"
  ),
  list(
    path = "/Volumes/FC spheroid/Kristin/23_9/selected_z_001-007_p4/",
    pattern_path = "/Volumes/FC spheroid/Kristin/23_9/selected_z_001-007_p4/Experiment_design_pattern_p4.csv",
    cell_type = "HepG2",
    identifier = "LPG_2"
  )
)

# a.2. Preprocess data for each plate
dfs <- map(plate_info, ~ preprocess_plate(.x$path, .x$pattern_path, .x$cell_type, .x$identifier))

# a.3. Bind all dataframes together
final_df <- bind_rows(dfs)



# b) EXAMPLE how to use the functions for one plate: ----
# b.1. Define plate information
plate_info <- list(
  path = "/Volumes/FC spheroid/Kristin/23_9/selected_z_001-007_p1/",
  pattern_path = "/Volumes/FC spheroid/Kristin/23_9/selected_z_001-007_p1/Experiment_design_pattern.csv",
  cell_type = "HepG2",
  identifier = "LPG_1"
)

# b.2. Preprocess data for the one plate
final_df <- preprocess_plate(plate_info$path, plate_info$pattern_path, plate_info$cell_type, plate_info$identifier)



### II. CONTINUE here after any plate import ----
# convert the time_stamp into hours

# Assuming interval is 8 hours for this example
interval <- 8

# Apply the function to your dataframe
final_df <- convert_time_stamp_to_hours(df, interval)


# III. Save final_df as 'merged_conditions.csv' in a desired directory ----
write_csv(final_df, "/Volumes/FC spheroid/Kristin/23_9/merged_conditions.csv")

