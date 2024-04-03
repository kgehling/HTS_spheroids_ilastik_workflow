# function to process the ilastik software output tables from multiple images into one merged data table

process_ilastik_output <- function(path, image_resolution = "4x", predicted_class = NULL) {
  # load packs
  library(tidyverse)
  library(glue)
 
  
  # pixel_factor function to calculate pixel factor based on image resolution of Cytation5 reader
  
  pixel_factor <- function(image_resolution) {
    if (image_resolution == "4x") {
      image_width_microns <- 1973
      pixel_width <- 1224
      image_height_microns <- 1457
      pixel_height <- 904
    } else if (image_resolution == "2x") {
      image_width_microns <- 3947
      pixel_width <- 1224
      image_height_microns <- 2915
      pixel_height <- 904
    } else {
      stop("Unsupported image resolution. Supported resolutions are '4x' and '2x'.")
    }
    factor_w <- image_width_microns / pixel_width
    factor_h <- image_height_microns / pixel_height
    factor <- mean(factor_h, factor_w)
    return(factor)
  }
  
  # read the list of files and create a tibble
  files <- list.files(path, pattern = 'table.csv') %>%
    as_tibble() %>%
    rename(file = value) # dataframe extension: new_name = old_name 
  # when converted to tibble, the values are automatically saved with # value
  
  # specify new column names, they depend on the image's Name (Cytation image IDs in my case similar to: H4_02_1_1Z4_Bright Field_001)
  new_col_names <- c(
    'file', 'well','unknown_number','plate_number','zstack','channel','time_stamp',
    'table')
  
  # split the file names and unnest them, change column names for the new ones
  file_metadata <- files %>%
    mutate(col_info = str_split(file, pattern = '_')) %>%
    unnest_wider(col_info, names_sep = "Name") %>% # preserves the rows, but changes the columns.
    rename_with(~new_col_names) 
  
  # fix error:
  file_metadata$plate_number <- file_metadata$plate_number %>% as.numeric()
  
  # loop to read each file and add the file name, it will help to join the metadata
  merge_file <- file_metadata %>%
    rowwise() %>%
    mutate(data = list(read_csv(glue('{path}/{file}')))) %>%
    unnest(data) %>%
    mutate(file = file, `User Label` = as.character(`User Label`))
  
  # Optional: filter for Predicted Class (ilastik object class names)
  if (!is.null(predicted_class)) {
    merge_file <- merge_file %>%
      filter(`Predicted Class` == predicted_class)
  }
  
  # calculate sizes from pixel values
  merged_tables <- merge_file %>%
    separate(col = well, into = c("Row", "Col"), sep = 1, remove = FALSE, convert = TRUE) %>%
    mutate(Size_in_µm2 = `Size in pixels` * pixel_factor(image_resolution)^2,
           Diameter_in_µm = Diameter * pixel_factor(image_resolution),
           Size_mm2 = (`Size in pixels` * pixel_factor(image_resolution)^2) / 1e6,
           Diameter_mm = (Diameter * pixel_factor(image_resolution)) / 1e3)
  
  # save the merged table in the givin import path directory
  write_csv(merged_tables, glue('{path}/merged_tables.csv'))
  
  # print the output path to be sure where to find data
  print(path)
  
  return(merged_tables)
}

# example 4x-images:
process_ilastik_output(path = "/Volumes/FC spheroid/Kristin/24_2_FUdR_enhanced/excess_growth/", 
                       image_resolution = "4x",
                       predicted_class = "Spheroid")

# example 2x-images:
process_ilastik_output(path = "/Volumes/FC spheroid/Kristin/24_2_FUdR_enhanced/cells_phen/", 
                       image_resolution = "2x")
