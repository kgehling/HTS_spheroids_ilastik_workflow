# High-throughput-screening approach with spheroids under various conditions using the ilastik software to process the imaging data acquired during the screening.

Steps taken beforehand: 
- ilastik pixel classification workflow
- ilastik object classification workflow exporting the data as csv files without changing the default export suffix (.e.g., file still ends with table.csv)

# 1. Step: process_ilastik_output_function
- helps to merge the output tables into one large data frame by automating tasks such as data loading, metadata extraction, filtering, measurement conversion, and saving
- function is adapted for images that were taken with the Cytation5 multi-mode reader


DETAILS:

1. Input Parameters: The function takes several input parameters:
- path: This is the path where the iLastik output files are located.
- image_resolution: This parameter specifies the resolution of the images used in ilastik, which can be either "4x" or "2x".
- predicted_class (optional): This parameter allows filtering the data based on a predicted class, typically used in machine learning scenarios.

3. Pixel Factor Calculation: It internally calculates a pixel factor based on the image resolution provided. The pixel factor is essential for converting pixel measurements to physical measurements (e.g., micrometers) and is different for each microscope! This script is adapted for the Cytation5 multimode reader with either a 2x or 4x magnification objective.

4. Data Processing:
- It loads and processes the output data files generated by ilastik.
- It combines the metadata from the file names with the data from the CSV files.
- Optionally, it filters the data based on a predicted class if specified.
- It performs additional calculations on the data, such as converting pixel measurements to micrometers and millimeters using the calculated pixel factor.

5. Output:
- The processed data is saved as a CSV file in the same directory where the input files are located.
- The function returns the processed data as a tibble (a tidyverse-friendly data frame), which can be further used for analysis or visualization.






# 2. Step: Import_metadata
