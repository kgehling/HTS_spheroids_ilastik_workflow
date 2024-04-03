# High-throughput-screening approach with spheroids under various conditions using the ilastik software to process the imaging data acquired during the screening.

Steps taken beforehand: 
- ilastik pixel classification workflow
- ilastik object classification workflow exporting the data as csv files without changing the default export suffix (.e.g., file still ends with table.csv)

# 1. Step: process_ilastik_output_function
- helps to merge the output tables into one large data frame by automating tasks such as data loading, metadata extraction, filtering, measurement conversion, and saving

# 2. Step: Import_metadata
