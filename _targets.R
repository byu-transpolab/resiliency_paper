library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/plots_tables.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf", "readxl"))

# End this file with a list of target objects.
list(
  tar_target(links, st_read("data/Risks Links Shapefile/risk_links_network.shp")),
  tar_target(projects, read_xlsx("images/logsumrank.xlsx")),
  tar_target(map, make_map(links, projects)),
  
  tar_target(mc_constants_file, "data/MC_Constants.csv", format = "file"),
  tar_target(mc_coefficients_file, "data/MC_Coefficients.csv", format = "file"),
  tar_target(dc_coefficients_file, "data/DC_Parameters.csv", format = "file"),
  tar_target(mc_constants, read_csv(mc_constants_file, col_types = "cnnn")),
  tar_target(mc_coefficients, read_csv(mc_coefficients_file, col_types = "cnnn")),
  tar_target(dc_coefficients, read_csv(dc_coefficients_file, col_types = "cnnn")),
  tar_target(coefficient_table, make_coefficient_table(mc_constants, mc_coefficients, 
                                                       dc_coefficients))
  
)
