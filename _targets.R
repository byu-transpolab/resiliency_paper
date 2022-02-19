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
source("R/project_calculator.R")

# this project requires cube output files that are too large to export and store.
# the following functions will create the files and store them the proper location
# copyoutoutfiles()
# makeproject_data()
# makeproject_costs()


# mode choice cost coefficient in c/mile
mc_cost_coef <- -0.0016

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf", "readxl", "leaflet"))

# End this file with a list of target objects.
list(
  # links with project id
  tar_target(links_file, "data/links.geojson", format = "file"),
  tar_target(links, st_read(links_file)),
  
  # taz polygons
  tar_target(taz_file, "data/ustm_taz.geojson", format = "file"),
  tar_target(taz, st_read(taz_file)),
  
  # costs for all scenarios
  tar_target(prod_rds, "data/productions.rds", format = "file"),
  tar_target(prod, read_rds(prod_rds)),
  tar_target(lsum_rds, "data/logsums.rds", format = "file"),
  tar_target(logsums, read_rds(lsum_rds)),
  tar_target(deltas, calculate_deltas(prod, logsums)),
  tar_target(costs, calculate_costs(deltas, mc_cost_coef)),
  
  # costs for one scenario 
  tar_target(taz_deltas, calculate_taz_deltas(prod, logsums, "ROAD50")),
  tar_target(ls_scenarios, calculate_scenario_ls(
    taz_deltas,  taz %>% filter(CO_NAME == "TOOELE") %>% pull(TAZID), mc_cost_coef)),
  tar_target(taz_timecosts_file, "data/road_50_costs.dbf", format = "file"),
  tar_target(taz_timecosts, foreign::read.dbf(taz_timecosts_file)),
  tar_target(tm_scenarios, caclulate_taz_timecosts(
    taz_timecosts, taz %>% filter(CO_NAME == "TOOELE") %>% pull(TAZID))),
  tar_target(zonal_mapdata, make_zonal_mapdata(taz_deltas, taz)),
  
  tar_target(scenario_comparison, compare_scenarios(ls_scenarios, tm_scenarios)),
  
  
  
  # project map
  tar_target(mapdata, make_mapdata(links, costs)),
  
  
  # utility coefficients
  tar_target(mc_constants_file, "data/MC_Constants.csv", format = "file"),
  tar_target(mc_coefficients_file, "data/MC_Coefficients.csv", format = "file"),
  tar_target(dc_coefficients_file, "data/DC_Parameters.csv", format = "file"),
  tar_target(mc_constants, read_csv(mc_constants_file, col_types = "cnnn")),
  tar_target(mc_coefficients, read_csv(mc_coefficients_file, col_types = "cnnn")),
  tar_target(dc_coefficients, read_csv(dc_coefficients_file, col_types = "cnnn")),
  tar_target(coefficient_table, make_coefficient_table(mc_constants, mc_coefficients, 
                                                       dc_coefficients))
  
)
