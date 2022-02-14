#' Calculate production-weighted logsum difference
#' 
#' @param prod
#' @param logsums
#' 
calculate_deltas <- function(prod, logsums){
  
  logsums %>%
    left_join(prod, by = c("purpose", "TAZ")) %>%
    mutate( total = productions * logsum ) %>%
    group_by(purpose, scenario) %>%
    summarise(total = sum(total)) %>%
    spread(scenario, total, fill = 0) %>%
    gather(scenario, total, -purpose, -BASE) %>%
    mutate(
      delta = total - BASE,
      pct_delta = delta / BASE * 100
    ) 
  
}

#' Calculate production-weighted logsum difference for one scenario by TAZ
#' 
#' @param prod
#' @param logsums
#' @param this_scenario Which scenario to build for
#' 
calculate_taz_deltas <- function(prod, logsums, this_scenario = "ROAD50", taz_ids){
  
  logsums %>%
    filter(scenario %in% c("BASE", this_scenario)) %>%
    spread(scenario, logsum) %>%
    rename(alt = !!this_scenario) %>%
    mutate(delta = alt - BASE) %>%
    left_join(prod, by = c("purpose", "TAZ")) %>%
    mutate( delta_total = productions * delta ) 
  
}


calculate_scenario_ls <- function(taz_deltas, taz_ids, mc_cost_coef){
  taz_deltas %>%
    mutate(inregion = TAZ %in% taz_ids) %>%
    group_by(purpose, inregion) %>% 
    summarise(delta_total = sum(delta_total)) %>% 
    mutate(cost = -1 * delta_total / mc_cost_coef * .01)
}


#' Calculate financial costs of closing links
#' 
#'  @param deltas Tibble with change in logsum by purpose and TAZ
#'  @param mc_cost_coefficient cost coefficient from mode choice model
#' 
calculate_costs <- function(deltas, mc_cost_coef){
  deltas %>%
    group_by(scenario) %>%
    summarise(total_delta = sum(delta)) %>%
    mutate(
      value = total_delta / mc_cost_coef
    )
}



#' Create project logsums database
#' 
#' @details NOT IN TARGETS STREAM. output file committed to git
#' 
#' 
#' 
#' 
#' 

copyoutputfiles <- function(){
  folder<-"C:/projects/ustm_resiliency/Base"
  new.folder <- "C:/projects/outputs_resiliency"
  files <- list.files(folder, full.names = TRUE, recursive = TRUE, pattern = "01_ROWSUMS")
  files_dn <- list.files(folder, full.names = FALSE, recursive = TRUE, pattern = "01_ROWSUMS")
  foldernamesinitial <- toupper(substr(files_dn, 1, 6))
  foldernames <- replace(foldernamesinitial, 1, "BASE")
  file_names_new <- paste0(foldernames, "_ROWSUMS.DBF")
  
  files_new <- paste0(new.folder, "/", file_names_new) 
  file.copy(files, files_new, overwrite = TRUE)
  

  files2 <- list.files(folder, full.names = TRUE, recursive = TRUE, pattern = "01_TRAVELTIME_COSTS.DBF")
  files_dn2 <- list.files(folder, full.names = FALSE, recursive = TRUE, pattern = "01_TRAVELTIME_COSTS.DBF")
  foldernamesinitial2 <- toupper(substr(files_dn2, 1, 6))
  foldernames2 <- replace(foldernamesinitial2, 1, "BASE")
  file_names_new2 <- paste0(foldernames2, "_TRAVELTIME_COSTS.DBF")
  
  files_new2 <- paste0(new.folder, "/", file_names_new2) 
  file.copy(files2, files_new2, overwrite = TRUE)
  
}


makeproject_data <- function(){
  
  scenarios_folder <- "C:/projects/outputs_resiliency"
  hh_folder <- "C:/projects/ustm_resiliency/Inputs"
  
  # read hh productions by purpose 
  prod <- foreign::read.dbf(file.path(hh_folder, "HH_PROD.DBF")) %>%
    as_tibble() %>%
    gather(purpose, productions, -TAZ) %>%
    mutate(purpose = gsub("P", "", purpose))
  
  write_rds(prod, "data/productions.rds")
  
  # read scenario_outputs
  outputs <- dir(scenarios_folder, pattern = "ROWSUMS.DBF")
  
  logsums <- lapply(outputs, function(scenario){
    # get scenario name
    scenario_name <- gsub("_ROWSUMS.DBF", "", scenario)
    print(scenario_name)
    logsums <- foreign::read.dbf(file.path(scenarios_folder, scenario))  %>%
      as_tibble() %>%
      gather(purpose, logsum, -TAZ) %>%
      mutate(
        scenario = scenario_name,
        purpose = gsub("LN", "", purpose)
      ) %>%
      filter(purpose %in% c("HBW", "HBO", "NHB")) 
  }) %>%
    bind_rows()
  
  write_rds(logsums, "data/logsums.rds")
  
}


makeproject_costs <- function(){
  
  scenarios_folder <- "C:/projects/outputs_resiliency"
  
  # read scenario_outputs
  outputs <- dir(scenarios_folder, pattern = "COSTS.DBF")
  
  #empty DBF
  #empty <- foreign::read.dbf("data/blank.DBF")
  
  costs <- lapply(outputs[1:10], function(scenario){
    # get scenario name
    scenario_name <- gsub("_TRAVELTIME_COSTS.DBF", "", scenario)
    print(scenario_name)
    costs <- foreign::read.dbf(file.path(scenarios_folder, scenario))  %>%
#      group_by(M) %>%
#      summarise(Cost = sum(V1)/100) %>%
#      filter(M != 9) %>%
      mutate(M = case_when(M == 1 ~ "IIF",
                           M == 2 ~ "XXF",
                           M == 3 ~ "IXF",
                           M == 4 ~ "HBW",
                           M == 5 ~ "HBO",
                           M == 6 ~ "NHB",
                           M == 7 ~ "REC",
                           M == 8 ~ "XXP",
                           M == 9 ~ "TimeDiff")) %>%
      mutate(scenario = scenario_name)
}) %>%
    bind_rows()
  
  write_rds(costs, "data/traveltimecosts.rds")
  
}
