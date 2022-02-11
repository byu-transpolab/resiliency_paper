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
    gather(scenario, total, -purpose, -base) %>%
    mutate(
      delta = total - base,
      pct_delta = delta / base * 100
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
    filter(scenario %in% c("base", this_scenario)) %>%
    spread(scenario, logsum) %>%
    rename(alt = !!this_scenario) %>%
    mutate(delta = alt - base) %>%
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

folder<-"C:/projects/ustm_resiliency/Base"
new.folder <- "C:/projects/outputs_resiliency"
files <- list.files(folder, full.names = TRUE, recursive = TRUE, pattern = "01_ROWSUMS")
files_dn <- list.files(folder, full.names = FALSE, recursive = TRUE, pattern = "01_ROWSUMS")
foldernamesinitial <- substr(files_dn, 1, 6) 
foldernames <- replace(foldernamesinitial, 1, "base")
file_names_new <- paste0(foldernames, "_ROWSUMS.DBF")

files_new <- paste0(new.folder, "/", file_names_new) 
file.copy(files, files_new)




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
  outputs <- dir(scenarios_folder)
  
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

