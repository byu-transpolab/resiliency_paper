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


summary_table <- function(deltas){
  
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
makeproject_data <- function(){
  
  scenarios_folder <- "~/Box/Macfarlane/research/resiliency/OUTPUTS/OUTPUTS/"
  
  # read hh productions by purpose 
  prod <- foreign::read.dbf(file.path(scenarios_folder, "HH_PROD.DBF")) %>%
    as_tibble() %>%
    gather(purpose, productions, -TAZ) %>%
    mutate(purpose = gsub("P", "", purpose))
  
  write_rds(prod, "data/productions.rds")
  
  
  # read scenario_outputs
  outputs <- dir(scenarios_folder, pattern = ".DBF")
  
  logsums <- lapply(outputs[outputs != "HH_PROD.DBF"], function(scenario){
    # get scenario name
    scenario_name <- gsub("ROWSUM.DBF", "", scenario)
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