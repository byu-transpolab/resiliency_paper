#' Create project difference database
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