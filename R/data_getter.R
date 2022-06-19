# FUNCTIONS BELOW THIS LINE ARE NOT PART OF TARGETS ========================================



#' Copy output files from scenario folders into a common folder
#' 
#' @details NOT IN TARGETS STREAM. Creates files needed by makeproject_data
#' 
#' 
copyoutputfiles <- function(){
  folder<-"E:/Base"
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

#' Read project logsum output files into a database
#' 
#' @details Not included in Targets stream. Output files committed to Git
makeproject_data <- function(){
  
  scenarios_folder <- "C:/projects/outputs_resiliency"
  hh_folder <- "C:/projects/resiliency_paper/data"
  
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


#' Read project travel time costs output files into a database
#' 
#' @details Not included in Targets stream. Output files are too big to be 
#' included in git. See the get_all_time_costs function below
makeproject_costs <- function(){
  
  scenarios_folder <- "C:/projects/outputs_resiliency"
  
  # read scenario_outputs
  outputs <- dir(scenarios_folder, pattern = "COSTS.DBF")
  
  #empty DBF
  #empty <- foreign::read.dbf("data/blank.DBF")
  
  costs <- lapply(outputs[36:42], function(scenario){
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
  
  write_rds(costs, "data/traveltimecosts4.rds")
  
}



#' Get travel time cost RDS files
#' 
#' @details not included in targets stream. On my computer (Macfarlane), could
#' not download before timeout. So need to manually download anyways. 
get_all_time_costs <- function(){
  downloadlist <- list(
    "data/traveltimecosts.rds"  = "https://byu.box.com/shared/static/hn03gac5tmo04uo7u3d0fx5fq76i961f.rds",
    "data/traveltimecosts2.rds" = "https://byu.box.com/shared/static/zstp3x1jtkyhgph25tk06qn26do633zw.rds",
    "data/traveltimecosts3.rds" = "https://byu.box.com/shared/static/xzv63hynpm7woi8juc0gj6s7t7qpls38.rds",
    "data/traveltimecosts4.rds" = "https://byu.box.com/shared/static/zcntx0pytduax0ytbkek9clgkd7f2k4x.rds"
  )
  
  for(i in 1:length(downloadlist)){
    if(!file.exists(names(downloadlist)[i])){
      download.file(downloadlist[[1]], destfile = names(downloadlist)[i])
    } else {
      message("file already exists")
    }
  }
}