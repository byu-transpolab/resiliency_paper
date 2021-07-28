#' Make a plot of the links
#' 
#' @param links An sf object with highway links and project IDs
#' @param projects A tibble with project data
#' 
#' @return A map
#' 
make_map <- function(links, projects){
  
  
}


make_coefficient_table <- function(mc_constants, mc_coefficients, dc_coefficients){
  
  # mode choice coefficients
  mc <- bind_rows(
    mc_constants,
    mc_coefficients
  ) %>%
    mutate(model = "Mode Choice")
  
  
  # destination choice coefficients
  dc <- dc_coefficients %>%
    mutate(model = "Destination Choice")
  
  bind_rows(dc, mc) %>%
    select(model, var, HBW, HBO, NHB)
  
}