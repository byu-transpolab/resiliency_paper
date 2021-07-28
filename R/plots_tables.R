#' Make a plot of the links
#' 
#' @param links An sf object with highway links and project IDs
#' @param projects A tibble with project data
#' 
#' @return A map
#' 
make_map <- function(links, projects){
  
  
}

#' Get links file
#' 
#' @details NOT RUN IN STREAM. The outputs are committed to git, this 
#' is just for records
#' 
get_links <- function(){
  
  big_links <- st_read("~/Box/Macfarlane/research/resiliency/Risks Links Shapefile/risk_links_network.shp")
  
  
  big_links  %>%
    select(A, B, STREET, FTCLASS, LINKID, CO_NAME, risk_id) %>%
    st_write("data/links.geojson")
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