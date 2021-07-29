#' Make a plot of the links
#' 
#' @param links An sf object with highway links and project IDs
#' @param costs A tibble with total scenario costs
#' 
#' @return A map
#' 
make_mapdata <- function(links, costs){
  
  links %>%
    mutate(risk_id = str_c("ROAD", risk_id)) %>%
    left_join(costs, by = c("risk_id" = "scenario")) 
  
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


#' Model utility coefficient table
#' 
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