#' Make a plot of the links
#' 
#' @param links An sf object with highway links and project IDs
#' @param costs A tibble with total scenario costs
#' 
#' @return A map
#' 
make_mapdata <- function(links, costs){
  
  links %>%
    left_join(costs, by = c("risk_id" = "scenario"))  %>%
    st_centroid() %>%
    st_transform(4326)
  
}



#' Make map of zonal changes
#' 
#' @param deltas 
#' @param taz Taz sf object
#' @param counties Character vector of counties to keep
#' @param scenario Scenario name to map
#' 
make_zonal_mapdata <- function(taz_deltas, taz){
  
  left_join(
    taz %>% select(TAZID),
    taz_deltas %>%
      select(TAZ, purpose, delta_total) %>%
      pivot_wider(names_from = purpose, values_from = delta_total),
    by = c("TAZID" = "TAZ")
  )
  
  
}


#' Get links file
#' 
#' @details NOT RUN IN STREAM. The outputs are committed to git, this 
#' is just for records
#' 
get_links <- function(){
  
  big_links <- st_read("~/Box/Macfarlane/research/resiliency/Risks Links Shapefile/risk_links_network.shp")
  
  project_info <- readxl::read_xlsx("images/linktable.xlsx") %>%
    mutate(LINK_ID = toupper(LINK_ID))
  
  big_links  %>%
    select(A, B, STREET, FTCLASS, LINKID, CO_NAME, risk_id) %>%
    mutate(risk_id = str_c("ROAD", risk_id)) %>%
    left_join(project_info, by = c("risk_id" = "LINK_ID")) %>%
    st_write("data/links.geojson", append = FALSE, delete_dsn = TRUE)
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