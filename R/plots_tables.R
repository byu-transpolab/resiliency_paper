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

make_sensitivity_plot <- function(sensitivity, prod){
  ls_sa <- read_rds(sensitivity)
  
  
    logsums_sa <- ls_sa %>%
      left_join(prod, by = c("purpose", "TAZ")) |> 
      mutate(total = productions * logsum) %>%
      group_by(purpose, scenario) %>%
    summarise(total = sum(total, na.rm = TRUE)) %>%
    mutate(link = case_when(str_length(scenario) < 10 ~ "BASE",
                            str_length(scenario) == 10 ~ substr(scenario, 5, 10),
                            str_length(scenario) == 11 ~ substr(scenario, 6, 11)))%>%
    mutate(iter = substr(scenario, 1, 4)) %>%
    group_by(link, iter) %>%
    summarize(total = sum(total)) %>%
    #select(-c("scenario")) %>%
    pivot_wider(names_from = link, values_from = total)%>%
    mutate(dROAD27 = ROAD27 - BASE, 
           dROAD38 = ROAD38 - BASE, 
           dROAD50 = ROAD50 - BASE) %>%
    mutate(CostROAD27 = dROAD27/-0.16, 
           CostROAD38 = dROAD38/-0.16, 
           CostROAD50 = dROAD50/-0.16)
  
  sa_graph_data <-  logsums_sa |> 
    select(c("iter","CostROAD27", "CostROAD38", "CostROAD50")) |> 
    # arrange iterations by decreasing value of scenario 50
    arrange(-CostROAD50) |> 
    # which iteration it is doesn't mater
    mutate(iter = row_number()) |> 
    pivot_longer(cols = c("CostROAD27", "CostROAD38", "CostROAD50"), 
                 names_to = "Link", values_to = "Cost") |> 
    mutate(Scenario = gsub("CostROAD", "", Link)) %>%
      group_by(Scenario) %>%
      arrange(-Cost, .by_group = TRUE)
  
  
  sa_graph_data
  
}

#' Make a plot of the trip length frequency distribution
#' 
#' @param tlfd_model Model-derived TLFD
#' @param tlfd_ustm USTM calibration TLFD
#' 
make_tlfd_plot <- function(tlfd_model, tlfd_ustm) {
  list(
     USTM = read_csv(tlfd_ustm,  col_types = list(Distance = col_number())),
     Model = read_csv(tlfd_model, col_types = list(Distance = col_number()))
  ) |> 
    bind_rows(.id = "Source") %>%
    pivot_longer(c(HBW, HBO, NHB), names_to = "Purpose", values_to = "Trips") |> 
    group_by(Source, Purpose) |> 
    mutate(Trips = Trips / sum(Trips)) |> 
    ggplot( aes(x = Distance, y = Trips, color = Source) ) +
    geom_line() +
    facet_wrap(~Purpose) +
    xlab("Distance [Miles]") + ylab("Trips [%]")
  
}