

# Multiple-Scenario Analysis Tables =====================================================

#' Calculate production-weighted logsum difference
#' 
#' @param prod
#' @param logsums
#' 
calculate_deltas <- function(prod, logsums){
  
  logsums %>%
    left_join(prod, by = c("purpose", "TAZ")) %>%
    mutate(total = productions * logsum) %>%
    group_by(purpose, scenario) %>%
    summarise(total = sum(total, na.rm = TRUE)) %>%
    spread(scenario, total, fill = 0) %>%
    gather(scenario, total, -purpose, -BASE) %>%
    mutate(
      delta = total - BASE,
      pct_delta = delta / BASE * 100
    ) 
  
}

#' Aggregate the time-based cost values
#' @param timecosts The four big travel time cost matrices
#' @return Aggregates by scenario and purpose
#' 
calculate_timecosts <- function(timecosts, timecosts2, timecosts3, timecosts4){
  bind_rows(list(
    timecosts,
    timecosts2,
    timecosts3,
    timecosts4,
  ))
}

reduce_timecosts <- function(timecosts_file){
  read_rds(timecosts_file) %>%
    group_by(M, scenario) %>%
    summarise(Cost = sum(V1)) %>%
    mutate(scenario = substr(scenario, 5,6)) %>%
    rename(Purpose = M) %>%
    pivot_wider(names_from = Purpose, values_from = Cost) %>%
    mutate(
      # Costs are given in cents; need to convert to dollars
      HBWHBONHB = (HBO + HBW + NHB)/100,
      FXPREC = (IIF + IXF + REC + XXF + XXP)/100,
      TOTAL = HBWHBONHB + FXPREC
    ) %>%
    select(scenario, HBWHBONHB, FXPREC, TOTAL) 
}

# Single-Scenario Analysis Tables =====================================================

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

#' Calculate scenario logsum totals
#' @param taz_deltas 
#' @param taz_ids A list of ID's to consider "in-region"
#' @param mc_cost_coef The value of time implied by the mode choice model
#' 
#' @return a tibble with logsum costs by purpose and region
#' 
calculate_scenario_ls <- function(taz_deltas, taz_ids, mc_cost_coef){
  taz_deltas %>%
    mutate(inregion = TAZ %in% taz_ids) %>%
    group_by(purpose, inregion) %>% 
    summarise(delta_total = sum(delta_total)) %>% 
    mutate(cost = -1 * delta_total / mc_cost_coef * .01)
}


#' Calculate financial costs of closing links for all scenarios
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



#' Calculate detailed travel time costs for one scenario
#' 
#' @param timecosts
#' @param taz_ids A list of ID's to consider "in-region"
#' @return a tibble with travel time costs by purpose and region
#' 
#' 
caclulate_taz_timecosts <- function(taz_timecosts, taz_ids){
  
  taz_timecosts %>%
    filter(M != "TimeDiff") %>%
    mutate(inregion = I %in% taz_ids)  %>%
    group_by(inregion, M)  %>%
    summarise(total = sum(V1))
}

#' Generate a comparison table between two scenarios
#' 
#' @param ls_scenarios The scenario costs from the logsum-based analysis
#' @param tm_scenarios The scenario costs from the travel time-based analysis
#' 
#' @return a tibble with the scenario data arranged in a way that it can 
#'  be presented effectively in the manuscript
compare_scenarios <- function(ls_scenarios, tm_scenarios){
  full_join(
    ls_scenarios %>%
      transmute(purpose, inregion, lscost = -1 * cost)  %>%
      pivot_wider(names_from = inregion, values_from = lscost, names_prefix = "ls",
                  values_fill = 0) %>%
      mutate(lsTOTAL = lsFALSE + lsTRUE),
    tm_scenarios  %>%
      transmute(purpose = M, inregion, total = total / 100) %>%
      pivot_wider(names_from = inregion, values_from = total, names_prefix = "tm", 
                  values_fill = 0) %>%
      mutate(tmTOTAL =  tmFALSE + tmTRUE),
   by = "purpose"
  )
}



