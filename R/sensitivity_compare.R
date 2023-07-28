library(tidyverse)
ls_sa <- read_rds("data/logsums_sensitivity25.rds")
prod <- read_rds("data/productions.rds")


logsums_sa <- ls_sa %>%
  left_join(prod, by = c("purpose", "TAZ")) %>%
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

sa_graph_data <- logsums_sa %>%
  select(c("iter","CostROAD27", "CostROAD38", "CostROAD50")) %>%
  pivot_longer(cols = c("CostROAD27", "CostROAD38", "CostROAD50"), names_to = "Link", values_to = "Cost")%>%
  mutate(Link = fct_reorder(Link, desc(Cost)))


ggplot(sa_graph_data) +
  aes(x = iter, y = Cost, colour = Link, group = Link) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal()



logsums_sa %>%
  ggplot(aes(x = iter)) +
  geom_point(aes(y = CostROAD27, color = "Road27")) +
  geom_point(aes(y = CostROAD38, color = "Road38")) +
  geom_point(aes(y = CostROAD50, color = "Road50")) +
  scale_color_hue(direction = 1) +
  theme_minimal()
