library(tidycensus)
vars_1990 <- load_variables(year = 1990, dataset = "sf3")
vars_1990 %>% 
  filter(concept == "Housing Subjects")
vars1 <- c(farm = "H0050003")
get_decennial(geography = "county", variables = vars1, year = 1990, state = "Texas")
