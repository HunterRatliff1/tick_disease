library(WVPlots)
library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(here)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
library(crsuggest)


ticks <- readxl::read_excel(here("data", "AllTBD2022_Public.xlsx"),
                            col_names = c("State","County", "FIPS", "Lyme", 
                                          "Tularemia", "Babesiosis",
                                          "Anaplasmosis", "Ehrlichiosis_ewingii", 
                                          "Ehrlichiosis_chaffeensis",
                                          "RMSF", 
                                          "ehrlichiosis_anaplasmosis"), skip = 1) %>%
  mutate(Ehrlichiosis_ewingii = Ehrlichiosis_ewingii + Ehrlichiosis_chaffeensis) %>%
  rename(Ehrlichiosis = Ehrlichiosis_ewingii) %>%
  select(-Ehrlichiosis_chaffeensis)

WNV <- readr::read_csv(here("data", "West Nile virus human and non-human activity by county, 1999-2024_.csv"), 
                skip = 1,
                col_names = c("GeoName", "Years", "FIPS", "Activity", "Cases_Human", "Cases_CNS", "BloodScreen", "Notes")) %>%
  mutate(Activity = recode(Activity, 
                           "Human infections"="Human",
                           "Non-human activity"="NonHuman",
                           "Human infections and non-human activity"="Both",
                           "Data unavailable"="Unavailable")) %>%
  select(-Notes)





# PairPlot(ticks, names(ticks)[4:10], 
#          title="Tick Borne Diseases",
#          alpha=0.5)

df_population <- tidycensus::get_decennial(geography = "county", 
                          variables = "P1_001N",   # Total population
                          year = 2020,
                          sumfile = "pl") %>%
  select(FIPS=GEOID, pop=value)

df_counties <- tigris::counties(cb=T) %>%
  select(FIPS=GEOID, State=STUSPS, County=NAME)

counties_all <- df_counties %>% 
  left_join(df_population) %>%
  filter(!State %in% c("PR", "AK", "HI", "GU", "VI", "AS", "MP")) 

rm(df_population, df_counties)

sf_states <- tigris::states(cb=T) %>%
  filter(!STUSPS %in% c("PR", "AK", "HI", "GU", "VI", "AS", "MP"))

sf_states_subset <- sf_states %>%
  filter(STUSPS %in% c("WV", "PA", "MD", "OH", "VA", "NC", "KY", "IN", "TN",
                       "NJ", "NY", "CT", "RI", "DE", "MA", "NH", "VT", "ME")) #%>%
  # st_transform(crs=6565)



counties_all %>%
  filter(State %in% c("WV", "PA", "MD", "OH", "VA", "NC", "KY", "IN", "TN",
                      "NJ", "NY", "CT", "RI", "DE", "MA", "NH", "VT", "ME")) %>%
  left_join(ticks, by = "FIPS") %>%
  mutate(Babesiosis = na_if(Babesiosis, 0)) %>%
  # View()

  ggplot(aes(fill=Babesiosis )) +
  geom_sf(color=NA) +
  geom_sf(fill=NA, data=sf_states_subset)




counties_all %>%
  # filter(State %in% c("WV", "PA", "MD", "OH", "VA", "NC", "KY", "IN", "TN",
  #                     "NJ", "NY", "CT", "RI", "DE", "MA", "NH", "VT", "ME")) %>%
  left_join(WNV, by = "FIPS") %>%
  ggplot(aes(fill=Cases_Human/pop*100000 )) +
  geom_sf(color=NA) +
  geom_sf(fill=NA, data=sf_states)


# Function to sample one point per infection
generate_points <- function(geom, n) {
  if (n > 0) {
    st_sample(geom, size = n, type = "random")
  } else {
    NULL
  }
}

x <- counties_all %>%
  filter(State %in% c("WV", "PA", "MD", "OH", "VA", "NC", "KY", "IN", "TN",
                      "NJ", "NY", "CT", "RI", "DE", "MA", "NH", "VT", "ME")) %>%
  left_join(ticks, by = "FIPS") %>%
  
  ### IMPORTANT: Filter to remove zeros
  filter(Babesiosis>0) %>%
  
  # Create dot-density using random sample
  rowwise() %>%
  mutate(x = map2(geometry, Babesiosis, generate_points)) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  # Make each one a row
  unnest_longer(x) %>%
  # Extract coordinates and make sf obj
  rowwise() %>%
  mutate(lon = map_dbl(x, ~sf::st_coordinates(x)[1])) %>%
  mutate(lat = map_dbl(x, ~sf::st_coordinates(x)[2])) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  select(-x) %>%
  st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
  st_transform(crs=4326)


ggplot() +
  geom_sf(data=x) +
  geom_sf(fill=NA, data=sf_states_subset)
  
# ggplot() +
#   # geom_sf() +
#   geom_sf(fill=NA, data=sf_states_subset)
