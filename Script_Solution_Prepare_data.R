library(tidyverse)
library(jsonlite)


## Read data ##
df <- read_csv("./data/ships.csv")


## Check number of NA values in each column ##
df %>%
  summarise_all(funs(sum(is.na(.))))


## Function to calculate distance ##
calculate_dist <- function(lat1, lon1, lat2, lon2) {
  
  R = 6371e3 # meters
  phi_1 = lat1 * pi/180 # phi, lambda in radians
  phi_2 = lat2 * pi/180
  delta_phi = (lat2-lat1) * pi/180
  delta_lambda = (lon2-lon1) * pi/180
  
  a <- sin(delta_phi/2) * sin(delta_phi/2) +
    cos(phi_1) * cos(phi_2) *
    sin(delta_lambda/2) * sin(delta_lambda/2)
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  d <- R * c # in meters
}

calc_function <- Vectorize(calculate_dist)


## Obtain and save data frame with calculated distance values ##
df_results <- df %>%
  filter(!grepl(pattern = "^[0-9]{1}$", SHIPNAME),
         SHIPNAME != "",
         is_parked == 0) %>%
  arrange(ship_type, SHIPNAME, SHIP_ID, DATETIME) %>%
  rownames_to_column() %>%
  mutate(rowname = as.numeric(rowname),
         SHIPNAME = str_replace(SHIPNAME, "^\\.", replacement = ""),
         SHIPNAME = str_replace(SHIPNAME, "^\\ ", replacement = ""),
         SHIPNAME = str_replace(SHIPNAME, "(?<=[[:alpha:]]) (?=\\d+)", replacement = "-")) %>%
  group_by(ship_type, SHIPNAME, SHIP_ID) %>%
  mutate(dist_value = calc_function(LAT, LON, lag(LAT), lag(LON))) %>%
  dplyr::select(rowname, ship_type, SHIPNAME, SHIP_ID, LAT, LON, dist_value, DATETIME)

df_max_distances <- df_results %>%
  ungroup() %>%
  group_by(ship_type, SHIPNAME) %>%
  mutate(max_dist = max(dist_value, na.rm = TRUE)) %>%
  filter(dist_value == max_dist) %>%
  mutate(max_DATETIME = max(DATETIME)) %>%
  filter(DATETIME == max_DATETIME,
         max_dist > 0) 

all_rows <- c(df_max_distances[["rowname"]], df_max_distances[["rowname"]] - 1)
all_rows <- sort(all_rows)

df_data <- df_results[all_rows, ] %>%
  left_join(df_max_distances[, c("ship_type", "SHIPNAME", "max_dist", "max_DATETIME")]) %>%
  distinct() %>%
  dplyr::select(-rowname)

write_csv(df_data, path = "./data/input_app_data.csv")


## Obtain and save a list with ship types and names ##
df_ship_types <- df_data %>%
  distinct(ship_type, SHIPNAME)

list_ship_types <- list()
for (shiptype in unique(df_ship_types[["ship_type"]])) {
  
  ship_names <- df_ship_types %>%
    filter(ship_type == shiptype) %>%
    distinct(SHIPNAME)
  
  list_ship_types[shiptype] <- ship_names
}

json_data <- toJSON(list_ship_types) 
write(json_data, file = "./data/ship_types.json")

