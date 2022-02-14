## Corrected data ##

library(tidyverse)
library(here)

# Full data ---------------------------------------------------------------

fdc <- read_csv(
  here("codogno100", "data-codogno100", "full_data_corrected.csv"))
fd <- read_csv(
  here("codogno100", "data-codogno100", "full-data.csv"))

# When are there two consecutive areas with the same type?
fd %>%
  filter(type == "sum-distance") %>%
  mutate(diff = id - lag(id)) %>%
  filter(diff == 1)

# Function to find duplicate types
duplicate_types <- function(df, which_type) {
  df %>%
    filter(type == which_type) %>%
    mutate(diff = id - lag(id)) %>%
    filter(diff == 1)
}

duplicate_types(fd, "locations")
duplicate_types(fd, "route-heading")

# Route headings ----------------------------------------------------------

rhc <- read_csv(
  here("codogno100", "data-codogno100", "route_headings_corrections.csv"))
rh <- read_csv(
  here("codogno100", "data-codogno100", "route-heading.csv"))
