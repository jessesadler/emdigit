## Corrected data ##

library(tidyverse)

# Full data ---------------------------------------------------------------

fdc <- read_csv("data/full_data_corrected.csv")
fd <- read_csv("data/full-data.csv")

fd %>%
  filter(type == "sum-distance") %>%
  mutate(diff = id - lag(id)) %>%
  filter(diff == 1)

duplicate_types <- function(df, which_type) {
  df %>%
    filter(type == which_type) %>%
    mutate(diff = id - lag(id)) %>%
    filter(diff == 1)
}

duplicate_types(fd, "locations")
duplicate_types(fd, "route-heading")

# Route headings ----------------------------------------------------------

rhc <- read_csv("data/route_headings_corrections.csv")
rh <- read_csv("data/route-heading.csv")
