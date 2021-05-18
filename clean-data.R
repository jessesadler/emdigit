## Clean data ##

library(tidyverse)
library(xml2)

# Import data -------------------------------------------------------------

x <- read_xml("1623__Codogno__Compendio_TR_and_Lines.xml")
x_list <- xml2::as_list(x)
txt_data <- x_list[[1]][[3]][[1]][[1]]


# Route headings ----------------------------------------------------------
type <- attributes(txt_data[[4]])$type

route_heading <- txt_data[4] %>%
  map(flatten_chr)

names(route_heading) <- type

# Flatten into single string
route_heading_flat <- map(route_heading, str_flatten)

# Remove white space and return character
route_heading_squish <- map(route_heading_flat, str_squish)

# Remove period
map(route_heading_squish, ~ str_remove(., "\\."))


# Distances ---------------------------------------------------------------

type <- attributes(txt_data[[6]])$type

distances <- txt_data[6] %>%
  map(flatten_chr)

names(distances) <- type

distances %>%
  map(str_squish) %>% # remove white space
  map(~ str_remove(., "p\\.")) %>% # remove letters
  map(as.numeric) # cast to numeric

