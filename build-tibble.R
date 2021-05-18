## Build tibble ##

library(tidyverse)
library(xml2)

# Import data -------------------------------------------------------------

data_xml <- read_xml("1623__Codogno__Compendio_TR_and_Lines.xml")
data_list <- xml2::as_list(data_xml)
txt_data <- data_list[[1]][[3]][[1]][[1]]

# Types as names ----------------------------------------------------------

# Remove where type is null: Page id
no_type <- txt_data %>%
  purrr::map(~ attributes(.)$type) %>%
  map_lgl(is.null)

txt_data <- txt_data[!no_type]

# Get types and use as names
types <- purrr::map_chr(txt_data, ~ attributes(.)$type)
names(txt_data) <- types

# Flatten to vectors within the lists
txt_data <- map(txt_data, flatten_chr)

# Route headings
route_headings_pos <- names(txt_data) == "route-heading"

tibble(type = names(txt_data),
       route = cumsum(route_headings_pos),
       img = cumsum(no_type)[!no_type],
       page = img + 123,
       data = txt_data) %>%
  rowid_to_column("id") %>%
  unnest(data)
