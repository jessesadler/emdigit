## Clean data ##

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


# Route headings ----------------------------------------------------------

## Single heading ##
route_heading <- txt_data[3]

# Flatten into single string
route_heading_flat <- map(route_heading, str_flatten)

# Remove white space and return character
route_heading_squish <- map(route_heading_flat, str_squish)

# Remove period
map(route_heading_squish, ~ str_remove(., "\\."))


## All route headings ##
route_headings_pos <- names(txt_data) == "route-heading"

route_headings_list <- txt_data[route_headings_pos]

route_headings_vct <- route_headings_list %>%
  map(str_flatten) %>%
  map(str_squish) %>%
  map(~ str_remove_all(., "\\.")) %>% # get rid of period
  map(~ str_remove_all(., "¬ ")) %>%  # get rid of dash characters
  map(~ str_remove_all(., "- ")) %>%
  flatten_chr()

tibble(route = route_headings_vct) %>%
  rowid_to_column("id")


# Page numbers ------------------------------------------------------------

# Transkribus does not reliably find pages as seen below
# It only finds 90 out of 100 pages
pages_pos <- names(txt_data) == "page-number"
pages <- txt_data[pages_pos] %>%
  flatten_chr() %>%
  as.numeric()

# Remove NA created by heading interpreted as page number
pages <- pages[!is.na(pages)]

# Better way to find page numbers is with image ids.
cumsum(no_type)[!no_type]


# Distances ---------------------------------------------------------------

distances_pos <- names(txt_data) == "distances"
distances <- txt_data[distances_pos]

# remove white space
raw_distances <- map(distances, str_squish)

raw_distances %>%
  map(~ str_replace_all(., "I", "1")) %>% # Replace capital I with 1
  map(~ str_replace_all(., "i", "1")) %>% # Replace lowercase i with 1
  map(~ str_replace_all(., "[^0-9]", "")) %>% # Remove non-numeric values
  map(as.numeric) %>%
  map(~ discard(., is.na))

# Sum distance ------------------------------------------------------------

sum_distances_pos <- names(txt_data) == "sum-distance"
sum_distances <- txt_data[sum_distances_pos]

sum_distances %>%
  map(~ str_replace_all(., "[^0-9]", "")) %>% # Remove non-numeric values
  map(as.numeric) %>%
  flatten_dbl()

# Header ------------------------------------------------------------------

headers_pos <- names(txt_data) == "header"
headers <- txt_data[headers_pos] %>%
  flatten_chr()

# Locations ---------------------------------------------------------------

locations <- txt_data[[4]]

locations
x <- locations %>%
  str_flatten() %>%
  str_squish() %>%
  str_remove_all(., "¬ ")

str_split(x, " a ") %>%
  flatten_chr()


# Associate route headings with data --------------------------------------

# Route heading
heading_ids <- cumsum(route_headings_pos)

# Associate route headings with data through attributes



