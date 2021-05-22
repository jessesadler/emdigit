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

# Remove empty data: where there is a type but only an empty character string
no_data <- map_lgl(txt_data, ~ identical(., character(0)))
txt_data <- txt_data[!no_data]

# Route headings
route_headings_pos <- names(txt_data) == "route-heading"

# First page number
first_pg <- txt_data[names(txt_data) == "page-number"][[1]] %>%
  as.numeric()

tbl <- tibble(type = names(txt_data),
       route = cumsum(route_headings_pos),
       img = cumsum(no_type)[!no_type][!no_data],
       page = img + first_pg - 1,
       data = txt_data) %>%
  rowid_to_column("id")

# Unknown types
unknown_type_pos <- names(txt_data) == ""
unknown_type <- txt_data[unknown_type_pos]

tbl %>%
  filter(type == "") %>%
  mutate(data = map_chr(data, str_flatten),
         data = str_squish(data))


# Route headings tbl ------------------------------------------------------

tbl_routes <- tbl %>%
  filter(type == "route-heading") %>%
  mutate(data = map(data, str_flatten)) %>%
  unnest(data) %>%
  mutate(data = str_squish(data),
         data = str_remove_all(data, "\\."),
         data = str_remove_all(data, "¬ "),
         data = str_remove_all(data, "- "))

unique(word(tbl_routes$data, 1))

# Distances tbl -----------------------------------------------------------

tbl_distances <- tbl %>%
  filter(type == "distances") %>%
  unnest(data) %>%
  mutate(data = str_squish(data),
         distance = data,
         distance = str_replace_all(distance, "I", "1"),
         distance = str_replace_all(distance, "i", "1"),
         distance = str_replace_all(distance, "[^0-9]", ""),
         distance = as.numeric(distance))

str_sort(unique(tbl_distances$data))
filter(tbl_distances, is.na(distance))

tbl_distances_sum <- tbl_distances %>%
  filter(!is.na(distance)) %>%
  group_by(route) %>%
  summarise(total_distance = sum(distance, na.rm = TRUE),
            nr_of_locs = n())


# Sum distance tbl --------------------------------------------------------

tbl_sum_distance <- tbl %>%
  filter(type == "sum-distance") %>%
  unnest(data) %>%
  mutate(first = word(data, 1)) %>%
  count(word)

str_sort(unique(tbl_sum_distance$data))

# Which routes are missing sum-distance
num_of_routes <- 1:nrow(tbl_routes)
sum_distance <- num_of_routes %in% unique(tbl_sum_distance$route)
num_of_routes[!sum_distance]



