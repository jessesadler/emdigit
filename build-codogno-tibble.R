## Build tibble ##

library(tidyverse)
library(xml2)

# Build full data and route headings data for Codogno

# Import data -------------------------------------------------------------

data_xml <- read_xml("data-raw/1623__Codogno__Compendio_TR_and_Lines.xml")
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


# Clean list data ---------------------------------------------------------

# 1. Flatten to vectors within the lists
txt_data <- map(txt_data, flatten_chr)

# 2. Remove empty data: where there is a type but only an empty character string
no_data <- map_lgl(txt_data, ~ identical(., character(0)))
txt_data <- txt_data[!no_data]

# 3. Remove extra page 157
# Index for 157
pg_157_pos <- names(txt_data) == "page-number" & map_chr(txt_data, str_flatten) == "157"
pg_157 <- which(pg_157_pos == TRUE)
pg_157_vct <- seq(from = pg_157[[1]], to = pg_157[[2]] - 1)

txt_data <- txt_data[-pg_157_vct]

# 4. Unknown types

# Create vector of names
type_names <- names(txt_data)
# Where are types/names empty
unknown_type_pos <- type_names == ""
# Look at data for empty names
unknown_type <- txt_data[unknown_type_pos]
# Index of empty names
unknown_type_vct <- which(unknown_type_pos == TRUE)

# Manually correct missing types
unknown_type_names <- c("route-heading", "locations", "locations", "catch-word",
                        "catch-word", "catch-word", "route-heading", "catch-word",
                        "catch-word", "catch-word", "route-heading", "distances",
                        "route-heading", "", "catch-word", "route-heading",
                        "route-heading", "route-heading")

# Update names vector then names in list
type_names[unknown_type_vct] <- unknown_type_names

names(txt_data) <- type_names
# Check
sum(names(txt_data) == "")


# Build tibble ------------------------------------------------------------

# Route headings
route_headings_pos <- names(txt_data) == "route-heading"

# Image and page numbers
# Use images because they are more consistent than page number

# Find images
img <- cumsum(no_type)[!no_type][!no_data][-pg_157_vct]

# First page number
first_pg <- txt_data[names(txt_data) == "page-number"][[1]] %>%
  as.numeric()

# Number pages, use if_else to deal with pg 157 appearing twice
pg <- if_else(img + first_pg - 1 < 157,
              img + first_pg - 1,
              img + first_pg - 2)

tbl <- tibble(type = names(txt_data),
       route = cumsum(route_headings_pos),
       img = img,
       page = pg,
       data = txt_data) %>%
  rowid_to_column("id")

# Save full data
# tbl %>%
#   mutate(data = map(data, str_flatten)) %>%
#   unnest(data) %>%
#   mutate(data = str_squish(data)) %>%
#   write_csv("data/full-data.csv")

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

# Save route data
# write_csv(tbl_routes, "data/route-heading.csv")

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

# Which routes do not have distances
which(1:max(cumsum(route_headings_pos)) %in% tbl_distances_sum$route == FALSE)


# Sum distance tbl --------------------------------------------------------

tbl_sum_distance <- tbl %>%
  filter(type == "sum-distance") %>%
  unnest(data) %>%
  mutate(first = word(data, 1)) %>%
  count(word)

str_sort(unique(tbl_sum_distance$data))

# Which routes are missing sum-distance
num_of_routes <- 1:max(cumsum(route_headings_pos))
sum_distance <- num_of_routes %in% unique(tbl_sum_distance$route)
num_of_routes[!sum_distance]


# Locations ---------------------------------------------------------------


# First words of location data
tbl %>%
  filter(type == "locations") %>%
  unnest(data) %>%
  mutate(first = word(data, 1)) %>%
  count(first, sort = TRUE)

# First two words
tbl %>%
  filter(type == "locations") %>%
  unnest(data) %>%
  mutate(first = word(data, start = 1, end = 2)) %>%
  count(first, sort = TRUE) %>%
  View()

# Each group of locations as a single character vector
tbl %>%
  filter(type == "locations") %>%
  mutate(data = map_chr(data, str_flatten),
         data = str_squish(data)) %>%
  group_by(route) %>%
  summarise(data = paste(data))

# Character column: Group by route and then summarise, keeping original character vectors
tbl %>%
  filter(type == "locations") %>%
  unnest(data) %>%
  mutate(data = str_squish(data)) %>%
  group_by(route) %>%
  summarise(data = paste(data)) %>% View()

# List column: Group by route and then summarise, keeping original character vectors
tbl %>%
  filter(type == "locations") %>%
  group_by(route) %>%
  summarise(data = list(data)) %>%
  mutate(data = map(data, flatten_chr))

# All locs in one vector then split ---------------------------------------

# Problem with this is how to split with multiple patterns
# and str_split takes out what is use to split

locs_vctr <- tbl %>%
  filter(type == "locations") %>%
  group_by(route) %>%
  summarise(data = list(data)) %>%
  mutate(data = map(data, flatten_chr),
         data = map(data, str_flatten, collapse = " "),
         data = map(data, str_squish)) %>%
  unnest(data) %>%
  pull(data)

x <- locs_vctr[[1]]
str_split(x, " a [A-Z]") %>%
  str_split("Passarete")

str_split(x, " a [A-Z]") %>%
  map(~ str_split(., "Pass"))

y <- locs_vctr[[1]]

str_split(y, "\\n\\sa")




# Maintain character vectors ----------------------------------------------

# Use TRUE to denote where location starts

tbl_locations <- tbl %>%
  filter(type == "locations") %>%
  pull(data)

# With one set of locations
z <- tbl_locations[[1]]

# Starts with a
start_a <- str_detect(z, "^a [A-Z]")
# Starts with passarete
start_pass <- str_detect(z, "Passar")
# Starts with al passo
start_al <- str_detect(z, "al ")

starts <- start_a | start_pass | start_al

# The ones that begin with passarete should not have a beginning with next "a"
not_start <- which(start_pass == TRUE) + 1

# The last one is passarete, so only want first three to be false
starts[not_start[c(1, 2, 3)]] <- FALSE

# First one is true
starts[1] <- TRUE

tibble(id = cumsum(starts), data = z) %>%
  group_by(id) %>%
  summarise(data = list(data)) %>%
  mutate(data = map_chr(data, str_flatten),
         data = str_squish(data))

# With more than 1 group of locations
x <- tbl_locations[1:2]

start_a <- map(x, ~ str_detect(., "^a [A-Z]"))
# Starts with passarete
start_pass <- map(x, ~ str_detect(., "Passar"))
# Starts with al
start_al <- map(x, ~ str_detect(., "al "))

starts <- pmap(list(start_a, start_pass, start_al), ~ ..1 | ..2 | ..3)

not_start <- map(start_pass, ~ which(. == TRUE) + 1)
not_start[[1]] <- not_start[[1]][c(1, 2, 3)]


for (i in seq_along(starts)) {
  starts[[i]][not_start[[i]]] <- FALSE
}

for (i in seq_along(starts)) {
  starts[[i]][1] <- TRUE
}

out <- vector("list", length = length(x))
for (i in seq_along(x)) {
  out[[i]] <- rep(i, length(x[[i]]))
}
route_id <- flatten_int(out)

tibble(route = route_id,
       id = flatten_int(map(starts, cumsum)),
       data = flatten_chr(x)) %>%
  group_by(route, id) %>%
  summarise(data = list(data)) %>%
  mutate(data = map_chr(data, str_flatten),
         data = str_squish(data))
