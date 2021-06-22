## Data structure ##

library(tidyverse)
library(xml2)


# Import data -------------------------------------------------------------

x <- read_xml("data-raw/1623__Codogno__Compendio_TR_and_Lines.xml")
x_list <- xml2::as_list(x)

# Look at the structure of the list ---------------------------------------

# Overall structure: List of three lists
str(x_list, max.level = 2)

# TEI Header data
str(x_list[[1]][[1]])


# Points data -------------------------------------------------------------

# Each item on the list is one page of the scan
length(x_list[[1]][[2]])

# Structure of first page
str(x_list[[1]][[2]][[1]], max.level = 1)

pts_data <- x_list[[1]][[2]]

attributes(pts_data[[1]][[5]])
# Attributes
# 1. $names: zone
# 2. $points
# 3. $rendition: TextRegion
# 4. $id
# 5. $subtype

# Flatten page data to get list of length 1728
pts_data_flat <- flatten(pts_data)

# Work with first page to build tibble

# pg1 data and remove empty $graphic list
pg1 <- compact(pts_data[[1]])

ids <- map_chr(pg1, ~ attributes(.)$id)

types <- map_chr(pg1, ~ attributes(.)$subtype)

pts <- map_chr(pg1, ~ attributes(.)$points)

tbl <- tibble(id = ids,
              type = types,
              pts = pts)

tbl_tidy <- tbl %>%
  separate_rows(pts, sep = " ") %>%
  separate(pts, c("x", "y"), convert = TRUE) %>%
  mutate(y = -y)

ggplot(tbl_tidy) +
  geom_point(aes(x = x, y = y, color = id))

# Make polygons with sf
tbl_sf <- sf::st_as_sf(tbl_tidy, coords = c("x", "y")) %>%
  group_by(id) %>%
  summarise() %>%
  sf::st_convex_hull() %>%
  left_join(select(tbl, id, type), by = "id")

ggplot(tbl_sf) +
  geom_sf(aes(color = type, fill = type), alpha = 0.4) +
  geom_sf_text(aes(label = id)) +
  theme_void()

# Which pages have problems identifying subtypes

subtype_list <- vector("list", length(pts_data))

for (i in seq_along(pts_data)) {
  subtype_list[[i]] <- map(pts_data[[i]], ~ attributes(.)$subtype)
  subtype_list[[i]] <- subtype_list[[i]][-1]
}

any_nulls <- vector("list", length(subtype_list))

for (i in seq_along(subtype_list)) {
  any_nulls[[i]] <- any(map_lgl(subtype_list[[i]], is.null))
}

which(flatten_lgl(any_nulls))

# Text data ---------------------------------------------------------------

# List of list of length 1728
length(x_list[[1]][[3]][[1]][[1]])

# One item from the list
str(x_list[[1]][[3]][[1]][[1]][[1]])
x_list[[1]][[3]][[1]][[1]][[1]]


# Breaking down text data -------------------------------------------------

# id: image number
x_list[[1]][[3]][[1]][[1]][[1]]
attributes(x_list[[1]][[3]][[1]][[1]][[1]])

# type: page-number
x_list[[1]][[3]][[1]][[1]][[2]]
# List of length 2
length(x_list[[1]][[3]][[1]][[1]][[2]])

# The list itself has a name: "ab": note the single bracket at the end
names(x_list[[1]][[3]][[1]][[1]][2])

# The list has name (names of the items of the list), "facs", and "type" attributes
# It is the "type" attribute that has the TEI encoding
attributes(x_list[[1]][[3]][[1]][[1]][[2]])

# Access type attribute data
attributes(x_list[[1]][[3]][[1]][[1]][[2]])$type

# First item is empty list with name "lb" with "facs" and "n" attributes
x_list[[1]][[3]][[1]][[1]][[2]][[1]]
# Second item is actual data of character vector with no attributes
x_list[[1]][[3]][[1]][[1]][[2]][[2]]

# type: header
# Same structure as above
x_list[[1]][[3]][[1]][[1]][[3]]

# route-heading
x_list[[1]][[3]][[1]][[1]][[4]]
length(x_list[[1]][[3]][[1]][[1]][[4]])
attributes(x_list[[1]][[3]][[1]][[1]][[4]])

# Each line of text has one empty list with attributes of "facs" and "n"
# and then a character vector of the data
x_list[[1]][[3]][[1]][[1]][[4]][[1]]
x_list[[1]][[3]][[1]][[1]][[4]][[2]]

# type: locations
# Same basic structure as above
# Number of lines x2, but can be multiple lines for each location
x_list[[1]][[3]][[1]][[1]][[5]]
length(x_list[[1]][[3]][[1]][[1]][[5]])

# type: distances
# Same basic structure as above
# Length / 2 should equal number of locations,
# unless there is a partial location after last distance
x_list[[1]][[3]][[1]][[1]][[6]]

# type: sum-distance
x_list[[1]][[3]][[1]][[1]][[9]]

# type: catch-word
x_list[[1]][[3]][[1]][[1]][[13]]


# Attributes to names -----------------------------------------------------

# Example with one piece of data
route <- x_list[[1]][[3]][[1]][[1]][[4]]

type <- attributes(route)$type

# Flatten route to a vector
route_vctr <- flatten_chr(route)
# Create a list and name it
route_list <- list(route_vctr)
names(route_list) <- type


# Multiple pieces of data
y <- x_list[[1]][[3]][[1]][[1]][1:20]

# names
names(y)

# Remove where name is pb, which is image number
non_pb <- names(y) != "pb"
y <- y[non_pb]

# Or, remove all for which type is null
no_type <- purrr::map(y, ~ attributes(.)$type) %>% map_lgl(., is.null)

y <- y[!no_type]

# Get types and use as names
types <- purrr::map_chr(y, ~ attributes(.)$type)
names(y) <- types

# Create list with vector of values: Flatten within the lists
data <- map(y, flatten_chr)



# All data ----------------------------------------------------------------

txt_data <- x_list[[1]][[3]][[1]][[1]]

unique(names(txt_data))
