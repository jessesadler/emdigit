## Data structure ##

library(tidyverse)
library(xml2)

# Import data -------------------------------------------------------------

data_list <- read_xml(
  here::here("data-raw", "1623__Codogno__Compendio_TR_and_Lines.xml")
) %>%
  xml2::as_list()

# Look at the structure of the list ---------------------------------------

# Overall structure: List of three lists
str(data_list, max.level = 2)

# 1. TEI Header data: list of 1
# 2. Points data: list of 100 (pages)
# 3. Text data: list of 1

# First level: TEI Header data --------------------------------------------

str(data_list[[1]][[1]])


# Second level: Points data ------------------------------------------------

# Each item on the list is one page of the scan
length(data_list[[1]][[2]])

# Structure of first page
str(data_list[[1]][[2]][[1]], max.level = 1)

# First element of each list is empty list with page id
data_list[[1]][[2]][[1]][[1]]

pts_page <- data_list[[1]][[2]]

# Length of each element is the number of lines/zones
length(pts_page[[1]][[5]])

# Each zone/line has its own id (suffix of l1) and set of points data
pts_page[[1]][[5]][[1]]

# Flatten page data and remove page id elements to get list of length 1628
# This is same length as txt_data when scan ids are removed
pts_data <- pts_page %>%
  flatten() %>%
  compact()


# Access attributes -------------------------------------------------------

# The important info is in the attributes of each element
attributes(pts_page[[1]][[5]])
# Attributes
# 1. $names: (zone) Number of zones or lines
# 2. $points: Bounding box of points
# 3. $rendition: TextRegion
# 4. $id
# 5. $subtype

# Transkribus id
trans_id <- map_chr(pts_data, ~ attributes(.)$id)

# Types of data: Called subtype in points data
# Missing types are NULL, so need to be converted to empty character
# This data is actually cleaner than comes from the text data

subtype <- map(pts_data, ~ attributes(.)$subtype)
subtype[map_lgl(subtype, is.null)] <- "" # Deal with missing types
subtype <- flatten_chr(subtype)

pts <- map_chr(pts_data, ~ attributes(.)$points)


# Work with first page to build tibble

# pg1 data and remove empty $graphic list
pg1 <- compact(pts_page[[1]])

id <- map_chr(pg1, ~ attributes(.)$id)

type <- map_chr(pg1, ~ attributes(.)$subtype)

pts <- map_chr(pg1, ~ attributes(.)$points)

tbl <- tibble(id = id,
              type = type,
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

subtype_list <- vector("list", length(pts_page))

for (i in seq_along(pts_page)) {
  subtype_list[[i]] <- map(pts_page[[i]], ~ attributes(.)$subtype)
  subtype_list[[i]] <- subtype_list[[i]][-1]
}

any_nulls <- vector("list", length(subtype_list))

for (i in seq_along(subtype_list)) {
  any_nulls[[i]] <- any(map_lgl(subtype_list[[i]], is.null))
}

which(flatten_lgl(any_nulls))

# Third level: Text data ---------------------------------------------------

# List of list of length 1728: Number of elements plus image ids
length(data_list[[1]][[3]][[1]][[1]])

# One item from the list
str(data_list[[1]][[3]][[1]][[1]][[1]])
data_list[[1]][[3]][[1]][[1]][[1]]


# Breaking down text data -------------------------------------------------

# id: image number
# Empty list with facs, n, and id attributes
# This is the best way to identify images/pages from the data
data_list[[1]][[3]][[1]][[1]][[1]]
attributes(data_list[[1]][[3]][[1]][[1]][[1]])

# type: page-number
data_list[[1]][[3]][[1]][[1]][[2]]
# List of length 2
length(data_list[[1]][[3]][[1]][[1]][[2]])

# The list itself has a name: "ab": note the single bracket at the end
names(data_list[[1]][[3]][[1]][[1]][2])

# The list has name (names of the items of the list), "facs", and "type" attributes
# It is the "type" attribute that has the TEI encoding.
# facs is the transkribus id.
attributes(data_list[[1]][[3]][[1]][[1]][[2]])

# Access type attribute data
attributes(data_list[[1]][[3]][[1]][[1]][[2]])$type

# First item is empty list with name "lb" with "facs" and "n" attributes
data_list[[1]][[3]][[1]][[1]][[2]][[1]]
# Second item is actual data of character vector with no attributes
data_list[[1]][[3]][[1]][[1]][[2]][[2]]

# type: header
# Same structure as above
data_list[[1]][[3]][[1]][[1]][[3]]

# route-heading
data_list[[1]][[3]][[1]][[1]][[4]]
length(data_list[[1]][[3]][[1]][[1]][[4]])
attributes(data_list[[1]][[3]][[1]][[1]][[4]])

# Each line of text has one empty list with attributes of "facs" and "n"
# and then a character vector of the data
data_list[[1]][[3]][[1]][[1]][[4]][[1]]
data_list[[1]][[3]][[1]][[1]][[4]][[2]]

# type: locations
# Same basic structure as above
# Number of lines x2, but can be multiple lines for each location
data_list[[1]][[3]][[1]][[1]][[5]]
length(data_list[[1]][[3]][[1]][[1]][[5]])

# type: distances
# Same basic structure as above
# Length / 2 should equal number of locations,
# unless there is a partial location after last distance
data_list[[1]][[3]][[1]][[1]][[6]]

# type: sum-distance
data_list[[1]][[3]][[1]][[1]][[9]]

# type: catch-word
data_list[[1]][[3]][[1]][[1]][[13]]


# Attributes to names -----------------------------------------------------

# Example with one piece of data
route <- data_list[[1]][[3]][[1]][[1]][[4]]

type <- attributes(route)$type

# Flatten route to a vector
route_vctr <- flatten_chr(route)
# Create a list and name it
route_list <- list(route_vctr)
names(route_list) <- type


# Multiple pieces of data
y <- data_list[[1]][[3]][[1]][[1]][1:20]

# names
names(y)

# Remove where name is pb, which is image number
non_pb <- names(y) != "pb"
y <- y[non_pb]

# Or, remove all for which type is null
no_type <- y %>%
  purrr::map(~ attributes(.)$type) %>%
  map_lgl(., is.null)

y <- y[!no_type]

# Get types and use as names
types <- purrr::map_chr(y, ~ attributes(.)$type)
names(y) <- types

# Create list with vector of values: Flatten within the lists
data <- map(y, flatten_chr)



# All data ----------------------------------------------------------------

txt_data <- data_list[[1]][[3]][[1]][[1]]

unique(names(txt_data))
