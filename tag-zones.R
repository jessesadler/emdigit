## Tag zones ##

library(tidyverse)
library(sf)
source(here::here("tag-zones-functions.R"))

# Plot zones of one page

zones_plot(tbl_elements, 70)
# ggsave("plots/codogno-page70-zones.pdf")

tbl_sf <- sf_polygons(tbl_elements)

# Plot of all polygons
ggplot(tbl_sf) +
  geom_sf(aes(color = type), alpha = 0.5) +
  facet_wrap(vars(type)) +
  labs(title = "All tag zones by type")

# ggsave("plots/tag-zones-facet.pdf")

# Bounding boxes ----------------------------------------------------------

# Bounding box for each polygon
bboxes <- tbl_sf %>%
  st_geometry() %>%
  map(st_bbox) %>%
  transpose() %>%
  as_tibble() %>%
  unnest(everything()) %>%
  bind_cols(select(tbl_elements, id, img, type, data))

# Width of bboxes to show ones that go into both columns
# Problems with those over 1100
bboxes %>%
  filter(type != "header" & type != "catch-word") %>%
  mutate(width = xmax - xmin) %>%
  arrange(desc(width))

# Boxes that go across the middle of the columns
bboxes %>%
  filter(type != "header",
         xmin < 600 & xmax > 1400)

# Division between header data and other data

# Header and page number minimum y
bboxes %>%
  filter(type %in% c("header", "page-number")) %>%
  group_by(type) %>%
  slice_min(ymin, n = 3)

# Max y of data
bboxes %>%
  filter(type %in% c("locations", "distances", "route-heading")) %>%
  group_by(type) %>%
  slice_max(ymax, n = 3)


# Centroids ---------------------------------------------------------------

centroids <- tbl_sf %>%
  sf::st_centroid() %>% # Warning is not important
  bind_cols(select(tbl_elements, img, data)) %>%
  select(id, img, type, data, geometry) # Rearrange columns

# See where centroids are
ggplot(centroids) +
  geom_sf(aes(color = type), alpha = 0.5) +
  facet_wrap(vars(type)) +
  labs(title = "Centroids of tag zones by type")

# ggsave("plots/centroids-facet.pdf")

# Get x, y coordinates from sf_point data
centroids_xy <- centroids %>%
  mutate(x = st_coordinates(centroids)[ , 1],
         y = st_coordinates(centroids)[ , 2])

# Route and location centroids close to the column border
centroids_xy %>%
  filter(type %in% c("route-heading", "locations"),
         x > 800 & x < 1100)

# Division between header data and other data

# Header and page number minimum y
centroids_xy %>%
  filter(type %in% c("header", "page-number")) %>%
  arrange(y)

# Max y of data
centroids_xy %>%
  filter(type %in% c("locations", "distances", "route-heading")) %>%
  arrange(desc(y))

# Dealing with placement problems -----------------------------------------

# Sort by column and y value then find ids that have difference > 2

centroid_placement <- centroids_xy %>%
  st_drop_geometry() %>%
  # Remove header and footer data
  filter(type != "page-number" & type != "header" & type != "catch-word") %>%
  # Divide data between column a and column b
  mutate(col = if_else(x < 1000, "a", "b")) %>%
  group_by(img, col) %>%
  nest(data = c(id, type, data, x, y)) %>%
  # Mutate within nested tibbles: Arrange by y then find diff in id
  mutate(data = map(data, ~ arrange(., desc(y))),
         data = map(data, ~ mutate(., diff = abs(id - lag(id))))) %>%
  unnest(data) %>%
  ungroup() %>%
  rowid_to_column("row_id") # Use row_id for placement in tbl


# Find problems, then get context of problems
centroid_placement %>%
  filter(diff > 2)

# If there are two ids per column, the earlier one is the issue
# If there is only one id per column, problem is that id if it is at end (id 294),
# But it is the previous one, if it is the first id of the column (id 785).

# Find which ids only have one id in the column
one_id <- centroid_placement %>%
  filter(diff > 2) %>%
  add_count(img, col) %>%
  filter(n == 1) %>%
  pull(row_id)

# Find which of the previous ids is NA
are_na <- centroid_placement %>%
  filter(row_id %in% (one_id - 1)) %>%
  pull(diff) %>%
  is.na() %>%
  which()

# Those which are NA are the problem
one_id[are_na] <- one_id[are_na] - 1

# Two ids > 2 in column: First one is the issue
centroid_placement %>%
  filter(diff > 2) %>%
  add_count(img, col) %>%
  filter(n == 2) %>%
  group_by(img, col) %>%
  slice(1) %>%
  select(-n) %>%
  # Add in one_id data
  bind_rows(filter(centroid_placement, row_id %in% (probs))) %>%
  arrange(row_id)


# Average position of catch-words -----------------------------------------

# Average position for catch-words
catch_words <- centroids %>%
  filter(type == "catch-word") %>%
  group_by(type) %>%
  summarise() %>%
  st_centroid() %>%
  st_geometry()

# Distance from average
catch_dist <- st_distance(filter(centroids, type == "catch-word"), catch_words) %>%
  as.numeric()

# Potential outliers
boxplot.stats(catch_dist)$out

outliers <- which(catch_dist %in% boxplot.stats(catch_dist)$out)

centroids %>%
  filter(type == "catch-word") %>%
  slice(outliers) %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = catch_words, color = "red")

centroids %>%
  filter(type == "catch-word") %>%
  slice(outliers) %>%
  left_join(select(tbl_elements, id, data, img), by = "id")
