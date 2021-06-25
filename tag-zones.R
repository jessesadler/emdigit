## Tag zones function ##

library(tidyverse)
library(sf)

# tbl: elements tbl from full-data-tbl
# scan: Image number

zones_plot <- function(tbl, scan) {

  tbl %>%
    dplyr::filter(img == scan) %>%
    sf_polygons() %>%
    # Plot
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(color = type, fill = type), alpha = 0.4) +
    ggrepel::geom_text_repel(aes(label = id, geometry = geometry),
                    stat = "sf_coordinates") +
    ggplot2::labs(title = paste0("Page ", scan)) +
    ggplot2::theme_void()
}

# ggsave("plots/codogno-page70-zones.pdf")

# sf polygons -------------------------------------------------------------

# tbl: elements tbl from full-data-tbl

sf_polygons <- function(tbl) {
  tbl %>%
    # Separate pts data
    tidyr::separate_rows(pts, sep = " ") %>%
    tidyr::separate(pts, c("x", "y"), sep = ",", convert = TRUE) %>%
    dplyr::mutate(y = -y) %>% # Reverse y for page orientation
    # Convert to sf and create polygons
    sf::st_as_sf(coords = c("x", "y")) %>%
    dplyr::group_by(id, type) %>%
    dplyr::summarise(.groups = "drop") %>%
    sf::st_convex_hull()
}

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

