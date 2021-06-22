## Tag zones function ##

library(tidyverse)
library(xml2)
library(sf)
library(ggrepel)

data_list <- read_xml("data-raw/1623__Codogno__Compendio_TR_and_Lines.xml") %>%
  xml2::as_list()

pts_data <- data_list[[1]][[2]]

zones_plot <- function(x, page) {
  # subset page number
  x <- compact(x[[page]])

  # Build tibble
  ids <- map_chr(x, ~ attributes(.)$id)
  pts <- map_chr(x, ~ attributes(.)$points)
  # Subtypes not always identified
  types <- map(x, ~ attributes(.)$subtype)
  types[map_lgl(types, is.null)] <- NA
  types <- flatten_chr(types)

  tbl <- tibble(id = ids,
                type = types,
                pts = pts)

  tbl_tidy <- tbl %>%
    separate_rows(pts, sep = " ") %>%
    separate(pts, c("x", "y"), convert = TRUE) %>%
    mutate(y = -y)

  # Convert to sf to make polygons
  tbl_sf <- tbl_tidy %>%
    sf::st_as_sf(coords = c("x", "y")) %>%
    group_by(id) %>%
    summarise() %>%
    sf::st_convex_hull() %>%
    left_join(select(tbl, id, type), by = "id")

  # Plot
  ggplot(tbl_sf) +
    geom_sf(aes(color = type, fill = type), alpha = 0.4) +
    geom_text_repel(aes(label = id, geometry = geometry),
                             stat = "sf_coordinates") +
    theme_void()

}
