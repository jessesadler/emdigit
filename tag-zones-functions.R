## Tag zones functions ##

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
    ggplot2::labs(title = paste0("Image ", scan)) +
    ggplot2::theme_void()
}

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
