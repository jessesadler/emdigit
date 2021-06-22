## Full data tibble ##

# Script to build a tibble from raw xml data

library(tidyverse)
library(xml2)

# Raw data ----------------------------------------------------------------

data_list <- read_xml("data-raw/1623__Codogno__Compendio_TR_and_Lines.xml") %>%
  xml2::as_list()


# Text data ---------------------------------------------------------------

txt_data <- data_list[[1]][[3]][[1]][[1]]

# Remove where type is null: Page id
no_type <- txt_data %>%
  purrr::map(~ attributes(.)$type) %>%
  map_lgl(is.null)

txt_data <- txt_data[!no_type]


# Points data -------------------------------------------------------------

pts_data <- data_list[[1]][[2]] %>%
  flatten() %>% # Remove first level of list: pages
  compact() # Remove page id items


# Build tibble ------------------------------------------------------------

# 1. Image numbers: cumsum of no_type without no_type
img <- cumsum(no_type)[!no_type]

# 2. Transkribus id
trans_id <- purrr::map_chr(txt_data, ~ attributes(.)$facs) %>%
  str_remove("#") # Remove "#" at beginning of each id

# 3. Types of data
type <- purrr::map_chr(txt_data, ~ attributes(.)$type)

# 4. Number of lines for each element
lines <- txt_data %>%
  map(flatten_chr) %>%
  map_int(length)

# 5. Actual text data
txt_flat <- txt_data %>%
  map(flatten_chr) %>%
  map(str_flatten) %>%
  map(str_squish)

# 6. Points data
pts <- map_chr(pts_data, ~ attributes(.)$points)


# 7. Build tibble
tbl_data <- tibble(id = seq_along(img),
                   trans_id = trans_id,
                   img = img,
                   type = type,
                   lines = lines,
                   data = txt_flat,
                   pts = pts) %>%
  unnest(data)
