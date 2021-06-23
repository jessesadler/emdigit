## Full data tibble ##

# Script to build a tibble from raw xml data: Each row is an element

library(tidyverse)
library(xml2)


# Data --------------------------------------------------------------------

data_list <- read_xml("data-raw/1623__Codogno__Compendio_TR_and_Lines.xml") %>%
  xml2::as_list()


# Text data
txt_data <- data_list[[1]][[3]][[1]][[1]]

# Remove where type is null: Page id
no_type <- txt_data %>%
  purrr::map(~ attributes(.)$type) %>%
  map_lgl(is.null)

txt_data <- txt_data[!no_type]


# Points data

pts_data <- data_list[[1]][[2]] %>%
  flatten() %>% # Remove first level of list: pages
  compact() # Remove page id items


# Build elements tibble ----------------------------------------------------

# Each row is an element

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
elmnt_txt <- txt_data %>%
  map(flatten_chr) %>%
  map(str_flatten) %>%
  map(str_squish)

# 6. Points data
pts <- map_chr(pts_data, ~ attributes(.)$points)


# 7. Build tibble
tbl_elements <- tibble(id = seq_along(img),
                       trans_id = trans_id,
                       img = img,
                       type = type,
                       lines = lines,
                       data = elmnt_txt,
                       pts = pts) %>%
  unnest(data)


# Build lines tibble ------------------------------------------------------

# Each row is a line

# 1. Text of lines
line_txt <- txt_data %>%
  map(flatten_chr)

# 2. Line numbers
line_nr <- map(line_txt, seq_along)


# Build line tibble
tbl_lines <- tibble(trans_id = trans_id,
                    img = img,
                    type = type,
                    line_nr = line_nr,
                    data = line_txt,
                    pts = pts) %>%
  unnest(c(data, line_nr)) %>%
  rowid_to_column("id")


# Nest data
tbl_lines %>%
  nest(data = c(id, line_nr, data))

# Unnest with
tbl_lines %>%
  nest(data = c(id, line_nr, data)) %>%
  unnest(data) %>%
  select(id, trans_id, img, type, line_nr, data, pts)

# Problems with line id and points data -----------------------------------

# Not all line ids or points data have text data
# This makes it problematic to include them in line tibble.

# Line id
line_id <- txt_data %>%
  map( ~ map(., ~ attributes(.)$facs)) %>%
  map(compact) %>%
  map(flatten_chr) %>%
  map(~ str_remove(., "#"))

# Line points data
line_pts <- pts_data %>%
  map( ~ map(., ~ attributes(.)$points)) %>%
  map(compact)

length(flatten(line_txt))
length(flatten(line_nr))
length(flatten(line_id))
length(flatten(line_pts))

line_id_lengths <- set_names(map_int(line_id, length), NULL)
line_nr_lengths <- set_names(map_int(line_nr, length), NULL)
line_txt_lengths <- set_names(map_int(line_txt, length), NULL)
line_pts_lengths <- set_names(map_int(line_pts, length), NULL)

which(line_nr_lengths != line_txt_lengths)
which(map_dbl(txt_data, ~ length(.) %% 2) == 1)
map_dbl(txt_data[which(map_dbl(txt_data, ~ length(.) %% 2) == 1)], length)

length(compact(flatten(line_pts)))
