## Full data tibble ##

# Script to build a tibble from raw xml data: Each row is an element

library(tidyverse)
library(xml2)


# Data --------------------------------------------------------------------

data_list <- read_xml("data-raw/1623__Codogno__Compendio_TR_and_Lines.xml") %>%
  xml2::as_list()


# Text data
txt_data <- data_list[[1]][[3]][[1]][[1]] %>%
  set_names(NULL)

# Remove where type is null: Page id
no_type <- txt_data %>%
  purrr::map(~ attributes(.)$type) %>%
  map_lgl(is.null)

txt_data <- txt_data[!no_type]

# Remove where there is no text data
no_text <- map_int(txt_data, length) == 0 | map_int(txt_data, length) == 1

txt_data <- txt_data[!no_text]

# Points data
pts_data <- data_list[[1]][[2]] %>%
  flatten() %>% # Remove first level of list: pages
  compact() %>% # Remove page id items
  set_names(NULL)

# Remove where there is no text data
pts_data <- pts_data[!no_text]

# Build elements tibble ----------------------------------------------------

# Each row is an element

# 1. Image numbers: cumsum of no_type without no_type and no_text
img <- cumsum(no_type)[!no_type][!no_text]

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
  map(flatten_chr) %>%
  map(str_squish)

# 2. Remove lines with no text data

# Create list of logical vectors where lines have no data
no_line_txt <- map(line_txt, ~ . == "")

# For loop to subset
for (i in seq_along(line_txt)) {
  line_txt[[i]] <- line_txt[[i]][!no_line_txt[[i]]]
}

# 3. Line numbers
line_nr <- map(line_txt, seq_along)


# 4. Build line tibble
tbl_lines <- tibble(id = seq_along(img),
                    trans_id = trans_id,
                    img = img,
                    type = type,
                    line_nr = line_nr,
                    data = line_txt,
                    pts = pts) %>%
  unnest(c(data, line_nr)) %>%
  rowid_to_column("line_id")


# Nest data
tbl_lines %>%
  nest(data = c(line_id, line_nr, data))

# Unnest with
tbl_lines %>%
  nest(data = c(id, line_nr, data)) %>%
  unnest(data) %>%
  select(line_id, id, trans_id, img, type, line_nr, data, pts)
