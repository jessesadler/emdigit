## Clean data ##

library(tidyverse)
library(xml2)

# This script shows different ways of cleaning the xml data from transkribus.

# Import data -------------------------------------------------------------

data_list <- read_xml("data-raw/1623__Codogno__Compendio_TR_and_Lines.xml") %>%
  xml2::as_list()
txt_data <- data_list[[1]][[3]][[1]][[1]] %>%
  set_names(NULL) # Remove names on lists: They are not necessary


# Remove scan id ----------------------------------------------------------

# Remove where type is null: Page id
no_type <- txt_data %>%
  map(~ attributes(.)$type) %>%
  map_lgl(is.null)

txt_data <- txt_data[!no_type]


# Items that have no text data --------------------------------------------

# Which items have a Transkribus id but no text data

# Length 0 = id but no text data
# Length 1 = id and line id but no text
which(map_int(txt_data, length) == 0 | map_int(txt_data, length) == 1)

no_text <- map_int(txt_data, length) == 0 | map_int(txt_data, length) == 1

# Number of items no text data
sum(no_text)

# Remove items with no text data

txt_data <- txt_data[!no_text]


# Types of data -----------------------------------------------------------

type <- map_chr(txt_data, ~ attributes(.)$type)

# Which elements are missing type data
which(type == "")
sum(type == "")

missing_type <- type == ""

# See text of data with missing type
map(txt_data, flatten_chr)[missing_type] %>%
  map(str_flatten) %>%
  map(str_squish)

# Line text data ----------------------------------------------------------

# Flatten to character vectors within the lists
# Each element within the lists is a line
# This gets rid of all attributes

line_txt <- txt_data %>%
  map(flatten_chr) %>%
  map(str_squish)

# Lines that have no text data: Much easier to do after using flatten_chr()

# There are a few lines that the only text data is a return: "\n      "
# Using str_squish() above turns "\n" into ""
no_line_txt <- map(line_txt, ~ . == "")

# How many lines have no text
flatten_lgl(no_line_txt) %>% sum()

# Which elements have lines with no text
which(map_lgl(no_line_txt, any))

# See these lines
line_txt[which(map_lgl(no_line_txt, any))]

# Number of lines now
line_txt %>%
  map_int(length) %>%
  sum()

# For loop to subset
for (i in seq_along(line_txt)) {
  line_txt[[i]] <- line_txt[[i]][!no_line_txt[[i]]]
}

# After removing lines with no text
line_txt %>%
  map_int(length) %>%
  sum()

# Look at lines now
line_txt[which(map_lgl(no_line_txt, any))]

# Element text ------------------------------------------------------------

# Further flatten line text into single string for each element/type
elmnt_txt <- txt_data %>%
  map(flatten_chr) %>%
  map(str_flatten)

# Route headings ----------------------------------------------------------

## Single heading ##
route_heading <- txt_data[3]

# Flatten into character vector of each line
route_heaing_chr <- map(route_heading, flatten_chr)

# Flatten into single string
route_heading_flat <- map(route_heaing_chr, str_flatten)

# Remove white space and return character
route_heading_squish <- map(route_heading_flat, str_squish)

# Remove period
map(route_heading_squish, ~ str_remove(., "\\."))


## All route headings ##
route_headings_pos <- type == "route-heading"

route_headings_list <- txt_data[route_headings_pos]

route_headings_vct <- route_headings_list %>%
  map(flatten_chr) %>%
  map(str_flatten) %>%
  map(str_squish) %>%
  map(~ str_remove_all(., "\\.")) %>% # get rid of period
  map(~ str_remove_all(., "¬ ")) %>%  # get rid of dash characters
  map(~ str_remove_all(., "- ")) %>%
  flatten_chr()

tibble(route = route_headings_vct) %>%
  rowid_to_column("id")


# Page numbers ------------------------------------------------------------

# Transkribus does not reliably find pages as seen below
# It only finds 90 out of 100 pages
pages_pos <- type == "page-number"
pages <- txt_data[pages_pos] %>%
  map(flatten_chr) %>%
  map(as.numeric) %>%
  flatten_dbl()

# Remove NA created by heading interpreted as page number
pages <- pages[!is.na(pages)]

# Better way to find page numbers is with image ids.
cumsum(no_type)[!no_type]


# Distances ---------------------------------------------------------------

distances_pos <- type == "distances"
distances <- txt_data[distances_pos]

# Character vector and remove white space
raw_distances <- distances %>%
  map(flatten_chr) %>%
  map(str_squish)

raw_distances %>%
  map(~ str_replace_all(., "I", "1")) %>% # Replace capital I with 1
  map(~ str_replace_all(., "i", "1")) %>% # Replace lowercase i with 1
  map(~ str_replace_all(., "[^0-9]", "")) %>% # Remove non-numeric values
  map(as.numeric) %>%
  map(~ discard(., is.na))

# Sum distance ------------------------------------------------------------

sum_distances_pos <- type == "sum-distance"
sum_distances <- txt_data[sum_distances_pos]

sum_distances %>%
  map(flatten_chr) %>%
  map(~ str_replace_all(., "[^0-9]", "")) %>% # Remove non-numeric values
  map(as.numeric) %>%
  flatten_dbl()

# Header ------------------------------------------------------------------

headers_pos <- type == "header"
headers <- txt_data[headers_pos] %>%
  map(flatten_chr) %>%
  compact() %>%
  map_chr(1L) %>% # Only take first element of the vector
  str_squish()

# Locations ---------------------------------------------------------------

locations <- flatten_chr(txt_data[[15]]) %>%
  str_squish()

locations

# Get rid of hyphen, attach next element, and then delete that element
hyphens <- which(str_detect(locations, "¬"))
replacements <- str_replace(locations[hyphens], "¬", locations[hyphens + 1])

locations2 <- locations
locations2[hyphens] <- replacements
locations2 <- locations2[-c(hyphens + 1)]

# Lines with few characters should be added to previous element without a space
short_lines <- which(str_length(locations2) < 6)

replacements <- str_c(locations2[short_lines - 1], locations2[short_lines])
locations2[short_lines - 1] <- replacements
locations2 <- locations2[-short_lines]


# Find which lines start in common ways
which(str_detect(locations2, "^a |^Si |^Pass"))

# Combine and then split
x <- locations %>%
  str_flatten() %>%
  str_remove_all(., "¬ ")

str_split(x, " a ") %>%
  flatten_chr()


# Associate route headings with data --------------------------------------

# Route heading
heading_ids <- cumsum(route_headings_pos)

# Associate route headings with data through attributes


# Transkribus ids ---------------------------------------------------------

trans_id <- map_chr(txt_data, ~ attributes(.)$facs) %>%
  str_remove("#") # Remove "#" at beginning of each id

# Transcribus line ids
line_id <- txt_data %>%
  map( ~ map(., ~ attributes(.)$facs)) %>%
  map(compact) %>%
  map(flatten_chr) %>%
  map(~ str_remove(., "#"))

# There are issues with line ids. Not every line id has text, so the number
# of line ids and line text are not equal. This makes this data difficult
# to work with.

length(flatten(line_txt))
length(flatten(line_id))

# Which ones are not equivalent
not_equal <- which(map_int(line_txt, length) != map_int(line_id, length))
# Where text data is not even length or where lines were removed
which(map_dbl(txt_data, ~ length(.) %% 2) == 1)

# Lengths of these lists

map_dbl(line_txt[not_equal], length)
map_dbl(line_id[not_equal], length)
