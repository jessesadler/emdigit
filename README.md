# emdigit

This project is designed to work out the structure of the data returned by Transkribus for the Codogno travel itinerary, locate problems in structuring the data, and figure out some solutions.

## Overview of scripts:

- `data-structure.R`: Short script to show the structure of the xml data exported by Transkribus.
- `clean-list-data.R`: Shows different ways to clean up the data when it is kept in list form instead of brought into a data frame as is done with the rest of the scripts.
- `build-tbl.R`: Provides a relatively agnostic way to transform the raw xml data into two different data frames. `data/codogno100-elements.csv` has a row for each element, region, or type. `data/codogno100-lines.csv` has a row for each line of xml data. This later is closer to the raw data.
- `tag-zones-functions.R`: Provides two functions to create geographic polygons with the points data provided by Transkribus and to plot them.
- `tag-zones.R`: Shows the problems that can be found and partially solved through these functions.

## codogno100

This subdirectory attempts to be more specific in focusing on the issues present in the Codogno text itself. There are two main scripts. `corrected.R` has a short comparison of the route headings created through these scripts compared to the hand corrected ones, but it is not complete.

- `build-codogno-tibble.R`: Provides the fullest dive into the Codogno data and attempts to deal with problems. It does this from the raw xml data.
- `correcting-codogno100.Rmd`: Shows the challenges of moving from the more generic `build-tibble.R` to a corrected data frame. It is not complete, but it shows some problems and solutions to dealing with the data. It has much in common with `build-codogno-tibble.R`, though the strategies used are sometimes different.
