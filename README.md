This was the final project for a Data and Information Visualization class that was part of the MSAN program. This project was completed by myself and another MSAN student.

Visualizing Craigslist Room Listings in the East Bay
==============================

| **Name**  | Mikaela Hoffman-Stapleton, Arda Aysu  |
|----------:|:-------------|
| **Email** | mhoffmanstapleton@usfca.edu, aaysu@usfca.edu |

Instructions
----------------------

The following packages must be installed prior to running this code:

- `shiny`
- `leaflet`
- `ggplot2`
- `ggvis`
- `rgdal`
- `dplyr`
- `reshape2`
- `stringi`


To run this code, please enter the following commands in R:

```
library(shiny)
shiny::runGitHub("usfviz/MikArda-final", subdir = "")
```

This will start the `shiny` app. See below for details on how to interact with the visualization.


## Data Set

The data was scraped from Craigslist on Friday, April 28th. It includes all room listings located in the East Bay. The dataset has columns for the listing's title, price, neighborhood, move-in date, room size, attributes, latitude, and longitude.

## Techniques

The first tab is a dot map that shows the specific locations of listings. You can filter according to price, size, and desired attributes. You can click on listings to get additional information.

The second tab is a choropleth map that shows the average room price for each region. You can click on regions to get additional information, aggregated by region.

The third tab is a small multiples plot that shows the percentage of listings that have certain attributes, broken up by neighborhood. You can select up to five neighorhoods to compare.

The fourth tab is a scatter plot that shows the relationship between room size and price. You can filter according to region and the neighorhoods in that region.  You can click on listings to get additional information.
