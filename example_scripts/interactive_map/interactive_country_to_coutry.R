# install.packages("tidyverse")
# install.paackages("sf")
# install.packages("leaflet")
# install.paackages("htmlwidgets")
library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)

working_dir <- getwd()

### FILL THIS LINE BEFORE RUNNING
dir.sci_dat_gadm1_nuts3_counties <-
  "sci_data/2020-12-16_country_country.tsv"

# Read in the detailed GADM SCI data (this dataset is quite large and
# this line of code will likely take a minute or so)
sci_dat <- read_tsv(dir.sci_dat_gadm1_nuts3_counties)
sci_dat <- rename(sci_dat, sci = scaled_sci)

# Download country borders into a temp directory, unzip and use
download.file(
  "https://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",
  "/tmp/countries.zip"
)
dir.create("/tmp/countries/", showWarnings = FALSE)
unzip("/tmp/countries.zip", exdir = "/tmp/countries")
countries_shapes <-
  st_read("/tmp/countries/TM_WORLD_BORDERS-0.3.shp") %>%
  filter(!ISO3 %in% c("ATF", "ATA"))

regions <- c("IT")

for (i in 1:length(regions)) {
  # Get the data for the ith region
  dat <- filter(sci_dat, user_loc == regions[i])
  
  # Merge with shape files
  dat_map <-
    right_join(dat,
               countries_shapes,
               by = c("fr_loc" = "ISO2")) %>%
    st_as_sf
  
  # Create measures to scale up from the overall 20th percentile country pair from country
  x1 <- quantile(dat_map$sci, .2, na.rm = T)
  x2 <- x1 * 2
  x3 <- x1 * 3
  x5 <- x1 * 5
  x10 <- x1 * 10
  x25 <- x1 * 25
  x100 <- x1 * 100
  
  # Create clean buckets for these levels
  dat_map <- dat_map %>%
    mutate(
      sci_bkt = case_when(
        sci < x1 ~ "< 1x (Country 20th percentile)",
        sci < x2 ~ "1-2x",
        sci < x3 ~ "2-3x",
        sci < x5 ~ "3-5x",
        sci < x10 ~ "5-10x",
        sci < x25 ~ "10-25x",
        sci < x100 ~ "25-100x",
        sci >= x100 ~ ">= 100x"
      )
    ) %>%
    mutate(sci_bkt = factor(
      sci_bkt,
      levels = c(
        "< 1x (Country 20th percentile)",
        "1-2x",
        "2-3x",
        "3-5x",
        "5-10x",
        "10-25x",
        "25-100x",
        ">= 100x"
      )
    ))
  
  # Get the map of the region you are in
  curr_region_outline <- dat_map %>%
    filter(fr_loc == regions[i])
  
  # Create labels for mouse over
  labels <- sprintf(
    "<strong>Key: </strong>%s<br/>
      <strong>Name: </strong>%s<br/>
      <strong>SCI: </strong>%s<br/>",
    dat_map$fr_loc,
    dat_map$NAME,
    dat_map$sci_bkt
  ) %>%
    lapply(htmltools::HTML)
  
  # We do this again so that there is a label on the home district as well
  labels2 <- sprintf(
    "<strong>Key: </strong>%s<br/>
      <strong>Name: </strong>%s<br/>",
    curr_region_outline$fr_loc,
    curr_region_outline$NAME
  ) %>%
    lapply(htmltools::HTML)
  
  pal <- colorFactor(palette = "GnBu", domain = dat_map$sci_bkt)
  
  m <- leaflet() %>%
    addProviderTiles("Esri.WorldStreetMap",
                     options = leafletOptions()) %>%
    addPolygons(
      data = dat_map,
      weight = 2,
      fillOpacity = 0.9,
      color = ~ pal(sci_bkt),
      fillColor = ~ pal(sci_bkt),
      group = "shapes1",
      label = labels,
      highlight = highlightOptions(weight = 4,
                                   color = "black")
    ) %>%
    addPolygons(
      data = curr_region_outline,
      fillColor = "Red",
      fillOpacity = 1,
      label = labels2,
      color = "Red",
      weight = 0.6
    )
  
  # htmlWidgets will often fail if not saving to current wd.
  # We reset it here then move it back.
  setwd(file.path(working_dir, "example_scripts",
                  "interactive_map",
                  "output"))
  saveWidget(m,
             paste0(regions[i], "_interactive.html"))
  setwd(working_dir)
}
