library(dplyr)
library(tidyr)
library(ggplot2)
library(tigris)   # for zip code
library(ggthemes) # Tableau color
library(stringr)
library(ggrepel) # add text label

# Data: Confirmed cases ###################################################################################
Dt <- read.csv('COVID-19 Confirmed Cases in WA 2021.csv')
head(Dt)

# Obtain a map for the Washington state and it is in county level 
Map_WA <- map_data(map = 'county', region =  'washington') %>% select(lon = long, lat, id = subregion) 
head(Map_WA)

# Convert first letter of every word to uppercase
require(stringr)
Map_WA$id <- str_to_title(Map_WA$id)

# Change "id" to "County"
colnames(Map_WA)[3] <- 'County'

# Join geographical data with COVID-19 data
Dt_Plot <- left_join(Map_WA, Dt, by = 'County')

# Highlight counties with confirmed cases > 50000
HighlightCounty <- Dt_Plot[which(Dt_Plot$County %in% c('King', 'Pierce', 'Snohomish')), ]

# Mark location of Seattle, Bellevue, Kirkland with 
PointedCity <- data.frame('City' = c('Seattle', 'Bellevue', 'Kirkland', 'Redmond'), 
                          'lon' = c(-122.335167, -122.200676, -122.20715, -122.121513), 
                          'lat' = c(47.608013, 47.610378, 47.676607, 47.673988))

# Figure ###################################################################################################
p1 <- 
  ggplot(Dt_Plot, aes(x = lon,              # x-axis: longitude
                      y = lat)) +           # y-axis: latitude
  geom_polygon(aes(group = County,          # Draw polygon for each county
                   fill = Frequency),       # Shade of color represented for magnitude of case counts
               alpha = 0.8,                 # Contrl transparency of color
               color = "grey70") +          # Color for boundary line
  geom_polygon(data = HighlightCounty,      # Use the new dataset to draw highlighted counties
               aes(col = County),           # Boudary line for each county with different color
               linewidth = 0.6,             # Control line width
               fill = NA) +                 # Do not fill any color for interior in this function
  geom_point(data = PointedCity,            # Use another dataset for marked cities
             aes(x = lon, y = lat,          # x-axis: longitude | y-axis: latitude 
                 group = 'King'),           # These cities are in King county
             pch = 20) +                    # Control point type
  geom_text_repel(data = PointedCity, 
                  aes(x = lon, y = lat, 
                      label = City, 
                      group = 'King'),
                  fontface = "bold",        # Make labels bold
                  nudge_x = c(-0.5, 0.5, 1, 1), 
                  nudge_y = c(0.5, -0.5, 0.5, -0.5)) + 
  coord_sf() + 
  scale_fill_gradient2_tableau(palette = 'Gold-Purple Diverging') + 
  theme_bw() + 
  guides(fill = guide_colorbar(title = c("COVID-19 Cases"))) +  # Modify legend title for color scale
  ggtitle('Overall Confirmed COVID-19 Cases in \nWashington State in 2021') + 
  theme(panel.border = element_blank(),   
        panel.grid = element_blank(),
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),                              
        axis.ticks.y = element_blank(),                  
        axis.title.x = element_blank(),                              
        axis.title.y = element_blank(),
        legend.title = element_text(color = 'black', face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'))  # Center main title
p1

# Data: Confirmed cases ###################################################################################
Dt <- read.csv('Covid-19 Cases In King County.csv')
Dt$geo_id <- as.character(Dt$geo_id)
head(Dt)

Zip <- zctas(cb = T, starts_with = c("98"), year = 2020, class = "sf")
Zip

Zip <- as.data.frame(Zip)                  # Change the dataset to dataframe
Zip <- Zip[, c('ZCTA5CE20', 'geometry')]   # Obtain zip code and sf object columns
colnames(Zip)[1] <- 'geo_id'               # Change the column name for zip code 

Dt_Plot <- left_join(Dt, Zip, by = 'geo_id') # Left join 

# Highlight cities
HighlightCity <- Dt_Plot[which(Dt_Plot$City %in% c('Seattle', 'Bellevue', 'Kirkland', 'Redmond')), ]

# Figure ###################################################################################################
p1 <- 
  ggplot(Dt_Plot) + 
  geom_sf(aes(fill = Frequency, 
              geometry = geometry), 
          alpha = 0.7) + 
  geom_sf(HighlightCity, 
          mapping = aes(geometry = geometry, col = City), 
          linewidth = 0.6, 
          fill = NA) + 
  scale_fill_gradient2_tableau(palette = 'Gold-Purple Diverging') + 
  theme_bw() + 
  guides(fill = guide_colourbar(title = c("COVID-19 Cases"))) + 
  ggtitle('Overall Confirmed COVID-19 Cases in \nKing County, WA up to May 24, 2023') + 
  theme(panel.border = element_blank(),   
        panel.grid = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),                              
        axis.ticks.y = element_blank(),                  
        axis.title.x = element_blank(),                              
        axis.title.y = element_blank(),
        legend.title = element_text(color = 'black', face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'))                  
p1








