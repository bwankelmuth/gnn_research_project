###############################################################################
# Map of Sweden
###############################################################################

# In this file we write the code for the map of Sweden, which can be found in appendix B of the master's thesis.
# Our code is based on various code examples covered in "Drawing beautiful maps programmatically with R, sf and 
# ggplot2 - Part 2: Layers" by Moreno and Basille (2018). It is the second part of a tutorial that explains how 
# to create maps in R combining the ggplot2 and sf packages. We also adopt a small part from a comment on a blog 
# post by Herman (2020). However, our code has been significantly extended in order to meet our needs.
# The second part of the mentioned tutorial is available at: 
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# The comment on the blog post is available at: 
# https://community.rstudio.com/t/ggmap-tidy-verse-non-usa-countries-with-county-municipality-subregion-data/68527

# Part 1: preparatory steps

# load the required packages
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(ggrepel)
theme_set(theme_bw())
sf_use_s2(FALSE)

# download map of countries of the entire world provided by package rnaturalearth and return an sf object
# download map of Swedish counties and process it
world <- ne_countries(scale = "medium", returnclass = "sf")
tmp <- tempfile()
download.file("http://api.thenmap.net/v2/se-4/geo/2020-06-03", destfile = tmp)
county <- read_sf(tmp)
county <- st_as_sf(county)
county <- cbind(county, st_coordinates(st_centroid(county)))

# We would like to design the map according to our ideas. To this end, we would like to mark the Swedish cities 
# with at least one hospital as well as the surrounding countries in the map. Moreover, we add a legend 
# specifying the Swedish counties in the map.
# Therefore, we still need to define some data frames and vectors that contain the data required for this 
# purpose.

# create data frame containing the latitude and longitude of every Swedish city with at least one hospital 
# These coordinates were looked up manually on GeoHack, accessed from the cities' Wikipedia pages.
# Only for Solna, for which there are no coordinates on Wikipedia,the coordinates were looked up at: 
# https://www.laengengrad-breitengrad.de/gps-koordinaten-von-solna
# Hint: You can also get the coordinates from GoogleMaps by using the Google Maps API.
# Unfortunately, this requires having a valid Google API key.
# Otherwise, the coordinates need to be looked up manually as was done here.
swcities <- data.frame(state = rep("Sweden", 29),
                       city = c("Borås", "Uppsala", "Stockholm", "Linköping", "Halmstad", "Lund", "Eskilstuna", 
                                "Skellefteå", "Örebro", "Karlskrona", "Uddevalla", "Sundsvall", "Umeå", "Göteborg", 
                                "Malmö", "Västerås", "Kalmar", "Solna", "Visby", "Jönköping", "Karlstad", 
                                "Växjö", "Falun", "Lidköping", "Helsingborg", "Östersund", "Skövde", "Varberg",
                                "Gävle"),
                       lat = c(57.720833, 59.859722, 59.325, 58.408333, 56.666667, 55.702778, 59.370833, 
                               64.75, 59.270833, 56.161111, 58.35, 62.388889, 63.826944, 57.706111, 
                               55.6025, 59.611111, 56.665278, 59.3688791, 57.638889, 57.780556, 59.380556, 
                               56.879167, 60.605556, 58.502778, 56.045833, 63.176389, 58.388889, 57.105556,
                               60.6756), 
                       lng = c(12.941667, 17.641111, 18.05, 15.625, 12.85, 13.193056, 16.513889, 
                               20.956944, 15.2125, 15.5875, 11.933333, 17.308333, 20.266944, 11.953611, 
                               13.000833, 16.545833, 16.355556, 18.0084334, 18.294444, 14.161111, 13.504722, 
                               14.806944, 15.630556, 13.158333, 12.694444, 14.638889, 13.844444, 12.25,
                               17.1458))
# convert data frame to sf object
swcities <- st_as_sf(swcities, coords = c("lng", "lat"), remove = FALSE, 
                      crs = 4326, agr = "constant")

# create data frame containing latitude and longitude of surrounding countries
# These latitude and longitude values were defined by myself depending on where exactly I wanted to have the 
# respective country labels. 
countries <- data.frame(country = c("Norway", "Finland", "Denmark", "Germany", "Poland", "Russia", "Belarus",
                                    "Estonia", "Lithuania", "Latvia", "Russia"),
                        lat = c(62, 62, 56, 54.3, 54.3, 58, 54.5, 58.75, 55.3, 56.75, 54.65),
                        lng = c(9, 26, 9.2, 9.8, 17.5, 29.5, 28.5, 25.75, 24.3, 25, 21.4))
# convert data frame to sf object
countries <- st_as_sf(countries, coords = c("lng", "lat"), remove = FALSE, 
                       crs = 4326, agr = "constant")

# create vector defining the horizontal offsets of city labels from the city locations
nudge_x = c(-4.8, 2, 2, 3, -1.5, 2.6, -6,
            2, -5, 2, -3, 2, 2, -4.1,
            0.5, 3, 2, 2, 2, 6, -4,
            4.5, 3.5, -4, 4, 5, 5.4, -4.8, 
            2.1)

# create vector defining the vertical offsets of city labels from the city locations
nudge_y = c(1.8, 1, -0.2, 0, 0, 0, 1.8,
            0, 1.5, 0, 1.5, 0, 0, 1.4,
            -0.7, 2, 0, 0.2, 0.2, 0.4, 1,
            0, 1.5, 1.5, 0, -0.5, 0.5, 1.6, 
            1.2)

# create vector containing the county labels to be used in the legend
labels = c("1: Stockholm", "10: Blekinge", "12: Skåne", "13: Halland", 
           "14: Västra Götaland", "17: Värmland", "18: Örebro", 
           "19: Västmanland", "20: Dalarna", "21: Gävleborg", 
           "22: Västernorrland", "23: Jämtland", "24: Västerbotten", 
           "25: Norrbotten", "3: Uppsala", "4: Södermanland", 
           "5: Östergötland", "6: Jönköping", "7: Kronoberg", "8: Kalmar", 
           "9: Gotland")

# Part 2: creation of map of Sweden

map <- ggplot(data = world) +
  geom_sf(fill = ifelse(world$geounit == "Sweden", 'aliceblue', 'antiquewhite1')) +
  geom_sf(data = county, aes(fill = id), color="white") +
  geom_text(data = county, aes(X, Y, label = id), size = 3,
            nudge_x = county$nudge_x, nudge_y = county$nudge_y) +
  geom_sf(data = swcities) +
  geom_text_repel(data = swcities, aes(x = lng, y = lat, label = city), size = 3, 
                  nudge_x = nudge_x, nudge_y = nudge_y) +
  geom_label(data = countries, aes(lng, lat, label = country), size = 3, fontface = "bold") + 
  coord_sf(xlim = c(4, 31), ylim = c(54, 70), expand = FALSE) +
  annotation_scale(location = "tl", width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(1.75, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") + 
  theme(panel.background = element_rect(fill = "aliceblue")) +
  scale_fill_discrete(name = "Counties", labels = labels) + 
  theme(legend.position = "bottom",
        legend.background = element_blank(), 
        legend.box.background = element_rect(colour = "black")) + 
  guides(fill = guide_legend(ncol = 7))

# look at map
map

# save map as png image
#ggsave("map_updated_180622.png", map, width=1920/72, height=1080/72) 

