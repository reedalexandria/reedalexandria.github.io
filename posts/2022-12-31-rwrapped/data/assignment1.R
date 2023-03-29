## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----load, include=TRUE, message=FALSE, warning=FALSE-----------------
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)
library(leaflet)
library(gridExtra)


## ----include=TRUE-----------------------------------------------------
africa = world |> 
  filter(continent == "Africa", !is.na(iso_a2)) |> 
  left_join(worldbank_df, by = "iso_a2") |> 
  dplyr::select(name, subregion, gdpPercap, HDI, pop_growth) |> 
  st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25")


## ----include=TRUE-----------------------------------------------------
zion = st_read((system.file("vector/zion.gpkg", package = "spDataLarge")))
data(nlcd, package = "spDataLarge")
force(nlcd)


## ----include=TRUE-----------------------------------------------------
#Using plot()
plot(africa["HDI"]) 

#Using tmap()
tm_shape(africa) +
  tm_polygons("HDI")


## ----include=TRUE-----------------------------------------------------
#creating HDI map
HDI_map <- tm_shape(africa) +
  tm_polygons("HDI",
              breaks = c(0, 0.55, 0.70, Inf),
              labels = c("Low", "Medium", "High"),
              title = "Human Development Index",
              palette = "viridis")

HDI_map


## ----include=TRUE-----------------------------------------------------
#creating subregion map
subregion_map <- tm_shape(africa) +
  tm_polygons("subregion",
              title = "Subregions of Africa",
              palette = "Dark2")

#combine HDI_map and subregion_map on one plot
current.mode <- tmap_mode("plot")
tmap_arrange(HDI_map, subregion_map)
#tmap_mode(current.mode) - plots the maps on top and bottom 


## ----include=TRUE-----------------------------------------------------
zion_map <- tm_shape(nlcd)+
  tm_raster(palette = c("dodgerblue4",
                        "dimgray",
                        "goldenrod3",
                        "darkolivegreen4",
                        "darkseagreen",
                        "burlywood4",
                        "red",
                        "darkcyan"
                        ),
            title = "Land Cover Categories") +
  tm_shape(zion) +
  tm_borders(col= "black",
             lwd = 1.5) +
  tm_layout(legend.outside = TRUE) +
  tm_compass(type = "4star", 
             position = c("right", "top")) +
  tm_scale_bar(text.size = 0.75,
               position = c("left", "bottom"))

utah_map <- us_states |>
 filter(NAME == "Utah") 

utah_map <- tm_shape(utah_map) +
  tm_borders() +
  tm_layout(title = "Utah",
            title.position = c("right", "top"),
            title.size = 1) +
  tm_shape(zion) +
  tm_borders()

library(grid) 
zion_map
print(utah_map, vp = viewport(0.8, 0.27, width = 0.5, height = 0.5))

#tmap_mode("plot") to get out of interactive version


## ----include=TRUE-----------------------------------------------------
#tmap version
world_map <- tm_shape(world) +
  tm_polygons(col = "gdpPercap",
              breaks = c(0, 10, 20, 30, 40, 50, 60, 70)*1000,
              title = "GDP Per Capita",
              palette = "GnBu")

tmap_mode("view")
world_map

#leaflet version
world_map_leaflet <- leaflet(world) |> 
  addTiles() 

pal <- colorQuantile(
  palette = "GnBu",
  domain = world$gdpPercap, n = 5)

world_map_leaflet |>
  addPolygons(stroke = FALSE,
              fillOpacity = 1,
              color = ~pal(gdpPercap)) |> 
  addLegend("bottomright", 
            pal = pal, 
            values = ~gdpPercap,
            title = "GDP Per Capita",
            opacity = 1)


## ----include=TRUE-----------------------------------------------------
tmap_mode("plot") 

worst_map <- tm_shape(world) +
  tm_polygons(col = "lifeExp",
              title = "life expectancy",
              palette = "Paired",
              legend.hist = TRUE,
              lty = "dashed",
              lwd = 4,
              bg.color = "red") +
  tm_layout(legend.position = c("center", "TOP"),
            legend.text.color = "green",
            legend.text.size = 0.5,
            legend.bg.color = "red",
            legend.bg.alpha = 0.3,
            bg.color = "yellow3",
            title = "WORST MAP EVER",
            title.size = 5,
            title.position = c("left", "bottom")) 

worst_map

