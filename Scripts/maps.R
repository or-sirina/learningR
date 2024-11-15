install.packages("spDataLarge", repos = "https://geocompr.r-universe.dev")


install.packages("sf")
install.packages("raster")
install.packages("dplyr")
install.packages("spData")
install.packages("tmap")    
install.packages("leaflet") 
install.packages("ggplot2") 
install.packages("grid")
install.packages("gifski")
install.packages("mapdeck")


library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    
library(leaflet) 
library(ggplot2) 
library(grid)
library(gifski)


#простые карты
tm_shape(nz) +
  tm_fill() 

#границы
tm_shape(nz) +
  tm_borders() 

#цвет + границы
tm_shape(nz) +
  tm_fill() +
  tm_borders() 


#сравнение карт по кастомизации
carta = tm_shape(nz) + tm_polygons()

carta_voda = st_union(nz) %>% st_buffer(22224) %>% 
  st_cast(to = "LINESTRING")


eshio_carta = carta +
  tm_shape(carta_voda) + tm_lines()


i_eshio_carta = eshio_carta +
  tm_shape(nz_height) + tm_dots(size = 3)


tmap_arrange(carta, eshio_carta, i_eshio_carta)



#карта с легендой
tm_shape(nz) + tm_fill(col = "Land_area")


#легенда с математическими выражениями
legend_title = expression("Area (km"^2*")")

carta_kakaya_to = tm_shape(nz) +
  tm_fill(col = "Land_area", title = legend_title) + tm_borders()

carta_kakaya_to


#категоризация чисоенной переменной
tm_shape(nz) + tm_polygons(col = "Median_income")
breaks = c(0, 3, 4, 5) * 10000
tm_shape(nz) + tm_polygons(col = "Median_income", breaks = breaks)
tm_shape(nz) + tm_polygons(col = "Median_income", n = 10)
tm_shape(nz) + tm_polygons(col = "Median_income", palette = "BuGn")


#ещё две раскраски
tm_shape(nz) + tm_polygons("Population", palette = "Blues")
tm_shape(nz) + tm_polygons("Population", palette = "YlOrBr")


#добавление картографической аттрибутики
carta + 
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)


#tm_layout()
carta_kakaya_to + tm_layout(sepia.intensity = 0.8)


#готовые стили
carta_kakaya_to + tm_style("bw")
carta_kakaya_to + tm_style("classic")
carta_kakaya_to + tm_style("cobalt")
carta_kakaya_to + tm_style("col_blind")


#карты - фасеты
urb_1970_2030 = urban_agglomerations %>% 
  filter(year %in% c(1970, 1990, 2010, 2030))

#базовая карта
tm_shape(world) +
  tm_polygons() +
  tm_shape(urb_1970_2030) +
  tm_symbols(col = "black", border.col = "white", size = "population_millions") +
  tm_facets(by = "year", nrow = 2, free.coords = FALSE)

#кастомизация
tm_shape(world) +
  tm_polygons(col = "pop") +
  tm_shape(urb_1970_2030) +
  tm_symbols(col = "population_millions", border.col = "white", size = "population_millions") +
  tm_facets(by = "year", nrow = 2, free.coords = FALSE) + tm_style("col_blind")


#мечта любого американиста
tm_shape(us_states) + tm_polygons()

st_crs(us_states)

us_states_map <- tm_shape(us_states, projection = 2163) + tm_polygons() + 
  tm_layout(frame = FALSE)
us_states_map


#добавляем Аляску
hawaiiiiiiiii = tm_shape(hawaii) + tm_polygons() + 
  tm_layout(title = "Hawaii", frame = FALSE, bg.color = NA, 
            title.position = c("LEFT", "BOTTOM"))
alaskaaaaaaa = tm_shape(alaska) + tm_polygons() + 
  tm_layout(title = "Alaska", frame = FALSE, bg.color = NA)


#объединяем Америку
us_states_map
print(hawaiiiiiiiii, vp = viewport(0.35, 0.1, width = 0.2, height = 0.5))
print(alaskaaaaaaa, vp = viewport(0.15, 0.15, width = 0.3, height = 0.3))


#анимациииия
jojo <- tm_shape(world) + tm_polygons() + 
  tm_shape(urban_agglomerations) + tm_dots(size = "population_millions") +
  tm_facets(along = "year", free.coords = FALSE)

tmap_animation(jojo, filename = "jojo.gif", delay = 40)


#интерактивные карты
tmap_mode("view")

carta_kakaya_to

us_states_map

tmap_mode("plot")

#запредельная красота 2.5 Д ульра хд блюрей
library(mapdeck)

#токен для доступа к инструментам мапбокс
set_token(Sys.getenv("MAPBOX"))

#получаем и очищаем данные
crash_data = read.csv("https://raw.githubusercontent.com/uber-common/deck.gl-data/master/examples/3d-heatmap/heatmap-data.csv")
crash_data = na.omit(crash_data)

#записываем стиль карты
ms = mapdeck_style("dark")


mapdeck(style = ms, pitch = 45, location = c(0, 52), zoom = 4) %>%
  add_grid(data = crash_data, lat = "lat", lon = "lng", cell_size = 1000,
           elevation_scale = 50, layer_id = "grid_layer",
           colour_range = viridisLite::plasma(6))
