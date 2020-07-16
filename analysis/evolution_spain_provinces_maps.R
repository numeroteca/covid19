# 8. Mapas ---------
# Prepare mapas de coropletas con tmap ---------------------------------------------------------------------

# Load libraries ------
library(tidyverse)
library(tmap)
library(gpclib)
library(maptools)
library(rgdal)

# load data -----
data_cases_sp_provinces <- readRDS(file = "data/output/spain/covid19-provincias-spain_consolidated.rds")

# Load shapes
provincias <- readOGR("data/original/spain/shapes/recintos_provinciales_inspire_peninbal_etrs89.json")
canarias <- readOGR("data/original/spain/shapes/recintos_provinciales_inspire_canarias_etrs89_simply.json")

# Create INE code from existing variable
provincias@data$ine_code <- substr(provincias@data$NATCODE,5,6) 
provincias@data$ine_code <- as.integer(provincias@data$ine_code)

canarias@data$ine_code <- substr(canarias@data$NATCODE,5,6)
canarias@data$ine_code <- as.integer(canarias@data$ine_code)

# get max date in the data
maxdate <-  max(data_cases_sp_provinces$date)

# adds data to shapes
# provincias@data is the way to access the data stored in the shapes
provincias@data <- left_join(provincias@data, data_cases_sp_provinces %>% filter(date == maxdate-2 ) %>% select(ine_code,deceassed_per_100000,province,date,poblacion)  , by= c("ine_code" = "ine_code"))
canarias@data <- left_join(canarias@data, data_cases_sp_provinces %>% filter(date == maxdate-2 ) %>% select(ine_code,deceassed_per_100000,province,date,poblacion)  , by= c("ine_code" = "ine_code"))

# tmap settings ----------------
colores <- c("#ededed", "#0cb2ff")
breaks.n <- c(seq(0,2400,by = 200))

# Calculates data per million (current data is per 100,000)
provincias@data$dp1 <- round(provincias@data$deceassed_per_100000 * 10,digits = 0)
# creates new label with first two charecters of province
provincias@data$label <- paste(provincias@data$dp1, substr(provincias@data$province,1,2))
# adds number to label
provincias@data <- provincias@data %>% mutate( label = ifelse(is.na(province),"",label),
                                               label = ifelse(is.na(deceassed_per_100000),"",label),
)

# do the same for Canarias shapes
canarias@data$dp1 <- round(canarias@data$deceassed_per_100000 * 10,digits = 0)
canarias@data$label <- paste(canarias@data$dp1, substr(canarias@data$province,1,2))
canarias@data <- canarias@data %>% mutate( label = ifelse(is.na(province),"",label),
                                               label = ifelse(is.na(deceassed_per_100000),"",label),
)

# Print map
png(filename="img/spain/maps/mapa-coropletas-muertos-per-100000_peninsula.png",width = 1100,height = 400)
tm_shape(provincias) +
  tm_polygons(col="dp1",
              palette = colores,
              breaks = breaks.n,
              # style = "cont",
              title = "Total deaths per million population (escovid19data)",
              border.alpha = 1, lwd = 0.4, legend.show = T, legend.outside = T,
              textNA="no data",
              colorNA = "#FFEEEE",
              legend.hist = TRUE
  ) +
  tm_text("label",
          size= 0.9, alpha = 0.8) +
  tm_layout(between.margin = 1, frame = FALSE,
            fontfamily = "Roboto Condensed", 
            title = "Data: Providencialdata19. By: @numeroteca. lab.montera34.com/covid19",
            # # title = paste(as.character( provincias@data$date[1] ), "COVID-19" ),
            title.fontface = "bold",
            title.size = 2,
            # # legend.title.size = 1.9,
            legend.format = list(text.separator = "-" ),
            # # legend.text.size = 1,
            legend.width = 2,
            # # title.position = "TOP",
            legend.outside = TRUE,
            legend.outside.position = c("left","bottom")
  ) +
  tm_legend(legend.text.size = 1.3, 
            legend.title.size = 1.4
            ) 
dev.off()


png(filename="img/spain/maps/mapa-coropletas-muertos-per-100000_canarias.png",width = 1100,height = 400)
tm_shape(canarias) +
  tm_polygons(col="dp1",
              palette = colores,
              breaks = breaks.n,
              style = "cont",
              title = "Total deaths per million population (escovid19data)",
              border.alpha = 1, lwd = 0.2, legend.show = T, legend.outside = T,
              textNA="no data",
              colorNA = "#FFEEEE"
  ) +
  tm_text("label",
          size= 0.9, alpha = 0.8) +
  tm_layout(between.margin = 1, frame = FALSE,
            fontfamily = "Roboto Condensed", 
            # title = "Data: Providencialdata19. By: @numeroteca. lab.montera34.com/covid19" ,
            title = paste(as.character( provincias@data$date[1] ), "COVID-19" ),
            title.fontface = "bold",
            title.size = 2,
            # legend.title.size = 1.9,
            legend.format = list(text.separator = "-" ),
            # legend.text.size = 1,
            legend.width = 2,
            # title.position = "TOP",
            legend.outside = TRUE,
            legend.outside.position = c("left","bottom")
  ) +
  tm_legend(legend.text.size = 1.3, 
            legend.title.size = 1.4
  ) 
dev.off()


# mapa conjunto -------------------
# Learning how to do this from https://enrdados.netlify.app/post/mapas-con-tmap/
pen_map <- tm_shape(provincias) +
  tm_polygons(col="dp1",
              palette = colores,
              breaks = breaks.n,
              # style = "cont",
              title = "Total deaths per million population (escovid19data)",
              border.alpha = 1, lwd = 0.4, legend.show = T, legend.outside = T,
              textNA="no data",
              colorNA = "#FFEEEE",
              legend.hist = TRUE
  ) +
  tm_text("label",
          size= 0.9, alpha = 0.8) +
  tm_layout(between.margin = 1, frame = FALSE,
            fontfamily = "Roboto Condensed", 
            title = paste(as.character( provincias@data$date[1] ), "COVID-19" ),
            title.fontface = "bold",
            title.size = 2,
            legend.format = list(text.separator = "-" ),
            legend.width = 2,
            legend.outside = TRUE,
            legend.outside.position = c("left","bottom")
  ) +
  tm_legend(legend.text.size = 1.3, 
            legend.title.size = 1.4
  ) 

canarias_map <- tm_shape(canarias) +
  tm_polygons(col="dp1",
              palette = colores,
              breaks = breaks.n,
              # style = "cont",
              title = "",
              border.alpha = 1, lwd = 0.2, legend.show = T, legend.outside = T,
              textNA="",
              colorNA = "#FFEEEE"
  ) +
  tm_text("label",
          size= 0.9, alpha = 0.8) +
  tm_layout(between.margin = 1, frame = TRUE,
            fontfamily = "Roboto Condensed",
            legend.show = FALSE
  ) +
  tm_legend(legend.text.size = 1.3, 
            legend.title.size = 1.4
  ) 


# junto los dos    
png(filename="img/spain/maps/mapa-coropletas-muertos-per-100000.png",width = 1100,height = 400)
pen_map
print(canarias_map, vp = grid::viewport(0.40, 0.12, width = 0.25, height = 0.18))
dev.off()

# Generate one map per day -----
# for (i in 8:length( unique(data_cases_sp_provinces$date) )) {
# for (i in 74:74) {
for (i in 86:length( unique(data_cases_sp_provinces$date) )) {
  print(unique(data_cases_sp_provinces$date)[i])
  print(i)
  provincias <- readOGR("data/original/spain/shapes/recintos_provinciales_inspire_peninbal_etrs89.json")
  canarias <- readOGR("data/original/spain/shapes/recintos_provinciales_inspire_canarias_etrs89_simply.json")

  provincias@data$ine_code <- substr(provincias@data$NATCODE,5,6) 
  provincias@data$ine_code <- as.integer(provincias@data$ine_code)
  
  canarias@data$ine_code <- substr(canarias@data$NATCODE,5,6)
  canarias@data$ine_code <- as.integer(canarias@data$ine_code)
  
  provincias@data <- left_join(provincias@data, data_cases_sp_provinces %>% filter(date == unique(data_cases_sp_provinces$date)[i] ) %>% select(ine_code,deceassed_per_100000,date)  , by= c("ine_code" = "ine_code"))
  canarias@data <- left_join(canarias@data, data_cases_sp_provinces %>% filter(date == unique(data_cases_sp_provinces$date)[i] ) %>% select(ine_code,deceassed_per_100000,date)  , by= c("ine_code" = "ine_code"))

  provincias@data$dp1 <- round(provincias@data$deceassed_per_100000 * 10,digits = 0)
  provincias@data$label <- paste(provincias@data$dp1, substr(provincias@data$province,1,1))

  canarias@data$dp1 <- round(canarias@data$deceassed_per_100000 * 10,digits = 0)
  canarias@data$label <- paste(canarias@data$dp1, substr(canarias@data$province,1,1))
    
  provincias@data <- provincias@data %>% mutate( label = ifelse(is.na(province),"",label),
                                                 label = ifelse(is.na(deceassed_per_100000),"",label),
                                                 )

  canarias@data <- canarias@data %>% mutate( label = ifelse(is.na(province),"",label),
                                                 label = ifelse(is.na(deceassed_per_100000),"",label),
  )
    
  png(filename= paste0("img/spain/maps/mapa-coropletas-muertos-per-100000_", unique(data_cases_sp_provinces$date)[i] ,".png"),width = 1000,height = 400)
  # mapprint <- tm_shape(provincias) +
  tm_shape(provincias) +
    tm_polygons(col="dp1",
                palette = colores,
                breaks = breaks.n,
                # style = "cont",
                title = "Total deaths per million population (escovid19data)",
                border.alpha = 1, lwd = 0.4, legend.show = T, legend.outside = T,
                textNA="no data",
                colorNA = "#FFEEEE",
                legend.hist = TRUE
    ) +
    tm_text("label",
            size= 0.9, alpha = 0.8) +
    tm_layout(between.margin = 1, frame = FALSE,
              fontfamily = "Roboto Condensed", 
              # title = "Data: Providencialdata19. By: @numeroteca. lab.montera34.com/covid19" ,
              title = paste(as.character( provincias@data$date[10] ), "COVID-19" ),
              title.fontface = "bold",
              title.size = 2,
              # legend.title.size = 1.9,
              legend.format = list(text.separator = "-" ),
              # legend.text.size = 1,
              # legend.width = 0.9,
              # title.position = "TOP",
              legend.outside = TRUE,
              legend.outside.position = c("left","bottom")
    ) +
    tm_legend(legend.text.size = 1.3, 
              legend.title.size = 1.4) 
  
  
  canarias_map <- tm_shape(canarias) +
    tm_polygons(col="dp1",
                palette = colores,
                breaks = breaks.n,
                # style = "cont",
                border.alpha = 1, lwd = 0.4, legend.show = T, legend.outside = T,
                textNA="no data",
                colorNA = "#FFEEEE",
                legend.hist = TRUE
    ) +
    tm_text("label",
            size= 0.9, alpha = 0.8) +
    tm_layout(between.margin = 1, frame = TRUE,
              fontfamily = "Roboto Condensed",
              legend.show = FALSE
    ) +
    tm_legend(legend.text.size = 1.3, 
              legend.title.size = 1.4
    ) 
  
  print(mapprint)
  print(canarias_map, vp = grid::viewport(0.35, 0.12, width = 0.25, height = 0.18))
  dev.off()
  
}

# convert -delay 55 -loop 0 mapa-coropletas-muertos-per-100000_2*.png animated-map-by-million-cumulative-deaths.gif

# Contagios en los últimos 14 días -----

zzzzzzz <- data_cases_sp_provinces %>%  
  select(ine_code,cases_per_cienmil,date) %>% mutate(
    incidencia14 = round( (cases_per_cienmil - lag(cases_per_cienmil,14) ) * 10,digits = 0)
  ) 
# %>% filter(date == unique(data_cases_sp_provinces$date)[i] )

for (i in 8:length( unique(data_cases_sp_provinces$date) )) {
  # for (i in 58:length( unique(data_cases_sp_provinces$date) )) {
  print(unique(data_cases_sp_provinces$date)[i])
  provincias <- readOGR("data/original/spain/shapes/recintos_provinciales_inspire_peninbal_etrs89.json")
  
  provincias@data$ine_code <- substr(provincias@data$NATCODE,5,6) 
  provincias@data$ine_code <- as.integer(provincias@data$ine_code)
  
    provincias@data <- left_join(provincias@data, data_cases_sp_provinces %>% 
                                 select(ine_code,cases_per_cienmil,date) %>% mutate(
                                   incidencia14 = round( (cases_per_cienmil - lag(cases_per_cienmil,14) ) * 10,digits = 0)
                                 ) %>% filter(date == unique(data_cases_sp_provinces$date)[i] ),
                               by= c("ine_code" = "ine_code"))
  
  provincias@data$label <- paste(provincias@data$incidencia14, substr(provincias@data$province,1,1))
  
  provincias@data <- provincias@data %>% mutate( label = ifelse(is.na(province),"",label),
                                                 label = ifelse(is.na(incidencia14),"",label),
  )
  
  png(filename= paste0("img/spain/maps/i14/mapa-coropletas-incidencia-14-dias-per-100000_", unique(data_cases_sp_provinces$date)[i] ,".png"),width = 1000,height = 400)
  mapprint <- tm_shape(provincias) +
    tm_polygons(col="incidencia14",
                palette = colores,
                # breaks = breaks.n,
                style = "cont",
                title = "Nuevos casos por millón (14 días) (escovid19data)",
                border.alpha = 1, lwd = 0.2, legend.show = T, legend.outside = T,
                textNA="no data",
                colorNA = "#FFEEEE"
    ) +
    tm_text("label",
            size= 0.9, alpha = 0.8) +
    tm_layout(between.margin = 1, frame = FALSE,
              fontfamily = "Roboto Condensed", 
              # title = "Data: Providencialdata19. By: @numeroteca. lab.montera34.com/covid19" ,
              title = paste(as.character( provincias@data$date[10] ), "COVID-19" ),
              title.fontface = "bold",
              title.size = 2,
              # legend.title.size = 1.9,
              legend.format = list(text.separator = "-" ),
              # legend.text.size = 1,
              # legend.width = 0.9,
              # title.position = "TOP",
              legend.outside = TRUE,
              legend.outside.position = c("left","bottom")
    ) +
    tm_legend(legend.text.size = 1.3, 
              legend.title.size = 1.4) 
  
  print(mapprint)
  dev.off()
  
}


# ------------------
# Export geojon ---
data_cases_sp_provinces <- readRDS(file = "data/output/spain/covid19-provincias-spain_consolidated.rds")

# Load shapes
provincias <- readOGR("data/original/spain/shapes/recintos_provinciales_inspire_peninbal_etrs89.json")
canarias <- readOGR("data/original/spain/shapes/recintos_provinciales_inspire_canarias_etrs89_simply.json")

# Create INE code
provincias@data$ine_code <- substr(provincias@data$NATCODE,5,6) 
provincias@data$ine_code <- as.integer(provincias@data$ine_code)

canarias@data$ine_code <- substr(canarias@data$NATCODE,5,6)
canarias@data$ine_code <- as.integer(canarias@data$ine_code)


maxdate <-  max(data_cases_sp_provinces$date)

# adds airbnb data to shapes
provincias@data <- left_join(provincias@data, data_cases_sp_provinces %>% filter(date == maxdate-2 ) %>% select(-comments,-source) , by= c("ine_code" = "ine_code"))
canarias@data <- left_join(canarias@data, data_cases_sp_provinces %>% filter(date == maxdate-2 ) %>% select(-comments,-source), by= c("ine_code" = "ine_code"))


proj4string(canarias) <- proj4string(provincias)
est <- rbind.SpatialPolygonsDataFrame( provincias, canarias )
# xx <- est@data

# 1. export geojson
writeOGR(est, "interactive/cartograma/provincias_geojson.json", layer="objects", driver="GeoJSON")
# 2. convert to topjson in mapshaper.io (and simplify)
# 3. create cartogram with topojson


# animation ------------
library(gganimate)
library(gifski)

  
zszs <- data_cases_sp_provinces %>% # filter(date == maxdate-2 ) %>%
ggplot( aes(poblacion,deceassed_per_100000) ) +
  # geom_point() +
  # geom_text_repel( aes(label= substr(province,1,3))  ) +
  geom_text( aes(label= substr(province,1,3))  ) +
  scale_x_continuous(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) 
    )+
  transition_time(date) +
  ease_aes('linear') +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    # axis.ticks.x = element_line(color = "#000000"),
    # axis.text.x = element_text(size = 9)
    # legend.position = "bottom"
  ) +
  labs(title = "Muertes COVID-19 / 100.000 hab VS Población",
       subtitle = paste0("por provincias en España. {frame_time}"),
       y = "Fallecimientos por 100.000 habitantes",
       x = "población",
       caption = "@numeroteca | Datos: esCOVID19data")

print(zszs)
anim_save(filename = "animation-03.gif", animation = zszs, path = "tmp",width = 900,height = 500)

