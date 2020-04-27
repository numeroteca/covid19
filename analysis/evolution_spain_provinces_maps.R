# 8. Mapas ---------
# Prepare mapas de coropletas con tmap ---------------------------------------------------------------------
library(tmap)
library(gpclib)
library(maptools)
library(rgdal)

# provincias <- readOGR("data/original/spain/shapes/ll_provinciales_inspire_peninbal_etrs89.json")
# provincias <- readOGR("data/original/spain/shapes/ll_provinciales_inspire_peninbal_etrs89_simple.json")
provincias <- readOGR("data/original/spain/shapes/recintos_provinciales_inspire_peninbal_etrs89.json")

provincias@data$ine_code <- substr(provincias@data$NATCODE,5,6) 
provincias@data$ine_code <- as.integer(provincias@data$ine_code)

ppp <- provincias@data


# adds airbnb data to shapes
provincias@data <- left_join(provincias@data, data_cases_sp_provinces %>% filter(date == max(date) ) %>% select(ine_code,deceassed_per_100000,date)  , by= c("ine_code" = "ine_code"))


# tmap numer of listings MAP ----------------
colores <- c("#ededed", "#0cb2ff")
breaks.n <- c(seq(0,2000,by = 200))

ppp <- provincias@data
names(ppp)

# provincias@data$initial <- substr(provincias@data$NAMEUNIT,1,2)
provincias@data$dp1 <- round(provincias@data$deceassed_per_100000 * 10,digits = 1)

png(filename="img/spain/maps/mapa-coropletas-muertos-per-100000.png",width = 800,height = 400)
tm_shape(provincias) +
  tm_polygons(col="dp1",
              palette = colores,
              breaks = breaks.n,
              style = "cont",
              title = "Cumulative deaths per million population",
              border.alpha = 1, lwd = 0.2, legend.show = T, legend.outside = T,
              textNA="no data",
              colorNA = "#FFEEEE"
  ) +
  tm_text("dp1",
          size= 0.8, alpha = 0.8) +
  tm_layout(between.margin = 1, frame = FALSE,
            fontfamily = "Roboto Condensed", 
            # title = "Data: Providencialdata19. By: @numeroteca. lab.montera34.com/covid19" ,
            title = paste(as.character( provincias@data$date[1] ), "COVID-19" ),
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
  tm_legend(legend.text.size = 1.2, 
            legend.title.size = 2) 
dev.off()

for (i in 8:length( unique(data_cases_sp_provinces$date) )) {
  print(unique(data_cases_sp_provinces$date)[i])
  provincias <- readOGR("data/original/spain/shapes/recintos_provinciales_inspire_peninbal_etrs89.json")
  
  provincias@data$ine_code <- substr(provincias@data$NATCODE,5,6) 
  provincias@data$ine_code <- as.integer(provincias@data$ine_code)
  
  provincias@data <- left_join(provincias@data, data_cases_sp_provinces %>% filter(date == unique(data_cases_sp_provinces$date)[i] ) %>% select(ine_code,deceassed_per_100000,date)  , by= c("ine_code" = "ine_code"))
  # ppp <- provincias@data
  provincias@data$dp1 <- round(provincias@data$deceassed_per_100000 * 10,digits = 1)
  
  png(filename= paste0("img/spain/maps/mapa-coropletas-muertos-per-100000_", unique(data_cases_sp_provinces$date)[i] ,".png"),width = 800,height = 400)
  mapprint <- tm_shape(provincias) +
    tm_polygons(col="dp1",
                palette = colores,
                breaks = breaks.n,
                style = "cont",
                title = "Cumulative deaths per million population",
                border.alpha = 1, lwd = 0.2, legend.show = T, legend.outside = T,
                textNA="no data",
                colorNA = "#FFEEEE"
    ) +
    tm_text("dp1",
            size= 0.8, alpha = 0.8) +
    tm_layout(between.margin = 1, frame = FALSE,
              fontfamily = "Roboto Condensed", 
              # title = "Data: Providencialdata19. By: @numeroteca. lab.montera34.com/covid19" ,
              title = paste(as.character( provincias@data$date[1] ), "COVID-19" ),
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
    tm_legend(legend.text.size = 1.2, 
              legend.title.size = 2) 
  
  print(mapprint)
  dev.off()
  
}

# convert -delay 40 -loop 0 mapa-coropletas-muertos-per-100000_2*.png animated-map-by-million-cumulative-deaths.gif
