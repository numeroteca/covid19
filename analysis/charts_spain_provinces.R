# Genera gráficos de Coronavirus COVID-19 en España por provincia
# Debe ejecutarse antes el script que procesa los datos : process_spain_provinces_data.R
# o descargarse los datos de https://github.com/montera34/escovid19data/blob/master/data/output/covid19-provincias-spain_consolidated.csv
# Los gráficos generados pueden verse en https://lab.montea34.com/covid9

# Este archivo era antes "evolution_spain_provinces.R"

library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping

# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption_en <- "By: lab.montera34.com/covid19 | Data: EsCOVID19data. Check code.montera34.com/covid19"
caption_provincia <- "Gráfico: @numeroteca (montera34.com) | Datos: esCOVID19data (github.com/montera34/escovid19data, lab.montera34.com/covid19)"
period <- "Actualizado: 2020-05-18. Para CCAA uniprovinciales casos es la suma de PCR+ y TestAc+ a partir de 2020.04.15"
filter_date <- as.Date("2020-05-18")

# Warning: you need to have loaded data_cases_sp_provinces by executing process_spain_provinces_data.R 
# or load it using:
data_cases_sp_provinces <- readRDS(file = "data/output/spain/covid19-provincias-spain_consolidated.rds")
data_cases_sp_provinces <- data_cases_sp_provinces %>% filter( (date > as.Date("2020-02-25") ) & ( date < filter_date ) )

# Set colors ---------
# extends color paletter
library(RColorBrewer)
# creates extended color palette https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
colourCount <- length(unique(data_cases_sp_provinces$ccaa))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
colors_prov <- getPalette(colourCount )
# Change yellow to blue
colors_prov[1] <- "#c60000"
colors_prov[12] <- "#84d3e7"

# Plots --------------------
# / 1. Cases ------------

# create temp dataframes to be able to plot all the values in small multiples
data_cases_sp_provinces_sm <- data_cases_sp_provinces %>% filter( date < filter_date)
data_cases_sp_provinces_sm$province_cp <- data_cases_sp_provinces[data_cases_sp_provinces$date < filter_date,]$province 

# Remove last day
data_cases_sp_provinces <- data_cases_sp_provinces %>% filter( date < filter_date)

# // 1.1 Small multiple ------------
# /// Provincias small multiple --------------
# Escala lineal
png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-lineal.png", sep = ""),width = 1400,height = 1000)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = data_cases_sp_provinces_sm %>% ungroup() %>% select(date,cases_accumulated,province_cp,-province) ,
            aes(date,cases_accumulated,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_accumulated,group=province), size = 1 ) +
  geom_point(aes(date,cases_accumulated,group=province), size = 0.5 ) +
  facet_wrap( ~province) +
  scale_y_continuous(
    limits = c(0,max(data_cases_sp_provinces$cases_accumulated) ),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9)
    # legend.position = "bottom"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# Escala logarítmica
png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-log.png", sep = ""),width = 1400,height = 1000)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = data_cases_sp_provinces_sm %>% ungroup() %>% select(date,cases_accumulated,province_cp,-province) ,
            aes(date,cases_accumulated,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_accumulated,group=province), size = 1 ) +
  geom_point(aes(date,cases_accumulated,group=province), size = 0.5 ) +
  scale_y_log10(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    limits = c(1,max(data_cases_sp_provinces$cases_accumulated)),
    minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  facet_wrap( ~province) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-region-grouped-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% 
  ggplot() +
  geom_line(aes(date,cases_accumulated,group=province, color = province), size = 0.7 ) +
  geom_text_repel(data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(cases_accumulated) ) %>% top_n(1, date),
                  aes(date+1, cases_accumulated, label=paste(format(cases_accumulated, nsmall=0, big.mark="."), substr(province,1,2) ) ),
                  nudge_x = 1, # adjust the starting x position of the text label
                  size=4,
                  hjust=0,
                  color = "#666666",
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  coord_cartesian(
    xlim = c( min(data_cases_sp_provinces$date + 9), max(data_cases_sp_provinces$date +24))
    # ylim = c(1, max( data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceased),]$deceased))
  ) +
  scale_y_continuous(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  facet_wrap( ~ccaa, scales = "free") +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma y provincias (escala lin.). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-region-grouped-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,cases_accumulated,province_cp,-province),
            aes(date,cases_accumulated,group=province_cp), color="#DBDBDB" ) +
  geom_line(aes(date,cases_accumulated,group=province, color = province), size = 0.7 ) +
  geom_text_repel(data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(cases_accumulated) ) %>% top_n(1, date),
                  aes(date+1, cases_accumulated, label=paste(format(cases_accumulated, nsmall=0, big.mark="."), substr(province,1,2) ) ),
                  nudge_x = 1, # adjust the starting x position of the text label
                  size=4,
                  hjust=0,
                  color = "#666666",
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  coord_cartesian(
    xlim = c( min(data_cases_sp_provinces$date + 9), max(data_cases_sp_provinces$date +19))
    # ylim = c(1, max( data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceased),]$deceased))
  ) +
  scale_y_log10(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    limits = c(1,max(data_cases_sp_provinces$cases_accumulated)*1.2),
    minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  facet_wrap( ~ccaa) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma y provincias (escala log.). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# // 1.2 Superpuesto ---------------
png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-superpuesto-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date,cases_accumulated,group=province, color=ccaa ), size = 0.8 ) +
  geom_point(aes(date,cases_accumulated,group=province, color=ccaa), size = 0.6 ) +
  geom_text_repel(data = data_cases_sp_provinces %>% group_by(province) %>% filter(!is.na(cases_accumulated) & cases_accumulated > 5000  ) %>% top_n(1, date),
    # data=filter( data_cases_sp_provinces,  
    #                            (date==max(data_cases_sp_provinces$date) & cases_accumulated > 1100 )  
    #                       ), 
        aes(date, cases_accumulated, color=ccaa, label=paste(format(cases_accumulated, nsmall=0, big.mark="."),province)),
              nudge_x = 3, # adjust the starting y position of the text label
              size=5,
              hjust=0,
              family = "Roboto Condensed",
              direction="y",
              segment.size = 0.1,
              segment.color="#777777"
  ) +
  # geom_text_repel(data=filter( data_cases_sp_provinces,  
  #                               (date==max(data_cases_sp_provinces$date-2) & province == "Barcelona")
  #                                       ), 
  #           aes(date, cases_accumulated, color=ccaa, label=paste(format(cases_accumulated, nsmall=1, big.mark="."),province)),
  #                     nudge_y = 2, # adjust the starting y position of the text label
  #                     size=5,
  #                     hjust=0,
  #                     family = "Roboto Condensed",
  #                     direction="y",
  #                     segment.size = 0.1,
  #                     segment.color="#777777"
  # ) +
  scale_color_manual(values = colors_prov) +
  scale_y_continuous(
    limits = c(0,max(data_cases_sp_provinces$cases_accumulated)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date + 22)),
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-superpuesto-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date,cases_accumulated,group=province, color=ccaa ), size = 0.8 ) +
  geom_point(aes(date,cases_accumulated,group=province, color=ccaa), size = 0.6 ) +
  geom_text_repel(data = data_cases_sp_provinces %>% group_by(province) %>% filter(!is.na(cases_accumulated) & cases_accumulated > 3000  ) %>% top_n(1, date),
                    # filter( data_cases_sp_provinces,
                    #            (date==max(data_cases_sp_provinces$date) & cases_accumulated > 1000 )  
                    #   ), 
                      aes(date, cases_accumulated, color=ccaa, label=paste(format(cases_accumulated, nsmall=0, big.mark=".", decimal.mark = ","),province)),
                      nudge_x = 6, # adjust the starting y position of the text label
                      size=5,
                      hjust=0,
                      family = "Roboto Condensed",
                      direction="y",
                      segment.size = 0.1,
                      segment.color="#777777"
  ) +
  # geom_text_repel(data=filter( data_cases_sp_provinces,  
  #                              # (date==as.Date("2020-04-14") & province == "Barcelona") |
  #                                (date==as.Date("2020-04-02") & ccaa == "Galicia")
  #                   ), 
  #                    aes(date, cases_accumulated, color=ccaa, label=paste(format(cases_accumulated, nsmall=0, big.mark="."),province)),
  #               nudge_y = 1, # adjust the starting y position of the text label
  #               size=5,
  #               hjust=0,
  #               family = "Roboto Condensed",
  #               # direction="y",
  #               segment.size = 0.1,
  #               segment.color="#777777"
  # ) +
  scale_color_manual(values = colors_prov) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 limits = c(1,max(data_cases_sp_provinces$cases_accumulated)*1.2),
                 # breaks = c(1,10,100,1000),
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000), seq(10000, 100000, 10000) ) ) +
  scale_x_date(date_breaks = "3 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date + 20)),
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.74)
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, cases_per_cienmil,group= province, color= ccaa), size= 0.8 ) +
  geom_point(aes(date,cases_per_cienmil, color=ccaa), size= 0.6 ) +
  geom_text_repel( data = data_cases_sp_provinces %>% group_by(province) %>% filter(!is.na(cases_per_cienmil) & cases_per_cienmil > 500  ) %>% top_n(1, date),
    # data=filter( data_cases_sp_provinces, 
    #                            date==max(data_cases_sp_provinces$date) 
  # ), 
  aes(date,cases_per_cienmil, color=ccaa, label=paste(format(cases_per_cienmil, nsmall=1, big.mark=".", digits = 1, decimal.mark = ","), province)),
          nudge_x = 5, # adjust the starting y position of the text label
          size=5,
          hjust=0,
          family = "Roboto Condensed",
          direction="y",
          segment.size = 0.1,
          segment.color="#333333"
  ) +
  scale_color_manual(values = colors_prov) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".",small.mark = ",", scientific = FALSE)
                      
                 ) +
  scale_x_date(date_breaks = "3 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date + 22)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-superpuesto-per-cienmil-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, cases_per_cienmil,group= province, color= ccaa), size= 0.8 ) +
  geom_point(aes(date,cases_per_cienmil, color=ccaa), size= 0.6 ) +
  geom_text_repel( data = data_cases_sp_provinces %>% group_by(province) %>% filter(!is.na(cases_per_cienmil) & cases_per_cienmil > 500  ) %>% top_n(1, date),
                   # data=filter( data_cases_sp_provinces, 
                   #                            date==max(data_cases_sp_provinces$date) 
                   # ), 
                      aes(date,cases_per_cienmil, color=ccaa, label=paste(format(cases_per_cienmil, nsmall=1, big.mark=".", digits = 1, decimal.mark = ","), province)),
                              nudge_x = 6, # adjust the starting y position of the text label
                              size=5,
                              hjust=0,
                              family = "Roboto Condensed",
                              direction="y",
                              segment.size = 0.1,
                              segment.color="#333333"
                              # xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2020-01-4"))
                      ) +
  scale_color_manual(values = colors_prov) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 limits = c(1,max( data_cases_sp_provinces[!is.na(data_cases_sp_provinces$cases_per_cienmil),]$cases_per_cienmil)*2),
                 minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  scale_x_date(date_breaks = "3 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date + 22)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()


# 2. Daily cases --------------

# SM Lineal
png(filename=paste("img/spain/provincias/covid19_casos-por-dia-provincia-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  # geom_smooth(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_cases_avg7,province_cp,-province),
  #             aes(date,daily_cases_avg7,group=province_cp), se = FALSE, span = 0.6, color="#CACACA", size=0.5 ) +
  # geom_smooth(aes(date, daily_cases_avg7,group=province), size= 0.7, se = FALSE, span = 0.6, color="#000000" ) +
  geom_point(aes(date, daily_cases), size= 0.5, alpha=1 ) +
  geom_text(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
            aes(date,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$daily_cases_avg7),]$daily_cases_avg7)/2, label=paste(format(daily_cases_avg7, nsmall=1, big.mark="."))),
            size=4,
            hjust=1,
            family = "Roboto Condensed"
  ) +
  facet_wrap(~province) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+15, max(data_cases_sp_provinces$date)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Casos por día (7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()


# for ( i in 1:52 ) {
# # for ( i in 1:2 ) {
# la_prov <- unique(data_cases_sp_provinces$province)[i]
# png(filename=paste0("img/spain/provincias/daily-deaths/covid19_casos-por-dia-provincia-lineal", substr(la_prov,1,4),".png"),width = 600,height = 400)
# the_prov <- data_cases_sp_provinces %>% filter ( province == la_prov ) %>%
#   ggplot() +
#   geom_point(aes(date, daily_cases), size= 2, alpha=1 ) +
#   # geom_text(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
#   #           aes(date,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$daily_cases_avg7),]$daily_cases_avg7)/4, label=paste(format(daily_cases_avg7, nsmall=1, big.mark="."))),
#   #           size=3,
#   #           hjust=1,
#   #           family = "Roboto Condensed"
#   # ) +
#   scale_x_date(date_breaks = "2 day", 
#                date_labels = "%d",
#                limits=c( min(data_cases_sp_provinces$date)+15, max(data_cases_sp_provinces$date)),
#                expand = c(0,1) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 20) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = c(0.1,0.6)
#   ) +
#   labs(title = paste0("",la_prov),
#        # subtitle = paste0("Casos por día"),
#        # y = "casos por día",
#        # x = "fecha"
#        y = "",
#        x = ""
#        # caption = caption_provincia
#   )
# 
# print(the_prov)
# dev.off()
# 
# }

# make tiled image with imagemagick from command line
# montage covid19_casos-por-dia-provincia-lineal* -geometry 400x tiles_02.png


# SM Lineal
# png(filename=paste("img/spain/provincias/covid19_casos-por-dia-provincia-media-lineal.png", sep = ""),width = 1200,height = 800)
# data_cases_sp_provinces %>%
#   ggplot() +
#   geom_smooth(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_cases_avg7,province_cp,-province),
#               aes(date,daily_cases_avg7,group=province_cp), se = FALSE, span = 0.6, color="#CACACA", size=0.5 ) +
#   geom_smooth(aes(date, daily_cases_avg7,group=province), size= 0.9, se = FALSE, span = 0.6, color  = "#444444") +
#   geom_point(aes(date, daily_cases), size= 0.5, alpha=0.3 ) +
#   geom_text(data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(daily_cases) ) %>% top_n(1, date),
#             aes(date,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$daily_cases_avg7),]$daily_cases_avg7)/1.2, label=paste(format(daily_cases_avg7, nsmall=1, big.mark="."))),
#             size=4,
#             hjust=1,
#             family = "Roboto Condensed"
#   ) +
#   facet_wrap(~province) +
#   scale_x_date(date_breaks = "7 day", 
#                date_labels = "%d",
#                limits=c( min(data_cases_sp_provinces$date)+15, max(data_cases_sp_provinces$date)),
#                expand = c(0,0) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = c(0.1,0.6),
#     axis.text.x = element_text(size = 9)
#   ) +
#   labs(title = "Media de casos por día (7 días) por COVID-19 en España",
#        subtitle = paste0("Por provincia (escala lineal). ",period),
#        y = "Meded de casos por día",
#        x = "fecha",
#        caption = caption_provincia)
# dev.off()

png(filename=paste("img/spain/provincias/covid19_casos-por-dia-provincia-media-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_smooth(aes(date, daily_cases_avg7,group=province, color=province), size= 1.1, se = FALSE, span = 0.6, color  = "#444444") +
  geom_point(aes(date, daily_cases), size= 0.5, alpha=0.3 ) +
  geom_text(data= data_cases_sp_provinces %>% group_by(province)%>% filter(!is.na(daily_cases) ) %>% top_n(1, date),
            aes(max(data_cases_sp_provinces$date)-10, daily_cases_avg7*2.5, label=paste(format(daily_cases_avg7, nsmall=1, big.mark=".", decimal.mark = ","))),
            size=4,
            hjust=0,
            color  = "#666666",
            family = "Roboto Condensed"
  ) +
  facet_wrap(~province, scale="free") +
  scale_x_date(date_breaks = "10 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+15, max(data_cases_sp_provinces$date) ),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none",
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 10)
  ) +
  labs(title = "Media de casos por día (7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "Meded de casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# SM Log
png(filename=paste("img/spain/provincias/covid19_casos-por-dia-provincia-media-log.png", sep = ""),width = 1200,height = 900)
data_cases_sp_provinces %>%
  ggplot() +
  geom_smooth(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_cases_avg7,province_cp,-province),
              aes(date,daily_cases_avg7,group=province_cp), se = FALSE, span = 0.6, color="#CACACA", size=0.5 ) +
  geom_smooth(aes(date, daily_cases_avg7,group=province), size= 0.7, se = FALSE, span = 0.6, color="#000000" ) +
  geom_point(aes(date, daily_cases), size= 0.7, alpha=0.3 ) +
  geom_text(data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(daily_cases) ) %>% top_n(1, date),
            aes(date,0.1, 
                label=paste(format(daily_cases_avg7, nsmall=1, big.mark="."))),
            size=4,
            hjust=1,
            family = "Roboto Condensed"
  ) +
  facet_wrap(~province) +
  scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+15, max(data_cases_sp_provinces$date)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Media de casos por día (7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "media de casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# SM CCAA grouped ------
png(filename=paste("img/spain/provincias/covid19_casos-por-dia-provincia-media-lineal_ccaa.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  # geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_cases_avg7,province_cp,-province),
  #           aes(date,daily_cases_avg7,group=province_cp), color="#CACACA" ) +
  # geom_line(aes(date, daily_cases_avg7,group=province) ) +
  geom_smooth(aes(date, daily_cases_avg7,group=province, color = province ), size= 1.2, se = FALSE, span = 0.7 ) +
  geom_line(aes(date, daily_cases_avg7,group=province), size= 0.4, color="#777777", alpha = 0.6 ) +
  # geom_point(aes(date, daily_cases_avg7), size= 0.5 ) +
  geom_text_repel(
    data = data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(daily_cases) ) %>% top_n(1, date),
    aes(date, daily_cases_avg7, label=paste(format(daily_cases_avg7, nsmall=1, big.mark="."), substr(province,1,2) ) ),
    nudge_x = 2, # adjust the starting x position of the text label
    size=4,
    hjust=0,
    family = "Roboto Condensed",
    direction="y",
    segment.size = 0.1,
    color = "#666666",
    segment.color="#777777"
  ) +
  facet_wrap(~ccaa, scales = "free_y") +
  coord_cartesian(
    xlim= c( as.Date("2020-03-15"),max(data_cases_sp_provinces$date)+20 )
  ) +
  scale_y_continuous( 
    # minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               expand = c(0,20) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "none"
  ) +
  labs(
    title = "Media de casos por día (ventana 7 días) por COVID-19 en España",
    subtitle = paste0("Por provincia (escala logarítmica). ",period),
    y = "media de casos por día",
    x = "fecha",
    caption = caption_provincia
  )
dev.off()

# SM CCAA grouped ------
png(filename=paste("img/spain/provincias/covid19_casos-por-dia-provincia-media-lineal-per-cienmil_ccaa.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_cases_avg7,province_cp,-province,poblacion),
            aes(date,daily_cases_avg7/poblacion*100000,group=province_cp), color="#CACACA", alpha = 0.3  ) +
  # geom_line(aes(date, daily_cases_avg7,group=province) ) +
  geom_smooth(aes(date, daily_cases_avg7/poblacion*100000,group=province, color = province ), size= 1.2, se = FALSE, span = 0.7 ) +
  geom_line(aes(date, daily_cases_avg7/poblacion*100000,group=province), size= 0.4, color="#777777") +
  # geom_point(aes(date, daily_cases_avg7), size= 0.5 ) +
  geom_text_repel(
    data = data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(daily_cases) ) %>% top_n(1, date),
    aes(date, daily_cases_avg7/poblacion*100000, label=paste(format( round(daily_cases_avg7/poblacion*100000, digits = 1), nsmall=1, big.mark="."), substr(province,1,2) ) ),
    nudge_x = 2, # adjust the starting x position of the text label
    size=4,
    hjust=0,
    family = "Roboto Condensed",
    direction="y",
    segment.size = 0.1,
    color = "#666666",
    segment.color="#777777"
  ) +
  facet_wrap(~ccaa) + #, scales = "free_y"
  coord_cartesian(
    xlim= c( as.Date("2020-03-15"),max(data_cases_sp_provinces$date) )
  ) +
  scale_y_continuous( 
    # minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               expand = c(0,15) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "none"
  ) +
  labs(
    title = "Media de casos por día (ventana 7 días) por COVID-19 por 100.000 habitantes en España",
    subtitle = paste0("Por provincia (escala logarítmica). ",period),
    y = "media de casos por día",
    x = "fecha",
    caption = caption_provincia
  )
dev.off()

png(filename=paste("img/spain/provincias/covid19_casos-por-dia-provincia-media-lineal_ccaa.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  # geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_cases_avg7,province_cp,-province),
  #           aes(date,daily_cases_avg7,group=province_cp), color="#CACACA" ) +
  # geom_line(aes(date, daily_cases_avg7,group=province) ) +
  geom_smooth(aes(date, daily_cases_avg7,group=province, color=province), size= 0.7, se = FALSE, span = 0.6 ) +
  # geom_point(aes(date, daily_cases_avg7), size= 0.5 ) +
  geom_text_repel(
    data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(daily_cases) ) %>% top_n(1, date),
    aes(date, daily_cases_avg7, label=paste(format(daily_cases_avg7, nsmall=1, big.mark="."), substr(province,1,2) ) ),
    nudge_x = 2, # adjust the starting x position of the text label
    size=4,
    hjust=0,
    color = "#666666",
    family = "Roboto Condensed",
    direction="y",
    segment.size = 0.1,
    segment.color="#777777"
  ) +
  facet_wrap(~ccaa, scales = "free") +
  coord_cartesian(
    xlim= c( as.Date("2020-03-15"),max(data_cases_sp_provinces$date) )
  ) +
  scale_y_continuous(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "10 day", 
               date_labels = "%d",
               expand = c(0,18) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Media de casos por día (medía 7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "media casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_casos-por-dia-provincia-media-log_ccaa.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_cases_avg7,province_cp,-province),
            aes(date,daily_cases_avg7,group=province_cp), color="#CACACA" ) +
  # geom_line(aes(date, daily_cases_avg7,group=province) ) +
  geom_smooth(aes(date, daily_cases_avg7,group=province), size= 0.7, se = FALSE, span = 0.6, color="#333333" ) +
  # geom_point(aes(date, daily_cases_avg7), size= 0.5 ) +
  geom_text_repel(
    data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(daily_cases) ) %>% top_n(1, date),
    aes(date, daily_cases_avg7, label=paste(format(daily_cases_avg7, nsmall=1, big.mark="."), substr(province,1,2) ) ),
    nudge_x = 2, # adjust the starting x position of the text label
    size=4,
    hjust=0,
    color = "#666666",
    family = "Roboto Condensed",
    direction="y",
    segment.size = 0.1,
    segment.color="#777777"
  ) +
  facet_wrap(~ccaa) +
  coord_cartesian(
    xlim= c( as.Date("2020-03-15"),max(data_cases_sp_provinces$date) )
  ) +
  scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               expand = c(0,22) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Media de casos por día (medía 7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "media casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# Superpuesto Lineal ------
png(filename=paste("img/spain/provincias/covid19_casos-por-dia-provincia-media-superpuesto-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, daily_cases,group=province, color=ccaa), size= 0.8 ) +
  geom_point(aes(date, daily_cases, color=ccaa), size= 1 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & daily_cases > 5), 
                  aes(date, daily_cases, color=ccaa, label=paste(format(daily_cases, nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  coord_cartesian(
    ylim = c( 0,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$daily_cases),]$daily_cases) )
  ) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  # scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #                minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
  #                expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+5, max(data_cases_sp_provinces$date +12)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Casos por día por COVID-19 en España",
       subtitle = paste0("Por provincia ",period),
       y = "casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_casos-por-dia-provincia-media-superpuesto-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% 
  ggplot() +
  geom_line(aes(date, daily_cases,group=province, color=ccaa), size= 0.5 ) +
  # geom_point(aes(date, daily_cases, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & daily_cases > 5), 
                  aes(date, daily_cases, color=ccaa, label=paste(format(daily_cases, nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  coord_cartesian(
    # ylim = c(1,500)
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 expand = c(0,0.2) ) +
  # scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #                minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
  #                expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+5, max(data_cases_sp_provinces$date +12)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Casos por día por COVID-19 en España",
       subtitle = paste0("Por provincia ",period),
       y = "casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_casos-por-dia-provincia-media-superpuesto-lineal_media.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% 
  ggplot() +
  # geom_smooth(aes(date, daily_cases_avg7,group=province, color=ccaa), size= 1, se = FALSE, span = 0.6 ) +
  geom_line(aes(date, daily_cases_avg7,group=province, color=ccaa), size= 1 ) +
  geom_line(aes(date, daily_cases,group=province, color=ccaa), size= 0.2 ) +
  geom_point(aes(date, daily_cases, color=ccaa), size= 0.5, alpha = 0.5 ) +
  # geom_point(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)), aes(date, daily_cases_avg7, color=province), size= 1.5, alpha = 0.3 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & daily_cases_avg7 > 5), 
                  aes(date, daily_cases_avg7, color=ccaa, label=paste(format(daily_cases_avg7, nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # marca un día
  geom_text_repel(data=filter( data_cases_sp_provinces, date==as.Date("2020-03-30") &  province == "Madrid" ),
                  aes(date,daily_cases, label=paste("casos en un día en una provincia")),
                  nudge_x = -1, # adjust the starting y position of the text label
                  size=5,
                  hjust=1,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  # marca la línea
  geom_text_repel(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-02") &  province == "Madrid" ),
                  aes(date+0.5,294, label=paste("media de 7 días")),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  coord_cartesian(
    ylim = c( 0,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$daily_cases),]$daily_cases) )
  ) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  # scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #                minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
  #                expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+5, max(data_cases_sp_provinces$date +14)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    # legend.position = c(0.1,0.6)
    legend.position = "none"
  ) +
  labs(title = "Media de casos por día (media 7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia. Escala logarítmica  ",period),
       y = "casos por día (media 7 días)",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# Superpuesto Log
png(filename=paste("img/spain/provincias/covid19_casos-por-dia-provincia-media-superpuesto-log_media.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% 
  ggplot() +
  # geom_smooth(aes(date, daily_cases_avg7,group=province, color=ccaa), size= 1, se = FALSE, span = 0.6 ) +
  geom_line(aes(date, daily_cases_avg7,group=province, color=ccaa), size= 0.9 ) +
  geom_point(aes(date, daily_cases, color=ccaa), size= 1.1, alpha = 0.5) +
  # geom_point(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)), aes(date, daily_cases_avg7, color=province), size= 1.5, alpha = 0.3 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & daily_cases > 0), 
                  aes(date, daily_cases_avg7, color=ccaa, label=paste(format(daily_cases_avg7, nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # marca un día
  geom_text_repel(data=filter( data_cases_sp_provinces, date==as.Date("2020-03-27") &  province == "Madrid" ),
                  aes(date,daily_cases, label=paste("casos en un día en una provincia")),
                  nudge_y = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  # marca la línea
  geom_text_repel(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-23") &  province == "Madrid" ),
                  aes(date+0.5,1000, label=paste("media de 7 días")),
                  nudge_y = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.2,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  coord_cartesian(
    # ylim = c(1,500)
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+5, max(data_cases_sp_provinces$date +16)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Media de casos por día (media 7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia. Escala logarítmica  ",period),
       y = "casos por día (media 7 días)",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# Superpuesto Log por CCAA -------------


# data_cases_sp_provinces %>% filter (ccaa == "Extremadura" ) %>%
#   ggplot() +
#   geom_line(aes(date, daily_cases_avg7,group=province, color=province), size= 1.5, se = FALSE, span = 0.6 ) +
#   geom_line(aes(date, daily_cases_PCR_avg3,group=province, color=province), size= 1, se = FALSE, span = 0.6, linetype = "dashed") + #, 
#   guides( 
#          linetype=guide_legend(keywidth = 3, keyheight = 1)
#          ) +
#   scale_linetype_manual(values=c("twodash", "dotted")) +
#   geom_point(aes(date, daily_cases, color=province), size= 1.2, alpha = 0.5 ) +
#   geom_text_repel(
#     data = data_cases_sp_provinces %>% filter( ccaa == "Extremadura" ) %>% group_by(province) %>% filter(!is.na(daily_cases) ) %>% top_n(1, date),
#     aes(date, daily_cases_avg7, color=province, 
#         label=paste(format(daily_cases_avg7, nsmall=1, big.mark=".", decimal.mark = ","),province)),
#     nudge_x = 1, # adjust the starting y position of the text label
#     size=5,
#     hjust=0,
#     family = "Roboto Condensed",
#     direction="y",
#     segment.size = 0.1,
#     segment.color="#777777"
#   )


for ( i in 1:length(levels(data_cases_sp_provinces$ccaa))  ) {
  # for ( i in 3:3  ) {
  
  prov <- levels(data_cases_sp_provinces$ccaa)[i]
  unaprov <- data_cases_sp_provinces %>% filter (ccaa == prov ) %>% select (province) %>% first() 
  unaprov <- unaprov[1]
  
  print(prov)
  if (i == 8 ) {
    png(filename=paste0("img/spain/provincias/covid19_casos-por-dia-provincia-media-superpuesto-lineal_media-", tolower( substr(prov,1,4) ),"leon.png", sep = ""),width = 1200,height = 800)
  } else if (i == 18 ) {
    png(filename=paste0("img/spain/provincias/covid19_casos-por-dia-provincia-media-superpuesto-lineal_media-pais.png", sep = ""),width = 1200,height = 800)
  } else {
    png(filename=paste0("img/spain/provincias/covid19_casos-por-dia-provincia-media-superpuesto-lineal_media-", tolower( substr(prov,1,4) ),".png", sep = ""),width = 1200,height = 800)
  }
  the_province  <- data_cases_sp_provinces %>% filter (ccaa == prov ) %>%
    ggplot() +
    geom_line(aes(date, daily_cases_avg7,group=province, color=province), size= 1.5, se = FALSE, span = 0.6 ) +
    geom_point(aes(date, daily_cases, color=province), size= 1.2, alpha = 0.5 ) +
    geom_text_repel(
      data = data_cases_sp_provinces %>% filter( ccaa == prov ) %>% group_by(province) %>% filter(!is.na(daily_cases) ) %>% top_n(1, date),
      aes(date, daily_cases_avg7, color=province, 
          label=paste(format(daily_cases_avg7, nsmall=1, big.mark=".", decimal.mark = ","),province)),
      nudge_x = 1, # adjust the starting y position of the text label
      size=5,
      hjust=0,
      family = "Roboto Condensed",
      direction="y",
      segment.size = 0.1,
      segment.color="#777777"
    ) +
    scale_color_manual(values = colors_prov) +
    scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
                        # expand = c(0,0.2)
    ) +
    scale_x_date(date_breaks = "2 day", 
                 date_labels = "%d",
                 limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date +9)),
                 expand = c(0,0) 
    ) + 
    theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      # panel.grid.minor.y = element_blank(),
      axis.ticks.x = element_line(color = "#000000"),
      legend.position =  "none"
    ) +
    labs(title = paste0("Media de casos por día (media 7 días) por COVID-19 en ",prov ),
         subtitle = paste0("PCR: línea de puntos, media de 7 días. Por provincia. ",period),
         y = "casos por día (media 7 días)",
         x = "fecha",
         caption = caption_provincia)
  
  if ( prov=="Asturias, Principado de" | prov=="Balears, Illes" | prov=="Cantabria"  |  prov=="Ceuta" |  prov=="Castilla - La Mancha" |   prov=="Melilla"  | prov=="Comunitat Valenciana"  | 
       prov=="Extremadura" | prov=="Madrid, Comunidad de" | prov=="Murcia, Región de" | prov=="Navarra, Comunidad Foral de" | prov=="Rioja, La" | prov=="País Vasco") {
    # TODO: display PCR accumulated
    the_province  <- the_province + geom_line( aes(date, daily_cases_PCR_avg7,group=province, color=province), size= 1, se = FALSE, span = 0.6, linetype = "dashed")
  }
  
  print(the_province)
  print(paste("plot",prov))
  dev.off()
}

# Log
for ( i in 1:length(levels(data_cases_sp_provinces$ccaa))  ) {
  
  prov <- levels(data_cases_sp_provinces$ccaa)[i]
  unaprov <- data_cases_sp_provinces %>% filter (ccaa == prov ) %>% select (province) %>% first() 
  unaprov <- unaprov[1]
  
  print(prov)
  if (i == 8 ) {
    png(filename=paste0("img/spain/provincias/covid19_casos-por-dia-provincia-media-superpuesto-log_media-", tolower( substr(prov,1,4) ),"leon.png", sep = ""),width = 1200,height = 800)
  } else if (i == 18 ) {
    png(filename=paste0("img/spain/provincias/covid19_casos-por-dia-provincia-media-superpuesto-log_media-pais.png", sep = ""),width = 1200,height = 800)
  } else {
    png(filename=paste0("img/spain/provincias/covid19_casos-por-dia-provincia-media-superpuesto-log_media-", tolower( substr(prov,1,4) ),".png", sep = ""),width = 1200,height = 800)
  }
  the_province  <- data_cases_sp_provinces %>% filter (ccaa == prov ) %>%
    # data_cases_sp_provinces %>% filter (ccaa == prov ) %>%
    ggplot() +
    geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_cases_avg7,province_cp,-province),
              aes(date,daily_cases_avg7,group=province_cp), color="#CACACA", size = 0.3 ) +
    geom_line(aes(date, daily_cases_avg7,group=province, color=province), size= 1.5, se = FALSE, span = 0.6 ) +
    geom_point(aes(date, daily_cases, color=province), size= 1.2, alpha = 0.5 ) +
    geom_text_repel(
      data = data_cases_sp_provinces %>% filter( ccaa == prov ) %>% group_by(province)  %>% filter(!is.na(daily_cases) ) %>% top_n(1, date),
      aes(date, daily_cases_avg7, color=province, label=paste(format(daily_cases_avg7, nsmall=1, big.mark=".", decimal.mark = ","),province)),
      nudge_x = 1, # adjust the starting y position of the text label
      size=5,
      hjust=0,
      family = "Roboto Condensed",
      direction="y",
      segment.size = 0.1,
      segment.color="#777777"
    ) +
  scale_color_manual(values = colors_prov) +
    # coord_cartesian(
    #   ylim = c(1,500)
    # ) +
    scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                   minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                   expand = c(0,0.2) ) +
    scale_x_date(date_breaks = "2 day", 
                 date_labels = "%d",
                 limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date +9)),
                 expand = c(0,0) 
    ) + 
    theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      # panel.grid.minor.y = element_blank(),
      axis.ticks.x = element_line(color = "#000000"),
      legend.position =  "none"
    ) +
    labs(title = paste0("Media de casos por día (media 7 días) por COVID-19 en ",prov ),
         subtitle = paste0("PCR: línea de puntos, media de 7 días. Por provincia. Escala logarítmica  ",period),
         y = "casos por día (media 7 días)",
         x = "fecha",
         caption = caption_provincia)
  
  if ( prov=="Asturias, Principado de" | prov=="Balears, Illes" | prov=="Cantabria"  |  prov=="Ceuta" |  prov=="Melilla"  | 
       prov=="Extremadura" | prov=="Madrid, Comunidad de" | prov=="Murcia, Región de" | prov=="Navarra, Comunidad Foral de" | prov=="Rioja, La") {
    # TODO: display PCR accumulated
    the_province  <- the_province + geom_line( aes(date, daily_cases_PCR_avg7,group=province, color=province), size= 1, se = FALSE, span = 0.6, linetype = "dashed")
  }
  
  print(the_province)
  print(paste("plot",prov))
  dev.off()
}


# Interactive ---------------
library(plotly)

interactive_dp  <- data_cases_sp_provinces %>% ungroup(province) %>%
  ggplot() +
  geom_line(aes(date, daily_cases_avg7, group = province, color=ccaa,
                text = paste0("<b>", province, " (", ccaa, ")</b><br>", format( round(daily_cases_avg7, digits = 1), 
                                                                                big.mark=".", decimal.mark = ","), " media casos diaria (ventana 7 días)" ,"<br>",date )),                   
            size= 0.7, se = FALSE, span = 0.6 ) +
  geom_point(aes(date, daily_cases, color=ccaa,
                 text = paste0("<b>", province, " (", ccaa, ")</b><br>", format( round(daily_cases, digits = 1), 
                                                                                 big.mark=".", decimal.mark = ","), " casos el día " ,"<br>",date )),    
             size= 0.3
  ) +
  scale_color_manual(values = colors_prov) +
  coord_cartesian(
    ylim = c(1,1200)
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "3 day",
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+13, max(data_cases_sp_provinces$date + 1)),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  c(0.3,0.9)
  ) +
  labs(title = paste0("Media de casos por día (media 7 días) por COVID-19. España" ),
       subtitle = paste0("Por provincia. Escala logarítmica ",period),
       y = "casos por día (media 7 días)",
       x = "fecha",
       caption = caption_provincia)


# save interactvive
interactive_dp_p <- ggplotly(interactive_dp, tooltip = "text") %>% 
  layout(title = list(text = paste0('Media de casos por día (media 7 días) por COVID-19 por provincias',
                                    '<br>',
                                    '<sup>',
                                    'Por provincia. Escala logarítmica. Actualizado: 2020.04.28',
                                    '</sup>'))
         , annotations = 
           list(x = 1, y = -0.11, text = "<a style='color:grey;' href='https://lab.montera34.com/covid19'>lab.montera34.com/covid19</a> | Data: <a  style='color:grey;'href='https://github.com/montera34/escovid19data9'>esCOVID19data</a>", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="grey")
           )
  )

htmlwidgets::saveWidget(as_widget(interactive_dp_p), paste0(getwd(),"/interactive/daily-cases-provinces-index.html") )

# / 3. Deceassed (Fallecimientos) ------------

# creates curves for missing data regions (ccaa)
# ccaa_missing <- data_all_export %>% filter( region == "Galicia") %>% filter( date < filter_date) %>% 
#   mutate (province = region, ccaa = region)

# // 3.1 Fallecimientos Small multiple ----------
png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  # geom_line(data = data_cases_sp_provinces_sm %>% ungroup() %>% select(date,deceased,province_cp,-province),
  #           aes(date,deceased,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, deceased,group=province),size=1 ) +
  geom_point(aes(date, deceased), size= 0.5 ) +
  geom_text(
            data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
                  # aes(date,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceased),]$deceased)/4, label=paste(format(deceased, nsmall=1, big.mark="."))),
                  aes(date,40, label=paste(format(deceased, nsmall=0, big.mark="."))),
                  size=4,
                  hjust=1,
                  family = "Roboto Condensed"
  ) +
  facet_wrap(~province, scales = "free") +
  scale_y_continuous(
    # limits = c(0,max(data_cases_sp_provinces$cases_accumulated) ),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "10 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date)+1),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,deceased,province_cp,-province),
            aes(date,deceased,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, deceased,group=province), size = 1 ) +
  geom_point(aes(date, deceased), size= 0.5 ) +
  geom_text(data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
            aes(date,9, label=paste(format(deceased, nsmall=1, big.mark="."))),
            size=4,
            hjust=1,
            family = "Roboto Condensed"
  ) +
  facet_wrap(~province) +
  scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
                 ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# SM Agrupado por CCAA -----
png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-lineal_ccaa.png", sep = ""),width = 1200,height = 800)
# prov_sm <- data_cases_sp_provinces %>%
data_cases_sp_provinces %>%
  # ggplot() +
  # geom_line(data = select(data_cases_sp_provinces_sm,date,deceased,province_cp,-province),
  #           aes(date,deceased,group=province_cp), color="#CACACA" ) +
  # add missing ccaa
  # geom_line(data = ccaa_missing, aes(date, deceassed,group=province), size= 0.7, linetype = 2 ) +
  # geom_text_repel(data=filter( ccaa_missing, 
  #                              date==max(data_cases_sp_provinces$date)), 
  #                                     aes(date+1, deceassed, label=paste0("(",format(deceassed, nsmall=1, big.mark="."), " ", substr(province,1,3), ")") ),
  #                                     # nudge_x = 3, # adjust the starting y position of the text label
  #                                     size=4,
  #                                     hjust=0,
  #                                     family = "Roboto Condensed",
  #                                     direction="y",
  #                                     segment.size = 0.1,
#                                     segment.color="#777777"
# ) +
# all the provinces 
geom_line(aes(date, deceased,group=province), size= 0.7 ) +
  geom_text_repel(data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
                  aes(date+1, deceased, label=paste(format(deceased, nsmall=1, big.mark="."), substr(province,1,3) ) ),
                  nudge_x = 1, # adjust the starting x position of the text label
                  size=4,
                  hjust=0,
                  color = "#666666",
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  facet_wrap(~ccaa) +
  coord_cartesian(
    xlim = c( min(data_cases_sp_provinces$date + 9), max(data_cases_sp_provinces$date +18)),
    ylim = c(1, max( data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceased),]$deceased))
  ) +
  scale_y_continuous( 
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               expand = c(0,1) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala lin.). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-log_ccaa.png", sep = ""),width = 1200,height = 800)
# prov_sm <- data_cases_sp_provinces %>%
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,deceased,province_cp,-province),
            aes(date,deceased,group=province_cp), color="#CACACA" ) +
  # add missing ccaa
  # geom_line(data = ccaa_missing, aes(date, deceassed,group=province), size= 0.7, linetype = 2 ) +
  # geom_text_repel(data=filter( ccaa_missing, 
  #                              date==max(data_cases_sp_provinces$date)), 
  #                                     aes(date+1, deceassed, label=paste0("(",format(deceassed, nsmall=1, big.mark="."), " ", substr(province,1,3), ")") ),
  #                                     # nudge_x = 3, # adjust the starting y position of the text label
  #                                     size=4,
  #                                     hjust=0,
  #                                     family = "Roboto Condensed",
  #                                     direction="y",
  #                                     segment.size = 0.1,
  #                                     segment.color="#777777"
  # ) +
  # all the provinces 
  geom_line(aes(date, deceased,group=province), size= 0.7 ) +
  geom_text_repel(data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
                                aes(date+1, deceased, label=paste(format(deceased, nsmall=1, big.mark="."), substr(province,1,3) ) ),
                                nudge_x = 1, # adjust the starting x position of the text label
                                size=4,
                                hjust=0,
                                color = "#666666",
                                family = "Roboto Condensed",
                                direction="y",
                                segment.size = 0.1,
                                segment.color="#777777"
                                ) +
  facet_wrap(~ccaa) +
  coord_cartesian(
    xlim = c( min(data_cases_sp_provinces$date + 9), max(data_cases_sp_provinces$date +18)),
    ylim = c(1, max( data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceased),]$deceased))
  ) +
  scale_y_log10( 
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)) ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               expand = c(0,1) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-per-cienmil-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = data_cases_sp_provinces_sm %>% ungroup() %>% select(date,deceassed_per_100000,province_cp,-province),
            aes(date,deceassed_per_100000,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, deceassed_per_100000,group=province), size = 1) +
  # geom_point(aes(date, deceassed_per_100000), size= 0.5 ) +
  geom_text_repel(data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
                  aes(min(data_cases_sp_provinces$date)+15, 200, label=paste(format(deceassed_per_100000, nsmall=1, big.mark=".", decimal.mark = ",") ) ),
                  nudge_x = 1, # adjust the starting x position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  facet_wrap(~province) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+12, max(data_cases_sp_provinces$date)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia  (escala lineal). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-per-cienmil-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% 
  ggplot() +
  geom_line(data = data_cases_sp_provinces_sm %>% ungroup() %>% select(date,deceassed_per_100000,province_cp,-province),
            aes(date,deceassed_per_100000,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, deceassed_per_100000,group=province), size = 1) +
  # geom_point(aes(date, deceassed_per_100000), size= 0.5 ) +
  geom_text_repel(data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
                  aes(date, 0.3, label=paste(format(deceassed_per_100000, nsmall=1, big.mark=".", decimal.mark = ",") ) ),
                  nudge_x = 1, # adjust the starting x position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  facet_wrap(~province) +
  scale_y_log10(  
    limits = c(0.05,max(data_cases_sp_provinces$deceassed_per_100000)),
    minor_breaks =  c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+15, max(data_cases_sp_provinces$date) ),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-per-cienmil-log-ccaa.png", sep = ""),width = 1200,height = 700)
data_cases_sp_provinces %>% 
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,deceassed_per_100000,province_cp,-province),
            aes(date,deceassed_per_100000,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, deceassed_per_100000,group=province), size= 0.7 ) +
  geom_text_repel(data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceassed_per_100000) ) %>% top_n(1, date),
                  aes(date+1, deceassed_per_100000, label=paste(format( round(deceassed_per_100000, digits = 1), nsmall=1, big.mark=".", decimal.mark = ","), substr(province,1,3) ) ),
                  nudge_x = 1, # adjust the starting x position of the text label
                  size=4,
                  hjust=0,
                  color = "#666666",
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  facet_wrap(~ccaa) +
  coord_cartesian(
    xlim = c( min(data_cases_sp_provinces$date + 9), max(data_cases_sp_provinces$date +18)),
  ) +
  scale_y_log10(  
    limits = c(0.05,max(data_cases_sp_provinces$deceassed_per_100000)),
    minor_breaks =  c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d"
               # limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date) ) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# // 3.2 Superpuesto----------------------
png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-superpuesto-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, deceased,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, deceased, color=ccaa), size= 1 ) +
  geom_text_repel(
    data = data_cases_sp_provinces %>% group_by(province) %>% filter(!is.na(deceased) & deceased > 400  ) %>% top_n(1, date),
    # data=filter( data_cases_sp_provinces, 
    #                            ( date==max(data_cases_sp_provinces$date) & deceased > 400 ) 
    #                            # |
    #                            #   (date==as.Date("2020-04-27") & province == "Barcelona")
    #                            ), 
                  aes(date, deceased, color=ccaa, label=paste(format(deceased, nsmall=0, big.mark="."),province)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # geom_text(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-27") & province == "Barcelona"),
  #           aes(date, deceased, color=ccaa, label=paste(format(deceased, nsmall=1, big.mark="."),province)),
  #           nudge_x = 0, # adjust the starting y position of the text label
  #           size=5,
  #           hjust=0,
  #           family = "Roboto Condensed",
  #           direction="y",
  #           segment.size = 0.1,
  #           segment.color="#777777"
  # ) +
  scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date + 17)),
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-superpuesto-log.png", sep = ""),width = 1300,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, deceased,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, deceased, color=ccaa), size= 1 ) +
  geom_text_repel(
    data = data_cases_sp_provinces %>% group_by(province) %>% filter(!is.na(deceased) & deceased > 400  ) %>% top_n(1, date),
    # data=filter( data_cases_sp_provinces, 
    #                              ( date==max(data_cases_sp_provinces$date) & deceased > 200 ) 
    #              # |
    #              #                   (date==as.Date("2020-04-27") & province == "Barcelona")
    #                 ), 
                  aes(date, deceased, color=ccaa, label=paste0(format(deceased, nsmall=0, big.mark="."), " ", province, " (+", daily_deaths,", +", daily_deaths_inc ,"%)")),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=6,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # geom_text(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-14") & province == "Barcelona"),
  #           aes(date, deceased, color=ccaa, label=paste(format(deceased, nsmall=1, big.mark="."),province)),
  #           nudge_x = 2, # adjust the starting y position of the text label
  #           size=5,
  #           hjust=0,
  #           family = "Roboto Condensed",
  #           direction="y",
  #           segment.size = 0.1,
  #           segment.color="#777777"
  # ) +
  scale_color_manual(values = colors_prov) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 expand = c(0,0.1) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date + 22)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.07,0.7)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# Per 100.000 ---------
png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, deceassed_per_100000,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, deceassed_per_100000, color=ccaa), size= 1 ) +
  geom_text_repel(   
                  data = data_cases_sp_provinces %>% group_by(province) %>% filter(!is.na(deceassed_per_100000) & deceassed_per_100000 > 50  ) %>% top_n(1, date),
                  # data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & deceassed_per_100000 > 15), 
                  aes(date, deceassed_per_100000, color=ccaa, 
                      label=paste(format( round(deceassed_per_100000, digits = 1) , nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # geom_text(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-14") & province == "Barcelona"),
  #                 aes(date, deceassed_per_100000, color=ccaa, 
  #                     label=paste(format(round(deceassed_per_100000, digits = 1) , nsmall=1, big.mark=".", decimal.mark = ","),province)),
  #                 nudge_x = 1, # adjust the starting y position of the text label
  #                 size=5,
  #                 hjust=0,
  #                 family = "Roboto Condensed",
  #                 direction="y",
  #                 segment.size = 0.1,
  #                 segment.color="#777777"
  # ) +
  scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date +14)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia  (escala lineal). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-superpuesto-per-cienmil-log.png", sep = ""),width = 1300,height = 900)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, deceassed_per_100000,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, deceassed_per_100000, color=ccaa), size= 1 ) +
  geom_text_repel(                  data = data_cases_sp_provinces %>% group_by(province) %>% filter(!is.na(deceassed_per_100000) & deceassed_per_100000 > 50  ) %>% top_n(1, date),
    # data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & deceassed_per_100000 > 12), 
                  aes(date, deceassed_per_100000, color=ccaa, 
                      label=paste(format(round(deceassed_per_100000, digits = 1) , nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 6, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # geom_text(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-14") & province == "Barcelona"),
  #           aes(date, deceassed_per_100000, color=ccaa, 
  #               label=paste(format(round(deceassed_per_100000, digits = 1) , nsmall=1, big.mark=".", decimal.mark = ","),province)),
  #             nudge_x = 1, # adjust the starting y position of the text label
  #             size=5,
  #             hjust=0,
  #             family = "Roboto Condensed",
  #             direction="y",
  #             segment.size = 0.1,
  #             segment.color="#777777"
  # ) +
  scale_color_manual(values = colors_prov) +
  coord_cartesian( 
    ylim=c(0.5, max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceassed_per_100000),]$deceassed_per_100000)*1.2 )
  ) +
  scale_y_log10(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    # limits = c(0.05,max(data_cases_sp_provinces$deceassed_per_100000)*1.1),
    minor_breaks =  c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ),
    expand = c(0,0.2)
    ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date + 22)),
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# 4. Daily deaths --------------

# SM Lineal
png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  # geom_smooth(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg7,province_cp,-province),
  #             aes(date,daily_deaths_avg7,group=province_cp), se = FALSE, span = 0.6, color="#CACACA", size=0.5 ) +
  # geom_smooth(aes(date, daily_deaths_avg7,group=province), size= 0.7, se = FALSE, span = 0.6, color="#000000" ) +
  geom_point(aes(date, daily_deaths), size= 0.5, alpha=1 ) +
  geom_text(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
            aes(date,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$daily_deaths_avg7),]$daily_deaths_avg7)/2, label=paste(format(daily_deaths_avg7, nsmall=1, big.mark="."))),
            size=4,
            hjust=1,
            family = "Roboto Condensed"
  ) +
  facet_wrap(~province) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+15, max(data_cases_sp_provinces$date)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Muertes por día (7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "fallecidos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()


# for ( i in 1:52 ) {
# # for ( i in 1:2 ) {
#   la_prov <- unique(data_cases_sp_provinces$province)[i]
#   png(filename=paste0("img/spain/provincias/daily-deaths/covid19_muertes-por-dia-provincia-lineal", substr(la_prov,1,4),".png"),width = 600,height = 400)
#   the_prov <- data_cases_sp_provinces %>% filter ( province == la_prov ) %>%
#     ggplot() +
#     geom_point(aes(date, daily_deaths), size= 2, alpha=0.5 ) +
#     # geom_text(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
#     #           aes(date,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$daily_deaths_avg7),]$daily_deaths_avg7)/4, label=paste(format(daily_deaths_avg7, nsmall=1, big.mark="."))),
#     #           size=3,
#     #           hjust=1,
#     #           family = "Roboto Condensed"
#     # ) +
#     scale_x_date(date_breaks = "2 day", 
#                  date_labels = "%d",
#                  limits=c( min(data_cases_sp_provinces$date)+15, max(data_cases_sp_provinces$date)),
#                  expand = c(0,1) 
#     ) + 
#     theme_minimal(base_family = "Roboto Condensed",base_size = 20) +
#     theme(
#       panel.grid.minor.x = element_blank(),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.y = element_blank(),
#       axis.ticks.x = element_line(color = "#000000"),
#       legend.position = c(0.1,0.6)
#     ) +
#     labs(title = paste0("",la_prov),
#          # subtitle = paste0("Muertes por día"),
#          # y = "fallecidos por día",
#          # x = "fecha"
#          y = "",
#          x = ""
#          # caption = caption_provincia
#          )
#   
#   print(the_prov)
#   dev.off()
#   
# }

# make tiled image with imagemagick from command line
# montage covid19_muertes-por-dia-provincia-lineal* -geometry 400x tiles_02.png


# SM Lineal
# png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-lineal.png", sep = ""),width = 1200,height = 800)
# data_cases_sp_provinces %>%
#   ggplot() +
#   geom_smooth(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg7,province_cp,-province),
#               aes(date,daily_deaths_avg7,group=province_cp), se = FALSE, span = 0.6, color="#CACACA", size=0.5 ) +
#   geom_smooth(aes(date, daily_deaths_avg7,group=province), size= 0.9, se = FALSE, span = 0.6, color  = "#444444") +
#   geom_point(aes(date, daily_deaths), size= 0.5, alpha=0.3 ) +
#   geom_text(data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
#             aes(date,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$daily_deaths_avg7),]$daily_deaths_avg7)/1.2, label=paste(format(daily_deaths_avg7, nsmall=1, big.mark="."))),
#             size=4,
#             hjust=1,
#             family = "Roboto Condensed"
#   ) +
#   facet_wrap(~province) +
#   scale_x_date(date_breaks = "7 day", 
#                date_labels = "%d",
#                limits=c( min(data_cases_sp_provinces$date)+15, max(data_cases_sp_provinces$date)),
#                expand = c(0,0) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = c(0.1,0.6),
#     axis.text.x = element_text(size = 9)
#   ) +
#   labs(title = "Media de muertes por día (7 días) por COVID-19 en España",
#        subtitle = paste0("Por provincia (escala lineal). ",period),
#        y = "Meded de fallecidos por día",
#        x = "fecha",
#        caption = caption_provincia)
# dev.off()

png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_smooth(aes(date, daily_deaths_avg7,group=province), size= 1.1, se = FALSE, span = 0.6, color  = "#444444") +
  geom_point(aes(date, daily_deaths), size= 0.5, alpha=0.3 ) +
  geom_text(data= data_cases_sp_provinces %>% group_by(province)%>% filter(!is.na(deceased) ) %>% top_n(1, date),
            aes(max(data_cases_sp_provinces$date)-10, daily_deaths_avg7*2.5, label=paste(format(daily_deaths_avg7, nsmall=1, big.mark=".", decimal.mark = ","))),
            size=4,
            hjust=0,
            color  = "#666666",
            family = "Roboto Condensed"
  ) +
  facet_wrap(~province, scale="free") +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+15, max(data_cases_sp_provinces$date)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 10)
  ) +
  labs(title = "Media de muertes por día (7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "Meded de fallecidos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# SM Log
png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-log.png", sep = ""),width = 1200,height = 900)
data_cases_sp_provinces %>%
  ggplot() +
  geom_smooth(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg7,province_cp,-province),
              aes(date,daily_deaths_avg7,group=province_cp), se = FALSE, span = 0.6, color="#CACACA", size=0.5 ) +
  geom_smooth(aes(date, daily_deaths_avg7,group=province), size= 0.7, se = FALSE, span = 0.6, color="#000000" ) +
  geom_point(aes(date, daily_deaths), size= 0.7, alpha=0.3 ) +
  geom_text(data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
            aes(date,0.1, 
                label=paste(format(daily_deaths_avg7, nsmall=1, big.mark="."))),
            size=4,
            hjust=1,
            family = "Roboto Condensed"
  ) +
  facet_wrap(~province) +
  scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+15, max(data_cases_sp_provinces$date)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Media de muertes por día (7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "media de fallecidos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# SM CCAA grouped ------
png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-lineal_ccaa.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  # geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg7,province_cp,-province),
  #           aes(date,daily_deaths_avg7,group=province_cp), color="#CACACA" ) +
  # geom_line(aes(date, daily_deaths_avg7,group=province) ) +
  geom_smooth(aes(date, daily_deaths_avg7,group=province, color = ccaa ), size= 1.2, se = FALSE, span = 0.7 ) +
  geom_line(aes(date, daily_deaths_avg7,group=province), size= 0.4, color="#777777", alpha = 0.6 ) +
  # geom_point(aes(date, daily_deaths_avg7), size= 0.5 ) +
  geom_text_repel(
                      data = data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
                      aes(date, daily_deaths_avg7, label=paste(format(daily_deaths_avg7, nsmall=1, big.mark="."), substr(province,1,2) ) ),
                      nudge_x = 2, # adjust the starting x position of the text label
                      size=4,
                      hjust=0,
                      family = "Roboto Condensed",
                      direction="y",
                      segment.size = 0.1,
                      color = "#666666",
                      segment.color="#777777"
  ) +
  facet_wrap(~ccaa, scales = "free_y") +
  coord_cartesian(
    xlim= c( as.Date("2020-03-15"),max(data_cases_sp_provinces$date) )
  ) +
  scale_y_continuous( 
    # minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               expand = c(0,15) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "none"
  ) +
  labs(
        title = "Media de muertes por día (ventana 7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "media de fallecidos por día",
       x = "fecha",
       caption = caption_provincia
       )
dev.off()

# SM CCAA grouped ------
png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-lineal-per-cienmil_ccaa.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg7,province_cp,-province,poblacion),
            aes(date,daily_deaths_avg7/poblacion*100000,group=province_cp), color="#CACACA", alpha = 0.3  ) +
  # geom_line(aes(date, daily_deaths_avg7,group=province) ) +
  geom_smooth(aes(date, daily_deaths_avg7/poblacion*100000,group=province, color = province ), size= 1.2, se = FALSE, span = 0.7 ) +
  geom_line(aes(date, daily_deaths_avg7/poblacion*100000,group=province), size= 0.4, color="#777777") +
  # geom_point(aes(date, daily_deaths_avg7), size= 0.5 ) +
  geom_text_repel(
    data = data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
    aes(date, daily_deaths_avg7/poblacion*100000, label=paste(format( round(daily_deaths_avg7/poblacion*100000, digits = 1), nsmall=1, big.mark="."), substr(province,1,2) ) ),
    nudge_x = 2, # adjust the starting x position of the text label
    size=4,
    hjust=0,
    family = "Roboto Condensed",
    direction="y",
    segment.size = 0.1,
    color = "#666666",
    segment.color="#777777"
  ) +
  facet_wrap(~ccaa) + #, scales = "free_y"
  coord_cartesian(
    xlim= c( as.Date("2020-03-15"),max(data_cases_sp_provinces$date) )
  ) +
  scale_y_continuous( 
    # minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               expand = c(0,15) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "none"
  ) +
  labs(
    title = "Media de muertes por día (ventana 7 días) por COVID-19 por 100.000 habitantes en España",
    subtitle = paste0("Por provincia (escala logarítmica). ",period),
    y = "media de fallecidos por día",
    x = "fecha",
    caption = caption_provincia
  )
dev.off()

png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-lineal_ccaa.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  # geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg7,province_cp,-province),
  #           aes(date,daily_deaths_avg7,group=province_cp), color="#CACACA" ) +
  # geom_line(aes(date, daily_deaths_avg7,group=province) ) +
  geom_smooth(aes(date, daily_deaths_avg7,group=province, color=province), size= 0.7, se = FALSE, span = 0.6 ) +
  # geom_point(aes(date, daily_deaths_avg7), size= 0.5 ) +
  geom_text_repel(
    data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
    aes(date, daily_deaths_avg7, label=paste(format(daily_deaths_avg7, nsmall=1, big.mark="."), substr(province,1,2) ) ),
    nudge_x = 2, # adjust the starting x position of the text label
    size=4,
    hjust=0,
    color = "#666666",
    family = "Roboto Condensed",
    direction="y",
    segment.size = 0.1,
    segment.color="#777777"
  ) +
  facet_wrap(~ccaa, scales = "free") +
  coord_cartesian(
    xlim= c( as.Date("2020-03-15"),max(data_cases_sp_provinces$date) )
  ) +
  scale_y_continuous(
                 labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "10 day", 
               date_labels = "%d",
               expand = c(0,12) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Media de muertes por día (medía 7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "media fallecidos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-log_ccaa.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg7,province_cp,-province),
            aes(date,daily_deaths_avg7,group=province_cp), color="#CACACA" ) +
  # geom_line(aes(date, daily_deaths_avg7,group=province) ) +
  geom_smooth(aes(date, daily_deaths_avg7,group=province), size= 0.7, se = FALSE, span = 0.6, color="#000000" ) +
  # geom_point(aes(date, daily_deaths_avg7), size= 0.5 ) +
  geom_text_repel(
                    data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
                    aes(date, daily_deaths_avg7, label=paste(format(daily_deaths_avg7, nsmall=1, big.mark="."), substr(province,1,2) ) ),
                    nudge_x = 2, # adjust the starting x position of the text label
                    size=4,
                    hjust=0,
                    color = "#666666",
                    family = "Roboto Condensed",
                    direction="y",
                    segment.size = 0.1,
                    segment.color="#777777"
  ) +
  facet_wrap(~ccaa) +
  coord_cartesian(
    xlim= c( as.Date("2020-03-15"),max(data_cases_sp_provinces$date) )
  ) +
  scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               expand = c(0,12) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Media de muertes por día (medía 7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "media fallecidos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# Superpuesto Lineal ------
png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-superpuesto-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, daily_deaths,group=province, color=ccaa), size= 0.8 ) +
  geom_point(aes(date, daily_deaths, color=ccaa), size= 1 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & daily_deaths > 5), 
                  aes(date, daily_deaths, color=ccaa, label=paste(format(daily_deaths, nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  # coord_cartesian(
  #   ylim = c(1,500)
  # ) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  # scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #                minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
  #                expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date +9)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Muertes por día por COVID-19 en España",
       subtitle = paste0("Por provincia ",period),
       y = "fallecidos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-superpuesto-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% 
  ggplot() +
  geom_line(aes(date, daily_deaths,group=province, color=ccaa), size= 0.7 ) +
  # geom_point(aes(date, daily_deaths, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & daily_deaths > 5), 
                  aes(date, daily_deaths, color=ccaa, label=paste(format(daily_deaths, nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  coord_cartesian(
    ylim = c(1,500)
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 expand = c(0,0) ) +
  # scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #                minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
  #                expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date +9)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Muertes por día por COVID-19 en España",
       subtitle = paste0("Por provincia ",period),
       y = "fallecidos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-superpuesto-lineal_media.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% 
  ggplot() +
  # geom_smooth(aes(date, daily_deaths_avg7,group=province, color=ccaa), size= 1, se = FALSE, span = 0.6 ) +
  geom_line(aes(date, daily_deaths_avg7,group=province, color=ccaa), size= 1 ) +
  geom_line(aes(date, daily_deaths,group=province, color=ccaa), size= 0.2 ) +
  geom_point(aes(date, daily_deaths, color=ccaa), size= 0.5, alpha = 0.5 ) +
  # geom_point(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)), aes(date, daily_deaths_avg7, color=province), size= 1.5, alpha = 0.3 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & daily_deaths_avg7 > 5), 
                  aes(date, daily_deaths_avg7, color=ccaa, label=paste(format(daily_deaths_avg7, nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # marca un día
  geom_text_repel(data=filter( data_cases_sp_provinces, date==as.Date("2020-03-27") &  province == "Madrid" ),
                  aes(date,daily_deaths, label=paste("muertes en un día en una provincia")),
                  nudge_x = -1, # adjust the starting y position of the text label
                  size=5,
                  hjust=1,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  # marca la línea
  geom_text_repel(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-02") &  province == "Madrid" ),
                  aes(date+0.5,294, label=paste("media de 7 días")),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  # coord_cartesian(
  #   ylim = c(1,500)
  # ) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
                 ) +
  # scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #                minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
  #                expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date +9)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Media de muertes por día (media 7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia. Escala logarítmica  ",period),
       y = "fallecidos por día (media 7 días)",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# Superpuesto Log
png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-superpuesto-log_media.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% 
  ggplot() +
  # geom_smooth(aes(date, daily_deaths_avg7,group=province, color=ccaa), size= 1, se = FALSE, span = 0.6 ) +
  geom_line(aes(date, daily_deaths_avg7,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, daily_deaths, color=ccaa), size= 1.5, alpha = 0.5) +
  # geom_point(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)), aes(date, daily_deaths_avg7, color=province), size= 1.5, alpha = 0.3 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & daily_deaths > 0), 
                  aes(date, daily_deaths_avg7, color=ccaa, label=paste(format(daily_deaths_avg7, nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # marca un día
  geom_text_repel(data=filter( data_cases_sp_provinces, date==as.Date("2020-03-27") &  province == "Madrid" ),
                  aes(date,daily_deaths, label=paste("muertes en un día en una provincia")),
                  nudge_y = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  # marca la línea
  geom_text_repel(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-11") &  province == "Madrid" ),
                  aes(date+0.5,190, label=paste("media de 7 días")),
                  nudge_y = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.2,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  coord_cartesian(
    ylim = c(1,500)
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date +14)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Media de muertes por día (media 7 días) por COVID-19 en España",
       subtitle = paste0("Por provincia. Escala logarítmica  ",period),
       y = "fallecidos por día (media 7 días)",
       x = "fecha",
       caption = caption_provincia)
dev.off()


# Superpuesto Log por CCAA -------------

for ( i in 1:length(levels(data_cases_sp_provinces$ccaa))  ) {
# for ( i in 3:3  ) {
  
  prov <- levels(data_cases_sp_provinces$ccaa)[i]
  unaprov <- data_cases_sp_provinces %>% filter (ccaa == prov ) %>% select (province) %>% first() 
  unaprov <- unaprov[1]
  
  print(prov)
  if (i == 8 ) {
    png(filename=paste0("img/spain/provincias/covid19_muertes-por-dia-provincia-media-superpuesto-lineal_media-", tolower( substr(prov,1,4) ),"leon.png", sep = ""),width = 1200,height = 800)
  } else if (i == 18 ) {
    png(filename=paste0("img/spain/provincias/covid19_muertes-por-dia-provincia-media-superpuesto-lineal_media-pais.png", sep = ""),width = 1200,height = 800)
  } else {
    png(filename=paste0("img/spain/provincias/covid19_muertes-por-dia-provincia-media-superpuesto-lineal_media-", tolower( substr(prov,1,4) ),".png", sep = ""),width = 1200,height = 800)
  }
  the_province  <- data_cases_sp_provinces %>% filter (ccaa == prov ) %>%
    # data_cases_sp_provinces %>% filter (ccaa == prov ) %>%
    ggplot() +
    # geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg7,province_cp,-province),
    #           aes(date,daily_deaths_avg7,group=province_cp), color="#CACACA", size = 0.3 ) +
    geom_line(aes(date, daily_deaths_avg7,group=province, color=province), size= 1.5, se = FALSE, span = 0.6 ) +
    geom_point(aes(date, daily_deaths, color=province), size= 1.5, alpha = 0.5 ) +
    # geom_point(aes(date, daily_deaths_avg7 ), size= 2, color= "#999999" ) +
    # geom_point(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)), aes(date, daily_deaths_avg7, color=province), size= 1.5, alpha = 0.3 ) +
    # geom_shadowtext( 
    #       aes(date, daily_deaths, label = paste(format(daily_deaths_avg7, nsmall=1, big.mark=".", decimal.mark = ","),province) ), 
    #                 hjust=0, 
    #                 vjust = 0, 
    #                 data = . %>% group_by(province) %>% top_n(1, date), bg.color = "white") +
    geom_text_repel(
      # data= data_cases_sp_provinces %>% filter (ccaa == prov ) %>% filter(date==max(data_cases_sp_provinces[data_cases_sp_provinces$ccaa == prov,]$date) & (ccaa == prov )),
      data = data_cases_sp_provinces %>% filter( ccaa == prov ) %>% group_by(province) %>% filter(!is.na(deceased) ) %>% top_n(1, date),
                    aes(date, daily_deaths_avg7, color=province, 
                        label=paste(format(daily_deaths_avg7, nsmall=1, big.mark=".", decimal.mark = ","),province)),
                    nudge_x = 1, # adjust the starting y position of the text label
                    size=5,
                    hjust=0,
                    family = "Roboto Condensed",
                    direction="y",
                    segment.size = 0.1,
                    segment.color="#777777"
    ) +
    # marca un día
    geom_text_repel(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-02") &  province == unaprov ),
                    aes(date,daily_deaths, label=paste("muertes en un día en una provincia")),
                    nudge_y = 5, # adjust the starting y position of the text label
                    size=5,
                    hjust=0,
                    family = "Roboto Condensed",
                    # direction="x",
                    segment.size = 0.5,
                    segment.color="#777777"
    ) +
  scale_color_manual(values = colors_prov) +
    # coord_cartesian(
    #   ylim = c(1,500)
    # ) +
    scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
                   # expand = c(0,0.2)
                   ) +
    scale_x_date(date_breaks = "2 day", 
                 date_labels = "%d",
                 limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date +9)),
                 expand = c(0,0) 
    ) + 
    theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      # panel.grid.minor.y = element_blank(),
      axis.ticks.x = element_line(color = "#000000"),
      legend.position =  "none"
    ) +
    labs(title = paste0("Media de muertes por día (media 7 días) por COVID-19 en ",prov ),
         subtitle = paste0("Por provincia. ",period),
         y = "fallecidos por día (media 7 días)",
         x = "fecha",
         caption = caption_provincia)
  
  print(the_province)
  print(paste("plot",prov))
  dev.off()
}

# Log
for ( i in 1:length(levels(data_cases_sp_provinces$ccaa))  ) {
  
  prov <- levels(data_cases_sp_provinces$ccaa)[i]
  unaprov <- data_cases_sp_provinces %>% filter (ccaa == prov ) %>% select (province) %>% first() 
  unaprov <- unaprov[1]
  
  print(prov)
  if (i == 8 ) {
    png(filename=paste0("img/spain/provincias/covid19_muertes-por-dia-provincia-media-superpuesto-log_media-", tolower( substr(prov,1,4) ),"leon.png", sep = ""),width = 1200,height = 800)
  } else if (i == 18 ) {
    png(filename=paste0("img/spain/provincias/covid19_muertes-por-dia-provincia-media-superpuesto-log_media-pais.png", sep = ""),width = 1200,height = 800)
  } else {
    png(filename=paste0("img/spain/provincias/covid19_muertes-por-dia-provincia-media-superpuesto-log_media-", tolower( substr(prov,1,4) ),".png", sep = ""),width = 1200,height = 800)
  }
  the_province  <- data_cases_sp_provinces %>% filter (ccaa == prov ) %>%
  # data_cases_sp_provinces %>% filter (ccaa == prov ) %>%
  ggplot() +
  geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg7,province_cp,-province),
            aes(date,daily_deaths_avg7,group=province_cp), color="#CACACA", size = 0.3 ) +
  geom_line(aes(date, daily_deaths_avg7,group=province, color=province), size= 1.5, se = FALSE, span = 0.6 ) +
  geom_point(aes(date, daily_deaths, color=province), size= 1.5, alpha= 0.5 ) +
  # geom_point(aes(date, daily_deaths_avg7 ), size= 2, color= "#999999" ) +
  # geom_point(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)), aes(date, daily_deaths_avg7, color=province), size= 1.5, alpha = 0.3 ) +
  geom_text_repel(
    data = data_cases_sp_provinces %>% filter( ccaa == prov ) %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
    # data= data_cases_sp_provinces %>% filter (ccaa == prov ) %>% filter(date==max(data_cases_sp_provinces[data_cases_sp_provinces$ccaa == prov,]$date) & (ccaa == prov )), 
                  aes(date, daily_deaths_avg7, color=province, label=paste(format(daily_deaths_avg7, nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # marca un día
  geom_text_repel(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-02") &  province == unaprov ),
                  aes(date,daily_deaths, label=paste("muertes en un día en una provincia")),
                  nudge_y = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  # # marca la línea
  # geom_text_repel(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-04") &  province == "Madrid" ),
  #                 aes(date+0.5,282, label=paste("media de 6 días")),
  #                 nudge_y = 2, # adjust the starting y position of the text label
  #                 size=5,
  #                 hjust=0,
  #                 family = "Roboto Condensed",
  #                 # direction="x",
  #                 segment.size = 0.5,
  #                 segment.color="#777777"
  # ) +
  scale_color_manual(values = colors_prov) +
  # coord_cartesian(
  #   ylim = c(1,500)
  # ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date +9)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "none"
  ) +
  labs(title = paste0("Media de muertes por día (media 7 días) por COVID-19 en ",prov ),
       subtitle = paste0("Por provincia. Escala logarítmica  ",period),
       y = "fallecidos por día (media 7 días)",
       x = "fecha",
       caption = caption_provincia)

  print(the_province)
  print(paste("plot",prov))
  dev.off()
}


# Interactive ---------------
library(plotly)

interactive_dp  <- data_cases_sp_provinces %>% ungroup(province) %>%
  ggplot() +
  geom_line(aes(date, daily_deaths_avg7, group = province, color=ccaa,
                text = paste0("<b>", province, " (", ccaa, ")</b><br>", format( round(daily_deaths_avg7, digits = 1), 
                 big.mark=".", decimal.mark = ","), " media muertes diaria (ventana 7 días)" ,"<br>",date )),                   
                size= 0.7, se = FALSE, span = 0.6 ) +
  geom_point(aes(date, daily_deaths, color=ccaa,
                 text = paste0("<b>", province, " (", ccaa, ")</b><br>", format( round(daily_deaths, digits = 1), 
                        big.mark=".", decimal.mark = ","), " muertes el día " ,"<br>",date )),    
                 size= 0.3
                          ) +
  scale_color_manual(values = colors_prov) +
  coord_cartesian(
    ylim = c(1,400)
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "3 day",
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+13, max(data_cases_sp_provinces$date + 1)),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  c(0.3,0.9)
  ) +
  labs(title = paste0("Media de muertes por día (media 7 días) por COVID-19. España" ),
       subtitle = paste0("Por provincia. Escala logarítmica ",period),
       y = "fallecidos por día (media 7 días)",
       x = "fecha",
       caption = caption_provincia)


# save interactvive
interactive_dp_p <- ggplotly(interactive_dp, tooltip = "text") %>% 
  layout(title = list(text = paste0('Media de muertes por día (media 7 días) por COVID-19 por provincias',
                                    '<br>',
                                    '<sup>',
                                    'Por provincia. Escala logarítmica. Actualizado: 2020.04.28',
                                    '</sup>'))
         , annotations = 
  list(x = 1, y = -0.11, text = "<a style='color:grey;' href='https://lab.montera34.com/covid19'>lab.montera34.com/covid19</a> | Data: <a  style='color:grey;'href='https://github.com/montera34/escovid19data9'>esCOVID19data</a>", 
       showarrow = F, xref='paper', yref='paper', 
       xanchor='right', yanchor='auto', xshift=0, yshift=0,
       font=list(size=15, color="grey")
       )
  )
  
htmlwidgets::saveWidget(as_widget(interactive_dp_p), paste0(getwd(),"/interactive/daily-deaths-provinces-index.html") )

# 6. Hospitalizados por día -----------

for ( i in 1:length(levels(data_cases_sp_provinces$ccaa))  ) {
# for ( i in 1:c(7,8,11,12,18)  ) { print(i)}
  # for ( i in 3:3  ) {
  if (i %in% c(7,8,11,12,18) ) { 
    
  prov <- levels(data_cases_sp_provinces$ccaa)[i]
  unaprov <- data_cases_sp_provinces %>% filter (ccaa == prov ) %>% select (province) %>% first() 
  unaprov <- unaprov[1]
  
  print(prov)
  if (i == 8 ) {
    png(filename=paste0("img/spain/provincias/covid19_hosp-por-dia-provincia-media-superpuesto-lineal_media-", tolower( substr(prov,1,4) ),"leon.png", sep = ""),width = 1200,height = 800)
  } else if (i == 18 ) {
    png(filename=paste0("img/spain/provincias/covid19_hosp-por-dia-provincia-media-superpuesto-lineal_media-pais.png", sep = ""),width = 1200,height = 800)
  } else {
    png(filename=paste0("img/spain/provincias/covid19_hosp-por-dia-provincia-media-superpuesto-lineal_media-", tolower( substr(prov,1,4) ),".png", sep = ""),width = 1200,height = 800)
  }
  the_province  <- data_cases_sp_provinces %>% filter (ccaa == prov ) %>%
    ggplot() +
    geom_line(aes(date, hospitalized ,group=province, color=province), size= 1.2, se = FALSE, span = 0.6 ) +
    geom_point(aes(date, hospitalized, color=province), size= 1, alpha = 0.5 ) +
    geom_text_repel(
      data = data_cases_sp_provinces %>% filter( ccaa == prov ) %>% group_by(province) %>% filter(!is.na(deceased) ) %>% top_n(1, date),
      aes(date, hospitalized, color=province, 
          label=paste(format(hospitalized, nsmall=1, big.mark=".", decimal.mark = ","),province)),
      nudge_x = 1, # adjust the starting y position of the text label
      size=5,
      hjust=0,
      family = "Roboto Condensed",
      direction="y",
      segment.size = 0.1,
      segment.color="#777777"
    ) +
    scale_color_manual(values = colors_prov) +
    scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
    ) +
    scale_x_date(date_breaks = "2 day", 
                 date_labels = "%d",
                 limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date +9)),
                 expand = c(0,0) 
    ) + 
    theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      # panel.grid.minor.y = element_blank(),
      axis.ticks.x = element_line(color = "#000000"),
      legend.position =  "none"
    ) +
    labs(title = paste0("Hospitalizados por día por COVID-19 en ",prov ),
         subtitle = paste0("Por provincia  ",period),
         y = "hospitalizados",
         x = "fecha",
         caption = caption_provincia)
  
  print(the_province)
  print(paste("plot",prov))
  dev.off()
  
  } else { 
    # do not print thesese CCAA
    print ("relax: ")
    print(i)
    }
}

# / 5. Hospitalizados ------------

# // 5.1 Hospitalizados Small multiple ----------
png(filename=paste("img/spain/provincias/covid19_hospitalizados-provincia-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = data_cases_sp_provinces_sm %>% ungroup() %>% select(date,hospitalized,province_cp,-province),
            aes(date,hospitalized,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, hospitalized,group=province) ) +
  geom_point(aes(date, hospitalized), size= 0.5 ) +
  geom_text(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
            aes(date,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$hospitalized),]$hospitalized)/4, label=paste(format(hospitalized, nsmall=1, big.mark="."))),
            size=3,
            hjust=1,
            family = "Roboto Condensed"
  ) +
  facet_wrap(~province) +
  scale_y_continuous(
    # limits = c(0,max(data_cases_sp_provinces$cases_accumulated) ),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date)+1),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de hospitalizados ¿acumulados? por COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "hospitalizados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_hospitalizados-provincia-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,hospitalized,province_cp,-province),
            aes(date,hospitalized,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, hospitalized,group=province) ) +
  geom_point(aes(date, hospitalized), size= 0.5 ) +
  geom_text(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
            aes(date,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$hospitalized),]$hospitalized)/4, label=paste(format(hospitalized, nsmall=1, big.mark="."))),
            size=3,
            hjust=1,
            family = "Roboto Condensed"
  ) +
  facet_wrap(~province) +
  scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de hospitalizados ¿acumulados? por COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "hospitalizados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# SM Agrupado por CCAA -----
png(filename=paste("img/spain/provincias/covid19_hospitalizados-provincia-log_ccaa.png", sep = ""),width = 1200,height = 800)
# prov_sm <- data_cases_sp_provinces %>%
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,hospitalized,province_cp,-province),
            aes(date,hospitalized,group=province_cp), color="#CACACA" ) +
  # add missing ccaa
  # geom_line(data = ccaa_missing, aes(date, hospitalized_per_100000,group=province), size= 0.7, linetype = 2 ) +
  # geom_text_repel(data=filter( ccaa_missing, 
  #                              date==max(data_cases_sp_provinces$date)), 
  #                                     aes(date+1, hospitalized_per_100000, label=paste0("(",format(hospitalized_per_100000, nsmall=1, big.mark="."), " ", substr(province,1,3), ")") ),
  #                                     # nudge_x = 3, # adjust the starting y position of the text label
  #                                     size=4,
  #                                     hjust=0,
  #                                     family = "Roboto Condensed",
  #                                     direction="y",
  #                                     segment.size = 0.1,
#                                     segment.color="#777777"
# ) +
# all the provinces 
geom_line(aes(date, hospitalized,group=province), size= 0.7 ) +
  geom_point(aes(date, hospitalized), size= 0.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, 
                               date==max(data_cases_sp_provinces$date) & hospitalized > 0
                  ), 
                  aes(date+1, hospitalized, label=paste(format(round(as.integer(hospitalized), digits = 0), big.mark="."), substr(province,1,2) ) ),
                  nudge_x = 2, # adjust the starting x position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  facet_wrap(~ccaa) +
  coord_cartesian(
    xlim = c( min(data_cases_sp_provinces$date + 9), max(data_cases_sp_provinces$date +20)),
    ylim = c(1, max( data_cases_sp_provinces[!is.na(data_cases_sp_provinces$hospitalized),]$hospitalized))
  ) +
  scale_y_log10( 
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)) ) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               expand = c(0,1) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de hospitalizados ¿acumulados? por COVID-19 registrados en España.",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "hospitalizados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_hospitalizados-provincia-per-cienmil-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = data_cases_sp_provinces_sm %>% ungroup() %>% select(date,hospitalized_per_100000_per_100000,province_cp,-province),
            aes(date,hospitalized_per_100000_per_100000,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, hospitalized_per_100000_per_100000,group=province)) +
  geom_point(aes(date, hospitalized_per_100000_per_100000), size= 0.5 ) +
  facet_wrap(~province) +
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de hospitalizados ¿acumulados? por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia  (escala lineal). ",period),
       y = "hospitalizados por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_hospitalizados-provincia-per-cienmil-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% 
  ggplot() +
  geom_line(data = data_cases_sp_provinces_sm %>% ungroup() %>% select(date,hospitalized_per_100000_per_100000,province_cp,-province),
            aes(date,hospitalized_per_100000_per_100000,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, hospitalized_per_100000_per_100000,group=province)) +
  geom_point(aes(date, hospitalized_per_100000_per_100000), size= 0.5 ) +
  facet_wrap(~province) +
  scale_y_log10(  
    limits = c(0.05,max(data_cases_sp_provinces$hospitalized_per_100000_per_100000)),
    minor_breaks =  c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "3 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date) ),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de hospitalizados ¿acumulados? por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "hospitalizados por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_hospitalizados-provincia-per-cienmil-log-ccaa.png", sep = ""),width = 1000,height = 600)
data_cases_sp_provinces %>% 
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,hospitalized_per_100000_per_100000,province_cp,-province),
            aes(date,hospitalized_per_100000_per_100000,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, hospitalized_per_100000_per_100000,group=province)) +
  geom_point(aes(date, hospitalized_per_100000_per_100000), size= 0.5 ) +
  facet_wrap(~ccaa) +
  scale_y_log10(  
    limits = c(0.05,max(data_cases_sp_provinces$hospitalized_per_100000_per_100000)),
    minor_breaks =  c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "3 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date) ) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de hospitalizados ¿acumulados? por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "hospitalizados por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# // 5.2 Superpuesto----------------------
png(filename=paste("img/spain/provincias/covid19_hospitalizados-provincia-superpuesto-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, hospitalized,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, hospitalized, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, 
                               date==max(data_cases_sp_provinces$date) & hospitalized > 200 
  ), 
  aes(date, hospitalized, color=ccaa, label=paste(format(hospitalized, nsmall=1, big.mark="."),province)),
  nudge_x = 2, # adjust the starting y position of the text label
  size=5,
  hjust=0,
  family = "Roboto Condensed",
  direction="y",
  segment.size = 0.1,
  segment.color="#777777"
  ) +
  # geom_text(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-14") & province == "Barcelona"),
  #           aes(date, hospitalized, color=ccaa, label=paste(format(hospitalized, nsmall=1, big.mark="."),province)),
  #           nudge_x = 2, # adjust the starting y position of the text label
  #           size=5,
  #           hjust=0,
  #           family = "Roboto Condensed",
  #           direction="y",
  #           segment.size = 0.1,
  #           segment.color="#777777"
  # ) +
  scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date + 7)),
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de hospitalizados ¿acumulados? por COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "hospitalizados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_hospitalizados-provincia-superpuesto-log.png", sep = ""),width = 1300,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, hospitalized,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, hospitalized, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & hospitalized > 10), 
                  aes(date, hospitalized, color=ccaa, label=paste0(format(hospitalized, nsmall=1, big.mark="."), " ", province, "")),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # geom_text(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-14") & province == "Barcelona"),
  #           aes(date, hospitalized, color=ccaa, label=paste(format(hospitalized, nsmall=1, big.mark="."),province)),
  #           nudge_x = 2, # adjust the starting y position of the text label
  #           size=5,
  #           hjust=0,
  #           family = "Roboto Condensed",
  #           direction="y",
  #           segment.size = 0.1,
  #           segment.color="#777777"
  # ) +
  scale_color_manual(values = colors_prov) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 expand = c(0,0.1) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date + 16)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.07,0.7)
  ) +
  labs(title = "Número de hospitalizados ¿acumulados? por COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "hospitalizados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# Per 100.000 ---------
png(filename=paste("img/spain/provincias/covid19_hospitalizados-provincia-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, hospitalized_per_100000_per_100000,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, hospitalized_per_100000_per_100000, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & hospitalized_per_100000_per_100000 > 7), 
                  aes(date, hospitalized_per_100000_per_100000, color=ccaa, 
                      label=paste(format( round(hospitalized_per_100000_per_100000, digits = 1) , nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # geom_text(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-14") & province == "Barcelona"),
  #           aes(date, hospitalized_per_100000_per_100000, color=ccaa, 
  #               label=paste(format(round(hospitalized_per_100000_per_100000, digits = 1) , nsmall=1, big.mark=".", decimal.mark = ","),province)),
  #           nudge_x = 1, # adjust the starting y position of the text label
  #           size=5,
  #           hjust=0,
  #           family = "Roboto Condensed",
  #           direction="y",
  #           segment.size = 0.1,
  #           segment.color="#777777"
  # ) +
  scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date +12)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de hospitalizados ¿acumulados? por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia  (escala lineal). ",period),
       y = "hospitalizados por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_hospitalizados-provincia-superpuesto-per-cienmil-log.png", sep = ""),width = 1300,height = 900)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, hospitalized_per_100000_per_100000,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, hospitalized_per_100000_per_100000, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & hospitalized_per_100000_per_100000 > 12), 
                  aes(date, hospitalized_per_100000_per_100000, color=ccaa, 
                      label=paste(format(round(hospitalized_per_100000_per_100000, digits = 1) , nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # geom_text(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-14") & province == "Barcelona"),
  #           aes(date, hospitalized_per_100000_per_100000, color=ccaa, 
  #               label=paste(format(round(hospitalized_per_100000_per_100000, digits = 1) , nsmall=1, big.mark=".", decimal.mark = ","),province)),
  #           nudge_x = 1, # adjust the starting y position of the text label
  #           size=5,
  #           hjust=0,
  #           family = "Roboto Condensed",
  #           direction="y",
  #           segment.size = 0.1,
  #           segment.color="#777777"
  # ) +
  scale_color_manual(values = colors_prov) +
  coord_cartesian( 
    ylim=c(0.5, max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$hospitalized_per_100000_per_100000),]$hospitalized_per_100000_per_100000)*1.1 )
  ) +
  scale_y_log10(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    # limits = c(0.05,max(data_cases_sp_provinces$hospitalized_per_100000_per_100000)*1.1),
    minor_breaks =  c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ),
    expand = c(0,0.2)
  ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date + 9)),
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de hospitalizados ¿acumulados? por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "hospitalizados por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()


# 7. Deaths vs weekly deaths ------
# lineal --------
png(filename=paste("img/spain/provincias/covid19_trayectoria-provincia-facet-lineal.png", sep = ""),width = 1300,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  # geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(deaths_cum_last_week,deaths_last_week,province_cp,-province),
  #           aes(deaths_cum_last_week,deaths_last_week,group=province_cp), se = FALSE, span = 0.6, color="#CACACA", size=0.5 ) +
  geom_line(aes(deceased,deaths_last_week,group=province, color=province), size= 0.4 ) +
  # geom_smooth(aes(deaths_cum_last_week,deaths_last_week,group=province), size= 0.5, se = FALSE, color = "black") +
  # geom_point(aes(deaths_cum_last_week,deaths_last_week ), size= 0.2 ) +
  geom_text_repel(
    data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
    # data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
                  aes(deceased,deaths_last_week, label= substr(province,1,3) ),
                  nudge_x = 0.8, # adjust the starting y position of the text label
                  size=4,
                  # hjust=0,
                  family = "Roboto Condensed",
                  # direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  facet_wrap( ~ccaa, scales = "free" ) +
  # scale_color_manual(values = colors_prov ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 19) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =9 ),
    legend.position = "none"
  ) +
  labs(title = "Fallecidos 7 días anteriores / total fallecidos por COVID-19 en España",
       subtitle = paste0("Por provincia. ", period),
       y = "fallecidos 7 días anteriores",
       x = "total de fallecidos",
       caption = paste0( caption_provincia , " | Ver web https://aatishb.com/covidtrends/" )
  )
dev.off()

# lineal --------
png(filename=paste("img/spain/provincias/covid19_trayectoria-provincia-facet-lineal_rejilla.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(deceased,deaths_last_week,group=province, color=ccaa), size= 0.4 ) +
  facet_wrap( ~province, scales = "free" ) +
  scale_color_manual(values = colors_prov ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 19) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =9 ),
    legend.position = "none"
  ) +
  labs(title = "Fallecidos 7 días anteriores / total fallecidos por COVID-19 en España",
       subtitle = paste0("Por provincia. ", period),
       y = "fallecidos 7 días anteriores",
       x = "total de fallecidos",
       caption = paste0( caption_provincia , " | Ver web https://aatishb.com/covidtrends/" )
  )
dev.off()

# log --------
png(filename=paste("img/spain/provincias/covid19_trayectoria-provincia-facet-log.png", sep = ""),width = 1300,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(deceased,deaths_last_week,province_cp,-province),
              aes(deceased,deaths_last_week,group=province_cp), se = FALSE, span = 0.6, color="#CACACA", size=0.5 ) +
  geom_line(aes(deceased,deaths_last_week,group=province), size= 0.4 ) +
  # geom_smooth(aes(deaths_cum_last_week,deaths_last_week,group=province), size= 0.5, se = FALSE, color = "black") +
  # geom_point(aes(deaths_cum_last_week,deaths_last_week ), size= 0.2 ) +
  geom_text_repel(
                  # data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
                  data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
                  aes(deceased,deaths_last_week, color=ccaa, label= province ),
                  nudge_x = 0.8, # adjust the starting y position of the text label
                  size=4,
                  # hjust=0,
                  family = "Roboto Condensed",
                  # direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  facet_wrap( ~ccaa ) +
  scale_color_manual(values = colors_prov ) +
  scale_y_log10(
    breaks = c(0,1,5,10,50,100,500,1000,5000 ),
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  ) +
  scale_x_log10(
    breaks = c(0,1,5,10,50,100,500,1000,5000 ),
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 19) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =9 ),
    legend.position = "none"
  ) +
  labs(title = "Fallecidos 7 días anteriores / total fallecidos por COVID-19 en España",
       subtitle = paste0("Por provincia. ", period),
       y = "fallecidos 7 días anteriores (log)",
       x = "total de fallecidos (log)",
       caption = paste0( caption_provincia , " | Ver web https://aatishb.com/covidtrends/" )
  )
       
dev.off()


# lineal --------
png(filename=paste("img/spain/provincias/covid19_trayectoria-provincia-superpuesto-lineal.png", sep = ""),width = 1300,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(deceased,deaths_last_week,group=province,color=ccaa), size= 0.4 ) +
  # geom_smooth(aes(deaths_cum_last_week,deaths_last_week,group=province,color=ccaa), size= 0.5, se = FALSE ) +
  # geom_point(aes(deaths_cum_last_week,deaths_last_week ), size= 0.2 ) +
  geom_text_repel(
    data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
    # data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
                  aes(deceased,deaths_last_week, color=ccaa, label= province ),
                  nudge_x = 0.8, # adjust the starting y position of the text label
                  size=4,
                  # hjust=0,
                  family = "Roboto Condensed",
                  # direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov ) +
  # scale_y_log10(
  #   breaks = c(0,1,5,10,50,100,500,1000,5000 ),
  #   labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #   minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  # ) +
  # scale_x_log10(
  #   breaks = c(0,1,5,10,50,100,500,1000,5000 ),
  #   labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #   minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  # ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 19) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =9 ),
    legend.position = "none"
  ) +
  labs(title = "Fallecidos 7 días anteriores / total fallecidos por COVID-19 en España",
       subtitle = paste0("Por provincia. ", period),
       y = "fallecidos 7 días anteriores (log)",
       x = "total de fallecidos (log)",
       caption = paste0( caption_provincia , " | Ver web https://aatishb.com/covidtrends/" )
  )

dev.off()


png(filename=paste("img/spain/provincias/covid19_trayectoria-provincia-superpuesto-lineal2.png", sep = ""),width = 1300,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(deceased,deaths_last_week,group=province,color=ccaa), size= 0.4 ) +
  # geom_smooth(aes(deaths_cum_last_week,deaths_last_week,group=province,color=ccaa), size= 0.5, se = FALSE ) +
  # geom_point(aes(deaths_cum_last_week,deaths_last_week ), size= 0.2 ) +
  geom_text_repel(
    data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
    aes(deceased,deaths_last_week, color=ccaa, label= province ),
    nudge_x = 0.8, # adjust the starting y position of the text label
    size=4,
    family = "Roboto Condensed",
    segment.size = 0.1,
    segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov ) +
  coord_cartesian(
    ylim = c(0,250),
    xlim = c(0,1000)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 19) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =9 ),
    legend.position = "none"
  ) +
  labs(title = "Fallecidos 7 días anteriores / total fallecidos por COVID-19 en España",
       subtitle = paste0("Por provincia. ", period),
       y = "fallecidos 7 días anteriores (log)",
       x = "total de fallecidos (log)",
       caption = paste0( caption_provincia , " | Ver web https://aatishb.com/covidtrends/" )
  )
dev.off()

png(filename=paste("img/spain/provincias/covid19_trayectoria-provincia-superpuesto-lineal3.png", sep = ""),width = 1300,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(deceased,deaths_last_week,group=province,color=ccaa), size= 0.4 ) +
  # geom_smooth(aes(deaths_cum_last_week,deaths_last_week,group=province,color=ccaa), size= 0.5, se = FALSE ) +
  # geom_point(aes(deaths_cum_last_week,deaths_last_week ), size= 0.2 ) +
  geom_text_repel(
    data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
    aes(deceased,deaths_last_week, color=ccaa, label= province ),
    nudge_x = 0.8, # adjust the starting y position of the text label
    size=4,
    family = "Roboto Condensed",
    segment.size = 0.1,
    segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov ) +
  coord_cartesian(
    ylim = c(0,100),
    xlim = c(0,500)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 19) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =9 ),
    legend.position = "none"
  ) +
  labs(title = "Fallecidos 7 días anteriores / total fallecidos por COVID-19 en España",
       subtitle = paste0("Por provincia. ", period),
       y = "fallecidos 7 días anteriores (log)",
       x = "total de fallecidos (log)",
       caption = paste0( caption_provincia , " | Ver web https://aatishb.com/covidtrends/" )
  )
dev.off()

# log --------
png(filename=paste("img/spain/provincias/covid19_trayectoria-provincia-superpuesto-log.png", sep = ""),width = 1300,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(deceased,deaths_last_week,group=province,color=ccaa), size= 0.4 ) +
  # geom_smooth(aes(deaths_cum_last_week,deaths_last_week,group=province,color=ccaa), size= 0.5, se = FALSE ) +
  # geom_point(aes(deaths_cum_last_week,deaths_last_week ), size= 0.2 ) +
  geom_text_repel(
                  # data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
                  data= data_cases_sp_provinces %>% group_by(province)  %>% filter(!is.na(deceased) ) %>% top_n(1, date),
                  aes(deceased,deaths_last_week, color=ccaa, label= province ),
                  nudge_x = 0.8, # adjust the starting y position of the text label
                  size=4,
                  # hjust=0,
                  family = "Roboto Condensed",
                  # direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov ) +
  scale_y_log10(
    breaks = c(0,1,5,10,50,100,500,1000,5000 ),
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  ) +
  scale_x_log10(
    breaks = c(0,1,5,10,50,100,500,1000,5000 ),
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 19) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =9 ),
    legend.position = "none"
  ) +
  labs(title = "Fallecidos 7 días anteriores / total fallecidos por COVID-19 en España",
       subtitle = paste0("Por provincia. ", period),
       y = "fallecidos 7 días anteriores (log)",
       x = "total de fallecidos (log)",
       caption = paste0( caption_provincia , " | Ver web https://aatishb.com/covidtrends/" )
  )

dev.off()

# 10. Scatter polts ------------

# last_day_available <- data_cases_sp_provinces %>% group_by(province) %>% arrange(date,province) %>% 
#   filter( row_number()==n() ) %>%
#   select(province,date,cases_accumulated,deceased,ccaa) 

letalidad_a <- data.frame(c(0,60000),c(0,8000),c("p","p"))
names(letalidad_a) <- c("x","y","group")
  
# --------- Relaciones --------
png(filename=paste("img/spain/provincias/covid19_muertes-vs-casos-provincia.png", sep = ""),width = 1200,height = 900)
data_cases_sp_provinces %>% # filter(province == "Rioja, La") %>%
  ggplot() +
  # geom_line( aes(date,cases_per_cienmil, group=province, color=ccaa), size= 0.6)
  # geom_line(data = letalidad_a, aes(x,y,group=group)) +
  geom_line( aes(cases_accumulated,deceased, group=province, color=ccaa), size= 0.4) +
  # geom_point( data = data_cases_sp_provinces %>% filter ( date == max(date) ),
  #             aes(cases_per_cienmil,deceassed_per_100000, color=ccaa), size= 4,alpha=0.8 ) +
  geom_point( data = data_cases_sp_provinces %>% top_n(1, date),
              aes(cases_accumulated,deceased, color=ccaa), size= 4,alpha=0.8 ) +
  # lines(x = c(0,0), y = c(20,1000)) +
  # geom_abline(slope = 0.25) +
  # Annotations
  # geom_text(aes(cases_per_cienmil,death_per_cienmil+0.5, color=CCAA,label=paste( substr(date,7,10 ))), size= 3, color="#000000") +
  # geom_text_repel(data= data_cases_sp_provinces %>% group_by(province) %>% filter(date==max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$date),]$date)),
  #                 aes(cases_per_cienmil,deceassed_per_100000, color=ccaa, label=province),
  #                 nudge_y = 5, # adjust the starting y position of the text label
  #                 size=5,
  #                 # hjust=0,
  #                 family = "Roboto Condensed",
  #                 direction="y",
#                 segment.size = 0.1,
#                 segment.color="#777777"
# ) +
geom_text_repel(data= data_cases_sp_provinces %>% top_n(1, date),
                aes(cases_accumulated,deceased, color=ccaa, label=province),
                nudge_y = 5, # adjust the starting y position of the text label
                size=5,
                # hjust=0,
                family = "Roboto Condensed",
                direction="y",
                segment.size = 0.1,
                segment.color="#777777"
) +
  # coord_cartesian(
  #   ylim = c(0,100),
  #   xlim = c(0,1000)
  # ) +
  scale_color_manual(values = colors_prov ) +
  scale_y_continuous( 
    # breaks = c(200,400,600,800,1000,1200,1400,1600,1800,2000)
    # minor_breaks = c(70,80,90,100)
  ) +
  scale_x_continuous( 
    # breaks = c(50,100,150,200,250,300,350)
    # minor_breaks = c(1000,1100,1200,1300)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.9,0.3)
  ) +
  labs(title = "Fallecimientos y casos acumulados COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma.",period),
       y = "fallecimientos",
       x = "casos acumulados",
       caption = caption_provincia,
       color= ""
  )
dev.off()

# last_day_available <- data_cases_sp_provinces %>% group_by(province) %>% arrange(date,province) %>% 
#   filter( row_number()==n() ) %>%
#   select(province,date,cases_per_cienmil,deceassed_per_100000,ccaa)

png(filename=paste("img/spain/provincias/covid19_muertes-vs-casos-provincia-relativo.png", sep = ""),width = 1200,height = 900)
data_cases_sp_provinces %>% # filter(province == "Rioja, La") %>%
  ggplot() +
  # geom_line( aes(date,cases_per_cienmil, group=province, color=ccaa), size= 0.6) 
  geom_line( aes(cases_per_cienmil,deceassed_per_100000, group=province, color=ccaa), size= 0.4) +
  # geom_point( data = data_cases_sp_provinces %>% filter ( date == max(date) ),
  #             aes(cases_per_cienmil,deceassed_per_100000, color=ccaa), size= 4,alpha=0.8 ) +
  geom_point( data = data_cases_sp_provinces %>% top_n(1, date),
              aes(cases_per_cienmil,deceassed_per_100000, color=ccaa), size= 4,alpha=0.8 ) +
  # lines(x = c(0,0), y = c(20,1000)) +
  # geom_abline(slope = 0.25) +
  # Annotations
  # geom_text(aes(cases_per_cienmil,death_per_cienmil+0.5, color=CCAA,label=paste( substr(date,7,10 ))), size= 3, color="#000000") +
  # geom_text_repel(data= data_cases_sp_provinces %>% group_by(province) %>% filter(date==max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$date),]$date)),
  #                 aes(cases_per_cienmil,deceassed_per_100000, color=ccaa, label=province),
  #                 nudge_y = 5, # adjust the starting y position of the text label
  #                 size=5,
  #                 # hjust=0,
  #                 family = "Roboto Condensed",
  #                 direction="y",
  #                 segment.size = 0.1,
  #                 segment.color="#777777"
  # ) +
  geom_text_repel(data= data_cases_sp_provinces %>% top_n(1, date),
                  aes(cases_per_cienmil,deceassed_per_100000, color=ccaa, label=province),
                  nudge_y = 5, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov ) +
  scale_y_continuous( 
    # breaks = c(200,400,600,800,1000,1200,1400,1600,1800,2000)
    # minor_breaks = c(70,80,90,100)
  ) +
  scale_x_continuous( 
    # breaks = c(50,100,150,200,250,300,350)
    # minor_breaks = c(1000,1100,1200,1300)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.9,0.3)
  ) +
  labs(title = "Fallecimientos y casos acumulados COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma.",period),
       y = "fallecimientos por 100.000 habitantes",
       x = "casos acumulados por 100.000 habitantes",
       caption = caption_provincia,
       color= ""
       )
dev.off()


# Días de la semana -------------------
data_cases_sp_provinces$weekday <- weekdays(data_cases_sp_provinces$date)

data_cases_sp_provinces$weekday <- factor(data_cases_sp_provinces$weekday, levels = c("lunes","martes", "miércoles", "jueves", "viernes",
                                              "sábado","domingo" ) )

png(filename=paste("tmp/weekdays/dias-semana-euskadi-01.png", sep = ""),width = 1150,height = 400)
data_cases_sp_provinces %>% filter ( ccaa == "País Vasco") %>% # filter ( province == "Bizkaia") %>%
  ggplot() +
  geom_col( aes(date,daily_deaths, fill=weekday), width = 1 ) +
  geom_line( aes(date,daily_deaths_avg7, group=province), size = 2 ) +
  # geom_col( data = data_cases_sp_provinces %>% filter(weekday == "jueves"), aes(date,daily_deaths, group=province, fill="#DD0000")) +
  facet_wrap(~province) +
  scale_fill_manual(values = c("#AAAAAA","#AAAAAA","#FF88AA","#AAAAAA","#AAAAAA","#CCAAAA","#CCAAAA") ) +
  scale_y_continuous( 
    # breaks = c(200,400,600,800,1000,1200,1400,1600,1800,2000)
    # minor_breaks = c(70,80,90,100)
  ) + 
  scale_x_date(
    date_breaks = "7 day",
     date_labels = "%d",
     limits=c( min(data_cases_sp_provinces$date)+20, max(data_cases_sp_provinces$date)+2),
     expand = c(0,1)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "top"
  ) +
  labs(title = "Muertes por día COVID-19 en Euskadi",
       subtitle = paste0("Buscando un patrón para la fecha de notificación de fallecidos. Línea: media de 7 días. Actualizado: ", filter_date ),
       y = "fallecimientos por día",
       x = "fecha",
       caption = caption_provincia,
       fill = "día de la semana"
  )
  dev.off()
# 
# png(filename=paste("tmp/weekdays/dias-semana-euskadi-02.png", sep = ""),width = 900,height = 900)
# data_cases_sp_provinces %>% filter ( ccaa == "País Vasco") %>%
#   ggplot() +
#   geom_col( aes(date,daily_deaths, fill=weekday), size = 5 ,width = 5  ) +
#   # geom_line( aes(date,daily_deaths_avg7, group=province) ) +
#   # geom_col( data = data_cases_sp_provinces %>% filter(weekday == "jueves"), aes(date,daily_deaths, group=province, fill="#DD0000")) +
#   facet_wrap(~weekday) +
#   scale_fill_manual(values = c("#AAAAAA","#AAAAAA","#8888AA","#AAAAAA","#AAAAAA","#CCAAAA","#CCAAAA") ) +
#   scale_y_continuous( 
#     # breaks = c(200,400,600,800,1000,1200,1400,1600,1800,2000)
#     # minor_breaks = c(70,80,90,100)
#   ) + 
#   scale_x_date(date_breaks = "7 day", 
#                date_labels = "%d",
#                limits=c( min(data_cases_sp_provinces$date)+20, max(data_cases_sp_provinces$date)),
#                expand = c(0,0)
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     # panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "top"
#   ) +
#   labs(title = "Muertes por día COVID-19 en Euskadi",
#        subtitle = paste0("Buscando un patrón para la fecha de notificación de fallecidos. Línea: media de 7 días"),
#        y = "fallecimientos por día",
#        x = "fecha",
#        caption = caption_provincia,
#        fill = "día de la semana"
#   )
# dev.off()

for ( i in 1:52 ) {
# for ( i in 1:2 ) {
la_prov <- unique(data_cases_sp_provinces$province)[i]

png(filename=paste("tmp/weekdays/dias-semana-prov-",i,".png", sep = ""),width = 1030,height = 400)

weekdaychart <- data_cases_sp_provinces %>% filter ( province == la_prov ) %>%
  ggplot() +
  geom_col( aes(date,daily_deaths, fill=weekday), width = 1 ) +
  geom_line( aes(date,daily_deaths_avg7, group=province), size = 2 ) +
  # geom_col( data = data_cases_sp_provinces %>% filter(weekday == "jueves"), aes(date,daily_deaths, group=province, fill="#DD0000")) +
  facet_wrap(~province) +
  scale_fill_manual(values = c("#AAAAAA","#AAAAAA","#FF88AA","#AAAAAA","#AAAAAA","#CCAAAA","#CCAAAA") ) +
  scale_y_continuous( 
    # breaks = c(200,400,600,800,1000,1200,1400,1600,1800,2000)
    # minor_breaks = c(70,80,90,100)
  ) + 
  scale_x_date(date_breaks = "7 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+20, max(data_cases_sp_provinces$date)+2),
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "top"
  ) +
  labs(title = paste0("Muertes por día COVID-19 en ",la_prov),
       subtitle = paste0("Buscando un patrón para la fecha de notificación de fallecidos. Línea: media de 7 días. Actualizado: 2020.05.06"),
       y = "fallecimientos por día",
       x = "fecha",
       caption = caption_provincia,
       fill = "día de la semana"
  )
print(weekdaychart)
dev.off()

}

# Incidencia acumulada 14 días -----
# Superpuesto Lineal 
png(filename=paste("img/spain/provincias/covid19_incidencia-14dias-provincia-superpuesto-lineal-per-cienmil.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  # geom_line(aes(date, daily_cases,group=province, color=ccaa), size= 0.8 ) +
  geom_line(aes(date, cases_14days/poblacion,group=province, color=ccaa), size= 0.8 ) +
  # geom_point(aes(date, daily_cases, color=ccaa), size= 1 ) +
  geom_point(aes(date, cases_14days/poblacion, color=ccaa), size= 1 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & daily_cases > 5), 
                  aes(date, cases_14days/poblacion, color=ccaa, label=paste(format(daily_cases, nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  coord_cartesian(
    # ylim = c( 0,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$daily_cases),]$daily_cases) )
  ) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  # scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #                minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
  #                expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+15, max(data_cases_sp_provinces$date +12)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Incidencia acumulada 14 días por 100.000 habitantes por COVID-19 en España",
       subtitle = paste0("Por provincia ",period),
       y = "Incidencia acumulada 14 días",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_incidencia-14dias-provincia-superpuesto-lineal-per-cienmil_pais.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% filter( ccaa == "País Vasco") %>%
  ggplot() +
  # geom_line(aes(date, daily_cases,group=province, color=ccaa), size= 0.8 ) +
  geom_line(aes(date, cases_14days/poblacion*100000,group=province, color=province), size= 0.8 ) +
  # geom_point(aes(date, daily_cases, color=ccaa), size= 1 ) +
  geom_point(aes(date, cases_14days/poblacion*100000, color=province), size= 1 ) +
  geom_text_repel(data= data_cases_sp_provinces %>% group_by(province) %>% filter( ccaa == "País Vasco" ) %>% top_n(1, date), 
                  aes(date, cases_14days/poblacion*100000, color=ccaa, label=paste(format(daily_cases, nsmall=1, big.mark=".", decimal.mark = ","),province)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  # coord_cartesian(
  #   # ylim = c( 0,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$daily_cases),]$daily_cases) )
  # ) +
  # scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  # ) +
  # scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #                minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
  #                expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+15, max(data_cases_sp_provinces$date +12)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Incidencia acumulada 14 días por 100.000 habitantes por COVID-19 en Euskadi",
       subtitle = paste0("Por provincia ",period),
       y = "Incidencia acumulada 14 días",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# Sources ----
xxx <- as.data.frame(table(data_cases_sp_provinces$source_name))


df <- read.table(textConnection("1|a,b,c\n2|a,c\n3|b,d\n4|e,f"), header = F, sep = "|", stringsAsFactors = F)
df
s <- strsplit(df$V2, split = ",")
data.frame(V1 = rep(df$V1, sapply(s, length)), V2 = unlist(s))


zz <- strsplit( data_cases_sp_provinces$source_name, split =';')
zz
xx <- data.frame(V1 = rep(data_cases_sp_provinces$source_name, sapply(zz, length)), V2 = unlist(zz))

zz<- as.data.frame(zz)
yy <- zz[643]

s <- strsplit(df$V2, split = ",")

x1 <- separate_rows(data_cases_sp_provinces, source_name)

data_cases_sp_provinces %>% m