# Analizar datos de Coronavirus COVID-19 en España por provincia
  
# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping

# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption <- "Gráfico: lab.montera34.com/covid19 | Datos: Ministerio de Sanidad de España extraídos por Datadista.com"
caption_en <- "By: lab.montera34.com/covid19 | Data: ProvidencialData19. Check code.montera34.com/covid19"
caption_provincia <- "Gráfico: montera34.com | Datos: Varias fuentes (recopilado por Providencialdata19). Ver lab.montera34.com/covid19"
period <- "2020.02.27 - 03.31"

# Load Data ---------
# / Population -------------
ccaa_poblacion <-  read.delim("data/original/spain/ccaa-poblacion.csv",sep = ";")
provincias_poblacion <-  read.delim("data/original/spain/provincias-poblacion.csv",sep = ",")

# / COVID-19 in Spain -----------
# / By province -----------
data_cases_sp_provinces <- read.delim("data/original/spain/covid19_spain_provincias.csv",sep = ",")  

# Process data ------
# Create date variable
data_cases_sp_provinces$date  <- as.Date(data_cases_sp_provinces$date)

# add population data
data_cases_sp_provinces <- merge( data_cases_sp_provinces, select(provincias_poblacion,provincia,poblacion,ine_code), by.x = "province", by.y = "provincia", all = TRUE   )

data_cases_sp_provinces <- filter(data_cases_sp_provinces, !is.na(date))

# calculate values per 
data_cases_sp_provinces$cases_per_cienmil <- round( data_cases_sp_provinces$cases_accumulated / data_cases_sp_provinces$poblacion * 100000, digits = 2)
data_cases_sp_provinces$intensive_care_per_1000000 <- round( data_cases_sp_provinces$intensive_care / data_cases_sp_provinces$poblacion * 100000, digits = 2)
data_cases_sp_provinces$deceassed_per_100000 <- round( data_cases_sp_provinces$deceased / data_cases_sp_provinces$poblacion * 100000, digits = 2)

data_cases_sp_provinces <- data_cases_sp_provinces %>% filter( date != as.Date("2020-04-01"))


write.csv(data_cases_sp_provinces, file = "data/output/spain/covid19-provincias-spain_consolidated.csv", row.names = FALSE)


# colors ---------
# extends color paletter
library(RColorBrewer)
# creates extended color palette https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
colourCount <- length(unique(data_cases_sp_provinces$ccaa))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

# Plots --------------------
# / 1. Cases ------------

# create temp dataframes to be able to plot all the values in small multiples
data_cases_sp_provinces_sm <- data_cases_sp_provinces %>% filter( date != as.Date("2020-04-01"))
data_cases_sp_provinces_sm$province_cp <- data_cases_sp_provinces[data_cases_sp_provinces$date != as.Date("2020-04-01"),]$province

# Remove last day
data_cases_sp_provinces <- data_cases_sp_provinces %>% filter( date != as.Date("2020-04-01"))

# // 1.1 Small multiple ------------
# /// Provincias small multiple --------------
# Escala lineal
png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-lineal.png", sep = ""),width = 1400,height = 1000)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,cases_accumulated,province_cp,-province),
            aes(date,cases_accumulated,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_accumulated,group=province) ) +
  geom_point(aes(date,cases_accumulated,group=province), size = 0.5 ) +
  facet_wrap( ~province) +
  scale_y_continuous(
    limits = c(0,max(data_cases_sp_provinces$cases_accumulated) ),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "3 day", 
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
       subtitle = paste0("Por provincia [Cataluña: Lleida y Girona, Canarias por islas] (escala lineal). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# Escala logarítmica
png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-log.png", sep = ""),width = 1400,height = 1000)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,cases_accumulated,province_cp,-province),
            aes(date,cases_accumulated,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_accumulated,group=province), size = 1 ) +
  geom_point(aes(date,cases_accumulated,group=province), size = 0.5 ) +
  scale_y_log10(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    limits = c(1,max(data_cases_sp_provinces$cases_accumulated)),
    minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  facet_wrap( ~province) +
  scale_x_date(date_breaks = "3 day", 
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
       subtitle = paste0("Por provincia [Cataluña: Lleida y Girona, Canarias por islas] (escala logarítmica). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption_provincia)
dev.off()


png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-region-grouped-log.png", sep = ""),width = 1200,height = 700)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,cases_accumulated,province_cp,-province),
            aes(date,cases_accumulated,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_accumulated,group=province), size = 0.7 ) +
  geom_point(aes(date,cases_accumulated,group=province), size = 0.5 ) +
  scale_y_log10(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    limits = c(1,max(data_cases_sp_provinces$cases_accumulated)),
    minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  facet_wrap( ~ccaa) +
  scale_x_date(date_breaks = "3 day", 
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
       subtitle = paste0("Por comunidad autónoma y provincias [Cataluña: Lleida y Girona, Canarias por islas] (escala logarítmica). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# // 1.2 Superpuesto ---------------
# ---- Provincias superpuesto -----
png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-superpuesto-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date,cases_accumulated,group=province, color=ccaa ), size = 1.0  ) +
  geom_point(aes(date,cases_accumulated,group=province, color=ccaa), size = 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, 
                               date==max(data_cases_sp_provinces$date) & cases_accumulated > 100
  ), 
        aes(date, cases_accumulated, color=ccaa, label=paste(format(cases_accumulated, nsmall=1, big.mark="."),province)),
              nudge_x = 3, # adjust the starting y position of the text label
              size=5,
              hjust=0,
              family = "Roboto Condensed",
              direction="y",
              segment.size = 0.1,
              segment.color="#777777"
  ) +
  scale_color_manual(values = getPalette(colourCount )) +
  scale_y_continuous(
    limits = c(0,max(data_cases_sp_provinces$cases_accumulated)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date + 8))
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por provincia [Cataluña: Lleida y Girona, Canarias por islas] (escala lineal). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-superpuesto-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date,cases_accumulated,group=province, color=ccaa ), size = 1 ) +
  geom_point(aes(date,cases_accumulated,group=province, color=ccaa), size = 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, 
                               date==max(data_cases_sp_provinces$date) & cases_accumulated > 100
  ), 
      aes(date, cases_accumulated, color=ccaa, label=paste(format(cases_accumulated, nsmall=1, big.mark="."), province)),
          nudge_x = 3, # adjust the starting y position of the text label
          size=5,
          hjust=0,
          family = "Roboto Condensed",
          direction="y",
          segment.size = 0.1,
          segment.color="#333333"
  ) +
  scale_color_manual(values = getPalette(colourCount )) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 limits = c(1,max(data_cases_sp_provinces$cases_accumulated)),
                 # breaks = c(1,10,100,1000),
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000), seq(10000, 100000, 10000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date + 7))
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
       subtitle = paste0("Por comunidad autónoma [Cataluña: Lleida y Girona, Canarias por islas] (escala logarítmica). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, cases_per_cienmil,group= province, color= ccaa), size= 1 ) +
  geom_point(aes(date,cases_per_cienmil, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, 
                               date==max(data_cases_sp_provinces$date) 
  ), 
  aes(date,cases_per_cienmil, color=ccaa, label=paste(format(cases_per_cienmil, nsmall=1, big.mark=".", digits = 1), province)),
          nudge_x = 2, # adjust the starting y position of the text label
          size=5,
          hjust=0,
          family = "Roboto Condensed",
          direction="y",
          segment.size = 0.1,
          segment.color="#333333"
  ) +
  scale_color_manual(values = getPalette(colourCount )) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".",small.mark = ",", scientific = FALSE),
                 ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date + 7)) 
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
       subtitle = paste0("Por provincia [Cataluña: Lleida y Girona, Canarias por islas] (escala lineal). ",period),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_casos-registrados-por-provincia-superpuesto-per-cienmil-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, cases_per_cienmil,group= province, color= ccaa), size= 1 ) +
  geom_point(aes(date,cases_per_cienmil, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, 
                               date==max(data_cases_sp_provinces$date) & cases_per_cienmil > 50
                      ), 
                      aes(date,cases_per_cienmil, color=ccaa, label=paste(format(cases_per_cienmil, nsmall=1, big.mark=".", digits = 1), province)),
                              nudge_x = 4, # adjust the starting y position of the text label
                              size=5,
                              # hjust=1,
                              family = "Roboto Condensed",
                              direction="y",
                              segment.size = 0.1,
                              segment.color="#333333"
                              # xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2020-01-4"))
                      ) +
  scale_color_manual(values = getPalette(colourCount )) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 limits = c(1,max(data_cases_sp_provinces$cases_per_cienmil)),
                 minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date + 7)) 
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
       subtitle = paste0("Por provincia [Cataluña: Lleida y Girona, Canarias por islas] (escala logarítmica). ",period),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# / 2. UCI (intensive care) -------------------

# // 2.1 UCI Small multiple 

# / 3. Deceassed (Fallecimientos) ------------

# create temp dataframes to be able to plot all the values in small multiples
data_cases_sp_provinces_sm <- data_cases_sp_provinces
data_cases_sp_provinces_sm$province_cp <- data_cases_sp_provinces$province

# // 3.1 Fallecimientos Small multiple ----------
png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,deceased,province_cp,-province),
            aes(date,deceased,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, deceased,group=province) ) +
  geom_point(aes(date, deceased), size= 0.5 ) +
  geom_text(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
                  aes(date,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceased),]$deceased)/4, label=paste(format(deceased, nsmall=1, big.mark="."))),
                  size=3,
                  hjust=1,
                  family = "Roboto Condensed"
  ) +
  facet_wrap(~province) +
  scale_y_continuous(
    # limits = c(0,max(data_cases_sp_provinces$cases_accumulated) ),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "3 day", 
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
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por provincia [Cataluña: Lleida y Girona, Canarias por islas] (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,deceased,province_cp,-province),
            aes(date,deceased,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, deceased,group=province) ) +
  geom_point(aes(date, deceased), size= 0.5 ) +
  geom_text(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
            aes(date,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceased),]$deceased)/4, label=paste(format(deceased, nsmall=1, big.mark="."))),
            size=3,
            hjust=1,
            family = "Roboto Condensed"
  ) +
  facet_wrap(~province) +
  scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
                 ) +
  scale_x_date(date_breaks = "3 day", 
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
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por provincia [Cataluña: Lleida y Girona, Canarias por islas] (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-log_ccaa.png", sep = ""),width = 1000,height = 600)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,deceased,province_cp,-province),
            aes(date,deceased,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, deceased,group=province) ) +
  geom_point(aes(date, deceased), size= 0.5 ) +
  facet_wrap(~ccaa) +
  scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100)) ) +
  scale_x_date(date_breaks = "3 day", 
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
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por provincia [Cataluña: Lleida y Girona, Canarias por islas] (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-per-cienmil-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% filter(ccaa != "Canarias") %>%
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,deceassed_per_100000,province_cp,-province),
            aes(date,deceassed_per_100000,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, deceassed_per_100000,group=province)) +
  geom_point(aes(date, deceassed_per_100000), size= 0.5 ) +
  facet_wrap(~province) +
  scale_x_date(date_breaks = "3 day", 
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
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia  [Cataluña: Lleida y Girona, Canarias por islas] (escala lineal). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-per-cienmil-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% filter(ccaa != "Canarias") %>%
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,deceassed_per_100000,province_cp,-province),
            aes(date,deceassed_per_100000,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, deceassed_per_100000,group=province)) +
  geom_point(aes(date, deceassed_per_100000), size= 0.5 ) +
  facet_wrap(~province) +
  scale_y_log10(  
    limits = c(0.05,max(data_cases_sp_provinces$deceassed_per_100000)),
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
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia [Cataluña: Lleida y Girona, Canarias por islas] (escala logarítmica). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-per-cienmil-log-ccaa.png", sep = ""),width = 1000,height = 600)
data_cases_sp_provinces %>% filter(ccaa != "Canarias") %>%
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,deceassed_per_100000,province_cp,-province),
            aes(date,deceassed_per_100000,group=province_cp), color="#CACACA" ) +
  geom_line(aes(date, deceassed_per_100000,group=province)) +
  geom_point(aes(date, deceassed_per_100000), size= 0.5 ) +
  facet_wrap(~ccaa) +
  scale_y_log10(  
    limits = c(0.05,max(data_cases_sp_provinces$deceassed_per_100000)),
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
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia [Cataluña: Lleida y Girona, Canarias por islas] (escala logarítmica). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# // 3.2 Superpuesto----------------------
png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-superpuesto-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, deceased,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, deceased, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & deceased > 30), 
                  aes(date, deceased, color=ccaa, label=paste(format(deceased, nsmall=1, big.mark="."),province)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = getPalette(colourCount )) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date + 6)),
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
       subtitle = paste0("Por provincia [Cataluña: Lleida y Girona, Canarias por islas] (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-superpuesto-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, deceased,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, deceased, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & deceased > 10), 
                  aes(date, deceased, color=ccaa, label=paste(format(deceased, nsmall=1, big.mark="."),province)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = getPalette(colourCount )) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 expand = c(0,0.1) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date + 6)),
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
       subtitle = paste0("Por provincia [Cataluña: Lleida y Girona, Canarias por islas] (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, deceassed_per_100000,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, deceassed_per_100000, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & deceassed_per_100000 > 0.5), 
                  aes(date, deceassed_per_100000, color=ccaa, 
                      label=paste(format(deceassed_per_100000, nsmall=1, big.mark="."),province)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = getPalette(colourCount )) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date + 6)),
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
       subtitle = paste0("Por provincia  [Cataluña: Lleida y Girona, Canarias por islas] (escala lineal). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-superpuesto-per-cienmil-log.png", sep = ""),width = 1300,height = 900)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(aes(date, deceassed_per_100000,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, deceassed_per_100000, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & deceassed_per_100000 > 7), 
                  aes(date, deceassed_per_100000, color=ccaa, 
                      label=paste(format(deceassed_per_100000, nsmall=1, big.mark="."),province)),
                  nudge_x = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = getPalette(colourCount )) +
  coord_cartesian( 
    ylim=c(0.05, max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceassed_per_100000),]$deceassed_per_100000)*1.05 )
  ) +
  scale_y_log10(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    # limits = c(0.05,max(data_cases_sp_provinces$deceassed_per_100000)),
    minor_breaks =  c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ),
    expand = c(0,0.1)) +
  scale_x_date(date_breaks = "1 day", 
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
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por provincia [Cataluña: Lleida y Girona, Canarias por islas] (escala logarítmica). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()
  
