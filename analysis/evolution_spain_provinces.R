# Analizar datos de Coronavirus COVID-19 en España por provincia
  
# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping

# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption <- "Gráfico: lab.montera34.com/covid19 | Datos: Ministerio de Sanidad de España extraídos por Datadista.com"
caption_en <- "By: lab.montera34.com/covid19 | Data: ProvidencialData19. Check code.montera34.com/covid19"
caption_provincia <- "Gráfico: montera34.com | Datos: recopilado por Providencialdata19 (lab.montera34.com/covid19, bit.ly/amadrinaunaccaa)"
period <- "2020.02.27 - 04.13"
filter_date <- as.Date("2020-04-14")

# Load Data ---------
# / Population -------------
ccaa_poblacion <-  read.delim("data/original/spain/ccaa-poblacion.csv",sep = ";")
provincias_poblacion <-  read.delim("data/original/spain/provincias-poblacion.csv",sep = ",")

# / COVID-19 in Spain -----------
# / By province -----------
data_cases_sp_provinces <- read.delim("data/original/spain/covid19_spain_provincias.csv",sep = ",")
# data_cases_canarias <- read.delim("data/original/spain/covid19_canarias.csv",sep = ",")

# Process data ------
# Create date variable
data_cases_sp_provinces$date  <- as.Date(data_cases_sp_provinces$date)

# Agreggate Canary islands -------
canarias <- data_cases_sp_provinces %>% filter( ccaa == "Canarias")
names(canarias)
# Group by province
tenerife <- canarias %>% filter(province == "La Gomera" | province =="La Palma" | province == "Tenerife" | province == "El Hierro") %>% group_by(date) %>% summarise(
  province = "Santa, Cruz de Tenerife",
  ccaa = "Canarias",
  new_cases = sum(new_cases),
  activos = sum(activos),
  hospitalized = sum(hospitalized),
  intensive_care = sum(intensive_care),
  deceased = sum(deceased),
  cases_accumulated = sum(cases_accumulated),
  recovered = sum(recovered),
  source = paste(source, collapse = ";"),
  comments = paste(comments, collapse = ";")
)
palmas <- canarias %>% filter(province == "Fuerteventura" | province =="Lanzarote" | province == "Gran Canaria") %>% group_by(date) %>% summarise(
  province = "Palmas, Las",
  ccaa = "Canarias",
  new_cases = sum(new_cases),
  activos = sum(activos),
  hospitalized = sum(hospitalized),
  intensive_care = sum(intensive_care),
  deceased = sum(deceased),
  cases_accumulated = sum(cases_accumulated),
  recovered = sum(recovered),
  source = paste(source, collapse = ";"),
  comments = paste(comments, collapse = ";")
)

# bind Palmas and Tenerife
canarias_bind <- rbind(tenerife,palmas)

# Remove Canarias and adds it as provinces
data_cases_sp_provinces <-  data_cases_sp_provinces %>% filter( ccaa != "Canarias")
# Add Canarias
data_cases_sp_provinces <- rbind(data_cases_sp_provinces,canarias_bind)

# Remove last -usually incomplete- day
data_cases_sp_provinces <- filter(data_cases_sp_provinces, !is.na(date))
data_cases_sp_provinces <- data_cases_sp_provinces %>% filter( date != filter_date) %>% arrange(date)

# Add missin Barcelona data -------- 
# # Do not use as data have been hasd coded in the original data
# #  select all the cataluña provinces (Barcelona is not available) to calculate how many deaths
# prov_cat <- data_cases_sp_provinces %>% filter( ccaa == "Cataluña" & province != "Barcelona"  & date > as.Date("2020-03-08") ) %>%
#   group_by(date ) %>% summarise( tot_menos_bc = sum(deceased, na.rm = TRUE) )
# prov_cat$date
# # selects dates of DAtadista database for Cataluña region
# catalunya_datadista <- data_all_export %>% filter( region == "Cataluña" & date > as.Date("2020-03-08") &  date < as.Date("2020-04-13") ) %>%
#   select(date,deceassed)
# catalunya_datadista$date
# # insert Cataluña deaths bu Barcelona
# catalunya_datadista$tot_menos_bcn <- prov_cat$tot_menos_bc
# catalunya_datadista <- as.data.frame(catalunya_datadista)
# # calculates Barcelona deaths
# catalunya_datadista$deaths_bcn <- catalunya_datadista$deceassed - catalunya_datadista$tot_menos_bc
# catalunya_datadista$province <- "Barcelona"
# 
# # compare dates to see they are the same
# catalunya_datadista[catalunya_datadista$date > as.Date("2020-03-05") &
#                       catalunya_datadista$date < as.Date("2020-04-13") ,]$date
# data_cases_sp_provinces[  ( data_cases_sp_provinces$date > min(catalunya_datadista$date +2 ) ) &
#                             ( data_cases_sp_provinces$date < as.Date("2020-04-13") ) &
#                             data_cases_sp_provinces$province == "Barcelona", ]$date
# 
# # inserts Barcelona deaths
# data_cases_sp_provinces[  ( data_cases_sp_provinces$date > min(catalunya_datadista$date +2 ) ) &
#                             ( data_cases_sp_provinces$date < as.Date("2020-04-13") ) &
#                             data_cases_sp_provinces$province == "Barcelona", ]$deceased <- catalunya_datadista[catalunya_datadista$date > as.Date("2020-03-05") &
#                                                                                                                  catalunya_datadista$date < as.Date("2020-04-13") ,]$deaths_bcn

# add population data -----
data_cases_sp_provinces <- merge( data_cases_sp_provinces, select(provincias_poblacion,provincia,poblacion,ine_code), by.x = "province", by.y = "provincia", all = TRUE   )

# calculate values per 
data_cases_sp_provinces$cases_per_cienmil <- round( data_cases_sp_provinces$cases_accumulated / data_cases_sp_provinces$poblacion * 100000, digits = 2)
data_cases_sp_provinces$intensive_care_per_1000000 <- round( data_cases_sp_provinces$intensive_care / data_cases_sp_provinces$poblacion * 100000, digits = 2)
data_cases_sp_provinces$deceassed_per_100000 <- round( data_cases_sp_provinces$deceased / data_cases_sp_provinces$poblacion * 100000, digits = 2)

# Calculates daily deaths
data_cases_sp_provinces <- data_cases_sp_provinces %>% 
  group_by(province) %>% arrange(date) %>% 
  mutate( daily_deaths = deceased - lag(deceased),
          daily_deaths_inc = round((deceased - lag(deceased)) /lag(deceased) * 100, digits = 1),
          daily_deaths_avg3 =  round( ( daily_deaths + lag(daily_deaths,1)+lag(daily_deaths,2) ) / 3, digits = 1 ), # average of daily deaths of 3 last days
          daily_deaths_avg6 =  round( ( daily_deaths + lag(daily_deaths,1)+lag(daily_deaths,2)+
                                          lag(daily_deaths,3)+lag(daily_deaths,4)+lag(daily_deaths,5) ) / 6, digits = 1 ),  # average of dayly deaths of 6 last days
          deaths_cum_last_week = ( deceased + lag(deceased,1) + lag(deceased,2) + lag(deceased,3) + lag(deceased,4) + lag(deceased,5) + lag(deceased,6) ) / 7,  
          deaths_last_week =  daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) + lag(daily_deaths,4) + lag(daily_deaths,5) + lag(daily_deaths,6)  
  )

data_cases_sp_provinces <- data_cases_sp_provinces %>% select(date,province,ine_code,everything()) %>%
                                                  select(-source,-comments,source,comments)

write.csv(data_cases_sp_provinces, file = "data/output/spain/covid19-provincias-spain_consolidated.csv", row.names = FALSE)


# colors ---------
# extends color paletter
library(RColorBrewer)
# creates extended color palette https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
colourCount <- length(unique(data_cases_sp_provinces$ccaa))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
colors_prov <- getPalette(colourCount )
# Change yellow to blue
colors_prov[12] <- "#84d3e7"

# Plots --------------------
# / 1. Cases ------------

data_cases_sp_provinces <- data_cases_sp_provinces %>% filter(date > as.Date("2020-02-25"))

# create temp dataframes to be able to plot all the values in small multiples
data_cases_sp_provinces_sm <- data_cases_sp_provinces %>% filter( date != filter_date)
data_cases_sp_provinces_sm$province_cp <- data_cases_sp_provinces[data_cases_sp_provinces$date != filter_date,]$province 

# Remove last day
data_cases_sp_provinces <- data_cases_sp_provinces %>% filter( date != filter_date)

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
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
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
  # geom_line(cattotal,aes(date,cases_accumulated,group=province), size = 1, color="red" ) +
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
       subtitle = paste0("Por comunidad autónoma y provincias (escala logarítmica). ",period),
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
                               (date==max(data_cases_sp_provinces$date) & cases_accumulated > 400 )  
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
  geom_text_repel(data=filter( data_cases_sp_provinces,  
                                (date==max(data_cases_sp_provinces$date-2) & province == "Barcelona")
                                        ), 
            aes(date, cases_accumulated, color=ccaa, label=paste(format(cases_accumulated, nsmall=1, big.mark="."),province)),
                      nudge_y = 2, # adjust the starting y position of the text label
                      size=5,
                      hjust=0,
                      family = "Roboto Condensed",
                      direction="y",
                      segment.size = 0.1,
                      segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  scale_y_continuous(
    limits = c(0,max(data_cases_sp_provinces$cases_accumulated)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date + 14)),
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
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
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
                               (date==max(data_cases_sp_provinces$date) & cases_accumulated > 400 )  
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
  geom_text_repel(data=filter( data_cases_sp_provinces,  
                               (date==max(data_cases_sp_provinces$date-3) & province == "Barcelona") |
                                 (date==max(data_cases_sp_provinces$date-8) & ccaa == "Galicia")
  ), 
  aes(date, cases_accumulated, color=ccaa, label=paste(format(cases_accumulated, nsmall=1, big.mark="."),province)),
  nudge_y = 1, # adjust the starting y position of the text label
  size=5,
  hjust=0,
  family = "Roboto Condensed",
  # direction="y",
  segment.size = 0.1,
  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 limits = c(1,max(data_cases_sp_provinces$cases_accumulated)),
                 # breaks = c(1,10,100,1000),
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000), seq(10000, 100000, 10000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date + 10)),
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
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
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
  scale_color_manual(values = colors_prov) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".",small.mark = ",", scientific = FALSE),
                 ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date + 10)),
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
  geom_line(aes(date, cases_per_cienmil,group= province, color= ccaa), size= 1 ) +
  geom_point(aes(date,cases_per_cienmil, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, 
                               date==max(data_cases_sp_provinces$date) & cases_per_cienmil > 100
                      ), 
                      aes(date,cases_per_cienmil, color=ccaa, label=paste(format(cases_per_cienmil, nsmall=1, big.mark=".", digits = 1), province)),
                              nudge_x = 5, # adjust the starting y position of the text label
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
                 limits = c(1,max(data_cases_sp_provinces$cases_per_cienmil)),
                 minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date), max(data_cases_sp_provinces$date + 18)),
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

# / 2. UCI (intensive care) -------------------

# // 2.1 UCI Small multiple 

# / 3. Deceassed (Fallecimientos) ------------

# create temp dataframes to be able to plot all the values in small multiples
data_cases_sp_provinces_sm <- data_cases_sp_provinces 
data_cases_sp_provinces_sm$province_cp <- data_cases_sp_provinces$province  

ccaa_missing <- data_all_export %>% filter( region == "Galicia") %>% filter( date < filter_date) %>% 
  mutate (province = region, ccaa = region)

# // 3.1 Fallecimientos Small multiple ----------
png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = data_cases_sp_provinces_sm %>% ungroup() %>% select(date,deceased,province_cp,-province),
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
  scale_x_date(date_breaks = "4 day", 
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
  scale_x_date(date_breaks = "4 day", 
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
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# SM Agrupado por CCAA -----
png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-log_ccaa.png", sep = ""),width = 1200,height = 800)
# prov_sm <- data_cases_sp_provinces %>%
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data = select(data_cases_sp_provinces_sm,date,deceased,province_cp,-province),
            aes(date,deceased,group=province_cp), color="#CACACA" ) +
  # add missing ccaa
  geom_line(data = ccaa_missing, aes(date, deceassed,group=province), size= 0.7, linetype = 2 ) +
  geom_text_repel(data=filter( ccaa_missing, 
                               date==max(data_cases_sp_provinces$date)), 
                                      aes(date+1, deceassed, label=paste0("(",format(deceassed, nsmall=1, big.mark="."), " ", substr(province,1,3), ")") ),
                                      # nudge_x = 3, # adjust the starting y position of the text label
                                      size=4,
                                      hjust=0,
                                      family = "Roboto Condensed",
                                      direction="y",
                                      segment.size = 0.1,
                                      segment.color="#777777"
  ) +
  # all the provinces 
  geom_line(aes(date, deceased,group=province), size= 0.7 ) +
  geom_point(aes(date, deceased), size= 0.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, 
                               date==max(data_cases_sp_provinces$date) & deceased > 0
                                ), 
                                aes(date+1, deceased, label=paste(format(deceased, nsmall=1, big.mark="."), substr(province,1,2) ) ),
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
    xlim = c( min(data_cases_sp_provinces$date + 9), max(data_cases_sp_provinces$date +10)),
    ylim = c(1, max( data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceased),]$deceased))
  ) +
  scale_y_log10( 
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)) ) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d",
               expand = c(0,1) 
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
  geom_line(aes(date, deceassed_per_100000,group=province)) +
  geom_point(aes(date, deceassed_per_100000), size= 0.5 ) +
  facet_wrap(~province) +
  scale_x_date(date_breaks = "4 day", 
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
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/provincias/covid19_fallecimientos-registrados-por-provincia-per-cienmil-log-ccaa.png", sep = ""),width = 1000,height = 600)
data_cases_sp_provinces %>% 
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
  geom_point(aes(date, deceased, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, 
                               date==max(data_cases_sp_provinces$date) & deceased > 200 
                               ), 
                  aes(date, deceased, color=ccaa, label=paste(format(deceased, nsmall=1, big.mark="."),province)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 day", 
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
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
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
                  aes(date, deceased, color=ccaa, label=paste0(format(deceased, nsmall=1, big.mark="."), " ", province, " (+", daily_deaths,", +", daily_deaths_inc ,"%)")),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 expand = c(0,0.1) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases_sp_provinces$date)+7, max(data_cases_sp_provinces$date + 14)),
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
  geom_point(aes(date, deceassed_per_100000, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & deceassed_per_100000 > 0.5), 
                  aes(date, deceassed_per_100000, color=ccaa, 
                      label=paste(format(deceassed_per_100000, nsmall=1, big.mark="."),province)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 day", 
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
  geom_point(aes(date, deceassed_per_100000, color=ccaa), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & deceassed_per_100000 > 7), 
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
  scale_color_manual(values = colors_prov) +
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
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption_provincia)
dev.off()
  

# 4. Daily deaths --------------
# SM Lineal
png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_smooth(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg6,province_cp,-province),
              aes(date,daily_deaths_avg6,group=province_cp), se = FALSE, span = 0.6, color="#CACACA", size=0.5 ) +
  geom_smooth(aes(date, daily_deaths_avg6,group=province), size= 0.7, se = FALSE, span = 0.6, color="#000000" ) +
  geom_point(aes(date, daily_deaths), size= 0.5, alpha=0.3 ) +
  geom_text(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
            aes(date,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$daily_deaths_avg6),]$daily_deaths_avg6)/4, label=paste(format(daily_deaths_avg6, nsmall=1, big.mark="."))),
            size=3,
            hjust=1,
            family = "Roboto Condensed"
  ) +
  facet_wrap(~province) +
  scale_x_date(date_breaks = "3 day", 
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
  labs(title = "Media de muertes por día en los 5 días anteriores por COVID-19 en España",
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "fallecidos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# SM Log
png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-log.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_smooth(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg6,province_cp,-province),
              aes(date,daily_deaths_avg6,group=province_cp), se = FALSE, span = 0.6, color="#CACACA", size=0.5 ) +
  geom_smooth(aes(date, daily_deaths_avg6,group=province), size= 0.7, se = FALSE, span = 0.6, color="#000000" ) +
  geom_point(aes(date, daily_deaths), size= 0.5, alpha=0.3 ) +
  geom_text(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
            aes(date,max(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$daily_deaths_avg6),]$daily_deaths_avg6)/4, label=paste(format(daily_deaths_avg6, nsmall=1, big.mark="."))),
            size=3,
            hjust=1,
            family = "Roboto Condensed"
  ) +
  facet_wrap(~province) +
  scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "4 day", 
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
  labs(title = "Media de muertes por día en los 5 días anteriores por COVID-19 en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "fallecidos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# SM CCAA grouped ------
png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-log_ccaa.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg6,province_cp,-province),
            aes(date,daily_deaths_avg6,group=province_cp), color="#CACACA" ) +
  # geom_line(aes(date, daily_deaths_avg6,group=province) ) +
  geom_smooth(aes(date, daily_deaths_avg6,group=province), size= 0.7, se = FALSE, span = 0.6, color="#000000" ) +
  # geom_point(aes(date, daily_deaths_avg6), size= 0.5 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, 
                               date==max(data_cases_sp_provinces$date)
                    ), 
                    aes(date, daily_deaths_avg6, label=paste(format(daily_deaths_avg6, nsmall=1, big.mark="."), substr(province,1,2) ) ),
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
    xlim= c( as.Date("2020-03-15"),max(data_cases_sp_provinces$date) )
  ) +
  scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d",
               expand = c(0,7) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = "Media de muertes por día en los 5 días anteriores por COVID-19 en España",
       subtitle = paste0("Por provincia (escala logarítmica). ",period),
       y = "fallecidos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()



# Superpuesto Lineal ------
png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-superpuesto-lineal.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% 
  ggplot() +
  geom_line(aes(date, daily_deaths,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, daily_deaths, color=ccaa), size= 1.5 ) +
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
  scale_x_date(date_breaks = "1 day", 
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
  geom_line(aes(date, daily_deaths,group=province, color=ccaa), size= 1 ) +
  geom_point(aes(date, daily_deaths, color=ccaa), size= 1.5 ) +
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
                 expand = c(0,0.2) ) +
  # scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #                minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
  #                expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "1 day", 
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
  geom_smooth(aes(date, daily_deaths_avg6,group=province, color=ccaa), size= 1, se = FALSE, span = 0.6 ) +
  geom_point(aes(date, daily_deaths, color=ccaa), size= 1.5 ) +
  # geom_point(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)), aes(date, daily_deaths_avg6, color=province), size= 1.5, alpha = 0.3 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & daily_deaths > 5), 
                  aes(date, daily_deaths_avg6, color=ccaa, label=paste(format(daily_deaths_avg6, nsmall=1, big.mark=".", decimal.mark = ","),province)),
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
                  aes(date+0.5,294, label=paste("media de 6 días")),
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
  scale_x_date(date_breaks = "1 day", 
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
  labs(title = "Media de muertes por día en los 5 días anteriores por COVID-19 en España",
       subtitle = paste0("Por provincia. Escala logarítmica  ",period),
       y = "fallecidos por día (media 6 días)",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# Superpuesto Log
png(filename=paste("img/spain/provincias/covid19_muertes-por-dia-provincia-media-superpuesto-log_media.png", sep = ""),width = 1200,height = 800)
data_cases_sp_provinces %>% 
  ggplot() +
  geom_smooth(aes(date, daily_deaths_avg6,group=province, color=ccaa), size= 1, se = FALSE, span = 0.6 ) +
  geom_point(aes(date, daily_deaths, color=ccaa), size= 1.5 ) +
  # geom_point(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)), aes(date, daily_deaths_avg6, color=province), size= 1.5, alpha = 0.3 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date) & daily_deaths > 0), 
                  aes(date, daily_deaths_avg6, color=ccaa, label=paste(format(daily_deaths_avg6, nsmall=1, big.mark=".", decimal.mark = ","),province)),
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
                  nudge_y = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  # marca la línea
  geom_text_repel(data=filter( data_cases_sp_provinces, date==as.Date("2020-04-04") &  province == "Madrid" ),
                  aes(date+0.5,282, label=paste("media de 6 días")),
                  nudge_y = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors_prov) +
  coord_cartesian(
    ylim = c(1,500)
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
                 expand = c(0,0.2) ) +
  scale_x_date(date_breaks = "1 day", 
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
  labs(title = "Media de muertes por día en los 5 días anteriores por COVID-19 en España",
       subtitle = paste0("Por provincia. Escala logarítmica  ",period),
       y = "fallecidos por día (media 6 días)",
       x = "fecha",
       caption = caption_provincia)
dev.off()


# Superpuesto Log por CCAA -------------

for ( i in 1:length(levels(data_cases_sp_provinces$ccaa))  ) {
  
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
    # geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg6,province_cp,-province),
    #           aes(date,daily_deaths_avg6,group=province_cp), color="#CACACA", size = 0.3 ) +
    geom_line(aes(date, daily_deaths_avg6,group=province, color=province), size= 1.5, se = FALSE, span = 0.6 ) +
    geom_point(aes(date, daily_deaths, color=province), size= 1.5 ) +
    # geom_point(aes(date, daily_deaths_avg6 ), size= 2, color= "#999999" ) +
    # geom_point(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)), aes(date, daily_deaths_avg6, color=province), size= 1.5, alpha = 0.3 ) +
    geom_text_repel(data= data_cases_sp_provinces %>% filter (ccaa == prov ) %>% filter(date==max(data_cases_sp_provinces[data_cases_sp_provinces$ccaa == prov,]$date) & daily_deaths > 0 & (ccaa == prov )), 
                    aes(date, daily_deaths_avg6, color=province, label=paste(format(daily_deaths_avg6, nsmall=1, big.mark=".", decimal.mark = ","),province)),
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
    scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                   expand = c(0,0.2) ) +
    scale_x_date(date_breaks = "1 day", 
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
    labs(title = paste0("Media de muertes por día en los 5 días anteriores por COVID-19 en ",prov ),
         subtitle = paste0("Por provincia. Escala logarítmica  ",period),
         y = "fallecidos por día (media 6 días)",
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
  geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(date,daily_deaths_avg6,province_cp,-province),
            aes(date,daily_deaths_avg6,group=province_cp), color="#CACACA", size = 0.3 ) +
  geom_line(aes(date, daily_deaths_avg6,group=province, color=province), size= 1.5, se = FALSE, span = 0.6 ) +
  geom_point(aes(date, daily_deaths, color=province), size= 1.5 ) +
  # geom_point(aes(date, daily_deaths_avg6 ), size= 2, color= "#999999" ) +
  # geom_point(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)), aes(date, daily_deaths_avg6, color=province), size= 1.5, alpha = 0.3 ) +
  geom_text_repel(data= data_cases_sp_provinces %>% filter (ccaa == prov ) %>% filter(date==max(data_cases_sp_provinces[data_cases_sp_provinces$ccaa == prov,]$date) & daily_deaths > 0 & (ccaa == prov )), 
                  aes(date, daily_deaths_avg6, color=province, label=paste(format(daily_deaths_avg6, nsmall=1, big.mark=".", decimal.mark = ","),province)),
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
  scale_x_date(date_breaks = "1 day", 
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
  labs(title = paste0("Media de muertes por día en los 5 días anteriores por COVID-19 en ",prov ),
       subtitle = paste0("Por provincia. Escala logarítmica  ",period),
       y = "fallecidos por día (media 6 días)",
       x = "fecha",
       caption = caption_provincia)

  print(the_province)
  print(paste("plot",prov))
  dev.off()
}


# 7. Deaths vs weekly deaths ------
# log --------
png(filename=paste("img/spain/provincias/covid19_trayectoria-provincia-facet-log.png", sep = ""),width = 1300,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  geom_line(data =  data_cases_sp_provinces_sm %>% ungroup() %>% select(deaths_cum_last_week,deaths_last_week,province_cp,-province),
              aes(deaths_cum_last_week,deaths_last_week,group=province_cp), se = FALSE, span = 0.6, color="#CACACA", size=0.5 ) +
  geom_line(aes(deaths_cum_last_week,deaths_last_week,group=province), size= 0.4 ) +
  # geom_smooth(aes(deaths_cum_last_week,deaths_last_week,group=province), size= 0.5, se = FALSE, color = "black") +
  # geom_point(aes(deaths_cum_last_week,deaths_last_week ), size= 0.2 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
                  aes(deaths_cum_last_week,deaths_last_week, color=ccaa, label= province ),
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


# log --------
png(filename=paste("img/spain/provincias/covid19_trayectoria-provincia-superpuesto-log.png", sep = ""),width = 1300,height = 800)
data_cases_sp_provinces %>%
  ggplot() +
  # geom_line(aes(deaths_cum_last_week,deaths_last_week,group=province,color=ccaa), size= 0.4 ) +
  geom_smooth(aes(deaths_cum_last_week,deaths_last_week,group=province,color=ccaa), size= 0.5, se = FALSE ) +
  # geom_point(aes(deaths_cum_last_week,deaths_last_week ), size= 0.2 ) +
  geom_text_repel(data=filter( data_cases_sp_provinces, date==max(data_cases_sp_provinces$date)),
                  aes(deaths_cum_last_week,deaths_last_week, color=ccaa, label= province ),
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
