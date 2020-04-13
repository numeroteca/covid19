# Analizar datos de Coronavirus COVID-19 en USA por provincia

# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping

# COVID-19 in USA -----------
# Load data
data_us_cases_original <- read.delim("http://covidtracking.com/api/states/daily.csv",sep = ",")  

# Population
# population_i_region <- read.delim("data/original/usa/population-region-italia.csv",sep = ",")  

# Process data
data_us_cases_original$date <- as.Date(as.character(data_us_cases_original$date), format="%Y%m%d")

data_us_cases <- data_us_cases_original

names(data_us_cases) <- c("date","region","positive","negative","pending","hospitalized","deceassed","cases_registered","dateChecked" )

data_us_cases$population <- ""
data_us_cases$cases_per_100000 <- ""
data_us_cases$intensive_care_per_1000000 <- ""
data_us_cases$deceassed_per_100000 <- ""
data_us_cases$recovered_per_100000 <- ""

# data_us_cases  <- data_us_cases %>% select( date,codice_regione, denominazione_regione, country, population, totale_casi, cases_per_100000,
#                                terapia_intensiva, intensive_care_per_1000000, deceduti, deceassed_per_100000, dimessi_guariti,recovered_per_100000  )
# 
# # rename variables to standarize
# names(data_us_cases) <-c("date", "region_code", "region", "country", "population","cases_registered","cases_per_100000","intensive_care",
#                         "intensive_care_per_1000000", "deceassed", "deceassed_per_100000", "recovered", "recovered_per_100000" )
# 
# # add population data
# data_us_cases <- data_us_cases %>% select(-population)
# data_us_cases <- merge( data_us_cases, select(population_i_region,region,population), by.x = "region", by.y = "region", all.x = TRUE)
# 
# data_us_cases  <- data_us_cases %>% select( date,region_code, region, country, population,cases_registered, cases_per_100000,
#                                           intensive_care, intensive_care_per_1000000, deceassed, deceassed_per_100000, 
#                                           recovered,recovered_per_100000  )
# 
# data_us_cases$cases_per_100000 <- round( data_us_cases$cases_registered / data_us_cases$population * 100000, digits = 2)
# data_us_cases$intensive_care_per_1000000 <- round( data_us_cases$intensive_care / data_us_cases$population * 100000, digits = 2)
# data_us_cases$deceassed_per_100000 <- round( data_us_cases$deceassed / data_us_cases$population * 100000, digits = 2)
# data_us_cases$recovered_per_100000 <- round( data_us_cases$recovered / data_us_cases$population * 100000, digits = 2)



# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption_us <- "Gráfico: @numeroteca (montera34.com). Datos: covidtracking.com"
periodo_us <- "2020.02.24 - 04.01"
# 1. Cases ------------

# create temp dataframes to be able to plot all the   values in small multiples
data_us_cases_sm <-data_us_cases
data_us_cases_sm$region_cp <- data_us_cases$region

# ----- Small multiple ------------
# Escala lineal
png(filename=paste("img/usa/covid19_casos-registrados-por-estado-lineal.png", sep = ""),width = 1100,height = 800)
data_us_cases %>%
  ggplot() +
  geom_line(data = select(data_us_cases_sm,date,cases_registered,region_cp,-region),
            aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_registered,group=region) ) +
  geom_point(aes(date,cases_registered,group=region), size = 0.5 ) +
  facet_wrap( ~region) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d"
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
  labs(title = "Número de casos acumulados de COVID-19 registrados en USA",
       subtitle = paste0("Por estado (escala lineal). ",periodo_us),
       y = "casos registrados",
       x = "fecha",
       caption = caption_us)
dev.off()

# Escala logarítmica
png(filename=paste("img/usa/covid19_casos-registrados-por-estado-log.png", sep = ""),width = 1100,height = 800)
data_us_cases %>%
  ggplot() +
  geom_line(data = select(data_us_cases_sm,date,cases_registered,region_cp,-region),
            aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_registered,group=region) ) +
  geom_point(aes(date,cases_registered,group=region), size = 0.5 )  +
  scale_y_log10( 
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000), seq(10000 , 10000, 10000) ) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en USA",
       subtitle = paste0("Por estado (escala logarítmica). ",periodo_us),
       y = "casos registrados",
       x = "fecha",
       caption = caption_us)
dev.off()

png(filename=paste("img/usa/covid19_casos-registrados-por-estado-per-cienmil-lineal.png", sep = ""),width = 1100,height = 800)
data_us_cases %>%
  ggplot() +
  geom_line(data = select(data_us_cases_sm,date,cases_per_100000,region_cp,-region),
            aes(date,cases_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_per_100000,group=region) ) +
  geom_point(aes(date,cases_per_100000,group=region), size = 0.5 ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "2 day",
               date_labels = "%d"
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en USA",
       subtitle = paste0("Por estado (escala lineal). ",periodo_us),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_us)
dev.off()

png(filename=paste("img/usa/covid19_casos-registrados-por-estado-per-cienmil-log.png", sep = ""),width = 1100,height = 800)
data_us_cases %>%
  ggplot() +
  geom_line(data = select(data_us_cases_sm,date,cases_per_100000,region_cp,-region),
            aes(date,cases_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_per_100000,group=region) ) +
  geom_point(aes(date,cases_per_100000,group=region), size = 0.5 ) +
  scale_y_log10( 
    limits = c(0.04,max(data_us_cases$cases_per_100000)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 3000, 100) )) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "2 day",
               date_labels = "%d"
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en USA",
       subtitle = paste0("Por estado (escala logarítmica). ",periodo_us),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_us)
dev.off()

# ---------- Superpuesto ---------------
png(filename=paste("img/usa/covid19_casos-registrados-por-estado-superpuesto-lineal.png", sep = ""),width = 1100,height = 800)
data_us_cases %>%
  ggplot() +
  geom_line( aes(date,cases_registered, group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_registered, color=region), size = 2 ) +
  geom_text_repel(data=filter( data_us_cases, date==max(data_us_cases$date)), 
                  aes(date,cases_registered, color=region, label=paste(format(cases_registered, nsmall=1, big.mark="."),region)),
                  nudge_x = 6, # adjust the starting y position of the text label
                  size=5,
                  # hjust=1,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_us_cases$date), max(data_us_cases$date + 6)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en USA",
       subtitle = paste0("Por estado (escala lineal). ",periodo_us),
       y = "casos registrados",
       x = "fecha",
       caption = caption_us)
dev.off()

png(filename=paste("img/usa/covid19_casos-registrados-por-estado-superpuesto-log.png", sep = ""),width = 1100,height = 800)
data_us_cases %>%
  ggplot() +
  geom_line( aes(date,cases_registered, group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_registered, color=region), size = 2 ) +
  geom_text_repel(data=filter( data_us_cases, date==max(data_us_cases$date)), 
                  aes(date,cases_registered, color=region, label=paste(format(cases_registered, nsmall=1, big.mark="."),region)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 limits = c(0.95,max(data_us_cases$cases_registered)),
                 breaks = c(1,10,100,1000,12000),
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000), seq(10000 , 100000, 10000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_us_cases$date), max(data_us_cases$date +4)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en USA",
       subtitle = paste0("Por estado (escala logarítmica). ",periodo_us),
       y = "casos registrados",
       x = "fecha",
       caption = caption_us)
dev.off()

# Por 100.000 --------
# png(filename=paste("img/usa/covid19_casos-registrados-por-estado-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1100,height = 800)
# data_us_cases %>%
#   ggplot() +
#   geom_line(aes(date,cases_per_100000,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,cases_per_100000, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_us_cases, date==max(data_us_cases$date)), 
#                   aes(date,cases_per_100000, color=region, label=paste(format(cases_per_100000, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#333333"
#   ) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d",
#                limits=c( min(data_us_cases$date), max(data_us_cases$date + 3.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en USA",
#        subtitle = paste0("Por estado (escala lineal). ",periodo_us),
#        y = "casos registrados por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_us)
# dev.off()
# 
# png(filename=paste("img/usa/covid19_casos-registrados-por-estado-superpuesto-per-cienmil-log.png", sep = ""),width = 1100,height = 800)
# data_us_cases %>%
#   ggplot() +
#   geom_line(aes(date,cases_per_100000,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,cases_per_100000, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_us_cases, date==max(data_us_cases$date),  region != "Total"), 
#                   aes(date,cases_per_100000, color=region, label=paste(format(cases_per_100000, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#333333"
#                   # xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2020-01-4"))
#   ) +
#   # scale_color_brewer(palette = "Dark2", type = "discrete") +
#   scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
#                  limits = c(1,max(data_us_cases$cases_per_100000)),
#                  minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d",
#                limits=c( min(data_us_cases$date), max(data_us_cases$date + 3.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en USA",
#        subtitle = paste0("Por estado (escala logarítmica). ",periodo_us),
#        y = "casos registrados por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_us)
# dev.off()

# English ----------
# png(filename=paste("img/usa/covid19_casos-registrados-por-estado-superpuesto-per-cienmil-log_en.png", sep = ""),width = 1100,height = 800)
# data_us_cases %>%
#   ggplot() +
#   geom_line(aes(date,cases_per_100000,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,cases_per_100000, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_us_cases, date==max(data_us_cases$date),  region != "Total"), 
#                   aes(date,cases_per_100000, color=region, label=paste(format(cases_per_100000, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#333333"
#                   # xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2020-01-4"))
#   ) +
#   # scale_color_brewer(palette = "Dark2", type = "discrete") +
#   scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
#                  limits = c(1,max(data_us_cases$cases_per_100000)),
#                  minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d",
#                limits=c( min(data_us_cases$date), max(data_us_cases$date + 3.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Confirmed accumulated COVID-19 registed cases in Spain by 100.000 inhabitants",
#        subtitle = paste0("By region (log scale). ",periodo_us),
#        y = "registered cases  by 100.000 inhabitants (log scale)",
#        x = "fecha",
#        caption = caption_us)
# dev.off()


# 2. UCI (intensive care) -------------------


# 3. Deceassed -------------------

# / small multiple ----------
png(filename=paste("img/usa/covid19_fallecimientos-registrados-por-estado-lineal.png", sep = ""),width = 1100,height = 800)
data_us_cases %>% 
  ggplot() +
  geom_line(data = select(data_us_cases_sm,date,deceassed,region_cp,-region),
            aes(date,deceassed,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date, deceassed, group=region) ) +
  geom_point(aes(date,deceassed), size= 0.5 ) +
  facet_wrap( ~region) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_us_cases$date), max(data_us_cases$date)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en USA",
       subtitle = paste0("Por estado (escala lineal). ",periodo_us),
       y = "fallecidos",
       x = "fecha",
       caption = caption_us)
dev.off()

png(filename=paste("img/usa/covid19_fallecimientos-registrados-por-estado-log.png", sep = ""),width = 1100,height = 800)
data_us_cases %>% 
  ggplot() +
  geom_line(data = select(data_us_cases_sm,date,deceassed,region_cp,-region),
            aes(date,deceassed,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date, deceassed, group=region) ) +
  geom_point(aes(date,deceassed), size= 0.5 ) +
  facet_wrap( ~region) +
  scale_y_log10( 
    limits = c(1,max(data_us_cases$deceassed)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100)) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_us_cases$date), max(data_us_cases$date)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en USA",
       subtitle = paste0("Por estado (escala logarítmica). ",periodo_us),
       y = "fallecidos",
       x = "fecha",
       caption = caption_us)
dev.off()

png(filename=paste("img/usa/covid19_fallecimientos-registrados-por-estado-per-cienmil-log.png", sep = ""),width = 1100,height = 800)
data_us_cases %>% 
  ggplot() +
  geom_line(data = select(data_us_cases_sm,date,deceassed_per_100000,region_cp,-region),
            aes(date,deceassed_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date, deceassed_per_100000, group=region) ) +
  geom_point(aes(date,deceassed_per_100000), size= 0.5 ) +
  facet_wrap( ~region) +
  # coord_cartesian( ylim = c( 0 , max(data_us_cases$deceassed_per_100000))  )+
  scale_y_log10( 
    # limits = c( 0 , max(data_us_cases$deceassed_per_100000) ),
    labels= function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100)) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_us_cases$date), max(data_us_cases$date)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados  por 100.000 habitantes en USA",
       subtitle = paste0("Por estado (escala logarítmica). ",periodo_us),
       y = "fallecidos por 100.000 habitantes ",
       x = "fecha",
       caption = caption_us)
dev.off()

# / Fallecimientos superpuestos ----------
png(filename=paste("img/usa/covid19_fallecimientos-registrados-por-estado-superpuesto-lineal.png", sep = ""),width = 1100,height = 800)
data_us_cases %>% 
  ggplot() +
  geom_line(aes(date, deceassed, group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_us_cases, date==max(data_us_cases$date),  region != "Total"), 
                  aes(date, deceassed, color=region, label=paste(format( deceassed, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_us_cases$date), max(data_us_cases$date + 3)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en USA",
       subtitle = paste0("Por estado (escala lineal). ",periodo_us),
       y = "fallecidos",
       x = "fecha",
       caption = caption_us)
dev.off()

png(filename=paste("img/usa/covid19_fallecimientos-registrados-por-estado-superpuesto-log.png", sep = ""),width = 1100,height = 800)
data_us_cases %>% 
  ggplot() +
  geom_line(aes(date,deceassed,group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_us_cases, date==max(data_us_cases$date),  region != "Total"), 
                  aes(date,deceassed, color=region, label=paste(format(deceassed, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( 
    limits = c(1,max(data_us_cases$deceassed)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_us_cases$date), max(data_us_cases$date +3.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en USA",
       subtitle = paste0("Por estado (escala logarítmica). ",periodo_us),
       y = "fallecidos",
       x = "fecha",
       caption = caption_us)
dev.off()

png(filename=paste("img/usa/covid19_fallecimientos-registrados-por-estado-superpuesto-per-cienmil-log.png", sep = ""),width = 1100,height = 800)
data_us_cases %>% 
  ggplot() +
  geom_line(aes(date,deceassed_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed_per_100000, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_us_cases, date==max(data_us_cases$date),  region != "Total"), 
                  aes(date,deceassed_per_100000, color=region, label=paste(format(deceassed_per_100000, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( 
    limits = c(0.1,max(data_us_cases$deceassed_per_100000)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(0.1 , 1, 0.1),seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100)) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_us_cases$date), max(data_us_cases$date +3.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en USA",
       subtitle = paste0("Por estado (escala logarítmica). ",periodo_us),
       y = "fallecidospor 100.000 habitantes",
       x = "fecha",
       caption = caption_us)
dev.off()
