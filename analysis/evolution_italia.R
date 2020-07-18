# Analizar datos de Coronavirus COVID-19 en Italia por provincia

# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent   overlapping

# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de losS datos
caption_i <- "Gráfico: @numeroteca (lab.montera34.com/covid19). Datos: Protezione Civile (Italia)"
periodo_i <- "2020.02.24 - 07.18"

# COVID-19 in Italy -----------
# Load data
download.file("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv", 
              "data/original/italia/dpc-covid19-ita-regioni.csv")
data_i_cases_original <- read.delim("data/original/italia/dpc-covid19-ita-regioni.csv",sep = ",")  

# Population
population_i_region <- read.delim("data/original/italia/population-region-italia.csv",sep = ",")  

# Process data
data_i_cases_original$date <- as.Date(data_i_cases_original$data)

data_i_cases <- data_i_cases_original

data_i_cases$country <- "Italia"
data_i_cases$population <- ""
data_i_cases$cases_per_100000 <- ""
data_i_cases$intensive_care_per_1000000 <- ""
data_i_cases$deceassed_per_100000 <- ""
data_i_cases$recovered_per_100000 <- ""

data_i_cases  <- data_i_cases %>% select( date,codice_regione, denominazione_regione, country, population, totale_casi, cases_per_100000,
                               terapia_intensiva, intensive_care_per_1000000, deceduti, deceassed_per_100000, dimessi_guariti,recovered_per_100000  )

# rename variables to standarize
names(data_i_cases) <-c("date", "region_code", "region", "country", "population","cases_registered","cases_per_100000","intensive_care",
                        "intensive_care_per_1000000", "deceassed", "deceassed_per_100000", "recovered", "recovered_per_100000" )

# add population data
data_i_cases <- data_i_cases %>% select(-population)
data_i_cases <- merge( data_i_cases, select(population_i_region,region,population), by.x = "region", by.y = "region", all.x = TRUE)

data_i_cases  <- data_i_cases %>% select( date,region_code, region, country, population,cases_registered, cases_per_100000,
                                          intensive_care, intensive_care_per_1000000, deceassed, deceassed_per_100000, 
                                          recovered,recovered_per_100000  )

data_i_cases$cases_per_100000 <- round( data_i_cases$cases_registered / data_i_cases$population * 100000, digits = 2)
data_i_cases$intensive_care_per_1000000 <- round( data_i_cases$intensive_care / data_i_cases$population * 100000, digits = 2)
data_i_cases$deceassed_per_100000 <- round( data_i_cases$deceassed / data_i_cases$population * 100000, digits = 2)
data_i_cases$recovered_per_100000 <- round( data_i_cases$recovered / data_i_cases$population * 100000, digits = 2)

data_i_cases <- data_i_cases %>% group_by(region) %>% arrange(date) %>% mutate(
  daily_cases = cases_registered - lag(cases_registered),
  daily_cases_avg7 = round( ( daily_cases + lag(daily_cases,1) + lag(daily_cases,2) + lag(daily_cases,3) + lag(daily_cases,4) + lag(daily_cases,5) + lag(daily_cases,6)) / 7, digits = 1 ), # average of dayly cases of 7 last days
  daily_deaths = deceassed - lag(deceassed),
  daily_deaths_inc = round((deceassed - lag(deceassed)) /lag(deceassed) * 100, digits = 1),
  daily_deaths_avg6 =  round( ( daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) + lag(daily_deaths,4) + lag(daily_deaths,5) + lag(daily_deaths,6)) / 7, digits = 1 ), # average of dayly deaths of 7 last days
  daily_deaths_avg2 =  round( ( daily_deaths + lag(daily_deaths,1) ) / 2, digits = 1 ),
  daily_deaths_avg3 =  round( ( daily_deaths + lag(daily_deaths,1)+ lag(daily_deaths,2) ) / 3, digits = 1 ),
  daily_deaths_avg4 =  round( ( daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) ) / 4, digits = 1 )
)

# 1. Cases ------------

# create temp dataframes to be able to plot all the   values in small multiples
data_i_cases_sm <-data_i_cases
data_i_cases_sm$region_cp <- data_i_cases$region

# ----- Small multiple ------------
# Escala lineal
png(filename=paste("img/italia/covid19_casos-registrados-por-region-lineal.png", sep = ""),width = 1100,height = 800)
data_i_cases %>%
  ggplot() +
  geom_line(data = data_i_cases_sm %>% ungroup() %>% select(date,cases_registered,region_cp,-region),
            aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_registered,group=region) ) +
  geom_point(aes(date,cases_registered,group=region), size = 0.5 ) +
  facet_wrap( ~region) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%d/%m"
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
  labs(title = "Número de casos acumulados de COVID-19 registrados en Italia",
       subtitle = paste0("Por región (escala lineal). ",periodo_i),
       y = "casos registrados",
       x = "fecha",
       caption = caption_i)
dev.off()

# Escala logarítmica
png(filename=paste("img/italia/covid19_casos-registrados-por-region-log.png", sep = ""),width = 1100,height = 800)
data_i_cases %>%
  ggplot() +
  geom_line(data = data_i_cases_sm %>% ungroup() %>% select(date,cases_registered,region_cp,-region),
            aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_registered,group=region) ) +
  geom_point(aes(date,cases_registered,group=region), size = 0.5 )  +
  scale_y_log10( 
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%d/%m"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en Italia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "casos registrados",
       x = "fecha",
       caption = caption_i)
dev.off()

png(filename=paste("img/italia/covid19_casos-registrados-por-region-per-cienmil-lineal.png", sep = ""),width = 1100,height = 800)
data_i_cases %>%
  ggplot() +
  geom_line(data = data_i_cases_sm %>% ungroup() %>% select(date,cases_per_100000,region_cp,-region),
            aes(date,cases_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_per_100000,group=region) ) +
  geom_point(aes(date,cases_per_100000,group=region), size = 0.5 ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%d/%m"
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Italia",
       subtitle = paste0("Por región (escala lineal). ",periodo_i),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_i)
dev.off()

png(filename=paste("img/italia/covid19_casos-registrados-por-region-per-cienmil-log.png", sep = ""),width = 1100,height = 800)
data_i_cases %>%
  ggplot() +
  geom_line(data = data_i_cases_sm %>% ungroup() %>% select(date,cases_per_100000,region_cp,-region),
            aes(date,cases_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_per_100000,group=region) ) +
  geom_point(aes(date,cases_per_100000,group=region), size = 0.5 ) +
  scale_y_log10( 
    limits = c(0.04,max(data_i_cases$cases_per_100000)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 3000, 100) )) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%d/%m"
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Italia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_i)
dev.off()

# ---------- Superpuesto ---------------
png(filename=paste("img/italia/covid19_casos-registrados-por-region-superpuesto-lineal.png", sep = ""),width = 1100,height = 800)
data_i_cases %>%
  ggplot() +
  geom_line( aes(date,cases_registered, group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_registered, color=region), size = 1 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date)), 
                  aes(date,cases_registered, color=region, label=paste(format(cases_registered, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date + 15)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en Italia",
       subtitle = paste0("Por región (escala lineal). ",periodo_i),
       y = "casos registrados",
       x = "fecha",
       caption = caption_i)
dev.off()

png(filename=paste("img/italia/covid19_casos-registrados-por-region-superpuesto-log.png", sep = ""),width = 1100,height = 800)
data_i_cases %>%
  ggplot() +
  geom_line( aes(date,cases_registered, group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_registered, color=region), size = 1 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date)), 
                  aes(date,cases_registered, color=region, label=paste(format(cases_registered, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 limits = c(0.95,max(data_i_cases$cases_registered)),
                 # breaks = c(1,10,100,1000,12000),
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000), seq(10000, 100000, 10000) ) ) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date +16)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en Italia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "casos registrados",
       x = "fecha",
       caption = caption_i)
dev.off()

# Por 100.000 --------
png(filename=paste("img/italia/covid19_casos-registrados-por-region-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1100,height = 800)
data_i_cases %>%
  ggplot() +
  geom_line(aes(date,cases_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_per_100000, color=region), size= 1 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date)), 
                  aes(date,cases_per_100000, color=region, label=paste(format(cases_per_100000, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date + 25)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Italia",
       subtitle = paste0("Por región (escala lineal). ",periodo_i),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_i)
dev.off()

png(filename=paste("img/italia/covid19_casos-registrados-por-region-superpuesto-per-cienmil-log.png", sep = ""),width = 1100,height = 800)
data_i_cases %>%
  ggplot() +
  geom_line(aes(date,cases_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_per_100000, color=region), size= 1 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date),  region != "Total"), 
                  aes(date,cases_per_100000, color=region, label=paste(format(cases_per_100000, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
                  # xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2020-01-4"))
  ) +
  # scale_color_brewer(palette = "Dark2", type = "discrete") +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 limits = c(1,max(data_i_cases$cases_per_100000)),
                 minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date + 25)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Italia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_i)
dev.off()

# English ----------
png(filename=paste("img/italia/covid19_casos-registrados-por-region-superpuesto-per-cienmil-log_en.png", sep = ""),width = 1100,height = 800)
data_i_cases %>%
  ggplot() +
  geom_line(aes(date,cases_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_per_100000, color=region), size= 1 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date),  region != "Total"), 
                  aes(date,cases_per_100000, color=region, label=paste(format(cases_per_100000, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
                  # xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2020-01-4"))
  ) +
  # scale_color_brewer(palette = "Dark2", type = "discrete") +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 limits = c(1,max(data_i_cases$cases_per_100000)),
                 minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date + 25)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Confirmed accumulated COVID-19 registed cases in Spain by 100.000 inhabitants",
       subtitle = paste0("By region (log scale). ",periodo_i),
       y = "registered cases  by 100.000 inhabitants (log scale)",
       x = "fecha",
       caption = caption_i)
dev.off()


# 2. UCI (intensive care) -------------------


# 3. Deceassed -------------------

# / small multiple ----------
png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-lineal.png", sep = ""),width = 1100,height = 800)
data_i_cases %>% 
  ggplot() +
  geom_line(data = data_i_cases_sm %>% ungroup() %>% select(date,deceassed,region_cp,-region),
            aes(date,deceassed,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date, deceassed, group=region) ) +
  geom_point(aes(date,deceassed), size= 0.5 ) +
  facet_wrap( ~region) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en Italia",
       subtitle = paste0("Por región (escala lineal). ",periodo_i),
       y = "fallecidos",
       x = "fecha",
       caption = caption_i)
dev.off()

png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-log.png", sep = ""),width = 1100,height = 800)
data_i_cases %>% 
  ggplot() +
  geom_line(data = data_i_cases_sm %>% ungroup() %>% select(date,deceassed,region_cp,-region),
            aes(date,deceassed,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date, deceassed, group=region) ) +
  geom_point(aes(date,deceassed), size= 0.5 ) +
  facet_wrap( ~region) +
  scale_y_log10( 
    limits = c(1,max(data_i_cases$deceassed)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)) ) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en Italia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "fallecidos",
       x = "fecha",
       caption = caption_i)
dev.off()

png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-per-cienmil-log.png", sep = ""),width = 1100,height = 800)
data_i_cases %>% 
  ggplot() +
  geom_line(data = data_i_cases_sm %>% ungroup() %>% select(date,deceassed_per_100000,region_cp,-region),
            aes(date,deceassed_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date, deceassed_per_100000, group=region) ) +
  geom_point(aes(date,deceassed_per_100000), size= 0.5 ) +
  facet_wrap( ~region) +
  # coord_cartesian( ylim = c( 0 , max(data_i_cases$deceassed_per_100000))  )+
  scale_y_log10( 
    # limits = c( 0 , max(data_i_cases$deceassed_per_100000) ),
    labels= function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100)) ) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados  por 100.000 habitantes en Italia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "fallecidos por 100.000 habitantes ",
       x = "fecha",
       caption = caption_i)
dev.off()

# / Fallecimientos superpuestos ----------
png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-superpuesto-lineal.png", sep = ""),width = 1100,height = 800)
data_i_cases %>% 
  ggplot() +
  geom_line(aes(date, deceassed, group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed, color=region), size= 1 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date),  region != "Total"), 
                  aes(date, deceassed, color=region, label=paste(format( deceassed, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date + 25)),
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
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en Italia",
       subtitle = paste0("Por región (escala lineal). ",periodo_i),
       y = "fallecidos",
       x = "fecha",
       caption = caption_i)
dev.off()

png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-superpuesto-log.png", sep = ""),width = 1100,height = 800)
data_i_cases %>% 
  ggplot() +
  geom_line(aes(date,deceassed,group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed, color=region), size= 1 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date),  region != "Total"), 
                  aes(date,deceassed, color=region, 
                  label=paste0(format(deceassed, nsmall=1, big.mark="."), " ", region, " (+", daily_deaths,", +", daily_deaths_inc ,"%)")),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( 
    limits = c(1,max(data_i_cases$deceassed)),
    breaks = c(0,1,5,10,20,50,100,200,500,1000,2000,5000,10000,20000 ),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000), seq(10000 , 100000, 10000)),
    expand = c(0,0.1)
    ) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date +25)),
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
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en Italia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "fallecidos.",
       x = "fecha",
       caption = caption_i)
dev.off()

# Daily cases -------------
png(filename=paste("img/italia/covid19_casos-por-dia-region-superpuesto-lineal_media.png", sep = ""),width = 1200,height = 800)
data_i_cases %>%
  ggplot() +
  geom_line(aes(date, daily_cases_avg7,group=region, color=region), size= 1, se = FALSE  ) +
  geom_line(aes(date, daily_cases, color=region, group=region), size= 0.3, alpha=0.5) +
  geom_point(aes(date, daily_cases, color=region), size= 0.6, alpha=0.5 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date)), 
                  aes(date, daily_deaths_avg6, color=region, label=paste(format( daily_cases_avg7, nsmall=1, big.mark="."),region)),
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  coord_cartesian(
    ylim=c(0, max(data_i_cases[!is.na(data_i_cases$daily_cases),]$daily_cases)*1.01 )
  ) +
  scale_y_continuous( 
    # breaks = c(0,1,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
    # minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
    # expand = c(0,0.1)
  ) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date +25)),
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
  labs(title = "Media de casos (ventana de 7 días) por COVID-19 por día en Italia",
       subtitle = paste0("Por región. ",periodo_i),
       y = "casos",
       x = "fecha",
       caption = caption_i)
dev.off()


png(filename=paste("img/italia/covid19_casos-por-dia-region-superpuesto-log_media.png", sep = ""),width = 1200,height = 800)
data_i_cases %>%
  ggplot() +
  geom_line(aes(date, daily_cases_avg7,group=region, color=region), size= 1, se = FALSE  ) +
  geom_line(aes(date, daily_cases, color=region, group=region), size= 0.3, alpha=0.5) +
  geom_point(aes(date, daily_cases, color=region), size= 0.6, alpha=0.5 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date)), 
                  aes(date, daily_deaths_avg6, color=region, label=paste(format( daily_cases_avg7, nsmall=1, big.mark="."),region)),
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # coord_cartesian(
  #   ylim=c(0, max(data_i_cases[!is.na(data_i_cases$daily_cases),]$daily_cases)*1.01 )
  # ) +
  scale_y_log10( 
    # breaks = c(0,1,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
    # minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
    # expand = c(0,0.1)
  ) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date +25)),
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
  labs(title = "Media de casos (ventana de 7 días) por COVID-19 por día en Italia",
       subtitle = paste0("Por región. ",periodo_i),
       y = "casos",
       x = "fecha",
       caption = caption_i)
dev.off()

# 4. Daily deaths --------
png(filename=paste("img/italia/covid19_muertes-por-dia-region-superpuesto-log.png", sep = ""),width = 1100,height = 800)
data_i_cases %>% 
  ggplot() +
  geom_line(aes(date, daily_deaths,group=region, color=region), size= 0.6 ) +
  geom_point(aes(date, daily_deaths, color=region), size= 1 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date),  region != "Total"), 
                  aes(date, daily_deaths, color=region, label=paste(format( daily_deaths, nsmall=1, big.mark="."),region)),
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  coord_cartesian(
    ylim=c(1, max(data_i_cases[!is.na(data_i_cases$daily_deaths),]$daily_deaths)*1.05 )
  ) +
  scale_y_log10( 
    breaks = c(0,1,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
    expand = c(0,0.1)
  ) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date +25)),
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
  labs(title = "Número de muertes por COVID-19 registradas por día en Italia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "fallecidos.",
       x = "fecha",
       caption = caption_i)
dev.off()

png(filename=paste("img/italia/covid19_muertes-por-dia-region-superpuesto-lineal_media.png", sep = ""),width = 1100,height = 800)
data_i_cases %>%
  ggplot() +
  geom_line(aes(date, daily_deaths_avg6,group=region, color=region), size= 1, se = FALSE  ) +
  geom_line(aes(date, daily_deaths, color=region, group=region), size= 0.3, alpha=0.5) +
  geom_point(aes(date, daily_deaths, color=region), size= 0.6, alpha=0.5 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date)), 
                  aes(date, daily_deaths_avg6, color=region, label=paste(format( daily_deaths_avg6, nsmall=1, big.mark="."),region)),
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # marca un día
  geom_text_repel(data=filter( data_i_cases, date==as.Date("2020-03-15"), region == "Lombardia" ),
                  aes(date,daily_deaths, label=paste("muertes en un día en una región")),
                  nudge_y = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  # marca la línea
  geom_text_repel(data=filter( data_i_cases, date==as.Date("2020-04-06"), region == "Lombardia" ),
                  aes(date+0.5,329, label=paste("media de 7 días")),
                  nudge_y = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  coord_cartesian(
    ylim=c(0, max(data_i_cases[!is.na(data_i_cases$daily_deaths),]$daily_deaths)*1.01 )
  ) +
  scale_y_continuous( 
    # breaks = c(0,1,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
    # minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
    # expand = c(0,0.1)
  ) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date +25)),
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
  labs(title = "Media de muertes (ventana de 7 días) por COVID-19 registradas por día en Italia",
       subtitle = paste0("Por región. ",periodo_i),
       y = "fallecidos.",
       x = "fecha",
       caption = caption_i)
dev.off()

png(filename=paste("img/italia/covid19_muertes-por-dia-region-superpuesto-log_media.png", sep = ""),width = 1100,height = 800)
data_i_cases %>%
  ggplot() +
  # geom_smooth(aes(date, daily_deaths_avg6,group=region, color=region), size= 1, se = FALSE  ) +
  geom_line(aes(date, daily_deaths_avg6,group=region, color=region), size= 1, se = FALSE  ) +
  # geom_smooth(aes(date, daily_deaths_avg2,group=region, color=region), size= 1, se = FALSE  ) +
  # geom_smooth(aes(date, daily_deaths_avg3,group=region, color=region), size= 1, se = FALSE  ) +
  # geom_smooth(aes(date, daily_deaths_avg4,group=region, color=region), size= 1, se = FALSE  ) +
  geom_line(aes(date, daily_deaths, color=region, group=region), size= 0.3, alpha=0.5) +
  geom_point(aes(date, daily_deaths, color=region), size= 0.6, alpha=0.5 ) +
  # geom_point(aes(date, daily_deaths_avg6, color=region), size= 1.5, alpha = 0.5 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date)), 
                  aes(date, daily_deaths_avg6, color=region, label=paste(format( daily_deaths_avg6, nsmall=1, big.mark="."),region)),
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # marca un día
  geom_text_repel(data=filter( data_i_cases, date==as.Date("2020-03-15"), region == "Lombardia" ),
                  aes(date,daily_deaths, label=paste("muertes en un día en una región")),
                  nudge_y = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  # marca la línea
  geom_text_repel(data=filter( data_i_cases, date==as.Date("2020-04-06"), region == "Lombardia" ),
                  aes(date+0.5,329, label=paste("media de 7 días")),
                  nudge_y = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  coord_cartesian(
    ylim=c(1, max(data_i_cases[!is.na(data_i_cases$daily_deaths),]$daily_deaths)*1.05 )
  ) +
  scale_y_log10( 
    breaks = c(0,1,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
    expand = c(0,0.1)
  ) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date +25)),
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
  labs(title = "Media de muertes (ventana de 7 días) por COVID-19 registradas por día en Italia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "fallecidos.",
       x = "fecha",
       caption = caption_i)
dev.off()

png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-superpuesto-per-cienmil-log.png", sep = ""),width = 1100,height = 800)
data_i_cases %>% 
  ggplot() +
  geom_line(aes(date,deceassed_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed_per_100000, color=region), size= 1 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date),  region != "Total"), 
                  aes(date,deceassed_per_100000, color=region, label=paste(format(deceassed_per_100000, nsmall=1, big.mark="."),region)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  coord_cartesian(
    ylim=c(0.1, max(data_i_cases[!is.na(data_i_cases$deceassed_per_100000),]$deceassed_per_100000)*1.05 )
  ) +
  scale_y_log10( 
    # limits = c(0.1,max(data_i_cases$deceassed_per_100000)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    breaks = c(0,1,5,10,20,50,100,200 ),
    minor_breaks = c(seq(0.1 , 1, 0.1),seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
    expand = c(0,0.1)  ) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( min(data_i_cases$date), max(data_i_cases$date + 25)),
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
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en Italia.",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "fallecidospor 100.000 habitantes",
       x = "fecha",
       caption = caption_i)
dev.off()

  