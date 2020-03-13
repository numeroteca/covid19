# Analizar datos de Coronavirus COVID-19 en España por comunidad autónoma 

# Load libraries -----------
library(tidyverse)
library(ggrepel) # for geom_text_repel to prevent overlapping
# library(RColorBrewer) # extends color paletter

# Load world data --------
# World data https://github.com/RamiKrispin/coronavirus-csv/blob/master/coronavirus_dataset.csv
# world_data  <- read.delim("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv",sep = ",")

# Population
ccaa_poblacion <-  read.delim("data/ccaa-poblacion.csv",sep = ";")

# COVID-19 in Spain -----------
# Data by Ministerio de Sanidad de España (published in PDF format https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm)
# extracted by Datadista and published in this repository https://github.com/datadista/datasets/tree/master/COVID%2019
# Spanish data https://github.com/datadista/datasets/tree/master/COVID%2019
# data_cases_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/12-03-2020/casos_cccaa_12032020_covid19.csv",sep = ",")
data_cases_original <- read.delim("../coronavirus-datadista/COVID 19/12-03-2020/casos_cccaa_12032020_covid19.csv",sep = ",") #TODO loads local data to fix Aragón problem in 2020-02-28
data_death_original <- read.delim("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/12-03-2020/fallecidos_cccaa_12032020_covid19.csv",sep = ",")
data_uci_original <- read.delim("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/12-03-2020/uci_cccaa_12032020_covid19.csv",sep = ",")

# Process data ------
# Change to long format
# Casos registrados
data_cases <- melt(data_cases_original, id.vars = c("CCAA", "cod_ine")) 
data_cases$date <- as.Date(substr(data_cases$variable,2,12),"%d.%m.%Y")
data_cases <- select(data_cases,-variable)

# add population data
data_cases <- merge( data_cases, select(ccaa_poblacion,id,poblacion), by.x = "cod_ine", by.y = "id"   )
# calculate values per 
data_cases$per_million <- round( data_cases$value / data_cases$poblacion * 1000000, digits = 1)

# Personas UCI registradas
data_uci <- melt(data_uci_original, id.vars = c("CCAA"))
data_uci$date <- as.Date(substr(data_uci$variable,2,12),"%d.%m.%Y")
data_uci <- select(data_uci,-variable)

# add population data
data_uci <- merge( data_uci, data_cases %>% filter (date == as.Date("2020-02-27") ) %>% select(CCAA,poblacion), by.x = "CCAA", by.y = "CCAA" , all.x = TRUE  )
# calculate values per 
data_uci$per_million <- round( data_uci$value / data_uci$poblacion * 1000000, digits = 1)


# Fallecimientos registrados
data_death <- melt(data_death_original, id.vars = c("CCAA"))
data_death$date <- as.Date(substr(data_death$variable,2,12),"%d.%m.%Y")
data_death <- select(data_death,-variable)

# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption <- "Gráfico: @numeroteca (montera34.com). Datos: Ministerio de Sanidad de España extraídos por Datadista.com"
period <- "2020.02.27 - 03.12"
# Cases ------------

# ----- Small multiple ------------
# Escala lineal
png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-lineal.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
ggplot() +
  geom_line(aes(date,value,group=CCAA) ) +
  geom_point(aes(date,value,group=CCAA), size = 0.5 ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "1 day", 
                   date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "casos",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-log.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
ggplot() +
  geom_line(aes(date,value,group=CCAA) ) +
  geom_point(aes(date,value,group=CCAA), size = 0.5 ) +
  scale_y_log10( ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "casos",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-per-million-log.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,per_million,group=CCAA) ) +
  geom_point(aes(date,per_million,group=CCAA), size = 0.5 ) +
  # geom_point(aes(date,per_million) ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por millón de habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "casos por millón de habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# ---------- Superpuesto ---------------
png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,value,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,value, color=CCAA), size= 2 ) +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  CCAA != "Total"), 
                  aes(date,value,label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
                  # xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2020-01-4"))
  ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases$date), max(data_cases$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "casos",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-log.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,value,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,value,color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  CCAA != "Total"), 
                  aes(date,value,label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
                  # xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2020-01-4"))
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases$date), max(data_cases$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "casos por millón de habitantes",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-per-million-log.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,per_million,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,per_million, color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  CCAA != "Total"), 
                  aes(date,per_million,label=paste(format(per_million, nsmall=1, big.mark="."),CCAA)),
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
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases$date), max(data_cases$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por millón de habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "casos por millón",
       x = "fecha",
       caption = caption)
dev.off()

# UCI (intensive care) -------------------

# -------- UCI Small multiple ----------
# Escala lineal
png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-lineal.png", sep = ""),width = 1000,height = 700)
data_uci %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,value,group=CCAA) ) +
  geom_point(aes(date,value),size = 0.5 ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "personas en UCI",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-log.png", sep = ""),width = 1000,height = 700)
data_uci %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,value,group=CCAA) ) +
  geom_point(aes(date,value,group=CCAA), size = 0.5 ) +
  scale_y_log10( ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "personas en UCI",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-per-million-log.png", sep = ""),width = 1000,height = 700)
data_uci %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,per_million,group=CCAA) ) +
  geom_point(aes(date,per_million,group=CCAA), size = 0.5 ) +
  scale_y_log10( ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por millón de habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "personas en UCI por millón de habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# ------ UCI Superpuesto -------------
png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
data_uci %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,value,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,value, color=CCAA),size = 1.5 ) +
  geom_text_repel(data=filter( data_uci, date==max(data_uci$date),  CCAA != "Total"), 
                  aes(date,value,label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_uci$date), max(data_uci$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "personas en UCI",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-superpuesto-log.png", sep = ""),width = 1000,height = 700)
data_uci %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,value,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,value, color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_uci, date==max(data_uci$date),  CCAA != "Total"), 
                  aes(date,value,label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  # scale_color_brewer(palette = "Dark2", type = "discrete") +
  scale_y_log10( ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_uci$date), max(data_uci$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "personas en UCI",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-superpuesto-per-million-log.png", sep = ""),width = 1000,height = 700)
data_uci %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,per_million,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,per_million,  color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_uci, date==max(data_uci$date),  CCAA != "Total"), 
                  aes(date,per_million,label=paste(format(per_million, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  # scale_color_brewer(palette = "Dark2", type = "discrete") +
  scale_y_log10( ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_uci$date), max(data_uci$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por millón de habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "personas en UCI por millón de habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# Deaths ------------

# Escala lineal
png(filename=paste("img/covid19_fallecimientos-registrados-por-comunidad-autonoma-lineal.png", sep = ""),width = 1000,height = 700)
data_death %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,value,group=CCAA) ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/covid19_fallecimientos-registrados-por-comunidad-autonoma-log.png", sep = ""),width = 1000,height = 700)
data_death %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,value,group=CCAA) ) +
  scale_y_log10( ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_fallecimientos-registrados-por-comunidad-autonoma-superpuesto-log.png", sep = ""),width = 1000,height = 700)
data_death %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,value,group=CCAA, color=CCAA), size= 1 ) +
  geom_text_repel(data=filter( data_death, date==max(data_death$date),  CCAA != "Total"), 
                  aes(date,value,label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  # scale_color_brewer(palette = "Dark2", type = "discrete") +
  scale_y_log10( ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_death$date), max(data_death$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()
