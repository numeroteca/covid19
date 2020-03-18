# Analizar datos de Coronavirus COVID-19 en Italia por provincia

# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping

# COVID-19 in Italy -----------
data_i_cases_original <- read.delim("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",sep = ",")  

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

# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption_i <- "Gráfico: @numeroteca (montera34.com). Datos: Protezione Civile (Italia)"
periodo_i <- "2020.02.24 - 03.16"
# 1. Cases ------------

# ----- Small multiple ------------
# Escala lineal
png(filename=paste("img/italia/covid19_casos-registrados-por-region-lineal.png", sep = ""),width = 1000,height = 700)
data_i_cases %>%
  ggplot() +
  geom_line(aes(date,cases_registered,group=region) ) +
  geom_point(aes(date,cases_registered,group=region), size = 0.5 ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "1 day", 
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
  labs(title = "Número de casos acumulados de COVID-19 registrados en Italia",
       subtitle = paste0("Por región (escala lineal). ",periodo_i),
       y = "casos registrados",
       x = "fecha",
       caption = caption_i)
dev.off()

# Escala logarítmica
png(filename=paste("img/italia/covid19_casos-registrados-por-region-log.png", sep = ""),width = 1000,height = 700)
data_i_cases %>%
  ggplot() +
  geom_line(aes(date,cases_registered,group=region) ) +
  geom_point(aes(date,cases_registered,group=region), size = 0.5 )  +
  scale_y_log10( minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "1 day", 
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
  labs(title = "Número de casos acumulados de COVID-19 registrados en Italia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "casos registrados",
       x = "fecha",
       caption = caption_i)
dev.off()

# png(filename=paste("img/italia/covid19_casos-registrados-por-region-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
# data_i_cases %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=region) ) +
#   geom_point(aes(date,per_cienmil,group=region), size = 0.5 ) +
#   facet_wrap( ~region) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d"
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     axis.text.x = element_text(size = 9)
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Italia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "casos registrados por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()

# png(filename=paste("img/italia/covid19_casos-registrados-por-region-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
# data_i_cases %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=region) ) +
#   geom_point(aes(date,per_cienmil,group=region), size = 0.5 ) +
#   scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 3000, 100) )) +
#   facet_wrap( ~region) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d"
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     axis.text.x = element_text(size = 9)
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Italia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "casos registrados por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()

# ---------- Superpuesto ---------------
png(filename=paste("img/italia/covid19_casos-registrados-por-region-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
data_i_cases %>%
  ggplot() +
  geom_line( aes(date,cases_registered, group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_registered, color=region), size = 2 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date)), 
                  aes(date,cases_registered, color=region, label=paste(format(cases_registered, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_i_cases$date), max(data_i_cases$date + 4)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en Italia",
       subtitle = paste0("Por región (escala lineal). ",periodo_i),
       y = "casos registrados",
       x = "fecha",
       caption = caption_i)
dev.off()

png(filename=paste("img/italia/covid19_casos-registrados-por-region-superpuesto-log.png", sep = ""),width = 1000,height = 700)
data_i_cases %>%
  ggplot() +
  geom_line( aes(date,cases_registered, group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_registered, color=region), size = 2 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date)), 
                  aes(date,cases_registered, color=region, label=paste(format(cases_registered, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 limits = c(0.95,max(data_i_cases$cases_registered)),
                 breaks = c(1,10,100,1000,12000),
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_i_cases$date), max(data_i_cases$date +4)) 
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
# 
# png(filename=paste("img/italia/covid19_casos-registrados-por-region-superpuesto-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
# data_i_cases %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,per_cienmil, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date),  region != "Total"), 
#                   aes(date,per_cienmil, color=region, label=paste(format(per_cienmil, nsmall=1, big.mark="."),region)),
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
#                  minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d",
#                limits=c( min(data_i_cases$date), max(data_i_cases$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Italia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "casos registrados por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()

# 2. UCI (intensive care) -------------------

# // 2.1 UCI Small multiple ----------
# Escala lineal
png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-lineal.png", sep = ""),width = 1000,height = 700)
data_uci %>%
  ggplot() +
  geom_line(aes(date,uci,group=CCAA) ) +
  geom_point(aes(date,uci),size = 0.5 ) +
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
  geom_line(aes(date,uci,group=CCAA) ) +
  geom_point(aes(date,uci,group=CCAA), size = 0.5 ) +
  scale_y_log10( minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "personas en UCI",
       x = "fecha",
       caption = caption)
dev.off()

# Escala lineal
png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
data_uci %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,uci_per_cienmil,group=CCAA) ) +
  geom_point(aes(date,uci_per_cienmil,group=CCAA), size = 0.5 ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "personas en UCI por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
data_uci %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,uci_per_cienmil,group=CCAA) ) +
  geom_point(aes(date,uci_per_cienmil,group=CCAA), size = 0.5 ) +
  scale_y_log10( minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) )) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "personas en UCI por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# // 2.2 UCI Superpuesto -------------
png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
data_uci %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,uci,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,uci, color=CCAA),size = 1.5 ) +
  geom_text_repel(data=filter( data_uci, date==max(data_uci$date),  CCAA != "Total"), 
                  aes(date,uci, color=CCAA, label=paste(format(uci, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
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
  geom_line(aes(date,uci,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,uci, color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_uci, date==max(data_uci$date),  CCAA != "Total"), 
                  aes(date,uci, color=CCAA, label=paste(format(uci, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # scale_color_brewer(palette = "Dark2", type = "discrete") +
  scale_y_log10( minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_uci$date), max(data_uci$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "personas en UCI",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
data_uci %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,uci_per_cienmil,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,uci_per_cienmil,  color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_uci, date==max(data_uci$date),  CCAA != "Total"), 
                  aes(date,uci_per_cienmil, color = CCAA, label=paste(format(uci_per_cienmil, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_uci$date), max(data_uci$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "personas en UCI por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-superpuesto-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
data_uci %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,uci_per_cienmil,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,uci_per_cienmil,  color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_uci, date==max(data_uci$date),  CCAA != "Total"), 
                  aes(date,uci_per_cienmil, color = CCAA, label=paste(format(uci_per_cienmil, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( minor_breaks = c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_uci$date), max(data_uci$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "personas en UCI por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# Deceassed -------------------
# / Fallecimientos superpuestos ----------
png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
data_i_cases %>% 
  ggplot() +
  geom_line(aes(date, deceassed, group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date),  region != "Total"), 
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
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_i_cases$date), max(data_i_cases$date + 3)) 
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
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-superpuesto-log.png", sep = ""),width = 1000,height = 700)
data_i_cases %>% 
  ggplot() +
  geom_line(aes(date,deceassed,group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date),  region != "Total"), 
                  aes(date,deceassed, color=region, label=paste(format(deceassed, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( 
    limits = c(1,max(data_i_cases$deceassed)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100)) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_i_cases$date), max(data_i_cases$date +3)) 
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
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()
