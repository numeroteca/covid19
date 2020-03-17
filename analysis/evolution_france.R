# Analizar datos de Coronavirus COVID-19 en Francia por provincia

# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping

# COVID-19 in Italy -----------
data_f_cases_original <- read.delim("data/original/france/covid19.csv",sep = ",")  
data_f_cases_original <- read.delim("https://www.data.gouv.fr/fr/datasets/r/fa9b8fc8-35d5-4e24-90eb-9abe586b0fa5",sep = ",")  
data_f_cases<- melt(data_f_cases_original) 

names(data_f_cases) <- c("date","region","cases")

# Process data
data_f_cases$date <-  as.Date(data_f_cases$date)

# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption_i <- "Gráfico: montera34.com. Datos: data.gouv.fr"
periodo_i <- "2020.03.04 - 03.16"
# Cases ------------

# ----- Small multiple ------------
# Escala lineal
png(filename=paste("img/france/covid19_casos-registrados-por-region-lineal.png", sep = ""),width = 1000,height = 700)
data_f_cases %>%  
  ggplot() +
  geom_line(aes(date,cases,group=region) ) +
  geom_point(aes(date,cases,group=region), size = 0.5 ) +
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
  labs(title = "Número de casos acumulados de COVID-19 registrados en Francia",
       subtitle = paste0("Por región (escala lineal). ",periodo_i),
       y = "casos registrados",
       x = "fecha",
       caption = caption_i)
dev.off()

# Escala logarítmica
png(filename=paste("img/france/covid19_casos-registrados-por-region-log.png", sep = ""),width = 1000,height = 700)
data_f_cases %>%
  ggplot() +
  geom_line(aes(date,cases,group=region) ) +
  geom_point(aes(date,cases,group=region), size = 0.5 ) +
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
  labs(title = "Número de casos acumulados de COVID-19 registrados en Francia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "casos registrados",
       x = "fecha",
       caption = caption_i)
dev.off()

# png(filename=paste("img/france/covid19_casos-registrados-por-region-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
# data_f_cases %>% filter( region != "Total") %>%
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
#   labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Francia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "casos registrados por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()

# png(filename=paste("img/france/covid19_casos-registrados-por-region-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
# data_f_cases %>% filter( region != "Total") %>%
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
#   labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Francia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "casos registrados por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()

# ---------- Superpuesto ---------------
png(filename=paste("img/france/covid19_casos-registrados-por-region-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
data_f_cases %>%
  ggplot() +
  geom_line( aes(date,cases, group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases, color=region), size = 2 ) +
  geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date)), 
                  aes(date,cases, color=region, label=paste(format(cases, nsmall=1, big.mark="."),region)),
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
               limits=c( min(data_f_cases$date), max(data_f_cases$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en Francia",
       subtitle = paste0("Por región (escala lineal). ",periodo_i),
       y = "casos registrados",
       x = "fecha",
       caption = caption_i)
dev.off()

png(filename=paste("img/france/covid19_casos-registrados-por-region-superpuesto-log.png", sep = ""),width = 1000,height = 700)
data_f_cases %>%
  ggplot() +
  geom_line( aes(date,cases, group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases, color=region), size = 2 ) +
  geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date)), 
                  aes(date,cases, color=region, label=paste(format(cases, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 # limits = c(0.95,12000),
                 breaks = c(1,10,100,1000,12000),
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_f_cases$date), max(data_f_cases$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en Francia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "casos registrados",
       x = "fecha",
       caption = caption_i)
dev.off()
# 
# png(filename=paste("img/france/covid19_casos-registrados-por-region-superpuesto-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
# data_f_cases %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,per_cienmil, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date),  region != "Total"), 
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
#                limits=c( min(data_f_cases$date), max(data_f_cases$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Francia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "casos registrados por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()

# UCI (intensive care) -------------------

# ------------- TODO from HERE ----------------------

# # -------- UCI Small multiple ----------
# # Escala lineal
# png(filename=paste("img/france/covid19_casos-registrados-UCI-por-region-lineal.png", sep = ""),width = 1000,height = 700)
# data_f_uci %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=region) ) +
#   geom_point(aes(date,value),size = 0.5 ) +
#   facet_wrap( ~region) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d"
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000")
#     # legend.position = "bottom"
#   ) +
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en Francia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "personas en UCI",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# # Escala logarítmica
# png(filename=paste("img/france/covid19_casos-registrados-UCI-por-region-log.png", sep = ""),width = 1000,height = 700)
# data_f_uci %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=region) ) +
#   geom_point(aes(date,value,group=region), size = 0.5 ) +
#   scale_y_log10( minor_breaks = seq(0 , 2000, 10) ) +
#   facet_wrap( ~region) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d"
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000")
#     # legend.position = "bottom"
#   ) +
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en Francia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "personas en UCI",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# # Escala lineal
# png(filename=paste("img/france/covid19_casos-registrados-UCI-por-region-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
# data_f_uci %>% filter( region != "Total") %>%
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
#     axis.ticks.x = element_line(color = "#000000")
#     # legend.position = "bottom"
#   ) +
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por 100.000 habitantes en Francia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "personas en UCI por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# # Escala logarítmica
# png(filename=paste("img/france/covid19_casos-registrados-UCI-por-region-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
# data_f_uci %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=region) ) +
#   geom_point(aes(date,per_cienmil,group=region), size = 0.5 ) +
#   scale_y_log10( minor_breaks = seq(0 , 2000, 0.1)) +
#   facet_wrap( ~region) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d"
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000")
#     # legend.position = "bottom"
#   ) +
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por 100.000 habitantes en Francia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "personas en UCI por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# # ------ UCI Superpuesto -------------
# png(filename=paste("img/france/covid19_casos-registrados-UCI-por-region-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
# data_f_uci %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,value, color=region),size = 1.5 ) +
#   geom_text_repel(data=filter( data_f_uci, date==max(data_f_uci$date),  region != "Total"), 
#                   aes(date,value, color=region, label=paste(format(value, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   # hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d",
#                limits=c( min(data_f_uci$date), max(data_f_uci$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en Francia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "personas en UCI",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# png(filename=paste("img/france/covid19_casos-registrados-UCI-por-region-superpuesto-log.png", sep = ""),width = 1000,height = 700)
# data_f_uci %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,value, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_f_uci, date==max(data_f_uci$date),  region != "Total"), 
#                   aes(date,value, color=region, label=paste(format(value, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   # hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   # scale_color_brewer(palette = "Dark2", type = "discrete") +
#   scale_y_log10( minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d",
#                limits=c( min(data_f_uci$date), max(data_f_uci$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en Francia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "personas en UCI",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# png(filename=paste("img/france/covid19_casos-registrados-UCI-por-region-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
# data_f_uci %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,per_cienmil,  color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_f_uci, date==max(data_f_uci$date),  region != "Total"), 
#                   aes(date,per_cienmil, color = region, label=paste(format(per_cienmil, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   # hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d",
#                limits=c( min(data_f_uci$date), max(data_f_uci$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por 100.000 habitantes en Francia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "personas en UCI por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# png(filename=paste("img/france/covid19_casos-registrados-UCI-por-region-superpuesto-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
# data_f_uci %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,per_cienmil,  color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_f_uci, date==max(data_f_uci$date),  region != "Total"), 
#                   aes(date,per_cienmil, color = region, label=paste(format(per_cienmil, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   # hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   scale_y_log10( minor_breaks = c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d",
#                limits=c( min(data_f_uci$date), max(data_f_uci$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por 100.000 habitantes en Francia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "personas en UCI por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# # Fallecimientos ------------
# 
# # ---------Fallecimientos Small multiple ----------
# # Escala lineal
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-lineal.png", sep = ""),width = 1000,height = 700)
# data_f_death %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=region) ) +
#   facet_wrap( ~region) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d"
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000")
#     # legend.position = "bottom"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en Francia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# # Escala logarítmica
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-log.png", sep = ""),width = 1000,height = 700)
# data_f_death %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=region) ) +
#   scale_y_log10( minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) )) +
#   facet_wrap( ~region) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d"
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000")
#     # legend.position = "bottom"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en Francia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# # ---------Fallecimientos superpuestos ----------
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
# data_f_death %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,value, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_f_death, date==max(data_f_death$date),  region != "Total"), 
#                   aes(date,value, color=region, label=paste(format(value, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   # hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d",
#                limits=c( min(data_f_death$date), max(data_f_death$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en Francia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-superpuesto-log.png", sep = ""),width = 1000,height = 700)
# data_f_death %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,value, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_f_death, date==max(data_f_death$date),  region != "Total"), 
#                   aes(date,value, color=region, label=paste(format(value, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   # hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100)) ) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d",
#                limits=c( min(data_f_death$date), max(data_f_death$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en Francia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
# data_f_death %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,per_cienmil, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_f_death, date==max(data_f_death$date),  region != "Total"), 
#                   aes(date,per_cienmil, color=region, label=paste(format(per_cienmil, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   # hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d",
#                limits=c( min(data_f_death$date), max(data_f_death$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en Francia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "fallecidos por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-superpuesto-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
# data_f_death %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,per_cienmil, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_f_death, date==max(data_f_death$date),  region != "Total"), 
#                   aes(date,per_cienmil, color=region, label=paste(format(per_cienmil, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   # hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   scale_y_log10(  minor_breaks =  c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d",
#                limits=c( min(data_f_death$date), max(data_f_death$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en Francia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "fallecidos por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# 
# # --------- Relaciones --------
# # png(filename=paste("img/france/covid19_ .png", sep = ""),width = 1000,height = 700)
# data_f_all %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line( aes(cases_per_cienmil,death_per_cienmil, group=region, color=region), size= 1 ) +
#   geom_point(aes(cases_per_cienmil,death_per_cienmil, color=region), size= 2 ) +
#   # lines(x = c(0,0), y = c(20,1000)) +
#   geom_abline(slope = 0.25) +
#   # Annotations
#   geom_text(aes(cases_per_cienmil,death_per_cienmil+0.5, color=region,label=paste( substr(date,7,10 ))), size= 3, color="#000000") +
#   geom_text_repel(data=filter( data_f_all, date==max(data_f_all[!is.na(data_f_all$date),]$date),  region != "Total"),
#                   aes(cases_per_cienmil,death_per_cienmil, color=region, label=region),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   # hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   # scale_x_date(date_breaks = "1 day", 
#   #              date_labels = "%d",
#   #              limits=c( min(data_f_cases$date), max(data_f_cases$date + 1.5)) 
#   # ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     # panel.grid.minor.x = element_blank(),
#     # panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Fallecimientos y casos acumulados COVID-19 en Francia",
#        subtitle = paste0("Por región",periodo_i),
#        y = "fallecimientos por 100.000 habitantes",
#        x = "casos acumulados por 100.000 habitantes",
#        caption = caption_i)
# # dev.off()