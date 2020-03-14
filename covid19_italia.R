# Analizar datos de Coronavirus COVID-19 en Italia por provincia

# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping

# COVID-19 in Italy -----------
data_i_cases <- read.delim("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",sep = ",")  

# Process data
data_i_cases$date <-  as.Date(data_i_cases$data)

# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption_i <- "Gráfico: @numeroteca (montera34.com). Datos: Protezione Civile (Italia)"
periodo_i <- "2020.02.24 - 03.14"
# Cases ------------

# ----- Small multiple ------------
# Escala lineal
png(filename=paste("img/italia/covid19_casos-registrados-por-region-lineal.png", sep = ""),width = 1000,height = 700)
data_i_cases %>%  # filter( denominazione_regione != "Total") %>%
  ggplot() +
  geom_line(aes(date,totale_casi,group=denominazione_regione) ) +
  geom_point(aes(date,totale_casi,group=denominazione_regione), size = 0.5 ) +
  facet_wrap( ~denominazione_regione) +
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
  geom_line(aes(date,totale_casi,group=denominazione_regione) ) +
  geom_point(aes(date,totale_casi,group=denominazione_regione), size = 0.5 )  +
  scale_y_log10( minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  facet_wrap( ~denominazione_regione) +
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
# data_i_cases %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=denominazione_regione) ) +
#   geom_point(aes(date,per_cienmil,group=denominazione_regione), size = 0.5 ) +
#   facet_wrap( ~denominazione_regione) +
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
# data_i_cases %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=denominazione_regione) ) +
#   geom_point(aes(date,per_cienmil,group=denominazione_regione), size = 0.5 ) +
#   scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 3000, 100) )) +
#   facet_wrap( ~denominazione_regione) +
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
  geom_line( aes(date,totale_casi, group=denominazione_regione, color=denominazione_regione), size= 1 ) +
  geom_point(aes(date,totale_casi, color=denominazione_regione), size = 2 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date)), 
                  aes(date,totale_casi, color=denominazione_regione, label=paste(format(totale_casi, nsmall=1, big.mark="."),denominazione_regione)),
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
               limits=c( min(data_i_cases$date), max(data_i_cases$date + 1.5)) 
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
  geom_line( aes(date,totale_casi, group=denominazione_regione, color=denominazione_regione), size= 1 ) +
  geom_point(aes(date,totale_casi, color=denominazione_regione), size = 2 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date)), 
                  aes(date,totale_casi, color=denominazione_regione, label=paste(format(totale_casi, nsmall=1, big.mark="."),denominazione_regione)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 limits = c(0.95,12000),
                 breaks = c(1,10,100,1000,12000),
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_i_cases$date), max(data_i_cases$date + 1.5)) 
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
# data_i_cases %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=denominazione_regione, color=denominazione_regione), size= 1 ) +
#   geom_point(aes(date,per_cienmil, color=denominazione_regione), size= 1.5 ) +
#   geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date),  denominazione_regione != "Total"), 
#                   aes(date,per_cienmil, color=denominazione_regione, label=paste(format(per_cienmil, nsmall=1, big.mark="."),denominazione_regione)),
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

# UCI (intensive care) -------------------

# ------------- TODO from HERE ----------------------

# # -------- UCI Small multiple ----------
# # Escala lineal
# png(filename=paste("img/italia/covid19_casos-registrados-UCI-por-region-lineal.png", sep = ""),width = 1000,height = 700)
# data_i_uci %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=denominazione_regione) ) +
#   geom_point(aes(date,value),size = 0.5 ) +
#   facet_wrap( ~denominazione_regione) +
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
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en Italia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "personas en UCI",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# # Escala logarítmica
# png(filename=paste("img/italia/covid19_casos-registrados-UCI-por-region-log.png", sep = ""),width = 1000,height = 700)
# data_i_uci %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=denominazione_regione) ) +
#   geom_point(aes(date,value,group=denominazione_regione), size = 0.5 ) +
#   scale_y_log10( minor_breaks = seq(0 , 2000, 10) ) +
#   facet_wrap( ~denominazione_regione) +
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
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en Italia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "personas en UCI",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# # Escala lineal
# png(filename=paste("img/italia/covid19_casos-registrados-UCI-por-region-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
# data_i_uci %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=denominazione_regione) ) +
#   geom_point(aes(date,per_cienmil,group=denominazione_regione), size = 0.5 ) +
#   facet_wrap( ~denominazione_regione) +
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
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por 100.000 habitantes en Italia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "personas en UCI por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# # Escala logarítmica
# png(filename=paste("img/italia/covid19_casos-registrados-UCI-por-region-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
# data_i_uci %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=denominazione_regione) ) +
#   geom_point(aes(date,per_cienmil,group=denominazione_regione), size = 0.5 ) +
#   scale_y_log10( minor_breaks = seq(0 , 2000, 0.1)) +
#   facet_wrap( ~denominazione_regione) +
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
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por 100.000 habitantes en Italia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "personas en UCI por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# # ------ UCI Superpuesto -------------
# png(filename=paste("img/italia/covid19_casos-registrados-UCI-por-region-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
# data_i_uci %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=denominazione_regione, color=denominazione_regione), size= 1 ) +
#   geom_point(aes(date,value, color=denominazione_regione),size = 1.5 ) +
#   geom_text_repel(data=filter( data_i_uci, date==max(data_i_uci$date),  denominazione_regione != "Total"), 
#                   aes(date,value, color=denominazione_regione, label=paste(format(value, nsmall=1, big.mark="."),denominazione_regione)),
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
#                limits=c( min(data_i_uci$date), max(data_i_uci$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en Italia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "personas en UCI",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# png(filename=paste("img/italia/covid19_casos-registrados-UCI-por-region-superpuesto-log.png", sep = ""),width = 1000,height = 700)
# data_i_uci %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=denominazione_regione, color=denominazione_regione), size= 1 ) +
#   geom_point(aes(date,value, color=denominazione_regione), size= 1.5 ) +
#   geom_text_repel(data=filter( data_i_uci, date==max(data_i_uci$date),  denominazione_regione != "Total"), 
#                   aes(date,value, color=denominazione_regione, label=paste(format(value, nsmall=1, big.mark="."),denominazione_regione)),
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
#                limits=c( min(data_i_uci$date), max(data_i_uci$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en Italia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "personas en UCI",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# png(filename=paste("img/italia/covid19_casos-registrados-UCI-por-region-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
# data_i_uci %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=denominazione_regione, color=denominazione_regione), size= 1 ) +
#   geom_point(aes(date,per_cienmil,  color=denominazione_regione), size= 1.5 ) +
#   geom_text_repel(data=filter( data_i_uci, date==max(data_i_uci$date),  denominazione_regione != "Total"), 
#                   aes(date,per_cienmil, color = denominazione_regione, label=paste(format(per_cienmil, nsmall=1, big.mark="."),denominazione_regione)),
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
#                limits=c( min(data_i_uci$date), max(data_i_uci$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por 100.000 habitantes en Italia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "personas en UCI por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# png(filename=paste("img/italia/covid19_casos-registrados-UCI-por-region-superpuesto-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
# data_i_uci %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=denominazione_regione, color=denominazione_regione), size= 1 ) +
#   geom_point(aes(date,per_cienmil,  color=denominazione_regione), size= 1.5 ) +
#   geom_text_repel(data=filter( data_i_uci, date==max(data_i_uci$date),  denominazione_regione != "Total"), 
#                   aes(date,per_cienmil, color = denominazione_regione, label=paste(format(per_cienmil, nsmall=1, big.mark="."),denominazione_regione)),
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
#                limits=c( min(data_i_uci$date), max(data_i_uci$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados por 100.000 habitantes en Italia",
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
# png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-lineal.png", sep = ""),width = 1000,height = 700)
# data_i_death %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=denominazione_regione) ) +
#   facet_wrap( ~denominazione_regione) +
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
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en Italia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# # Escala logarítmica
# png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-log.png", sep = ""),width = 1000,height = 700)
# data_i_death %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=denominazione_regione) ) +
#   scale_y_log10( minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) )) +
#   facet_wrap( ~denominazione_regione) +
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
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en Italia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# # ---------Fallecimientos superpuestos ----------
# png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
# data_i_death %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=denominazione_regione, color=denominazione_regione), size= 1 ) +
#   geom_point(aes(date,value, color=denominazione_regione), size= 1.5 ) +
#   geom_text_repel(data=filter( data_i_death, date==max(data_i_death$date),  denominazione_regione != "Total"), 
#                   aes(date,value, color=denominazione_regione, label=paste(format(value, nsmall=1, big.mark="."),denominazione_regione)),
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
#                limits=c( min(data_i_death$date), max(data_i_death$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en Italia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-superpuesto-log.png", sep = ""),width = 1000,height = 700)
# data_i_death %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=denominazione_regione, color=denominazione_regione), size= 1 ) +
#   geom_point(aes(date,value, color=denominazione_regione), size= 1.5 ) +
#   geom_text_repel(data=filter( data_i_death, date==max(data_i_death$date),  denominazione_regione != "Total"), 
#                   aes(date,value, color=denominazione_regione, label=paste(format(value, nsmall=1, big.mark="."),denominazione_regione)),
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
#                limits=c( min(data_i_death$date), max(data_i_death$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en Italia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
# data_i_death %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=denominazione_regione, color=denominazione_regione), size= 1 ) +
#   geom_point(aes(date,per_cienmil, color=denominazione_regione), size= 1.5 ) +
#   geom_text_repel(data=filter( data_i_death, date==max(data_i_death$date),  denominazione_regione != "Total"), 
#                   aes(date,per_cienmil, color=denominazione_regione, label=paste(format(per_cienmil, nsmall=1, big.mark="."),denominazione_regione)),
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
#                limits=c( min(data_i_death$date), max(data_i_death$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en Italia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_i),
#        y = "fallecidos por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# png(filename=paste("img/italia/covid19_fallecimientos-registrados-por-region-superpuesto-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
# data_i_death %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,per_cienmil,group=denominazione_regione, color=denominazione_regione), size= 1 ) +
#   geom_point(aes(date,per_cienmil, color=denominazione_regione), size= 1.5 ) +
#   geom_text_repel(data=filter( data_i_death, date==max(data_i_death$date),  denominazione_regione != "Total"), 
#                   aes(date,per_cienmil, color=denominazione_regione, label=paste(format(per_cienmil, nsmall=1, big.mark="."),denominazione_regione)),
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
#                limits=c( min(data_i_death$date), max(data_i_death$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en Italia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
#        y = "fallecidos por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_i)
# dev.off()
# 
# 
# # --------- Relaciones --------
# # png(filename=paste("img/italia/covid19_ .png", sep = ""),width = 1000,height = 700)
# data_i_all %>% filter( denominazione_regione != "Total") %>%
#   ggplot() +
#   geom_line( aes(cases_per_cienmil,death_per_cienmil, group=denominazione_regione, color=denominazione_regione), size= 1 ) +
#   geom_point(aes(cases_per_cienmil,death_per_cienmil, color=denominazione_regione), size= 2 ) +
#   # lines(x = c(0,0), y = c(20,1000)) +
#   geom_abline(slope = 0.25) +
#   # Annotations
#   geom_text(aes(cases_per_cienmil,death_per_cienmil+0.5, color=denominazione_regione,label=paste( substr(date,7,10 ))), size= 3, color="#000000") +
#   geom_text_repel(data=filter( data_i_all, date==max(data_i_all[!is.na(data_i_all$date),]$date),  denominazione_regione != "Total"),
#                   aes(cases_per_cienmil,death_per_cienmil, color=denominazione_regione, label=denominazione_regione),
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
#   #              limits=c( min(data_i_cases$date), max(data_i_cases$date + 1.5)) 
#   # ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     # panel.grid.minor.x = element_blank(),
#     # panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Fallecimientos y casos acumulados COVID-19 en Italia",
#        subtitle = paste0("Por región",periodo_i),
#        y = "fallecimientos por 100.000 habitantes",
#        x = "casos acumulados por 100.000 habitantes",
#        caption = caption_i)
# # dev.off()