# Analizar datos de Coronavirus COVID-19 en Francia por provincia

# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping

# COVID-19 in France-----------
# data_f_cases_original <- read.delim("data/original/france/covid19.csv",sep = ",")  
data_f_cases_original <- read.delim("https://www.data.gouv.fr/fr/datasets/r/fa9b8fc8-35d5-4e24-90eb-9abe586b0fa5",sep = ",")  

# Population
population_f_region <- read.delim("data/original/france/ccaa-poblacion.csv",sep = ",")  

# Process data -------
data_f_cases<- melt(data_f_cases_original) 
names(data_f_cases) <- c("date","region","cases_registered")
data_f_cases$date <-  as.Date(data_f_cases$date)

levels(data_f_cases$region)
population_f_region$Region


# add population data
data_f_cases <- merge( data_f_cases, select(population_f_region,Region,X1er.janvier.2020..p.), by.x = "region", by.y = "Region", all.x = TRUE)
names(data_f_cases) <- c("region","date","cases_registered","population")
# calculate values per 
data_f_cases$cases_per_100000 <- round( data_f_cases$cases_registered / data_f_cases$population * 100000, digits = 2)

write.csv(data_f_cases, file = "data/output/france/covid19-cases-registered-region-france-by-day-accumulated.csv", row.names = FALSE)


# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption_i <- "Gráfico: montera34.com. Datos: data.gouv.fr"
periodo_i <- "2020.03.04 - 03.20"
# 1. Cases ------------

# create temp dataframes to be able to plot all the values in small multiples
data_f_cases_sm <-data_f_cases
data_f_cases_sm$region_cp <- data_f_cases$region

# ----- Small multiple ------------
# Escala lineal
png(filename=paste("img/france/covid19_casos-registrados-por-region-lineal.png", sep = ""),width = 1000,height = 700)
data_f_cases %>%
  ggplot() +
  geom_line(data = select(data_f_cases_sm,date,cases_registered,region_cp,-region),
            aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
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
  geom_line(data = select(data_f_cases_sm,date,cases_registered,region_cp,-region),
            aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_registered,group=region) ) +
  geom_point(aes(date,cases_registered,group=region), size = 0.5 ) +
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

png(filename=paste("img/france/covid19_casos-registrados-por-region-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
data_f_cases %>%
  ggplot() +
  geom_line(data = select(data_f_cases_sm,date,cases_per_100000,region_cp,-region),
            aes(date,cases_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_per_100000,group=region) ) +
  geom_point(aes(date,cases_per_100000,group=region), size = 0.5 ) +
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
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Francia",
       subtitle = paste0("Por región (escala lineal). ",periodo_i),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_i)
dev.off()

png(filename=paste("img/france/covid19_casos-registrados-por-region-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
data_f_cases %>% 
  ggplot() +
  geom_line(data = select(data_f_cases_sm,date,cases_per_100000,region_cp,-region),
            aes(date,cases_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_per_100000,group=region) ) +
  geom_point(aes(date,cases_per_100000,group=region), size = 0.5 ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 3000, 100) )) +
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
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Francia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_i)
dev.off()

# ---------- Superpuesto ---------------
png(filename=paste("img/france/covid19_casos-registrados-por-region-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
data_f_cases %>%
  ggplot() +
  geom_line( aes(date,cases_registered, group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_registered, color=region), size = 2 ) +
  geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date)), 
                  aes(date,cases_registered, color=region, label=paste(format(cases_registered, nsmall=1, big.mark="."),region)),
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
               limits=c( min(data_f_cases$date), max(data_f_cases$date + 2.5)) 
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
  geom_line( aes(date,cases_registered, group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_registered, color=region), size = 2 ) +
  geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date)), 
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
                 # limits = c(0.95,12000),
                 breaks = c(1,10,100,1000,12000),
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_f_cases$date), max(data_f_cases$date + 3.5)) 
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

png(filename=paste("img/france/covid19_casos-registrados-por-region-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
data_f_cases %>% 
  ggplot() +
  geom_line(aes(date,cases_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_per_100000, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date)),
                  aes(date,cases_per_100000, color=region, label=paste(format(cases_per_100000, nsmall=1, big.mark="."),region)),
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
               limits=c( min(data_f_cases$date), max(data_f_cases$date + 3.5))
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Francia",
       subtitle = paste0("Por región (escala lineal). ",periodo_i),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_i)
dev.off()


png(filename=paste("img/france/covid19_casos-registrados-por-region-superpuesto-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
data_f_cases %>% 
  ggplot() +
  geom_line(aes(date,cases_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_per_100000, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date)),
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
                 minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( min(data_f_cases$date), max(data_f_cases$date + 3.5))
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Francia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_i),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption_i)
dev.off()

# 2. UCI (intensive care) -------------------

# 3. Deceassed (Fallecimientos) ------------

# // 3.1 Fallecimientos Small multiple ----------
# # Escala lineal
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-lineal.png", sep = ""),width = 1000,height = 700)
# data_death %>% filter( CCAA != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,death,group=CCAA) ) +
#   facet_wrap( ~CCAA) +
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
#        subtitle = paste0("Por región (escala lineal). ",period),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption)
# dev.off()
# 
# # Escala logarítmica
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-log.png", sep = ""),width = 1000,height = 700)
# data_death %>% filter( CCAA != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,death,group=CCAA) ) +
#   scale_y_log10( minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) )) +
#   facet_wrap( ~CCAA) +
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
#        subtitle = paste0("Por región (escala logarítmica). ",period),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption)
# dev.off()
# 
# # // 3.2 Fallecimientos superpuestos ----------
# # // CCAA -------------------
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
# data_death %>% filter( CCAA != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,death,group=CCAA, color=CCAA), size= 1 ) +
#   geom_point(aes(date,death, color=CCAA), size= 1.5 ) +
#   geom_text_repel(data=filter( data_death, date==max(data_death$date),  CCAA != "Total"), 
#                   aes(date,death, color=CCAA, label=paste(format(death, nsmall=1, big.mark="."),CCAA)),
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
#                limits=c( min(data_death$date), max(data_death$date + 1.5)) 
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
#        subtitle = paste0("Por región (escala lineal). ",period),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption)
# dev.off()
# 
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-superpuesto-log.png", sep = ""),width = 1000,height = 700)
# data_death %>% filter( CCAA != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,death,group=CCAA, color=CCAA), size= 1 ) +
#   geom_point(aes(date,death, color=CCAA), size= 1.5 ) +
#   geom_text_repel(data=filter( data_death, date==max(data_death$date),  CCAA != "Total"), 
#                   aes(date,death, color=CCAA, label=paste(format(death, nsmall=1, big.mark="."),CCAA)),
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
#                limits=c( min(data_death$date), max(data_death$date + 1.5)) 
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
#        subtitle = paste0("Por región (escala logarítmica). ",period),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption)
# dev.off()
# 
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
# data_death %>% filter( CCAA != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,death_per_cienmil,group=CCAA, color=CCAA), size= 1 ) +
#   geom_point(aes(date,death_per_cienmil, color=CCAA), size= 1.5 ) +
#   geom_text_repel(data=filter( data_death, date==max(data_death$date),  CCAA != "Total"), 
#                   aes(date,death_per_cienmil, color=CCAA, label=paste(format(death_per_cienmil, nsmall=1, big.mark="."),CCAA)),
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
#                limits=c( min(data_death$date), max(data_death$date + 1.5)) 
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
#        subtitle = paste0("Por región (escala lineal). ",period),
#        y = "fallecidos por 100.000 habitantes",
#        x = "fecha",
#        caption = caption)
# dev.off()
# 
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-superpuesto-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
# data_death %>% filter( CCAA != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,death_per_cienmil,group=CCAA, color=CCAA), size= 1 ) +
#   geom_point(aes(date,death_per_cienmil, color=CCAA), size= 1.5 ) +
#   geom_text_repel(data=filter( data_death, date==max(data_death$date),  CCAA != "Total"), 
#                   aes(date,death_per_cienmil, color=CCAA, label=paste(format(death_per_cienmil, nsmall=1, big.mark="."),CCAA)),
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
#                limits=c( min(data_death$date), max(data_death$date + 1.5)) 
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
#        subtitle = paste0("Por región (escala logarítmica). ",period),
#        y = "fallecidos por 100.000 habitantes",
#        x = "fecha",
#        caption = caption)
# dev.off()
