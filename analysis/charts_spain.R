# Genera gráficos de Coronavirus COVID-19 en España agregados a partir de datos de provincias de CCAA
# Los gráficos generados pueden verse en https://lab.montea34.com/covid9

# Load libraries 
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption_provincia <- "Gráfico: @numeroteca (lab.montera34.com/covid19) | Datos: esCOVID19data (github.com/montera34/escovid19data)"
updated <- ""
period <- "(Actualizado: 2020-09-15)"
filter_date <- as.Date("2020-09-08")

# Warning: you need to have loaded spain by executing process_spain_provinces_data.R 
# or load it using:
spain_ccaa <- readRDS(file ="data/output/spain/covid19-ccaa-spain_consolidated.rds") %>% mutate(
  week_n = strftime(date, format = "%V"),
)
spain <- readRDS(file = "data/output/spain/covid19-spain_consolidated.rds") %>% mutate(
  week_n = strftime(date, format = "%V")
)

spain_week <- spain %>% group_by(week_n) %>% summarise(
  deaths = sum(daily_deaths) 
  ) %>% ungroup() %>% mutate (
  # week_n = week_n %>% str_replace("05","5"),
  # week_n = week_n %>% str_replace("06","6"),
  # week_n = week_n %>% str_replace("07","7"),
  # week_n = week_n %>% str_replace("08","5"),
  # week_n = week_n %>% str_replace("09","9"),
  week_n= as.numeric(week_n)
)

# create temp dataframes to be able to plot all the values in small multiples
spain <- spain %>% filter( date < filter_date)

png(filename=paste("img/spain/country/covid19_spain_casos-dia-lineal.png", sep = ""),width = 900,height = 600)
spain %>% filter( date > filter_date - 80 & date < filter_date - 0) %>%
  ggplot() +
  geom_col(aes(date, daily_cases_PCR), width= 1, fill="#DDDDDD" ) +
  geom_text(aes(date, daily_cases_PCR+1, label=daily_cases_PCR), color="#888888", vjust = 0.2 ) +
  geom_line(aes(date, daily_cases_PCR_avg7), size= 1 ) +
  # geom_point(aes(date, deceased, color=ccaa), size= 1 ) +
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( filter_date - 50, max(spain$date)),
               expand = c(0,0)
  ) + 
  scale_y_continuous(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
    # limits = c( 0, max(spain$daily_deaths_avg7))
    # limits = c( 0, 50)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = paste0("Casos PCR+ por día (media ventana 7 días) por COVID-19 en España ", updated ),
       subtitle = paste0("(escala lineal). ",period),
       y = "casos",
       x = "fecha",
       # color = "Comunidad autónoma",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/country/covid19_spain_hosp-lineal.png", sep = ""),width = 900,height = 600)
spain %>% filter( date > filter_date - 80 & date < filter_date - 0) %>%
  ggplot() +
  geom_col(aes(date, hospitalized), width= 1, fill="#DDDDDD" ) +
  geom_text(aes(date, hospitalized+20, label=hospitalized), color="#888888", vjust = 0.2, size = 3 ) +
  # geom_line(aes(date, daily_cases_PCR_avg7), size= 1 ) +
  # geom_point(aes(date, deceased, color=ccaa), size= 1 ) +
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( filter_date - 50, max(spain$date)),
               expand = c(0,0)
  ) + 
  scale_y_continuous(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
    # limits = c( 0, max(spain$daily_deaths_avg7))
    # limits = c( 0, 50)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = paste0("Hospitalizados por día (media ventana 7 días) por COVID-19 en España ", updated ),
       subtitle = paste0("(escala lineal). ",period),
       y = "casos",
       x = "fecha",
       # color = "Comunidad autónoma",
       caption = caption_provincia)
dev.off()


png(filename=paste("img/spain/country/covid19_spain_fallecimientos-lineal.png", sep = ""),width = 900,height = 600)
spain %>%
  ggplot() +
  geom_line(aes(date, deceased), size= 1 ) +
  # geom_point(aes(date, deceased, color=ccaa), size= 1 ) +
  geom_text_repel(
    data = spain %>% top_n(1, date),
    aes(date, deceased, label=paste(format(deceased, nsmall=0, big.mark="."))),
    nudge_x = 5, # adjust the starting y position of the text label
    size=5,
    hjust=0,
    family = "Roboto Condensed",
    direction="y",
    segment.size = 0.1,
    segment.color="#777777"
  ) +
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "2 week", 
               date_labels = "%d/%m",
               limits=c( min(spain$date)+7, max(spain$date + 46)),
               expand = c(0,0)
  ) + 
  scale_y_continuous(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = paste0("Fallecimientos acumulados por COVID-19 registrados en España ", updated ),
       subtitle = paste0("Agregado de datos por CCAA (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       color = "Comunidad autónoma",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/country/covid19_spain_muertes-dia-lineal.png", sep = ""),width = 900,height = 600)
spain %>% filter( date > filter_date - 50) %>%
  ggplot() +
  geom_col(aes(date, daily_deaths), size= 1 ) +
  geom_text(aes(date, daily_deaths+2, label=daily_deaths), color="#888888", vjust = 0.2, size = 3 ) +
  # geom_point(aes(date, deceased, color=ccaa), size= 1 ) +
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( filter_date - 50, max(spain$date)),
               expand = c(0,0)
  ) + 
  scale_y_continuous(
    limits = c( 0, max(spain$daily_deaths))
  ) +
theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = paste0("Muertes por día por COVID-19 en España ", updated ),
       subtitle = paste0("(escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       color = "Comunidad autónoma",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/country/covid19_spain_muertes-semana-lineal.png", sep = ""),width = 900,height = 600)
spain_week %>% filter( week_n < 35 & week_n > 25) %>%
  ggplot() +
  geom_col(aes(week_n, deaths), size= 1 ) +
  geom_text(aes(week_n, deaths+20, label=deaths), color="#888888", vjust = 0.2, size = 3 ) +
  # scale_color_manual(values = colors_prov) +
  # scale_x_date(date_breaks = "1 week", 
  #              date_labels = "%d/%m",
  #              limits=c( filter_date - 50, max(spain$date)),
  #              expand = c(0,0)
  # ) + 
  # scale_y_continuous(
  #   limits = c( 0, max(spain$daily_deaths))
  # ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = paste0("Muertes por semana por COVID-19 en España ", updated ),
       subtitle = paste0("(escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       # color = "Comunidad autónoma",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/country/covid19_spain_muertes-dia-media-lineal.png", sep = ""),width = 900,height = 600)
spain %>% filter( date > filter_date - 80) %>%
  ggplot() +
  geom_col(aes(date, daily_deaths), width= 1, fill="#DDDDDD" ) +
  geom_text(aes(date, daily_deaths+1, label=daily_deaths), color="#888888", vjust = 0.2 ) +
  geom_line(aes(date, daily_deaths_avg7), size= 1 ) +
  # geom_point(aes(date, deceased, color=ccaa), size= 1 ) +
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 week", 
               date_labels = "%d/%m",
               limits=c( filter_date - 50, max(spain$date)),
               expand = c(0,0)
  ) + 
  # scale_y_continuous(
  # limits = c( 0, max(spain$daily_deaths_avg7))
  # limits = c( 0, 50)
  # ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = paste0("Muertes por día (media ventana 7 días) por COVID-19 en España ", updated ),
       subtitle = paste0("(escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       color = "Comunidad autónoma",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/country/covid19_muertes-dia-ccaa.png", sep = ""),width = 900,height = 600)
spain_ccaa %>% filter( date > filter_date - 50) %>%
  ggplot() +
  geom_col(aes(date, daily_deaths, fill = "" ), size= 1, ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_deaths_avg7, group=ccaa, color=""), size= 1 ) +
  scale_color_manual(values=c("#565656")  )+
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%m",
               limits=c( filter_date - 50, max(spain$date)),
               expand = c(0,0)
  ) + 
  facet_wrap(~ccaa, scales = "free") +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "top"
  ) +
  labs(title = paste0("Fallecidos por día por COVID-19 en España ", updated ),
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       color = "Comunidad autónoma",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/country/covid19_muertes-dia-ccaa_not-free.png", sep = ""),width = 900,height = 600)
spain_ccaa %>% filter( date > filter_date - 50) %>%
  ggplot() +
  geom_col(aes(date, daily_deaths, fill = "" ), size= 1, ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_deaths_avg7, group=ccaa, color=""), size= 1 ) +
  scale_color_manual(values=c("#565656")  )+
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%m",
               limits=c( filter_date - 50, max(spain$date)),
               expand = c(0,0)
  ) + 
  facet_wrap(~ccaa) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "top"
  ) +
  labs(title = paste0("Fallecidos por día por COVID-19 en España ", updated ),
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       color = "Comunidad autónoma",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/country/covid19_casos-dia-ccaa.png", sep = ""),width = 900,height = 600)
spain_ccaa %>% filter( date > filter_date - 50) %>%
  ggplot() +
  geom_col(aes(date, daily_cases_PCR, fill = "" ), size= 1, ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_PCR_avg7, group=ccaa, color=""), size= 1 ) +
  scale_color_manual(values=c("#565656")  )+
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%m",
               limits=c( filter_date - 50, max(spain$date)),
               expand = c(0,0)
  ) + 
  facet_wrap(~ccaa, scales = "free") +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "top"
  ) +
  labs(title = paste0("Casos PCR+ por día COVID-19 en España ", updated ),
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       fill = "casos por día",
       colour = "media",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/country/covid19_casos-dia-ccaa_not-free.png", sep = ""),width = 900,height = 600)
spain_ccaa %>% filter( date > filter_date - 50) %>%
  ggplot() +
  geom_col(aes(date, daily_cases_PCR, fill = "" ), size= 1, ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_PCR_avg7, group=ccaa, color=""), size= 1 ) +
  scale_color_manual(values=c("#565656")  )+
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%m",
               limits=c( filter_date - 50, max(spain$date)),
               expand = c(0,0)
  ) + 
  facet_wrap(~ccaa) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "top"
  ) +
  labs(title = paste0("Casos PCR+ por día COVID-19 en España ", updated ),
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       fill = "casos por día",
       colour = "media",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/country/covid19_casos-acumulados-ccaa.png", sep = ""),width = 900,height = 600)
spain_ccaa %>% filter( date > filter_date - 50) %>%
  ggplot() +
  geom_col(aes(date, cases_accumulated_PCR), size= 1 ) +
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%m",
               limits=c( filter_date - 50, max(spain$date)),
               expand = c(0,0)
  ) + 
  facet_wrap(~ccaa, scales = "free") +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = paste0("Casos COVID-19 por PCR+ en España ", updated ),
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       color = "Comunidad autónoma",
       caption = caption_provincia)
dev.off()

# -------------------------------------------
png(filename=paste("img/spain/country/covid19_muertes-acumuladas-ccaa.png", sep = ""),width = 900,height = 600)
spain_ccaa %>% filter( date > filter_date - 50) %>%
  ggplot() +
  geom_line(aes(date, deceased), size= 1 ) +
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%m",
               limits=c( filter_date - 50, max(spain$date)),
               expand = c(0,0)
  ) + 
  facet_wrap(~ccaa, scales = "free") +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = paste0(" COVID-19 registrados en España ", updated ),
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       color = "Comunidad autónoma",
       caption = caption_provincia)
dev.off()

# to_agreggate %>%  filter (ccaa=="Castilla - La Mancha") %>%
#   ggplot() +
#   geom_line(aes(date, deceased), size= 1 ) +
#   # scale_color_manual(values = colors_prov) +
#   scale_x_date(date_breaks = "1 month", 
#                date_labels = "%m",
#                limits=c( min(spain$date)+7, max(spain$date + 46)),
#                expand = c(0,0)
#   ) + 
#   facet_wrap(~province, scales = "free") +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = c(0.1,0.6)
#   ) +
#   labs(title = paste0("Número de fallecimientos acumulados por COVID-19 registrados en España ", updated ),
#        subtitle = paste0("Por provincia (escala lineal). ",period),
#        y = "fallecidos",
#        x = "fecha",
#        color = "Comunidad autónoma",
#        caption = caption_provincia)

