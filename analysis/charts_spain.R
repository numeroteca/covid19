# Genera gráficos de Coronavirus COVID-19 en España agregados a partir de datos de provincias de CCAA
# Los gráficos generados pueden verse en https://lab.montea34.com/covid9

# Load libraries 
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping

# Cambia el pie del gráfico pero conserva la fuente de los datos
caption_en <- "By: lab.montera34.com/covid19 | Data: EsCOVID19data. Check code.montera34.com/covid19"
caption_provincia <- "Gráfico: @numeroteca (lab.montera34.com/covid19) | Datos: esCOVID19data (github.com/montera34/escovid19data)"
updated <- ""
period <- "(Actualizado: 2020-08-21)"
filter_date <- as.Date("2020-08-17")

# Warning: you need to have loaded spain by executing process_spain_provinces_data.R 
# or load it using:
spain <- readRDS(file = "data/output/spain/covid19-spain_consolidated.rds")
spain_ccaa <- readRDS(file ="data/output/spain/covid19-ccaa-spain_consolidated.rds")

# create temp dataframes to be able to plot all the values in small multiples
spain <- spain %>% filter( date < filter_date)

png(filename=paste("img/spain/country/covid19_fallecimientos-lineal.png", sep = ""),width = 900,height = 600)
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
  scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "2 week", 
               date_labels = "%d/%m",
               limits=c( min(spain$date)+7, max(spain$date + 46)),
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
  labs(title = paste0("Número de fallecimientos acumulados por COVID-19 registrados en España ", updated ),
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       color = "Comunidad autónoma",
       caption = caption_provincia)
dev.off()

png(filename=paste("img/spain/country/covid19_muertes-dia-lineal.png", sep = ""),width = 900,height = 600)
spain %>% filter( date > filter_date - 80) %>%
  ggplot() +
  geom_line(aes(date, daily_deaths), size= 1 ) +
  # geom_point(aes(date, deceased, color=ccaa), size= 1 ) +
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "2 week", 
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

png(filename=paste("img/spain/country/covid19_muertes-dia-media-lineal.png", sep = ""),width = 900,height = 600)
spain %>% filter( date > filter_date - 80) %>%
  ggplot() +
  geom_line(aes(date, daily_deaths_avg7), size= 1 ) +
  # geom_point(aes(date, deceased, color=ccaa), size= 1 ) +
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "2 week", 
               date_labels = "%d/%m",
               limits=c( filter_date - 50, max(spain$date)),
               expand = c(0,0)
  ) + 
  scale_y_continuous(
    limits = c( 0, max(spain$daily_deaths_avg7))
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
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

to_agreggate <- data_cases_sp_provinces %>% group_by(province) %>% arrange(date) %>% mutate (
  # deceased = ifelse( is.na(deceased), lag(deceased, 1), deceased),
  deceased = ifelse( is.na(deceased), lag(deceased, 1), deceased),
  deceased = ifelse( is.na(deceased), lag(deceased, 1), deceased),
  deceased = ifelse( is.na(deceased), lag(deceased, 1), deceased),
  deceased = ifelse( is.na(deceased), lag(deceased, 1), deceased),
  deceased = ifelse( is.na(deceased), lag(deceased, 1), deceased),
  deceased = ifelse( is.na(deceased), lag(deceased, 1), deceased),
  deceased = ifelse( is.na(deceased), lag(deceased, 1), deceased)
  # deceased_lag = lag(deceased, 1)
) # %>% select(date, province, deceased, deceased_lag)


spain_ccaa %>%
  ggplot() +
  geom_line(aes(date, deceased), size= 1 ) +
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%m",
               limits=c( min(spain$date)+7, max(spain$date + 46)),
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
  labs(title = paste0("Número de fallecimientos acumulados por COVID-19 registrados en España ", updated ),
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       color = "Comunidad autónoma",
       caption = caption_provincia)


to_agreggate %>%  filter (ccaa=="Castilla - La Mancha") %>%
  ggplot() +
  geom_line(aes(date, deceased), size= 1 ) +
  # scale_color_manual(values = colors_prov) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%m",
               limits=c( min(spain$date)+7, max(spain$date + 46)),
               expand = c(0,0)
  ) + 
  facet_wrap(~province, scales = "free") +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = c(0.1,0.6)
  ) +
  labs(title = paste0("Número de fallecimientos acumulados por COVID-19 registrados en España ", updated ),
       subtitle = paste0("Por provincia (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       color = "Comunidad autónoma",
       caption = caption_provincia)
