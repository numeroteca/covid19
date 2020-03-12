# Analizar datos de Coronavirus COVID-19 en España por Comunidad Autónoma 

# Load world data --------
# World data https://github.com/RamiKrispin/coronavirus-csv/blob/master/coronavirus_dataset.csv
# world_data  <- read.delim("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv",sep = ",")

# COVID19 in Spain -----------
# Data by Ministerio de Sanidad de España (published in PDF format https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm)
# extracted by Datadista and published in this repository https://github.com/datadista/datasets/tree/master/COVID%2019
# Spanish data https://github.com/datadista/datasets/tree/master/COVID%2019
data_cases_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/12-03-2020/casos_cccaa_12032020_covid19.csv",sep = ",")
data_death_original <- read.delim("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/12-03-2020/fallecidos_cccaa_12032020_covid19.csv",sep = ",")
data_uci_original <- read.delim("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/12-03-2020/uci_cccaa_12032020_covid19.csv",sep = ",")

# Process data ------
# Change to long format
# Casos registrados
data_cases <- melt(data_cases_original, id.vars = c("CCAA", "cod_ine")) 
data_cases$date <- as.Date(substr(data_cases$variable,2,12),"%d.%m.%Y")
data_cases <- select(data_cases,-variable)

# Personas UCI registradas
data_uci <- melt(data_uci_original, id.vars = c("CCAA"))
data_uci$date <- as.Date(substr(data_uci$variable,2,12),"%d.%m.%Y")
data_uci <- select(data_uci,-variable)

# Fallecimientos registrados
data_death <- melt(data_death_original, id.vars = c("CCAA"))
data_death$date <- as.Date(substr(data_death$variable,2,12),"%d.%m.%Y")
data_death <- select(data_death,-variable)

# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption <- "Gráfico: Numeroteca. Datos: Ministerio de Sanidad de España extraídos por Datadista.com"

# Cases ------------

# Escala lineal
png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-lineal.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
ggplot() +
  geom_step(aes(date,value,group=CCAA) ) +
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
  labs(title = "Número de casos de COVID19 registrados en España",
       subtitle = "Por Comunidad Autónoma (escala lineal)",
       y = "casos",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-log.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
ggplot() +
  geom_step(aes(date,value,group=CCAA) ) +
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
  labs(title = "Número de casos de COVID19 registrados en España",
       subtitle = "Por Comunidad Autónoma (escala logarítmica)",
       y = "casos",
       x = "fecha",
       caption = caption)
dev.off()

# UCI (intensive care) -------------------

# Escala lineal
png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-lineal.png", sep = ""),width = 1000,height = 700)
data_uci %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_step(aes(date,value,group=CCAA) ) +
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
  labs(title = "Número de personas en la UCI por COVID19 registrados en España",
       subtitle = "Por Comunidad Autónoma (escala lineal)",
       y = "personas en UCI",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-log.png", sep = ""),width = 1000,height = 700)
data_uci %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_step(aes(date,value,group=CCAA) ) +
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
  labs(title = "Número de personas en la UCI por COVID19 registrados en España",
       subtitle = "Por Comunidad Autónoma (escala logarítmica)",
       y = "personas en UCI",
       x = "fecha",
       caption = caption)
dev.off()

# Deaths ------------

# Escala lineal
png(filename=paste("img/covid19_fallecimientos-registrados-por-comunidad-autonoma-lineal.png", sep = ""),width = 1000,height = 700)
data_death %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_step(aes(date,value,group=CCAA) ) +
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
  labs(title = "Número de fallecimientos de COVID19 registrados en España",
       subtitle = "Por Comunidad Autónoma (escala lineal)",
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/covid19_fallecimientos-registrados-por-comunidad-autonoma-log.png", sep = ""),width = 1000,height = 700)
data_death %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_step(aes(date,value,group=CCAA) ) +
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
  labs(title = "Número de fallecimientos de COVID19 registrados en España",
       subtitle = "Por Comunidad Autónoma (escala logarítmica)",
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()
