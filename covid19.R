# Analizar datos de Coronavirus COVID-19 en España

# Load data --------
# World data https://github.com/RamiKrispin/coronavirus-csv/blob/master/coronavirus_dataset.csv
world_data  <- read.delim("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv",sep = ",")

# Spanish data https://github.com/datadista/datasets/blob/master/COVID%2019/casos_cccaa_11032020_covid19.csv 
# raw data https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/casos_cccaa_11032020_covid19.csv

# Settings
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption <- "Gráfico: Numeroteca. Datos: Ministerio de Sanidad de España extraídos por Datadista.com"

# COVID19 in Spain -----------
# Data by Ministerio de Sanidad de España (published in PDF format https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm)
# extracted by Datadista and published in this repository https://github.com/datadista/datasets/tree/master/COVID%2019

# Cases ------------
spanish_data <- read.delim("https://github.com/datadista/datasets/blob/master/COVID%2019/11-03-2020/casos_cccaa_11032020_covid19.csv",sep = ",")

# Change to long format
data <- melt(data_original, id.vars = c("CCAA", "cod_ine")) 
data$date <- as.Date(substr(data$variable,2,12),"%d.%m.%Y")
data <- select(data,-variable)

# Escala lineal
data %>% filter( CCAA != "Total") %>%
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

# Escala logarítmica
data %>% filter( CCAA != "Total") %>%
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
  labs(title = "Número de casos de COVID19  registrados en España",
       subtitle = "Por Comunidad Autónoma (escala logarítmica)",
       y = "casos",
       x = "fecha",
       caption = caption)

# UCI (intensive care) -------------------

uci_spanish_data <- read.delim("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/11-03-2020/uci_cccaa_11032020_covid19.csv",sep = ",")

# Change to long format
data_uci <- melt(data_original, id.vars = c("CCAA", "cod_ine"))
data_uci$date <- as.Date(substr(data_uci$variable,2,12),"%d.%m.%Y")
data_uci <- select(data_uci,-variable)


# Escala lineal
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
  labs(title = "Número de personas en la UCI por COVID19 registrados en España",
       subtitle = "Por Comunidad Autónoma (escala lineal)",
       y = "personas en UCI",
       x = "fecha",
       caption = caption)

# Escala logarítmica
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
  labs(title = "Número de personas en la UCI por COVID19 registrados en España",
       subtitle = "Por Comunidad Autónoma (escala logarítmica)",
       y = "personas en UCI",
       x = "fecha",
       caption = caption)

# Deaths ------------
spanish_data <- read.delim("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/11-03-2020/fallecidos_cccaa_11032020_covid19.csv",sep = ",")

# Change to long format
data_death <- melt(data_original, id.vars = c("CCAA", "cod_ine"))
data_death$date <- as.Date(substr(data_death$variable,2,12),"%d.%m.%Y")
data_death <- select(data_death,-variable)


# Escala lineal
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
  labs(title = "Número de fallcimientos de COVID19 registrados en España",
       subtitle = "Por Comunidad Autónoma (escala lineal)",
       y = "fallecidos",
       x = "fecha",
       caption = caption)

# Escala logarítmica
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
  labs(title = "Número de fallcimientos de COVID19registrados en España",
       subtitle = "Por Comunidad Autónoma (escala logarítmica)",
       y = "fallecidos",
       x = "fecha",
       caption = caption)
