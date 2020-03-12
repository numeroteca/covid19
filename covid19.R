# Analizar datos de Coronavirus COVID-19 en España

# Load data --------
# World data https://github.com/RamiKrispin/coronavirus-csv/blob/master/coronavirus_dataset.csv
world_data  <- read.delim("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv",sep = ",")

# Spanish data https://github.com/datadista/datasets/blob/master/COVID%2019/casos_cccaa_11032020_covid19.csv 
# raw data https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/casos_cccaa_11032020_covid19.csv

# COVID19 in Spain -----------
# Data by Ministerio de Sanidad de España (published in PDF format https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm)
# extracted by Datadista and published in this repository https://github.com/datadista/datasets/tree/master/COVID%2019

# Cases ------------
spanish_data <- read.delim("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/casos_cccaa_11032020_covid19.csv",sep = ",")

# Change to long format
data <- melt(data_original, id.vars = c("CCAA", "cod_ine"))
data$date <- as.Date(substr(data$variable,2,12),"%d.%m.%Y")

# Escala lineal
data %>% filter( CCAA != "Total") %>%
ggplot() +
  geom_step(aes(date,value,group=CCAA) ) +
  facet_wrap( ~CCAA)

# Escala logarítmica
data %>% filter( CCAA != "Total") %>%
ggplot() +
  geom_step(aes(date,value,group=CCAA) ) +
  scale_y_log10( ) +
  facet_wrap( ~CCAA)

# Deaths ------------
spanish_data <- read.delim("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/fallecidos_cccaa_11032020_covid19.csv",sep = ",")

# Change to long format
data_death <- melt(data_original, id.vars = c("CCAA", "cod_ine"))
data_death$date <- as.Date(substr(data$variable,2,12),"%d.%m.%Y")

# Escala lineal
data_death %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_step(aes(date,value,group=CCAA) ) +
  facet_wrap( ~CCAA)

# Escala logarítmica
data_death %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_step(aes(date,value,group=CCAA) ) +
  scale_y_log10( ) +
  facet_wrap( ~CCAA)
