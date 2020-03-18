# Analizar datos de Coronavirus COVID-19 en España por comunidad autónoma 

# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping
# library(RColorBrewer) # extends color paletter


# Load Data ---------
# / Population -------------
ccaa_poblacion <-  read.delim("data/original/spain/ccaa-poblacion.csv",sep = ";")

# / COVID-19 in Spain -----------
# Data by Ministerio de Sanidad de España (published in PDF format https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm)
# extracted by Datadista and published in this repository https://github.com/datadista/datasets/tree/master/COVID%2019
# Spanish data https://github.com/datadista/datasets/tree/master/COVID%2019
data_cases_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_casos.csv",sep = ",")  
# data_cases_original <- read.delim("../coronavirus-datadista/COVID 19/12-03-2020/casos_cccaa_12032020_covid19.csv",sep = ",") #loads local data 
data_uci_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_uci.csv",sep = ",")
data_death_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_fallecidos.csv",sep = ",")
data_altas_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_altas.csv",sep = ",")

# Process data ------
# Change to long format
# Casos registrados
data_cases <- melt(data_cases_original, id.vars = c("CCAA", "cod_ine")) 
data_cases$date <- as.Date(substr(data_cases$variable,2,12),"%d.%m.%Y")
data_cases <- select(data_cases,-variable)

# add population data
data_cases <- merge( data_cases, select(ccaa_poblacion,id,poblacion), by.x = "cod_ine", by.y = "id"   )
# calculate values per 
data_cases$per_cienmil <- round( data_cases$value / data_cases$poblacion * 100000, digits = 2)

write.csv(data_cases, file = "data/output/covid19-casos-registrados-por-ccaa-espana-por-dia-acumulado.csv", row.names = FALSE)

# Personas UCI registradas
data_uci <- melt(data_uci_original, id.vars = c("CCAA","cod_ine"))
data_uci$date <- as.Date(substr(data_uci$variable,2,12),"%d.%m.%Y")
data_uci <- select(data_uci,-variable)

# add population data
data_uci <- merge( data_uci, data_cases %>% filter (date == as.Date("2020-02-27") ) %>% select(CCAA,poblacion), by.x = "CCAA", by.y = "CCAA" , all.x = TRUE  )
# calculate values per 
data_uci$per_cienmil <- round( data_uci$value / data_uci$poblacion * 100000, digits = 2)

write.csv(data_uci, file = "data/output/covid19-ingresos-uci-por-ccaa-espana-por-dia-acumulado.csv", row.names = FALSE)

# Fallecimientos registrados
data_death <- melt(data_death_original, id.vars = c("CCAA","cod_ine"))
data_death$date <- as.Date(substr(data_death$variable,2,12),"%d.%m.%Y")
data_death <- select(data_death,-variable)

# add population data
data_death <- merge( data_death, data_cases %>% filter (date == as.Date("2020-02-27") ) %>% select(CCAA,poblacion), by.x = "CCAA", by.y = "CCAA" , all.x = TRUE  )
# calculate values per 
data_death$per_cienmil <- round( data_death$value / data_death$poblacion * 1000000, digits = 2)

write.csv(data_death, file = "data/output/covid19-fallecimientos-por-ccaa-espana-por-dia-acumulado.csv", row.names = FALSE)

# altas
data_altas <- melt(data_altas_original, id.vars = c("CCAA","cod_ine"))
data_altas$date <- as.Date(substr(data_altas$variable,2,12),"%d.%m.%Y")
data_altas <- select(data_altas,-variable)

# add population data
data_altas <- merge( data_altas, data_cases %>% filter (date == as.Date("2020-02-27") ) %>% select(CCAA,poblacion), by.x = "CCAA", by.y = "CCAA" , all.x = TRUE  )
# calculate values per 
data_altas$altas_per_cienmil <- round( data_altas$value / data_altas$poblacion * 1000000, digits = 2)

# / join data sets and export --------------
data_all <- data_cases
data_all$unique <- paste0(data_all$CCAA,data_all$date)
colnames(data_all)[3] <- "cases"
colnames(data_all)[6] <- "cases_per_cienmil"

data_uci$unique <- paste0(data_uci$CCAA,data_uci$date)
colnames(data_uci)[3] <- "uci"
colnames(data_uci)[6] <- "uci_per_cienmil"

data_all <- merge( data_all, select(data_uci,unique,uci,uci_per_cienmil ), by = "unique", all = TRUE  )

data_death$unique <- paste0(data_death$CCAA,data_death$date)
colnames(data_death)[3] <- "death"
colnames(data_death)[6] <- "death_per_cienmil"

data_all <- merge( data_all, select(data_death,unique,death,death_per_cienmil ), by = "unique", all = TRUE  )

data_altas$unique <- paste0(data_altas$CCAA,data_altas$date)
colnames(data_altas)[3] <- "altas"

data_all <- merge( data_all, select(data_altas,unique,altas,altas_per_cienmil ), by = "unique", all = TRUE  )

data_all_export <- select(data_all,  cod_ine , CCAA, cases, date ,poblacion, 
                          cases_per_cienmil, uci,uci_per_cienmil, death, death_per_cienmil, altas,altas_per_cienmil )

names(data_all_export) <- c("region_code" , "region",  "cases_registered" , "date"  ,"population"  , "cases_per_100000", "intensive_care",
                            "intensive_care_per_1000000", "deceassed", "deceassed_per_100000","recovered","recovered_per_100000")

data_all_export$country <- "Spain"

data_all_export <- data_all_export %>% select(date, region_code, region, country, population, cases_registered, cases_per_100000,
                                              intensive_care, intensive_care_per_1000000, deceassed, deceassed_per_100000,
                                              recovered, recovered_per_100000)

data_all_export <- data_all_export %>% filter(!is.na(region))

write.csv(data_all_export, file = "data/output/covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated.csv", row.names = FALSE)

# Create extra data ------

# length(unique(data_cases$date)))

test <- data.frame(qexp(1:100/100, rate = 22))
names(test) <- "value"
plot(test$value)
# test$date <- unique(data_cases$date)
plot(test$value)
test$value_d <- test$value * 100
plot(test$value_d)

# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption <- "Gráfico: lab.montera34.com/covid19 | Datos: Ministerio de Sanidad de España extraídos por Datadista.com"
caption_en <- "By: lab.montera34.com/covid19 | Data: various official sources. Check website."
caption_provincia <- "Gráfico: montera34.com | Datos: Varias fuentes. Ver lab.montera34.com"
period <- "2020.02.27 - 03.18"

# Plots --------------------
# 1. Cases ------------

# // 1.1 Small multiple ------------

# /// Comunidades autónomas small multiple --------------

# Escala lineal
png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-lineal.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
ggplot() +
  geom_line(aes(date,value,group=CCAA) ) +
  geom_point(aes(date,value,group=CCAA), size = 0.5 ) +
  facet_wrap( ~CCAA) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "2 day", 
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
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-log.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
ggplot() +
  geom_line(aes(date,value,group=CCAA) ) +
  geom_point(aes(date,value,group=CCAA), size = 0.5 ) +
  scale_y_log10(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "2 day", 
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
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,per_cienmil,group=CCAA) ) +
  geom_point(aes(date,per_cienmil,group=CCAA), size = 0.5 ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "2 day", 
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
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,per_cienmil,group=CCAA) ) +
  geom_point(aes(date,per_cienmil,group=CCAA), size = 0.5 ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 3000, 100) )) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "2 day", 
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
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# // 1.2 Superpuesto ---------------

# create growth ------------
# Contribution by @lorezmt

# create shorted dataframe
data_cases2 <- data_cases %>% filter(date >= "2020-03-09") # sets starting day

x <- seq_along(unique(data_cases2$date))
# creates empty vectors
y35 <- vector(length=length(x))
y32 <- vector(length=length(x))
y22 <- vector(length=length(x))
y10 <- vector(length=length(x))
y05 <- vector(length=length(x))
# fill firs value
y35[[1]] <- 469 # sets starting value
y32[[1]] <- 469
y22[[1]] <- 469
y10[[1]] <- 469
y05[[1]] <- 469
# create data series with certain % of growth
for (i in 2:length(x)) {
  y35[[i]] <- y35[[i-1]] + y35[[i-1]]*0.35 # grows 35%
  y32[[i]] <- y32[[i-1]] + y32[[i-1]]*0.32 # grows 35%
  y22[[i]] <- y22[[i-1]] + y22[[i-1]]*0.22 # grows 22%
  y10[[i]] <- y10[[i-1]] + y10[[i-1]]*0.1 # grows 10%
  y05[[i]] <- y10[[i-1]] + y10[[i-1]]*0.05 # grows 5%
}
# creates the data fame
data_unique <- arrange(data_cases2, date) %>% select(date) %>% unique()
crec <- data.frame(x = data_unique, y05 = y05, y10 = y10, y22 = y22, y32 = y32,y35 = y35)

# calculates growth
# data_cases <- data_cases %>% arrange(date)
# df <- data_cases %>%
#   # group_by(CCAA) %>%
#   filter (CCAA == "Madrid") %>%
#   # arrange(date) %>%
#   # mutate( growth = 100 *(value - lag(value))/lag(value))
#   mutate(Diff_date = date - lag(date),  # Difference in time (just in case there are gaps)
#          Diff_growth = value - lag(value), # Difference in route between years
#          Rate = round(Diff_growth / lag(value) * 100, digits = 1)
#          ) 
# 
# crec <- crec %>%
#   mutate(Diff_date = date - lag(date),  # Difference in time (just in case there are gaps)
#          Diff_growth = y10 - lag(y10), # Difference in route between years
#          Rate = round(Diff_growth / lag(y10) * 100, digits = 1)
#   ) 

# ------ CCAA  ----------
png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
data_cases %>%
  ggplot() +
  geom_line(aes(date,value,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,value, color=CCAA), size= 2 ) +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  CCAA != "Total"), 
                  aes(date,value, color=CCAA, label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
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
               limits=c( min(data_cases$date), max(data_cases$date + 2)) 
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
       y = "casos registrados",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-lineal_con-curva.png", sep = ""),width = 1000,height = 700)
data_cases %>%
  ggplot() +
  geom_line(aes(date,value,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,value, color=CCAA), size= 2 ) +
  geom_text(data = crec[1,],aes(as.Date("2020-03-8"),1500,label="hay un 32% más cada día"), 
            size = 8, base_family = "Roboto Condensed") +
  geom_line(data = crec, aes(x = date, y = y32), linetype = 2, size = 1, color ="#444444") +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  CCAA != "Total"), 
                  aes(date,value, color=CCAA, label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
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
               limits=c( min(data_cases$date), max(data_cases$date + 2)) 
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
       y = "casos registrados",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-log_with-curve.png", sep = ""),width = 1000,height = 700)
data_cases2 %>%
  ggplot(aes(x = unique(date))) +
  geom_line(aes(date,value,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,value,color=CCAA), size= 1.5 ) +
  geom_text(data = crec[1,],aes(as.Date("2020-03-13"),1000,label="hay un 32% más cada día"), 
            size = 5, base_family = "Roboto Condensed") +
  geom_line(data = crec, aes(y = y32), linetype = 2, size = 1, color ="#444444") +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date)), 
                  aes(date,value, color=CCAA, label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases2$date), max(data_cases2$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-log.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,value,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,value,color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  CCAA != "Total"), 
                  aes(date,value, color=CCAA, label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases$date), max(data_cases$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "casos registrados",
       x = "fecha",
       caption = caption)
dev.off()

# English ----
png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-log_en.png", sep = ""),width = 1000,height = 700)
data_cases %>%
  ggplot() +
  geom_line(aes(date,value,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,value,color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  CCAA != "Total"), 
                  aes(date,value, color=CCAA, label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases$date), max(data_cases$date + 1.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Confirmed accumulated COVID-19 registed cases in Spain",
       subtitle = paste0("By region (log scale). ",period),
       y = "registered cases (log scale)",
       x = "date",
       caption = caption)
dev.off()


# png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-log_comparativa.png", sep = ""),width = 1000,height = 700)
# data_cases %>% filter( CCAA != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=CCAA, color=CCAA), size= 1 ) +
#   geom_point(aes(date,value,color=CCAA), size= 1.5 ) +
#   geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  CCAA != "Total"), 
#                   aes(date,value, color=CCAA, label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#333333"
#   ) +
#   geom_abline( slope = -5) +
#   scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
#                  limits = c(0.95,12000),
#                  breaks = c(1,10,100,1000,12000),
#                  minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) )
#                   ) +
#   scale_x_date(date_breaks = "1 day", 
#                date_labels = "%d",
#                limits=c( min(data_i_cases$date), max(data_cases$date + 1.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados en España",
#        subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
#        y = "casos registrados",
#        x = "fecha",
#        caption = caption)
# dev.off()

# Por 100.000 --------
png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,per_cienmil,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,per_cienmil, color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  CCAA != "Total"), 
                  aes(date,per_cienmil, color=CCAA, label=paste(format(per_cienmil, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases$date), max(data_cases$date + 2.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,per_cienmil,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,per_cienmil, color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  CCAA != "Total"), 
                  aes(date,per_cienmil, color=CCAA, label=paste(format(per_cienmil, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
                  # xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2020-01-4"))
  ) +
  # scale_color_brewer(palette = "Dark2", type = "discrete") +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 limits = c(1,max(data_cases$per_cienmil)),
                 minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases$date), max(data_cases$date + 3.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "casos registrados por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# English ----------
png(filename=paste("img/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-per-cienmil-log_en.png", sep = ""),width = 1000,height = 700)
data_cases %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,per_cienmil,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,per_cienmil, color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  CCAA != "Total"), 
                  aes(date,per_cienmil, color=CCAA, label=paste(format(per_cienmil, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
                  # xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2020-01-4"))
  ) +
  # scale_color_brewer(palette = "Dark2", type = "discrete") +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 limits = c(1,max(data_cases$per_cienmil)),
                 minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases$date), max(data_cases$date + 3.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Confirmed accumulated COVID-19 registed cases in Spain by 100.000 inhabitants",
       subtitle = paste0("By region (log scale). ",period),
       y = "registered cases  by 100.000 inhabitants (log scale)",
       x = "fecha",
       caption = caption)
dev.off()

# 2. UCI (intensive care) -------------------

# // 2.1 UCI Small multiple ----------
# Escala lineal
png(filename=paste("img/covid19_casos-registrados-UCI-por-comunidad-autonoma-lineal.png", sep = ""),width = 1000,height = 700)
data_uci %>%
  ggplot() +
  geom_line(aes(date,uci,group=CCAA) ) +
  geom_point(aes(date,uci),size = 0.5 ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "2 day", 
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
  scale_x_date(date_breaks = "2 day", 
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
  scale_x_date(date_breaks = "2 day", 
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
  scale_x_date(date_breaks = "2 day", 
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
               limits=c( min(data_uci$date), max(data_uci$date + 2.5)) 
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
               limits=c( min(data_uci$date), max(data_uci$date + 2.5)) 
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
               limits=c( min(data_uci$date), max(data_uci$date + 2.5)) 
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

# # 3. Deceassed (Fallecimientos) ------------

# // 3.1 Fallecimientos Small multiple ----------
# Escala lineal
png(filename=paste("img/covid19_fallecimientos-registrados-por-comunidad-autonoma-lineal.png", sep = ""),width = 1000,height = 700)
data_death %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,death,group=CCAA) ) +
  geom_point(aes(date,death), size=0.5 ) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "2 day",
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
  geom_line(aes(date,death,group=CCAA) ) +
  geom_point(aes(date,death), size=0.5 ) +
  scale_y_log10( minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) )) +
  facet_wrap( ~CCAA) +
  scale_x_date(date_breaks = "2 day",
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
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()

# // 3.2 Fallecimientos superpuestos ----------
# // CCAA -------------------
png(filename=paste("img/covid19_fallecimientos-registrados-por-comunidad-autonoma-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
data_death %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,death,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,death, color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_death, date==max(data_death$date),  CCAA != "Total"),
                  aes(date,death, color=CCAA, label=paste(format(death, nsmall=1, big.mark="."),CCAA)),
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
               limits=c( min(data_death$date), max(data_death$date + 1.5))
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_fallecimientos-registrados-por-comunidad-autonoma-superpuesto-log.png", sep = ""),width = 1000,height = 700)
data_death %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,death,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,death, color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_death, date==max(data_death$date),  CCAA != "Total"),
                  aes(date,death, color=CCAA, label=paste(format(death, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100)) ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( min(data_death$date), max(data_death$date + 1.5))
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_fallecimientos-registrados-por-comunidad-autonoma-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1000,height = 700)
data_death %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,death_per_cienmil,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,death_per_cienmil, color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_death, date==max(data_death$date),  CCAA != "Total"),
                  aes(date,death_per_cienmil, color=CCAA, label=paste(format(death_per_cienmil, nsmall=1, big.mark="."),CCAA)),
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
               limits=c( min(data_death$date), max(data_death$date + 1.5))
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/covid19_fallecimientos-registrados-por-comunidad-autonoma-superpuesto-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
data_death %>% filter( CCAA != "Total") %>%
  ggplot() +
  geom_line(aes(date,death_per_cienmil,group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(date,death_per_cienmil, color=CCAA), size= 1.5 ) +
  geom_text_repel(data=filter( data_death, date==max(data_death$date),  CCAA != "Total"),
                  aes(date,death_per_cienmil, color=CCAA, label=paste(format(death_per_cienmil, nsmall=1, big.mark="."),CCAA)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10(  minor_breaks =  c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( min(data_death$date), max(data_death$date + 1.5))
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# --------- Relaciones --------
# png(filename=paste("img/covid19_ .png", sep = ""),width = 1000,height = 700)
data_all %>% filter( CCAA != "Total") %>%
ggplot() +
  geom_line( aes(cases_per_cienmil,death_per_cienmil, group=CCAA, color=CCAA), size= 1 ) +
  geom_point(aes(cases_per_cienmil,death_per_cienmil, color=CCAA), size= 2 ) +
  # lines(x = c(0,0), y = c(20,1000)) +
  geom_abline(slope = 0.25) +
  # Annotations
  geom_text(aes(cases_per_cienmil,death_per_cienmil+0.5, color=CCAA,label=paste( substr(date,7,10 ))), size= 3, color="#000000") +
  geom_text_repel(data=filter( data_all, date==max(data_all[!is.na(data_all$date),]$date),  CCAA != "Total"),
                  aes(cases_per_cienmil,death_per_cienmil, color=CCAA, label=CCAA),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # scale_x_date(date_breaks = "1 day", 
  #              date_labels = "%d",
  #              limits=c( min(data_cases$date), max(data_cases$date + 1.5)) 
  # ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Fallecimientos y casos acumulados COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma",period),
       y = "fallecimientos por 100.000 habitantes",
       x = "casos acumulados por 100.000 habitantes",
       caption = caption)
dev.off()
