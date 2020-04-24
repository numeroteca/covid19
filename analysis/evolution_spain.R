# Analizar datos de Coronavirus COVID-19 en España por comunidad autónoma 

# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping
# library(RColorBrewer) # extends color paletter

# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption <- "Gráfico: @numeroteca (Montera34). Web: lab.montera34.com/covid19 | Datos: Ministerio de Sanidad de España extraídos por Datadista.com"
caption_en <- "By: Montera34. lab.montera34.com/covid19 | Data: various official sources. Check website."
caption_provincia <- "Gráfico: @numeroteca (montera34.com) | Datos: Varias fuentes. Ver lab.montera34.com"
period <- "2020.02.27 - 04.23 (Actualizado: 2020.04.24)"
# warning <- " Nota: no se incluye Cataluña desde 2020.04.16"
warning <- ""

# Load Data ---------
# / Population -------------  
ccaa_poblacion <-  read.delim("data/original/spain/ccaa-poblacion.csv",sep = ";")

# / COVID-19 data  in Spain -----------
# Data by Ministerio de Sanidad de España (published in PDF format https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm)
# extracted by Datadista and published in this repository https://github.com/datadista/datasets/tree/master/COVID%2019
# Spanish data https://github.com/datadista/datasets/tree/master/COVID%2019
data_cases_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_casos.csv",sep = ",")  
data_uci_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_uci.csv",sep = ",")
data_death_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_fallecidos.csv",sep = ",")
data_altas_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_altas.csv",sep = ",")
data_hosp_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_hospitalizados.csv",sep = ",")

# Process Datadista data ------
# Change to long format
# Casos registrados
data_cases <- melt(data_cases_original, id.vars = c("CCAA", "cod_ine")) 
data_cases$date <- as.Date(substr(data_cases$variable,2,12),"%Y.%m.%d")
data_cases <- select(data_cases,-variable)

# add population data
data_cases <- merge( data_cases, select(ccaa_poblacion,id,poblacion), by.x = "cod_ine", by.y = "id"   )
# calculate values per 
data_cases$per_cienmil <- round( data_cases$value / data_cases$poblacion * 100000, digits = 2)

write.csv(data_cases, file = "data/output/covid19-casos-registrados-por-ccaa-espana-por-dia-acumulado.csv", row.names = FALSE)

# Personas UCI registradas
data_uci <- melt(data_uci_original, id.vars = c("CCAA","cod_ine"))
data_uci$date <- as.Date(substr(data_uci$variable,2,12),"%Y.%m.%d")
data_uci <- select(data_uci,-variable)

# add population data
data_uci <- merge( data_uci, data_cases %>% filter (date == as.Date("2020-02-27") ) %>% select(CCAA,poblacion), by.x = "CCAA", by.y = "CCAA" , all.x = TRUE  )
# calculate values per 
data_uci$per_cienmil <- round( data_uci$value / data_uci$poblacion * 100000, digits = 2)

write.csv(data_uci, file = "data/output/covid19-ingresos-uci-por-ccaa-espana-por-dia-acumulado.csv", row.names = FALSE)

# Fallecimientos registrados
data_death <- melt(data_death_original, id.vars = c("CCAA","cod_ine"))
data_death$date <- as.Date(substr(data_death$variable,2,12),"%Y.%m.%d")
data_death <- select(data_death,-variable)

# add population data
data_death <- merge( data_death, data_cases %>% filter (date == as.Date("2020-02-27") ) %>% select(CCAA,poblacion), by.x = "CCAA", by.y = "CCAA" , all.x = TRUE  )
# calculate values per 
data_death$per_cienmil <- round( data_death$value / data_death$poblacion * 100000, digits = 2)

# Calculates muertes por día
data_death <- data_death %>% group_by(CCAA) %>% 
  arrange(date) %>% mutate( daily_deaths = value - lag(value),
                            daily_deaths_inc = round((value - lag(value)) /lag(value) * 100, digits = 1),
                            daily_deaths_avg6 =  round( ( daily_deaths + lag(daily_deaths,1)+lag(daily_deaths,2)+lag(daily_deaths,3)+lag(daily_deaths,4)+lag(daily_deaths,5) ) / 6, digits = 1 ) # average of dayly deaths of 6 last days
  )

write.csv(data_death, file = "data/output/covid19-fallecimientos-por-ccaa-espana-por-dia-acumulado.csv", row.names = FALSE)

# altas 
data_altas <- melt(data_altas_original, id.vars = c("CCAA","cod_ine"))
data_altas$date <- as.Date(substr(data_altas$variable,2,12),"%Y.%m.%d")
data_altas <- select(data_altas,-variable)

data_altas<- data_altas %>% group_by(CCAA) %>% 
  arrange(date) %>% mutate( daily_altas = value - lag(value),
                            daily_altas_inc = round((value - lag(value)) /lag(value) * 100, digits = 1),
                            daily_altas_avg6 =  round( ( daily_altas + lag(daily_altas,1)+lag(daily_altas,2)+lag(daily_altas,3)+lag(daily_altas,4)+lag(daily_altas,5) ) / 6, digits = 1 ) # average of dayly altas of 6 last days
  )


# add population data
data_altas <- merge( data_altas, data_cases %>% filter (date == as.Date("2020-02-27") ) %>% select(CCAA,poblacion), by.x = "CCAA", by.y = "CCAA" , all.x = TRUE  )
# calculate values per 
data_altas$altas_per_cienmil <- round( data_altas$value / data_altas$poblacion * 100000, digits = 2)

#  hospitalizados
data_hosp <- melt(data_hosp_original, id.vars = c("CCAA","cod_ine"))
data_hosp$date <- as.Date(substr(data_hosp$variable,2,12),"%Y.%m.%d")
data_hosp <- select(data_hosp,-variable)

# add population data
data_hosp <- merge( data_hosp, data_cases %>% filter (date == as.Date("2020-02-27") ) %>% select(CCAA,poblacion), by.x = "CCAA", by.y = "CCAA" , all.x = TRUE  )
# calculate values per 
data_hosp$hosp_per_cienmil <- round( data_hosp$value / data_hosp$poblacion * 1000000, digits = 2)



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

data_all <- merge( data_all, data_death %>% ungroup() %>% select(unique,death,death_per_cienmil ), by = "unique", all = TRUE  )

data_altas$unique <- paste0(data_altas$CCAA,data_altas$date)
colnames(data_altas)[3] <- "altas"

data_all <- merge( data_all, select(data_altas,unique,altas,altas_per_cienmil ), by = "unique", all = TRUE  )

data_hosp$unique <- paste0(data_hosp$CCAA,data_hosp$date)
colnames(data_hosp)[3] <- "hospitalizados"

data_all <- merge( data_all, select(data_hosp,unique,hospitalizados,hosp_per_cienmil ), by = "unique", all = TRUE  )

# TODO: fix variables
data_all_export <- select(data_all,  cod_ine , CCAA, cases, date ,poblacion, 
                          cases_per_cienmil, uci,uci_per_cienmil, death, death_per_cienmil, 
                          altas,altas_per_cienmil, hospitalizados, hosp_per_cienmil )

names(data_all_export) <- c("region_code" , "region",  "cases_registered" , "date"  ,"population"  , "cases_per_100000", "intensive_care",
                            "intensive_care_per_100000", "deceassed", "deceassed_per_100000","recovered","recovered_per_100000",
                            "hospitalized","hospitalized_per_100000")

data_all_export$country <- "Spain"

data_all_export <- data_all_export %>% select(date, region_code, region, country, population, cases_registered, cases_per_100000,
                                              intensive_care, intensive_care_per_100000, deceassed, deceassed_per_100000,
                                              recovered, recovered_per_100000,hospitalized, hospitalized_per_100000)

data_all_export <- data_all_export %>% filter(!is.na(region))


# Use Instituto de Salud Carlos III data instead ------------
# import Instituto de Salud CIII 
ciii_original <- read.delim("https://covid19.isciii.es/resources/serie_historica_acumulados.csv",sep = ",")  
write.csv(ciii_original, file = "data/original/spain/iscii_data.csv", row.names = FALSE)

ciii <- ciii_original %>% head(nrow(ciii_original) - 5) %>% ungroup() #Cambia el número en función de las notas que incluya el csv original
ciii$date <- as.Date(ciii$FECHA, "%d/%m/%Y" )
names(ciii) <- c("region","fecha","cases_registered","hospitalized","intensive_care","deceassed","recovered","date")
ciii$region <- factor(ciii$region)
# translate iniciales
levels(ciii$region)
# rename comunidades autónomas
#                          "AN"         "AR"    "AS"       "CB"        "CE"     "CL"               "CM"                 "CN"          "CT"        "EX"           "GA"        "IB"        "MC"          "MD"      "ML"        "NC"                   "PV"       "RI"           "VC"
# levels(ciii$region) <- c("Andalucía","Aragón", "Asturias", "Cantabria","Ceuta", "Castilla y León","Castilla-La Mancha", "Canarias","Cataluña" , "Extremadura", "Galicia", "Baleares",   "Murcia","Madrid", "Melilla", "Navarra",  "País Vasco","La Rioja","C. Valenciana")  
levels(ciii$region) <- c("Andalucía","Aragón", "Asturias", "Cantabria","Ceuta", "Castilla y León","Castilla-La Mancha", "Canarias","Cataluña" , "Extremadura", "Galicia", "Baleares",   "Murcia","Madrid", "Melilla", "Navarra",  "País Vasco","La Rioja","C. Valenciana")  

# rename población by ccaa data
ccaa_poblacion$ccaa <- c("Andalucía","Aragón", "Asturias", "Baleares", "Canarias",
                         "Cantabria","Castilla y León","Castilla-La Mancha","Cataluña","C. Valenciana",
                         "Extremadura", "Galicia",   "Madrid", "Murcia","Navarra","País Vasco","La Rioja",
                         "Ceuta", "Melilla"  )  
# Reorder order of regions
ciii$region <- factor(ciii$region, levels = c("Andalucía","Aragón", "Asturias", "Baleares", "Canarias",
                                              "Cantabria","Castilla y León","Castilla-La Mancha","Cataluña","C. Valenciana",
                                              "Extremadura", "Galicia",   "Madrid", "Murcia","Navarra","País Vasco","La Rioja",
                                              "Ceuta", "Melilla"  )  
                      )

# add population data
ciii <- merge( ciii, ccaa_poblacion %>% select(id,ccaa,poblacion), by.x = "region", by.y = "ccaa" , all.x = TRUE  ) 
# calculate values per 
ciii <- ciii %>% mutate(
  cases_per_100000 = round( cases_registered / poblacion * 100000, digits = 2),
  deceassed_per_100000 = round( deceassed / poblacion * 100000, digits = 2),
  recovered_per_100000 = round( recovered / poblacion * 100000, digits = 2),
  intensive_care_per_100000 = round( intensive_care / poblacion * 100000, digits = 2),
  hospitalized_per_100000 = round( hospitalized / poblacion * 100000, digits = 2),
)

ciii$country <- "Spain"
head(ciii)
head(as.data.frame(data_all_export))

ciii <- ciii %>% rename( region_code = id, population = poblacion) %>%
  arrange(region, date) %>% select(date, region_code, region, country, population, cases_registered, cases_per_100000,
                                              intensive_care, intensive_care_per_100000, deceassed, deceassed_per_100000,
                                              recovered, recovered_per_100000,hospitalized, hospitalized_per_100000) 

# Export data ---

write.csv(data_all_export, file = "data/output/covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated.csv", row.names = FALSE)
write.csv(ciii, file = "data/output/covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated_isciii.csv", row.names = FALSE)

  
# Use this to switch to ISCIII data -----
data_all_export <-ciii
caption <- "Gráfico: @numeroteca (Montera34). Web: lab.montera34.com/covid19 | Datos: Instituto de Salud CIII (covid19.isciii.es)"
caption_en <- "By: Montera34. lab.montera34.com/covid19 | Data: Instituto de Salud CIII (covid19.isciii.es)"


# Create new variables per day----
data_all_export <- data_all_export %>% group_by(region) %>%
    arrange(date) %>% mutate(
                            daily_cases = cases_registered - lag(cases_registered),
                            daily_cases_avg =  round( ( daily_cases + lag(daily_cases,1) + lag(daily_cases,2) + lag(daily_cases,3) + lag(daily_cases,4) + lag(daily_cases,5) ) / 6, digits = 1 ), # average of dayly deaths of 6 last days
                            daily_deaths = deceassed - lag(deceassed),
                            daily_deaths_inc = round((deceassed - lag(deceassed)) / lag(deceassed) * 100, digits = 1),
                            # lag2 = lag(deceassed,2),
                            daily_deaths_avg6 =  round( ( daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) + lag(daily_deaths,4) + lag(daily_deaths,5) ) / 6, digits = 1 ), # average of dayly deaths of 6 last days
                            daily_recovered = recovered - lag(recovered),
                            daily_recovered_inc = round((recovered - lag(recovered)) /lag(recovered) * 100, digits = 1),
                            daily_recovered_avg6 =  round( ( daily_recovered + lag(daily_recovered,1)+lag(daily_recovered,2)+
                                                               lag(daily_recovered,3)+lag(daily_recovered,4)+lag(daily_recovered,5) ) / 6, digits = 1 ), # average of dayly recovered of 6 last days
                            deaths_cum_last_week = ( deceassed + lag(deceassed,1) + lag(deceassed,2) + lag(deceassed,3) + lag(deceassed,4) + lag(deceassed,5) + lag(deceassed,6) ) / 7,  
                            deaths_last_week =  daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) + lag(daily_deaths,4) + lag(daily_deaths,5) + lag(daily_deaths,6)  
                            
                             )

# create df for small multiples -----
data_all_export_sm <- data_all_export %>% ungroup
data_all_export_sm$region_cp <-data_all_export_sm$region


# Export uniprovincial data regions------
export_uniprovinciales <- data_all_export %>% select(date,region,hospitalized,intensive_care,deceassed,cases_registered, recovered) %>%
  filter( region == "Melilla" | region == "Asturias" | region == "Baleares" | region == "Cantabria" |
            region == "Ceuta" |region == "Madride" | region == "Murcia" | region == "Navarra" | region == "Madrid" |
            region == "La Rioja" )

write.csv(export_uniprovinciales, file = "data/output/spain/covid19-data-uniprovinciales.csv", row.names = FALSE)

export_uniprovinciales_ciii <- ciii %>% select(date,region,hospitalized,intensive_care,deceassed,cases_registered, recovered) %>%
  filter( region == "Melilla" | region == "Asturias" | region == "Baleares" | region == "Cantabria" |
            region == "Ceuta" |region == "Madride" | region == "Murcia" | region == "Navarra" | region == "Madrid" |
            region == "La Rioja" )

write.csv(export_uniprovinciales_ciii, file = "data/output/spain/covid19-data-uniprovinciales_isciii.csv", row.names = FALSE)

# colors ---------
# extends color paletter
library(RColorBrewer)
# creates extended color palette https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
colourCount <- length(unique(data_cases$CCAA))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
colors <- getPalette(colourCount )
# Change yellow to blue
colors[12] <- "#84d3e7"


# Plots --------------------
# 1. Cases ------------

# // 1.1 Small multiple ------------

  
# Escala lineal
png(filename=paste("img/spain/regions/covid19_casos-registrados-por-comunidad-autonoma-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
ggplot() +
  geom_line(data = select(data_all_export_sm,date, cases_registered, region_cp,-region),
            aes(date, cases_registered, group=region_cp), color="#CACACA" ) +
  geom_line(aes(date, cases_registered, group=region) ) +
  geom_point(aes(date, cases_registered, group=region), size = 0.5 ) +
  facet_wrap( ~region) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "3 day", 
                date_labels = "%d",
               expand = c(0,0)
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
png(filename=paste("img/spain/regions/covid19_casos-registrados-por-comunidad-autonoma-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
ggplot() +
  geom_line(data = select(data_all_export_sm,date, cases_registered, region_cp,-region),aes(date, cases_registered, group=region_cp), color="#CACACA" ) +
  geom_line(aes(date, cases_registered, group=region) ) +
  geom_point(aes(date, cases_registered, group=region), size = 0.5 ) +
  scale_y_log10(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "3 day", 
               date_labels = "%d",
               expand = c(0,0)
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

png(filename=paste("img/spain/regions/covid19_casos-registrados-por-comunidad-autonoma-per-cienmil-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = select(data_all_export_sm,date,cases_per_100000,region_cp,-region),
            aes(date,cases_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_per_100000,group=region) ) +
  geom_point(aes(date,cases_per_100000,group=region), size = 0.5 ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "3 day", 
               date_labels = "%d",
               expand = c(0,0)
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

png(filename=paste("img/spain/regions/covid19_casos-registrados-por-comunidad-autonoma-per-cienmil-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = select(data_all_export_sm,date,cases_per_100000,region_cp,-region),
            aes(date,cases_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_per_100000,group=region) ) +
  geom_point(aes(date,cases_per_100000,group=region), size = 0.5 ) +
  scale_y_log10( 
    limits = c(0.2,max(data_cases$cases_per_100000)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 3000, 100) )) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "3 day", 
               date_labels = "%d",
               expand = c(0,0)
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

# create growth curve ------------
# Contribution by @lorezmt
crec <- ""
# create shorted dataframe
data_cases2 <- data_all_export %>% filter(date >= "2020-03-23") # sets starting day

slope <- 3

x <- seq_along(unique(data_cases2$date))
# creates empty vectors
y_percent <- vector(length=length(x))
# fill firs value
y_percent[[1]] <- 10575 # sets starting value
# create data series with certain % of growth
for (i in 2:length(x)) {
  y_percent[[i]] <- y_percent[[i-1]] + y_percent[[i-1]]* (slope /100) # grows n %
}
# creates the data fame
data_unique <- arrange(data_cases2, date) %>% select(date) %>% unique()
crec <- data.frame(x = data_unique, y_percent = y_percent)

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
png(filename=paste("img/spain/regions/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(aes(date, cases_registered, group=region, color=region), size= 1 ) +
  geom_point(aes(date, cases_registered,  color=region), size= 2 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)), 
                  aes(date, cases_registered,  color=region, label=paste(format( cases_registered,  nsmall=1, big.mark="."),region)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_all_export$date), max(data_all_export$date + 5.5)) 
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

png(filename=paste("img/spain/regions/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-lineal_con-curva.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(aes(date, cases_registered, group=region, color=region), size= 1 ) +
  geom_point(aes(date, cases_registered,  color=region), size= 2 ) +
  geom_text(data = crec[1,],aes(as.Date("2020-03-24"),14000, label=paste0("curva: un ", slope, "% más de casos cada día")), 
            size = 7, family = "Roboto Condensed", hjust=1) +
  geom_line(data = crec, aes(x = x.date, y = y_percent), linetype = 2, size = 1, color ="#444444") +
  scale_color_manual(values = colors ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)), 
                  aes(date, cases_registered,  color=region, label=paste(format( cases_registered,  nsmall=1, big.mark="."),region)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  coord_cartesian( 
    ylim=c(1, max(data_all_export$value)*1.05 )
  ) +
  scale_y_continuous( 
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_all_export$date), max(data_all_export$date + 6)) 
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

png(filename=paste("img/spain/regions/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-log_with-curve.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = crec, aes(y = y_percent, x = date), linetype = 2, size = 2, color ="#444444") +
  geom_line(aes(date, cases_registered, group=region, color=region), size= 1 ) +
  geom_point(aes(date, cases_registered, color=region), size= 1.5 ) +
  geom_text(data = crec[1,],aes(as.Date("2020-03-23"),12000,label=paste0("línea: un ", slope, "% más de casos cada día")), 
            size = 5, family = "Roboto Condensed", hjust=1) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)), 
                  aes(date, cases_registered,  color=region, label=paste(format( cases_registered,  nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000), seq(10000, 100000, 10000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_all_export$date), max(data_all_export$date + 3)) 
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

png(filename=paste("img/spain/regions/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(aes(date, cases_registered, group=region, color=region), size= 1 ) +
  geom_point(aes(date, cases_registered, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)), 
                  aes(date, cases_registered,  color=region, label=paste(format( cases_registered,  nsmall=1, big.mark="."),region)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000), seq(10000, 100000, 10000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_all_export$date), max(data_all_export$date + 7)) 
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
png(filename=paste("img/spain/regions/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-log_en.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(aes(date, cases_registered, group=region, color=region), size= 1 ) +
  geom_point(aes(date, cases_registered, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)), 
                  aes(date, cases_registered,  color=region, label=paste(format( cases_registered,  nsmall=1, big.mark="."),region)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000), seq(10000, 100000, 10000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_all_export$date), max(data_all_export$date + 7)) 
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


# png(filename=paste("img/spain/regions/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-log_comparativa.png", sep = ""),width = 1200,height = 700)
# data_all_export %>% filter( CCAA != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,value,group=CCAA, color=CCAA), size= 1 ) +
#   geom_point(aes(date,value,color=CCAA), size= 1.5 ) +
#   geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  CCAA != "Total"), 
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
#                limits=c( min(data_i_cases$date), max(data_all_export$date + 1.5)) 
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
png(filename=paste("img/spain/regions/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(aes(date,cases_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_per_100000, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)), 
                  aes(date,cases_per_100000, color=region, label=paste(format(cases_per_100000, nsmall=1, big.mark=".", decimal.mark = ","),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_color_manual(values = colors ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_all_export$date), max(data_all_export$date + 7)) 
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

png(filename=paste("img/spain/regions/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-per-cienmil-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(aes(date,cases_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_per_100000, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)), 
                  aes(date,cases_per_100000, color=region, label=paste(format(cases_per_100000, nsmall=1, big.mark=".", decimal.mark = ","),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
                  # xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2020-01-4"))
  ) +
  scale_color_manual(values = colors ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 limits = c(0.1,max(data_all_export$cases_per_100000)),
                 minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_all_export$date), max(data_all_export$date + 7)) 
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
png(filename=paste("img/spain/regions/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-per-cienmil-log_en.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(aes(date,cases_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,cases_per_100000, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)), 
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
  scale_color_manual(values = colors ) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 limits = c(0.1,max(data_all_export$cases_per_100000)),
                 minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_all_export$date), max(data_all_export$date + 7)) 
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
png(filename=paste("img/spain/regions/covid19_casos-registrados-UCI-por-comunidad-autonoma-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = select(data_all_export_sm ,date,intensive_care,region_cp,-region),aes(date,intensive_care,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,intensive_care,group=region) ) +
  geom_point(aes(date,intensive_care),size = 0.5 ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en España por comunidad autónoma (escala lineal)",
       subtitle = paste0(period, ". Datos de CLM, CyL, C. Valenciana, Galicia y C. Madrid a partir del 02.04.2020 son de prevalencia (personas ingresadas a día de hoy)"),
       y = "personas en UCI",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/spain/regions/covid19_casos-registrados-UCI-por-comunidad-autonoma-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = select(data_all_export_sm ,date,intensive_care,region_cp,-region),aes(date,intensive_care,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,intensive_care,group=region) ) +
  geom_point(aes(date,intensive_care,group=region), size = 0.5 ) +
  scale_y_log10( minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en España por comunidad autónoma (escala logarítmica)",
       subtitle = paste0(period, ". Datos de CLM, CyL, C. Valenciana, Galicia y C. Madrid a partir del 02.04.2020 son de prevalencia (personas ingresadas a día de hoy)"),
       y = "personas en UCI",
       x = "fecha",
       caption = caption)
dev.off()

# Escala lineal
png(filename=paste("img/spain/regions/covid19_casos-registrados-UCI-por-comunidad-autonoma-per-cienmil-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = select(data_all_export_sm ,date,intensive_care_per_100000,region_cp,-region),
            aes(date,intensive_care_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,intensive_care_per_100000,group=region) ) +
  geom_point(aes(date,intensive_care_per_100000,group=region), size = 0.5 ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en España por comunidad autónoma",
       subtitle = paste0(period, ". Datos de CLM, CyL, C. Valenciana, Galicia y C. Madrid a partir del 02.04.2020 son de prevalencia (personas ingresadas a día de hoy)"),
       y = "personas en UCI por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/spain/regions/covid19_casos-registrados-UCI-por-comunidad-autonoma-per-cienmil-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = select(data_all_export_sm ,date,intensive_care_per_100000,region_cp,-region),
            aes(date,intensive_care_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,intensive_care_per_100000,group=region) ) +
  geom_point(aes(date,intensive_care_per_100000,group=region), size = 0.5 ) +
  scale_y_log10( minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) )) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de personas (acumulado) por 100.000 habitantes en España en la UCI por COVID-19 registrados en España por comunidad autónoma (escala log)",
       subtitle = paste0(period, ". Datos de CLM, CyL, C. Valenciana, Galicia y C. Madrid a partir del 02.04.2020 son de prevalencia (personas ingresadas a día de hoy)"),
       y = "personas en UCI por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# // 2.2 UCI Superpuesto -------------
png(filename=paste("img/spain/regions/covid19_casos-registrados-UCI-por-comunidad-autonoma-superpuesto-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(aes(date,intensive_care,group=region, color=region), size= 1 ) +
  geom_point(aes(date,intensive_care, color=region),size = 1.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"), 
                  aes(date,intensive_care, color=region, label=paste(format(intensive_care, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_all_export$date), max(data_all_export$date + 1.5)),
               
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en España por comunidad autónoma",
       subtitle = paste0(period, ". Datos de CLM, CyL, C. Valenciana, Galicia y C. Madrid a partir del 02.04.2020 son de prevalencia (personas ingresadas a día de hoy)"),
       y = "personas en UCI",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/spain/regions/covid19_casos-registrados-UCI-por-comunidad-autonoma-superpuesto-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(aes(date,intensive_care,group=region, color=region), size= 1 ) +
  geom_point(aes(date,intensive_care, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"), 
                  aes(date,intensive_care, color=region, label=paste(format(intensive_care, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_log10( minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_all_export$date), max(data_all_export$date + 2.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de personas (acumulado) en la UCI por COVID-19 registrados en España por comunidad autónoma",
       subtitle = paste0(period, ". Datos de CLM, CyL, C. Valenciana, Galicia y C. Madrid a partir del 02.04.2020 son de prevalencia (personas ingresadas a día de hoy)"),
       y = "personas en UCI",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/spain/regions/covid19_casos-registrados-UCI-por-comunidad-autonoma-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(aes(date,intensive_care_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,intensive_care_per_100000,  color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"), 
                  aes(date,intensive_care_per_100000, color = region, label=paste(format(intensive_care_per_100000, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_all_export$date), max(data_all_export$date + 2.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de personas (acumulado) por 100.000 habitantes en España en la UCI por COVID-19 registrados en España por comunidad autónoma",
       subtitle = paste0(period, ". Datos de CLM, CyL, C. Valenciana, Galicia y C. Madrid a partir del 02.04.2020 son de prevalencia (personas ingresadas a día de hoy)"),
       y = "personas en UCI por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/spain/regions/covid19_casos-registrados-UCI-por-comunidad-autonoma-superpuesto-per-cienmil-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(aes(date,intensive_care_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,intensive_care_per_100000,  color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"), 
                  aes(date,intensive_care_per_100000, color = region, label=paste(format(intensive_care_per_100000, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_log10( minor_breaks = c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_all_export$date), max(data_all_export$date + 2.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de personas (acumulado) por 100.000 habitantes en España en la UCI por COVID-19 registrados en España por comunidad autónoma",
       subtitle = paste0(period, ". Datos de CLM, CyL, C. Valenciana, Galicia y C. Madrid a partir del 02.04.2020 son de prevalencia (personas ingresadas a día de hoy)"),
       y = "personas en UCI por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# # 3. Deceassed (Fallecimientos) ------------

# create growth curve ------------
# Contribution by @lorezmt

# create shorted dataframe
data_death2 <- data_all_export %>% filter(date >= "2020-03-24") # sets starting day

xx <- seq_along(unique(data_death2$date))
# creates empty vectors
y_percent2 <- vector(length=length(xx))
# fill firs value
y_percent2[[1]] <- 1535 # sets starting value
# create data series with certain % of growth
for (i in 2:length(xx)) {
  y_percent2[[i]] <- y_percent2[[i-1]] + y_percent2[[i-1]]*0.04 # grows n 
  print(paste(i," valor i.") )
  print(y_percent2[[i]])
}
# creates the data fame
data_unique2 <- arrange(data_death2, date) %>% select(date) %>% unique() %>% filter( region == "Andalucía" )
crec2 <- data_unique2
crec2$y_percent2 <- y_percent2

# create temp dataframes to be able to plot all the values in small multiples -----
date==as.Date("2020-04-16") & country == "France"

# Warning: remove Cataluña last
# dateunique <- data_death %>% select(date) %>% ungroup() %>% unique() %>% filter(date > as.Date("2020-04-16") ) %>% ungroup() 
# data_death  <- data_death %>% filter( ! (date %in% dateunique$date & region == "Cataluña"  ) )

# // 3.1 Fallecimientos Small multiple ---------- 

date_limit_init_death <- as.Date("2020-03-08")

# Because of Warning, remove days and regions:

# Acumulativo
# Escala lineal
png(filename=paste("img/spain/regions/covid19_fallecimientos-registrados-por-comunidad-autonoma-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = data_all_export_sm %>%  ungroup() %>% select(date,deceassed,region_cp,-region),
            aes(date,deceassed,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,deceassed,group=region) ) +
  geom_point(aes(date,deceassed), size=0.5 ) +
  facet_wrap( ~region) +
  scale_y_continuous( 
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "5 day",
               date_labels = "%d",
               expand = c(0,0),
               limits = c(date_limit_init_death,max(data_all_export$date) )
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
png(filename=paste("img/spain/regions/covid19_fallecimientos-registrados-por-comunidad-autonoma-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = data_all_export_sm %>%  ungroup() %>% select(date,deceassed,region_cp,-region),
            aes(date,deceassed,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,deceassed,group=region) ) +
  geom_point(aes(date,deceassed), size=0.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)),
                  aes(date + 1,deceassed, label=paste(format(deceassed, nsmall=1, big.mark="."))),
                  # nudge_x = 3, # adjust the starting y position of the text label
                  size=4,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  # segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( 
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
    ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "5 day",
               date_labels = "%d",
               # limits=c( min(data_all_export$date), max(data_all_export$date + 1.5)),
               expand = c(0,1),
               limits = c(date_limit_init_death,max(data_all_export$date) )
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "bottom"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()

# Escala lineal / 100.000 hab
png(filename=paste("img/spain/regions/covid19_fallecimientos-registrados-por-comunidad-autonoma-per-cienmil-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = data_all_export_sm %>%  ungroup() %>% select(date,deceassed_per_100000,region_cp,-region),
            aes(date,deceassed_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,deceassed_per_100000,group=region) ) +
  geom_point(aes(date,deceassed_per_100000), size=0.5 ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "5 day",
               date_labels = "%d",
               expand = c(0,0),
               limits = c(date_limit_init_death,max(data_all_export$date) )
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "fallecidos por 100.000 habitantes ",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/spain/regions/covid19_fallecimientos-registrados-por-comunidad-autonoma-per-cienmil-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = data_all_export_sm %>%  ungroup() %>% select(date,deceassed_per_100000,region_cp,-region),
            aes(date,deceassed_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,deceassed_per_100000,group=region) ) +
  geom_point(aes(date,deceassed_per_100000), size=0.5 ) +
  scale_y_log10( 
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) )) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "5 day",
               date_labels = "%d",
               expand = c(0,0),
               limits = c(date_limit_init_death,max(data_all_export$date) )
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()

# // 3.2 Fallecimientos superpuestos ----------
# // CCAA -------------------
png(filename=paste("img/spain/regions/covid19_fallecimientos-registrados-por-comunidad-autonoma-superpuesto-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%  
  ggplot() +
  geom_line(aes(date,deceassed,group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, (date==max(data_all_export$date)  )  
                               ),
                  aes(date,deceassed, color=region, label=paste(format(deceassed, nsmall=1, big.mark="."),region)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.15,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_continuous(
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( min(data_all_export$date) + 6, max(data_all_export$date + 8)),
               expand = c(0,0),
               limits = c(date_limit_init_death,max(data_all_export$date) )
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
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period, warning),
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/spain/regions/covid19_fallecimientos-registrados-por-comunidad-autonoma-superpuesto-lineal-with-curve.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = filter(crec2, region == "Andalucía" ), aes(x = date, y = y_percent2), linetype = 2, size = 2, color ="#444444") +
  geom_text(data = crec2[1,],aes(as.Date("2020-03-24"),1200,label="curva: un 4% más de fallecimientos cada día"), 
            size = 8, base_family = "Roboto Condensed") +
  geom_line(aes(date,deceassed,group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed, color=region), size= 1.5 ) +
  scale_color_manual(values = colors ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)),
                  aes(date,deceassed, color=region, label=paste(format(deceassed, nsmall=1, big.mark="."),region)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( date_limit_init_death, max(data_all_export$date + 1.5))
               
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

png(filename=paste("img/spain/regions/covid19_fallecimientos-registrados-por-comunidad-autonoma-superpuesto-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>% 
  ggplot() +
  geom_line(aes(date,deceassed,group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, (date==max(data_all_export$date)  )  
                                  ),
                                    aes(date,deceassed, color=region, label=paste0(format(deceassed, nsmall=1, big.mark="."),
                                                                 " ",region, " (+", daily_deaths,", +", daily_deaths_inc ,"%)")),
                                  nudge_x = 2, # adjust the starting y position of the text label
                                  size=5,
                                  hjust=0,
                                  family = "Roboto Condensed",
                                  direction="y",
                                  segment.size = 0.15,
                                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_log10(
    breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( date_limit_init_death, max(data_all_export$date + 9))
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en España ",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). Entre paréntesis muertos del último día y % respecto día anterior. ",period),
       y = "fallecidos (escala logarítmica)",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/spain/regions/covid19_fallecimientos-registrados-por-comunidad-autonoma-superpuesto-log-with-curve.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = crec2, aes(x = date, y = y_percent2), linetype = 2, size = 2, color ="#444444") +
  geom_text(data = crec2[1,],aes(as.Date("2020-03-14"),750,label="línea: un 4% más de fallecimientos cada día"), 
            size = 8, base_family = "Roboto Condensed") +
  geom_line(aes(date,deceassed,group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)),
                  aes(date,deceassed, color=region, label=paste(format(deceassed, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)) ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( date_limit_init_death, max(data_all_export$date + 1.5))
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
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period, warning),
       y = "fallecidos",
       x = "fecha",
       caption = caption)
dev.off()


# deaths per 100.000
png(filename=paste("img/spain/regions/covid19_fallecimientos-registrados-por-comunidad-autonoma-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>% # filter( ! (date %in% dateunique$date & region == "Cataluña"  ) ) %>%
  ggplot() +
  geom_line(aes(date,deceassed_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed_per_100000, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, (date==max(data_all_export$date)  ) 
                  ),
                  aes(date,deceassed_per_100000, color=region, label=paste(format(deceassed_per_100000, nsmall=1, big.mark=".", decimal.mark = ","),region)),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( date_limit_init_death, max(data_all_export$date + 6))
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
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period, warning),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/spain/regions/covid19_fallecimientos-registrados-por-comunidad-autonoma-superpuesto-per-cienmil-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%  
  ggplot() +
  geom_line(aes(date,deceassed_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed_per_100000, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, (date==max(data_all_export$date) )
                        ),
                        aes(date,deceassed_per_100000, color=region, label=paste(format(deceassed_per_100000, nsmall=1, big.mark=".", decimal.mark = ","),region)),
                        nudge_x = 5, # adjust the starting y position of the text label
                        size=5,
                        hjust=0,
                        family = "Roboto Condensed",
                        direction="y",
                        segment.size = 0.1,
                        segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_log10(  
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ),
    expand = c(0,0.4)
  ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( date_limit_init_death, max(data_all_export$date + 12)),
               expand = c(0,0)
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
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "fallecidos por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# 4. Daily deaths ------------------

# 4.1 Small multiple ---------------
# Daily deaths lineal average SM --------
png(filename=paste("img/spain/regions/covid19_muertes-por-dia-comunidad-autonoma-lineal_media.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = data_all_export_sm %>%  ungroup() %>% select(date,daily_deaths_avg6,region_cp,-region),
            aes(date,daily_deaths_avg6,group=region_cp), color="#CACACA" ) +
  geom_point(aes(date,daily_deaths, color=region), size= 1.5, alpha = 0.5) +
  geom_smooth(aes(date,daily_deaths_avg6,group=region, color=region), size= 1, se = FALSE, span = 0.35 ) +
  # geom_point(data=filter( data_all_export, date==max(data_all_export$date)), aes(date, daily_deaths_avg6, color=region), size= 1.5, alpha = 0.3 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)),
                  aes(date,daily_deaths_avg6, 
                      label=paste(format(daily_deaths_avg6, nsmall=1, big.mark=".", decimal.mark = ","))),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#777777"
  ) +
  facet_wrap( ~region) +
  scale_color_manual(values = colors ) +
  coord_cartesian(
    ylim = c(1,max(data_all_export[!is.na(data_all_export$daily_deaths_avg6),]$daily_deaths_avg6)+100)
  ) +
  # scale_y_log10(
  #   breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
  #   labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #   minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  # ) +
  scale_x_date(date_breaks = "4 day",
               date_labels = "%d",
               limits=c( date_limit_init_death, max(data_all_export$date + 5)),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Media de muertes por día en los 6 días anteriores (último inclusive) por COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
       y = "fallecidos por día (media 6 días) (escala lineal)",
       x = "fecha",
       caption = caption)
dev.off()

# Daily deaths log average SM --------
png(filename=paste("img/spain/regions/covid19_muertes-por-dia-comunidad-autonoma-log_media.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(data = data_all_export_sm %>%  ungroup() %>% select(date,daily_deaths_avg6,region_cp,-region),
            aes(date,daily_deaths_avg6,group=region_cp), color="#CACACA" ) +
  geom_point(aes(date,daily_deaths, color=region), size= 1.5, alpha = 0.5) +
  geom_smooth(aes(date,daily_deaths_avg6,group=region, color=region), size= 1, se = FALSE, span = 0.35 ) +
  # geom_point(data=filter( data_all_export, date==max(data_all_export$date) ), aes(date, daily_deaths_avg6, color=CCAA), size= 1.5, alpha = 0.3 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date) ),
                  aes(date,daily_deaths_avg6, 
                      label=paste(format(daily_deaths_avg6, nsmall=1, big.mark=".", decimal.mark = ","))),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#777777"
  ) +
  facet_wrap( ~region) +
  scale_color_manual(values = colors ) +
  coord_cartesian(
    ylim = c(1,max(data_all_export[!is.na(data_all_export$daily_deaths_avg6) ,]$daily_deaths_avg6))
  ) +
  scale_y_log10(
    breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  ) +
  scale_x_date(date_breaks = "4 day",
               date_labels = "%d",
               limits=c( date_limit_init_death, max(data_all_export$date + 5)),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Media de muertes por día en los 6 días anteriores (último inclusive) por COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period, warning),
       y = "fallecidos por día (media 6 días) (escala logarítmica)",
       x = "fecha",
       caption = caption)
dev.off()

# 4.2 Superpuesto ---------
# lineal ----
png(filename=paste("img/spain/regions/covid19_muertes-por-dia-comunidad-autonoma-superpuesto-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>% 
  ggplot() +
  # geom_line(data=hubei, aes(date+50,daily_deaths,group=region), size= 4, color="#aaaaaa"  ) +
  geom_line(aes(date,daily_deaths,group=region, color=region), size= 1 ) +
  geom_point(aes(date,daily_deaths, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, ( date==max(data_all_export$date) )  
                               ),
                  aes(date,daily_deaths, color=region, label=paste(format(daily_deaths, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( date_limit_init_death, max(data_all_export$date) + 12),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de muertes por COVID-19 registradas por día en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal) ",period, warning),
       y = "fallecidos por día",
       x = "fecha",
       caption = caption)
dev.off()

# log --------
png(filename=paste("img/spain/regions/covid19_muertes-por-dia-comunidad-autonoma-superpuesto-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>% 
  ggplot() +
  geom_line(aes(date,daily_deaths,group=region, color=region), size= 1 ) +
  geom_point(aes(date,daily_deaths, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, ( date==max(data_all_export$date) )  
  ),
                  aes(date,daily_deaths, color=region, label=paste(format(daily_deaths, nsmall=1, big.mark="."),region)),
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_log10(
    breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( date_limit_init_death, max(data_all_export$date + 12)),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de muertes por COVID-19 registradas por día en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period, warning),
       y = "fallecidos por día (escala logarítmica)",
       x = "fecha",
       caption = caption)
dev.off()

# average log --------
png(filename=paste("img/spain/regions/covid19_muertes-por-dia-comunidad-autonoma-superpuesto-log_media.png", sep = ""),width = 1200,height = 700)
data_all_export %>%  
  ggplot() +
  # geom_smooth( data=hubei, aes(date+40,daily_deaths_avg6,group=region, color=region), size= 3, color="#aaaaaa", se = FALSE, span = 0.35 ) +
  geom_line(aes(date,daily_deaths_avg6,group=region, color=region), size= 1, se = FALSE, span = 0.35 ) +
  geom_point(aes(date,daily_deaths, color=region), size= 0.7, alpha=0.6 ) +
  geom_line(aes(date,daily_deaths, color=region, group=region), size= 0.3, alpha=0.6  ) +
  geom_point(data=filter( data_all_export, date==max(data_all_export$date)), aes(date, daily_deaths_avg6, color=region), size= 1, alpha = 0.3 ) +
  geom_text_repel(data=filter( data_all_export, ( date==max(data_all_export$date)  ) 
  ),
                  aes(date,daily_deaths_avg6, color=region, label=paste(format(daily_deaths_avg6, nsmall=1, big.mark=".", decimal.mark = ","),region)),
                  nudge_x = 4, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#777777"
  ) +
  # marca un día
  geom_text_repel(data=filter( data_all_export, date==as.Date("2020-04-03") &  region == "Madrid" ),
                  aes(date,daily_deaths, label=paste("muertes en un día en una comunidad autónoma")),
                  nudge_y = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  # marca la línea
  geom_text_repel(data=filter( data_all_export, date==as.Date("2020-04-17") &  region == "Madrid" ),
                  aes(date+0.5,149, label=paste("media de 6 días")),
                  nudge_y = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  coord_cartesian(
    ylim = c(1,max(data_all_export[!is.na(data_all_export$daily_deaths_avg6) & ( data_all_export$region != "Total"),]$daily_deaths_avg6))
  ) +
  scale_y_log10(
    breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( date_limit_init_death, max(data_all_export$date + 12)),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Media de muertes por día en los 6 días anteriores (último inclusive) por COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period, warning),
       y = "fallecidos por día (media 6 días) (escala logarítmica)",
       x = "fecha",
       caption = caption)
dev.off()

# lineal average --------
png(filename=paste("img/spain/regions/covid19_muertes-por-dia-comunidad-autonoma-superpuesto-lineal_media.png", sep = ""),width = 1200,height = 700)
data_all_export %>%  
  ggplot() +
  geom_smooth(aes(date,daily_deaths_avg6,group=region, color=region), size= 1, se = FALSE, span = 0.35 ) +
  geom_point(aes(date,daily_deaths, color=region), size= 1.5 ) +
  geom_point(data=filter( data_all_export, date==max(data_all_export$date)), aes(date, daily_deaths_avg6, color=region), size= 1.5, alpha = 0.3 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date) ),
                  aes(date,daily_deaths_avg6, color=region, label=paste(format(daily_deaths_avg6, nsmall=1, big.mark=".", decimal.mark = ","),region)),
                  nudge_x = 4, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#777777"
  ) +
  # marca un día
  geom_text_repel(data=filter( data_all_export, date==as.Date("2020-03-28") &  region == "Madrid" ),
                  aes(date,daily_deaths, label=paste("muertes en un día en una provincia")),
                  nudge_x = -1, # adjust the starting y position of the text label
                  size=5,
                  hjust=1,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  # marca la línea
  geom_text_repel(data=filter( data_all_export, date==as.Date("2020-04-04") &  region == "Madrid" ),
                  aes(date,275, label=paste("media de 6 días")),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  coord_cartesian(
    ylim = c(1,max(data_all_export[!is.na(data_all_export$daily_deaths_avg6) & ( data_all_export$region != "Total"),]$daily_deaths_avg6)*1.1)
  ) +
  # scale_y_log10(
  #   breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
  #   labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #   minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  # ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( date_limit_init_death, max(data_all_export$date + 11)),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Media de muertes por día en los 6 días anteriores (último inclusive) por COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period, warning),
       y = "fallecidos por día (media 6 días) (escala lineal)",
       x = "fecha",
       caption = caption)
dev.off()


# 5. Altas ---------
# 5.1 Small multiples --------------
# Acumulativo
# Escala lineal
png(filename=paste("img/spain/regions/covid19_altas-por-comunidad-autonoma-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>% 
  ggplot() +
  geom_line(data = data_all_export_sm %>% ungroup() %>% select(date,recovered,region_cp,-region),
            aes(date,recovered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,recovered,group=region) ) +
  geom_point(aes(date,recovered), size=0.5 ) +
  facet_wrap( ~region) +
  scale_y_continuous( 
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "3 day",
               date_labels = "%d",
               expand = c(0,0),
               limits = c(as.Date("2020-03-16"),max(data_all_export$date) )
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de altas acumuladas por COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period,warning),
       y = "altas",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/spain/regions/covid19_altas-por-comunidad-autonoma-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%  
  ggplot() +
  geom_line(data = data_all_export_sm %>% ungroup() %>% select(date,recovered,region_cp,-region),
            aes(date,recovered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,recovered,group=region) ) +
  geom_point(aes(date,recovered), size=0.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"),
                  aes(date + 1,recovered, label=paste(format(recovered, nsmall=1, big.mark="."))),
                  # nudge_x = 3, # adjust the starting y position of the text label
                  size=4,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  # segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( 
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "3 day",
               date_labels = "%d",
               # limits=c( min(data_all_export$date), max(data_all_export$date + 1.5)),
               expand = c(0,0),
               limits = c(as.Date("2020-03-16"),max(data_all_export$date) )
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "bottom"
  ) +
  labs(title = "Número de altas acumuladas por COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period,warning),
       y = "altas",
       x = "fecha",
       caption = caption)
dev.off()

# Escala lineal / 100.000 hab
png(filename=paste("img/spain/regions/covid19_altas-por-comunidad-autonoma-per-cienmil-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>% 
  ggplot() +
  geom_line(data = data_all_export_sm %>% ungroup() %>% select(date,recovered_per_100000,region_cp,-region),
            aes(date,recovered_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,recovered_per_100000,group=region) ) +
  geom_point(aes(date,recovered_per_100000), size=0.5 ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "3 day",
               date_labels = "%d",
               expand = c(0,0),
               limits = c(as.Date("2020-03-16"),max(data_all_export$date) )
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de altas acumuladas por COVID-19 por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period,warning),
       y = "altas por 100.000 habitantes ",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/spain/regions/covid19_altas-por-comunidad-autonoma-per-cienmil-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%  
  ggplot() +
  geom_line(data = data_all_export_sm %>% ungroup() %>% select(date,recovered_per_100000,region_cp,-region),
            aes(date,recovered_per_100000,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,recovered_per_100000,group=region) ) +
  geom_point(aes(date,recovered_per_100000), size=0.5 ) +
  scale_y_log10( 
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) )) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "2 day",
               date_labels = "%d",
               expand = c(0,0),
               limits = c(as.Date("2020-03-16"),max(data_all_export$date) )
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "bottom"
  ) +
  labs(title = "Número de altas acumuladas por COVID-19 por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period,warning),
       y = "altas",
       x = "fecha",
       caption = caption)
dev.off()

# # Altas log average --------
# png(filename=paste("img/spain/regions/covid19_altas-por-dia-comunidad-autonoma-log_media.png", sep = ""),width = 1200,height = 700)
# data_all_export %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(data = data_all_export_sm %>% filter( region != "Total") %>%  ungroup() %>% select(date,daily_recovered_avg6,region_cp,-region),
#             aes(date,daily_recovered_avg6,group=region_cp), color="#CACACA" ) +
#   geom_point(aes(date,daily_recovered, color=region), size= 1.5, alpha = 0.5) +
#   geom_smooth(aes(date,daily_recovered_avg6,group=region, color=region), size= 1, se = FALSE, span = 0.35 ) +
#   geom_point(data=filter( data_all_export, date==max(data_all_export$date) & region != "Total"), aes(date, daily_recovered_avg6, color=region), size= 1.5, alpha = 0.3 ) +
#   geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"),
#                   aes(date,daily_recovered_avg6, 
#                       label=paste(format(daily_recovered_avg6, nsmall=1, big.mark=".", decimal.mark = ","))),
#                   nudge_x = 2, # adjust the starting y position of the text label
#                   size=4,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.2,
#                   segment.color="#777777"
#   ) +
#   facet_wrap( ~region) +
#   scale_color_manual(values = colors ) +
#   coord_cartesian(
#     ylim = c(1,max(data_all_export[!is.na(data_all_export$daily_recovered_avg6) & ( data_all_export$region != "Total"),]$daily_recovered_avg6))
#   ) +
#   scale_y_log10(
#     breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
#     labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
#     minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
#   ) +
#   scale_x_date(date_breaks = "3 day",
#                date_labels = "%d",
#                limits=c( min(data_all_export$date), max(data_all_export$date + 5)),
#                expand = c(0,0)
#   ) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Media de altas por día en los 6 días anteriores (último inclusive) por COVID-19 en España",
#        subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
#        y = "fallecidos por día (media 6 días) (escala logarítmica)",
#        x = "fecha",
#        caption = caption)
# dev.off()
# 
# # Altas log average --------
# png(filename=paste("img/spain/regions/covid19_altas-por-dia-comunidad-autonoma-lineal_media.png", sep = ""),width = 1200,height = 700)
# data_all_export %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(data = data_all_export_sm %>% filter( region != "Total") %>%  ungroup() %>% select(date,daily_recovered_avg6,region_cp,-region),
#             aes(date,daily_recovered_avg6,group=region_cp), color="#CACACA" ) +
#   geom_point(aes(date,daily_recovered, color=region), size= 1.5, alpha = 0.5) +
#   geom_smooth(aes(date,daily_recovered_avg6,group=region, color=region), size= 1, se = FALSE, span = 0.35 ) +
#   geom_point(data=filter( data_all_export, date==max(data_all_export$date) & region != "Total"), aes(date, daily_recovered_avg6, color=region), size= 1.5, alpha = 0.3 ) +
#   geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"),
#                   aes(date,daily_recovered_avg6, 
#                       label=paste(format(daily_recovered_avg6, nsmall=1, big.mark=".", decimal.mark = ","))),
#                   nudge_x = 2, # adjust the starting y position of the text label
#                   size=4,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.2,
#                   segment.color="#777777"
#   ) +
#   facet_wrap( ~region) +
#   scale_color_manual(values = colors ) +
#   coord_cartesian(
#     ylim = c(1,max(data_all_export[!is.na(data_all_export$daily_recovered_avg6) & ( data_all_export$region != "Total"),]$daily_recovered_avg6)+100)
#   ) +
#   # scale_y_log10(
#   #   breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
#   #   labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
#   #   minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
#   # ) +
#   scale_x_date(date_breaks = "1 day",
#                date_labels = "%d",
#                limits=c( min(data_all_export$date), max(data_all_export$date + 5)),
#                expand = c(0,0)
#   ) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Media de altas por día en los 6 días anteriores (último inclusive) por COVID-19 en España",
#        subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
#        y = "fallecidos por día (media 6 días) (escala logarítmica)",
#        x = "fecha",
#        caption = caption)
# dev.off()


# // 5.2 Altas superpuestos ----------
# // region -------------------
png(filename=paste("img/spain/regions/covid19_altas-por-comunidad-autonoma-superpuesto-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%  
  ggplot() +
  geom_line(aes(date,recovered,group=region, color=region), size= 1 ) +
  geom_point(aes(date,recovered, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, ( date==max(data_all_export$date) )
                                
                                
                               ),
                  aes(date,recovered, color=region, label=paste(format(recovered, nsmall=1, big.mark="."),region)),
                  nudge_x = 4, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.15,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_continuous(
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits = c(as.Date("2020-03-16"),max(data_all_export$date) + 10),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de altas acumuladas por COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period,warning),
       y = "altas",
       x = "fecha",
       caption = caption)
dev.off()

# png(filename=paste("img/spain/regions/covid19_altas-por-comunidad-autonoma-superpuesto-lineal-with-curve.png", sep = ""),width = 1200,height = 700)
# data_all_export %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(data = crec2, aes(x = date, y = y_percent2), linetype = 2, size = 2, color ="#444444") +
#   geom_text(data = crec2[1,],aes(as.Date("2020-03-24"),1200,label="curva: un 18% más de fallecimientos cada día"), 
#             size = 8, base_family = "Roboto Condensed") +
#   geom_line(aes(date,recovered,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,recovered, color=region), size= 1.5 ) +
#   scale_color_manual(values = colors ) +
#   geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"),
#                   aes(date,recovered, color=region, label=paste(format(recovered, nsmall=1, big.mark="."),region)),
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
#                limits=c( min(data_all_export$date), max(data_all_export$date + 1.5))
#   ) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de altas acumuladas por COVID-19 en España",
#        subtitle = paste0("Por comunidad autónoma (escala lineal). ",period),
#        y = "altas",
#        x = "fecha",
#        caption = caption)
# dev.off()

png(filename=paste("img/spain/regions/covid19_altas-por-comunidad-autonoma-superpuesto-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>% 
  ggplot() +
  geom_line(aes(date,recovered,group=region, color=region), size= 1 ) +
  geom_point(aes(date,recovered, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, ( date==max(data_all_export$date) )
                                 
                                 
  ),
                  aes(date,recovered, color=region, label=paste0(format(recovered, nsmall=1, big.mark="."), " ",region, " (+", daily_recovered,", +", daily_recovered_inc ,"%)" )), 
                  nudge_x = 4, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_log10(
    breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000), seq(10000 , 100000, 10000) )
  ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits = c(as.Date("2020-03-16"),max(data_all_export$date) + 12 )
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de altas acumuladas por COVID-19 en España ",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period,warning),
       y = "altas (escala logarítmica)",
       x = "fecha",
       caption = caption)
dev.off()

# png(filename=paste("img/spain/regions/covid19_altas-por-comunidad-autonoma-superpuesto-log-with-curve.png", sep = ""),width = 1200,height = 700)
# data_all_export %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(data = crec2, aes(x = date, y = y_percent2), linetype = 2, size = 2, color ="#444444") +
#   geom_text(data = crec2[1,],aes(as.Date("2020-03-14"),750,label="línea: un 18% más de fallecimientos cada día"),
#             size = 8, base_family = "Roboto Condensed") +
#   geom_line(aes(date,recovered,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,recovered, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"),
#                   aes(date,recovered, color=region, label=paste(format(recovered, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   # hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   scale_color_manual(values = colors ) +
#   scale_y_log10( minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100)) ) +
#   scale_x_date(date_breaks = "1 day",
#                date_labels = "%d",
#                limits=c( min(data_all_export$date), max(data_all_export$date + 1.5))
#   ) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de altas acumuladas por COVID-19 en España",
#        subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
#        y = "altas",
#        x = "fecha",
#        caption = caption)
# dev.off()

# # Altas lineal
# png(filename=paste("img/spain/regions/covid19_altas-por-dia-comunidad-autonoma-superpuesto-lineal.png", sep = ""),width = 1200,height = 700)
# data_all_export %>%
#   ggplot() +
#   geom_line(aes(date,daily_recovered,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,daily_recovered, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"),
#                   aes(date,daily_recovered, color=region, label=paste(format(daily_recovered, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   # hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   scale_color_manual(values = colors ) +
#   scale_x_date(date_breaks = "1 day",
#                date_labels = "%d",
#                limits=c( min(data_all_export$date), max(data_all_export$date) + 4),
#                expand = c(0,0)
#   ) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de altas por COVID-19 registradas por día en España",
#        subtitle = paste0("Por comunidad autónoma (escala lineal) ",period),
#        y = "fallecidos por día",
#        x = "fecha",
#        caption = caption)
# dev.off()
# 
# # Altas log --------
# png(filename=paste("img/spain/regions/covid19_altas-por-dia-comunidad-autonoma-superpuesto-log.png", sep = ""),width = 1200,height = 700)
# data_all_export %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_line(aes(date,daily_recovered,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,daily_recovered, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"),
#                   aes(date,daily_recovered, color=region, label=paste(format(daily_recovered, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   # hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   scale_color_manual(values = colors ) +
#   scale_y_log10(
#     breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
#     labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
#     minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
#   ) +
#   scale_x_date(date_breaks = "1 day",
#                date_labels = "%d",
#                limits=c( min(data_all_export$date), max(data_all_export$date + 4)),
#                expand = c(0,0)
#   ) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de altas por COVID-19 registradas por día en España",
#        subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
#        y = "fallecidos por día (escala logarítmica)",
#        x = "fecha",
#        caption = caption)
# dev.off()
# 
# # Altas log average --------
# png(filename=paste("img/spain/regions/covid19_altas-por-dia-comunidad-autonoma-superpuesto-log_media.png", sep = ""),width = 1200,height = 700)
# data_all_export %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_smooth(aes(date,daily_recovered_avg6,group=region, color=region), size= 1, se = FALSE, span = 0.35 ) +
#   geom_point(aes(date,daily_recovered, color=region), size= 1.5 ) +
#   geom_point(data=filter( data_all_export, date==max(data_all_export$date) & region != "Total"), aes(date, daily_Altass_avg6, color=region), size= 1.5, alpha = 0.3 ) +
#   geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"),
#                   aes(date,daily_recovered_avg6, color=region, label=paste(format(daily_recovered_avg6, nsmall=1, big.mark=".", decimal.mark = ","),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.2,
#                   segment.color="#777777"
#   ) +
#   # marca un día
#   geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)-12,  region == "Madrid" ),
#                   aes(date,daily_recovered, color=region, label=paste("altas en un día en una provincia")),
#                   nudge_y = 5, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   # direction="x",
#                   segment.size = 0.5,
#                   segment.color="#777777"
#   ) +
#   # marca la línea
#   geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date )-4,  region == "Madrid" ),
#                   aes(date+0.5,300, color=region, label=paste("media de 6 días")),
#                   nudge_y = 2, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   # direction="x",
#                   segment.size = 0.5,
#                   segment.color="#777777"
#   ) +
#   scale_color_manual(values = colors ) +
#   coord_cartesian(
#     ylim = c(1,max(data_all_export[!is.na(data_all_export$daily_recovered_avg6) & ( data_all_export$region != "Total"),]$daily_recovered_avg6))
#   ) +
#   scale_y_log10(
#     breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
#     labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
#     minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
#   ) +
#   scale_x_date(date_breaks = "1 day",
#                date_labels = "%d",
#                limits=c( min(data_all_export$date), max(data_all_export$date + 5)),
#                expand = c(0,0)
#   ) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Media de altas por día en los 6 días anteriores (último inclusive) por COVID-19 en España",
#        subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
#        y = "fallecidos por día (media 6 días) (escala logarítmica)",
#        x = "fecha",
#        caption = caption)
# dev.off()
# 
# # Altas log average --------
# png(filename=paste("img/spain/regions/covid19_altas-por-dia-comunidad-autonoma-superpuesto-lineal_media.png", sep = ""),width = 1200,height = 700)
# data_all_export %>% filter( region != "Total") %>%
#   ggplot() +
#   geom_smooth(aes(date,daily_recovered_avg6,group=region, color=region), size= 1, se = FALSE, span = 0.35 ) +
#   geom_point(aes(date,daily_recovered, color=region), size= 1.5 ) +
#   geom_point(data=filter( data_all_export, date==max(data_all_export$date) & region != "Total"), aes(date, daily_recovered_avg6, color=region), size= 1.5, alpha = 0.3 ) +
#   geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"),
#                   aes(date,daily_recovered_avg6, color=region, label=paste(format(daily_recovered_avg6, nsmall=1, big.mark=".", decimal.mark = ","),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.2,
#                   segment.color="#777777"
#   ) +
#   # marca un día
#   geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)-12,  region == "Madrid" ),
#                   aes(date,daily_recovered, color=region, label=paste("altas en un día en una provincia")),
#                   nudge_y = 5, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   # direction="x",
#                   segment.size = 0.5,
#                   segment.color="#777777"
#   ) +
#   # marca la línea
#   geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date )-4,  region == "Madrid" ),
#                   aes(date+0.5,300, color=region, label=paste("media de 6 días")),
#                   nudge_y = 2, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   # direction="x",
#                   segment.size = 0.5,
#                   segment.color="#777777"
#   ) +
#   scale_color_manual(values = colors ) +
#   coord_cartesian(
#     ylim = c(1,max(data_all_export[!is.na(data_all_export$daily_recovered_avg6) & ( data_all_export$region != "Total"),]$daily_recovered_avg6))
#   ) +
#   # scale_y_log10(
#   #   breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
#   #   labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
#   #   minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
#   # ) +
#   scale_x_date(date_breaks = "1 day",
#                date_labels = "%d",
#                limits=c( min(data_all_export$date), max(data_all_export$date + 5)),
#                expand = c(0,0)
#   ) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Media de altas por día en los 6 días anteriores (último inclusive) por COVID-19 en España",
#        subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
#        y = "fallecidos por día (media 6 días) (escala logarítmica)",
#        x = "fecha",
#        caption = caption)
# dev.off()

# Altas per 100.000 -----------------
png(filename=paste("img/spain/regions/covid19_altas-por-comunidad-autonoma-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>% 
  ggplot() +
  geom_line(aes(date,recovered_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,recovered_per_100000, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, ( date==max(data_all_export$date) )
  ),
                  aes(date,recovered_per_100000, color=region, label=paste(format(recovered_per_100000, nsmall=1, big.mark=".", decimal.mark = ","),region)),
                  nudge_x = 4, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits = c(as.Date("2020-03-16"),max(data_all_export$date) + 11 )
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de altas acumuladas por COVID-19 por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period,warning),
       y = "altas por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

png(filename=paste("img/spain/regions/covid19_altas-por-comunidad-autonoma-superpuesto-per-cienmil-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>% 
  ggplot() +
  geom_line(aes(date,recovered_per_100000,group=region, color=region), size= 1 ) +
  geom_point(aes(date,recovered_per_100000, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, ( date==max(data_all_export$date) )
  ),
                  aes(date,recovered_per_100000, color=region, label=paste(format(recovered_per_100000, nsmall=1, big.mark=".", decimal.mark = ","),region)),
                  nudge_x = 4, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_log10(  
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.01 , 0.1, 0.01), seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ),
    expand = c(0,0.1)
  ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits = c(as.Date("2020-03-16"),max(data_all_export$date) + 12 ),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de altas acumuladas por COVID-19 por 100.000 habitantes en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period,warning),
       y = "altas por 100.000 habitantes",
       x = "fecha",
       caption = caption)
dev.off()

# 6. Daily Altas ---------

# 4.1 Small multiple ---------------
# Daily recovered lineal average SM --------
png(filename=paste("img/spain/regions/covid19_altas-por-dia-comunidad-autonoma-lineal_media.png", sep = ""),width = 1200,height = 700)
data_all_export %>% 
  ggplot() +
  geom_smooth(data = data_all_export_sm %>% ungroup() %>% 
                
                select(date,daily_recovered_avg6,region_cp,-region),
            aes(date,daily_recovered_avg6,group=region_cp), color="#CACACA", se = FALSE, span = 0.35, size= 0.5 ) +
  geom_point(aes(date,daily_recovered, color=region), size= 1.5, alpha = 0.5) +
  geom_smooth(aes(date,daily_recovered_avg6,group=region, color=region), size= 1, se = FALSE, span = 0.35 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)),
                  aes(date,daily_recovered_avg6, 
                      label=paste(format(daily_recovered_avg6, nsmall=1, big.mark=".", decimal.mark = ","))),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#777777"
  ) +
  facet_wrap( ~region) +
  scale_color_manual(values = colors ) +
  coord_cartesian(
    ylim = c(1,max(data_all_export[!is.na(data_all_export$daily_recovered_avg6) & ( data_all_export$region != "Total"),]$daily_recovered_avg6)+100)
  ) +
  # scale_y_log10(
  #   breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
  #   labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #   minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  # ) +
  scale_x_date(date_breaks = "4 day",
               date_labels = "%d",
               limits=c( min(data_all_export$date + 20), max(data_all_export$date + 5)),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Media de altas por día en los 6 días anteriores (último inclusive) por COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period,warning),
       y = "altas por día (media 6 días) (escala logarítmica)",
       x = "fecha",
       caption = caption)
dev.off()

# Daily recovered log average SM --------
png(filename=paste("img/spain/regions/covid19_altas-por-dia-comunidad-autonoma-log_media.png", sep = ""),width = 1200,height = 700)
data_all_export %>% 
  ggplot() +
  geom_smooth(data = data_all_export_sm %>%  ungroup() %>% 
                
                select(date,daily_recovered_avg6,region_cp,-region),
            aes(date,daily_recovered_avg6,group=region_cp), color="#CACACA", se = FALSE, span = 0.35, size= 0.5) +
  geom_point(aes(date,daily_recovered, color=region), size= 1.5, alpha = 0.5) +
  geom_smooth(aes(date,daily_recovered_avg6,group=region, color=region), size= 1, se = FALSE, span = 0.35 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date)),
                  aes(date,daily_recovered_avg6, 
                      label=paste(format(daily_recovered_avg6, nsmall=1, big.mark=".", decimal.mark = ","))),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#777777"
  ) +
  facet_wrap( ~region) +
  scale_color_manual(values = colors ) +
  coord_cartesian(
    ylim = c(1,max(data_all_export[!is.na(data_all_export$daily_recovered_avg6) & ( data_all_export$region != "Total"),]$daily_recovered_avg6))
  ) +
  scale_y_log10(
    breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  ) +
  scale_x_date(date_breaks = "4 day",
               date_labels = "%d",
               limits=c( min(data_all_export$date + 20), max(data_all_export$date + 5)),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Media de altas por día en los 6 días anteriores (último inclusive) por COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period,warning),
       y = "altas por día (media 6 días) (escala logarítmica)",
       x = "fecha",
       caption = caption)
dev.off()

# 6.2 superpuesto

# average log --------
png(filename=paste("img/spain/regions/covid19_altas-por-dia-comunidad-autonoma-superpuesto-log_media.png", sep = ""),width = 1200,height = 700)
data_all_export %>% 
  ggplot() +
  # geom_smooth( data=hubei, aes(date+40,daily_deaths_avg6,group=region, color=region), size= 3, color="#aaaaaa", se = FALSE, span = 0.35 ) +
  geom_smooth(aes(date,daily_recovered_avg6,group=region, color=region), size= 1, se = FALSE, span = 0.35 ) +
  geom_point(aes(date,daily_recovered, color=region), size= 1.5 ) +
  geom_point(data=filter( data_all_export, date==max(data_all_export$date)), aes(date, daily_recovered_avg6, color=region), size= 1, alpha = 0.3 ) +
  geom_text_repel(data=filter( data_all_export, ( date==max(data_all_export$date) )
  ),
                  aes(date,daily_recovered_avg6, color=region, label=paste(format(daily_recovered_avg6, nsmall=1, big.mark=".", decimal.mark = ","),region)),
                  nudge_x = 4, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#777777"
  ) +
  # marca un día
  geom_text_repel(data=filter( data_all_export, date==as.Date("2020-03-28") &  region == "Madrid" ),
                  aes(date,daily_recovered, label=paste("altas en un día en una provincia")),
                  nudge_y = 3, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  # marca la línea
  geom_text_repel(data=filter( data_all_export, date==as.Date("2020-04-04") &  region == "Madrid" ),
                  aes(date+0.5,1360, label=paste("media de 6 días")),
                  nudge_y = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  coord_cartesian(
    ylim = c(1,max(data_all_export[!is.na(data_all_export$daily_recovered_avg6) & ( data_all_export$region != "Total"),]$daily_recovered_avg6))
  ) +
  scale_y_log10(
    breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( min(data_all_export$date + 19), max(data_all_export$date + 12)),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Media de altas por día en los 6 días anteriores (último inclusive) por COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period,warning),
       y = "altas por día (media 6 días) (escala logarítmica)",
       x = "fecha",
       caption = caption)
dev.off()

# lineal average --------
png(filename=paste("img/spain/regions/covid19_altas-por-dia-comunidad-autonoma-superpuesto-lineal_media.png", sep = ""),width = 1200,height = 700)
data_all_export %>% 
  ggplot() +
  geom_smooth(aes(date,daily_recovered_avg6,group=region, color=region), size= 1, se = FALSE, span = 0.35 ) +
  geom_point(aes(date,daily_recovered, color=region), size= 1.5 ) +
  geom_point(data=filter( data_all_export, date==max(data_all_export$date)), aes(date, daily_recovered_avg6, color=region), size= 1.5, alpha = 0.3 ) +
  geom_text_repel(data=filter( data_all_export, ( date==max(data_all_export$date) )
  ),
                  aes(date,daily_recovered_avg6, color=region, label=paste(format(daily_recovered_avg6, nsmall=1, big.mark=".", decimal.mark = ","),region)),
                  nudge_x = 4, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#777777"
  ) +
  # marca un día
  geom_text_repel(data=filter( data_all_export, date==as.Date("2020-03-28") &  region == "Madrid" ),
                  aes(date,daily_recovered, label=paste("altas en un día en una provincia")),
                  nudge_x = -1, # adjust the starting y position of the text label
                  size=5,
                  hjust=1,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  # marca la línea
  geom_text_repel(data=filter( data_all_export, date==as.Date("2020-04-06") &  region == "Madrid" ),
                  aes(date,1330, label=paste("media de 6 días")),
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  # direction="x",
                  segment.size = 0.5,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  coord_cartesian(
    ylim = c(1,max(data_all_export[!is.na(data_all_export$daily_recovered_avg6) & ( data_all_export$region != "Total"),]$daily_recovered_avg6)*1.1)
  ) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%d",
               limits=c( min(data_all_export$date + 19), max(data_all_export$date + 11)),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Media de altas por día en los 6 días anteriores (último inclusive) por COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period,warning),
       y = "altas por día (media 6 días) (escala logarítmica)",
       x = "fecha",
       caption = caption)
dev.off()

# 7. Trajectory.  Deaths vs weekly deaths ------
# lineal --------
png(filename=paste("img/spain/regions/covid19_trayectoria-comunidad-autonoma-superpuesto-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(aes(deceassed,deaths_last_week,group=region, color=region), size= 1 ) +
  geom_point(aes(deceassed,deaths_last_week,color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"),
                  aes(deceassed,deaths_last_week, color=region, label=paste( region)),
                  nudge_x = 0.2, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  # direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  # scale_y_log10(
  #   breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
  #   labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #   minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  # ) +
  # scale_x_log10(
  #   breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
  #   labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  #   minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  # ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Fallecidos 7 días anteriores / total fallecidos por COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma. ",period, warning),
       y = "fallecidos en últimos 7 días (lineal)",
       x = "total de fallecidos (lineal)",
       caption = paste0("", caption , "| Ver web https://aatishb.com/covidtrends/" ) )
dev.off()



for ( i in 1:20  ) {
  la_ccaa <- unique(data_all_export$region)[i]
  png(filename=paste("img/spain/regions/small_multiple/covid19_trayectoria-comunidad-autonoma-superpuesto-lineal",substr(la_ccaa,1,5) ,".png", sep = ""),width = 1200,height = 700)
    the_chart <- data_all_export %>% filter ( !is.na(region) ) %>% filter ( region == la_ccaa ) %>%
    ggplot() +
    geom_line(aes(deceassed,deaths_last_week,group=region, color=region), size= 3 ) +
    geom_point(aes(deceassed,deaths_last_week,color=region), size= 1.5 ) +
    scale_color_manual(values = colors ) +
    theme_minimal(base_family = "Roboto Condensed",base_size = 37) +
      scale_y_log10() +
      scale_x_log10() +
    theme(
      panel.grid.minor.x = element_blank(),
      # panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks.x = element_line(color = "#000000"),
      legend.position = "none"
    ) +
    labs(title = paste0(la_ccaa), # ". Fallecidos 7 días anteriores / total fallecidos por COVID-19 en España"),
         # subtitle = paste0("Por comunidad autónoma. ",period, warning),
         y = "fallecidos en últimos 7 días",
         x = "total de fallecidos"
         # caption = paste0("", caption , "" ) 
         )
  print(the_chart)
  dev.off()
  
}

# make tiled image with imagemagick from command line
# montage c* -geometry 400x tiles_02.png

# log --------
png(filename=paste("img/spain/regions/covid19_trayectoria-comunidad-autonoma-superpuesto-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%
  ggplot() +
  geom_line(aes(deceassed,deaths_last_week,group=region, color=region), size= 1 ) +
  geom_point(aes(deceassed,deaths_last_week,color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date),  region != "Total"),
                  aes(deceassed,deaths_last_week, color=region, label=paste( region)),
                  nudge_x = 0.2, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  # direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_manual(values = colors ) +
  scale_y_log10(
    breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  ) +
  scale_x_log10(
    breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels = function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks =  c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) )
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Fallecidos 7 días anteriores / total fallecidos por COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma. ",period, warning),
       y = "fallecidos en últimos 7 días (log)",
       x = "total de fallecidos (log)",
       caption = paste0("", caption , "| Ver web https://aatishb.com/covidtrends/" ) )
dev.off()
  
# 8. Mix ------------
# // 8.1 Small multiple ------------

# /// Comunidades autónomas small multiple --------------

# Escala lineal
png(filename=paste("img/spain/regions/covid19_mix-comunidad-autonoma-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%  
  ggplot() +
  # geom_line(data = select(data_all_export_sm,date,cases_registered,region_cp,-region),
  #           aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_registered,group=region, color="#2222BB"), size = 1 ) +
  geom_line(aes(date,recovered,group=region, color="#339922"), size = 1 ) +
  geom_line(aes(date,deceassed,group=region,color="black"), size = 1 ) +
  scale_color_identity(
    guide = "legend",
    labels = c("Casos", "Recuperados", "Muertos"),
  ) +
  facet_wrap( ~region) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9),
    # strip.text = element_text( vjust = 0, debug = TRUE ),
    strip.placement = "inside" 
    # legend.position = "bottom"
  ) +
  labs(title = "Casos, recuperados y fallecidos acumulados de COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period, warning),
       y = "casos registrados",
       x = "fecha",
       caption = caption,
       colour = "")
dev.off()

# Escala logarítmica
png(filename=paste("img/spain/regions/covid19_mix-comunidad-autonoma-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%  
  ggplot() +
  # geom_line(data = select(data_all_export_sm,date,cases_registered,region_cp,-region),
  #           aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_registered,group=region, color="#2222BB"), size = 1 ) +
  geom_line(aes(date,recovered,group=region, color="#339922"), size = 1 ) +
  geom_line(aes(date,deceassed,group=region,color="black"), size = 1 ) +
  scale_color_identity(
    guide = "legend",
    labels = c("Casos", "Recuperados", "Muertos"),
    ) +
  scale_y_log10(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Casos, recuperados y fallecidos acumulados de COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period, warning),
       y = "casos registrados",
       x = "fecha",
       caption = caption,
       colour = "")
dev.off()

# per 100.000 inhab ----------
# Escala lineal
png(filename=paste("img/spain/regions/covid19_mix-comunidad-autonoma-per-cienmil-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%   
  ggplot() +
  # geom_line(data = select(data_all_export_sm,date,cases_registered,region_cp,-region),
  #           aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_per_100000,group=region, color="#2222BB"), size = 1 ) +
  geom_line(aes(date,recovered_per_100000,group=region, color="#339922"), size = 1 ) +
  geom_line(aes(date,deceassed_per_100000,group=region,color="black"), size = 1 ) +
  scale_color_identity(
    guide = "legend",
    labels = c("Casos", "Recuperados", "Muertos"),
  ) +
  facet_wrap( ~region) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9),
    # strip.text = element_text( vjust = 0, debug = TRUE ),
    strip.placement = "inside" 
    # legend.position = "bottom"
  ) +
  labs(title = "Casos, recuperados y fallecidos acumulados de COVID-19 por 100.000 hab. en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period, warning),
       y = "casos registrados",
       x = "fecha",
       caption = caption)
dev.off()

# Escala logarítmica
png(filename=paste("img/spain/regions/covid19_mix-comunidad-autonoma-per-cienmil-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%  
  ggplot() +
  # geom_line(data = select(data_all_export_sm,date,cases_registered,region_cp,-region),
  #           aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,cases_per_100000,group=region, color="#2222BB"), size = 1 ) +
  geom_line(aes(date,recovered_per_100000,group=region, color="#339922"), size = 1 ) +
  geom_line(aes(date,deceassed_per_100000,group=region,color="black"), size = 1 ) +
  scale_color_identity(
    guide = "legend",
    labels = c("Casos", "Recuperados", "Muertos"),
  ) +
  scale_y_log10(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Casos, recuperados y fallecidos acumulados de COVID-19 por 100.000 hab. en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period, warning),
       y = "casos registrados",
       x = "fecha",
       caption = caption)
dev.off()



# Daily data ------------

# Escala lineal
png(filename=paste("img/spain/regions/covid19_mix-daily-comunidad-autonoma-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%  
  ggplot() +
  # geom_line(data = select(data_all_export_sm,date,cases_registered,region_cp,-region),
  #           aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,daily_cases,group=region, color="#2222BB"), size = 1 ) +
  geom_line(aes(date,daily_recovered,group=region, color="#339922"), size = 1 ) +
  geom_line(aes(date,daily_deaths,group=region,color="black"), size = 1 ) +
  scale_color_identity(
    guide = "legend",
    labels = c("Casos", "Recuperados", "Muertos"),
  ) +
  facet_wrap( ~region) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9),
    # strip.text = element_text( vjust = 0, debug = TRUE ),
    strip.placement = "inside" 
    # legend.position = "bottom"
  ) +
  labs(title = "Nuevos casos, recuperados y fallecidos por día de COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period, warning),
       y = "casos registrados",
       x = "fecha",
       caption = caption,
       colour = "")
dev.off()

# Escala logarítmica
png(filename=paste("img/spain/regions/covid19_mix-daily-comunidad-autonoma-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%  
  ggplot() +
  # geom_line(data = select(data_all_export_sm,date,cases_registered,region_cp,-region),
  #           aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,daily_cases,group=region, color="#2222BB"), size = 1 ) +
  geom_line(aes(date,daily_recovered,group=region, color="#339922"), size = 1 ) +
  geom_line(aes(date,daily_deaths,group=region,color="black"), size = 1 ) +
  scale_color_identity(
    guide = "legend",
    labels = c("Casos", "Recuperados", "Muertos"),
  ) +
  scale_y_log10(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Nuevos casos, recuperados y fallecidos por día de COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period, warning),
       y = "casos registrados",
       x = "fecha",
       caption = caption,
       colour = "")
dev.off()



# Daily data avg------------

# Escala lineal
png(filename=paste("img/spain/regions/covid19_mix-daily-avg-comunidad-autonoma-lineal.png", sep = ""),width = 1200,height = 700)
data_all_export %>%  
  ggplot() +
  # geom_line(data = select(data_all_export_sm,date,cases_registered,region_cp,-region),
  #           aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,daily_cases_avg,group=region, color="#2222BB"), size = 1 ) +
  geom_line(aes(date,daily_recovered_avg6,group=region, color="#339922"), size = 1 ) +
  geom_line(aes(date,daily_deaths_avg6,group=region,color="black"), size = 1 ) +
  scale_color_identity(
    guide = "legend",
    labels = c("Casos", "Recuperados", "Muertos"),
  ) +
  facet_wrap( ~region) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9),
    # strip.text = element_text( vjust = 0, debug = TRUE ),
    strip.placement = "inside" 
    # legend.position = "bottom"
  ) +
  labs(title = "Nuevos casos, recuperados y fallecidos por día de COVID-19 en España (media 6 días)",
       subtitle = paste0("Por comunidad autónoma (escala lineal). ",period, warning),
       y = "casos registrados",
       x = "fecha",
       caption = caption,
       colour = "")
dev.off()

# Escala logarítmica
png(filename=paste("img/spain/regions/covid19_mix-daily-avg-comunidad-autonoma-log.png", sep = ""),width = 1200,height = 700)
data_all_export %>%   
  ggplot() +
  # geom_line(data = select(data_all_export_sm,date,cases_registered,region_cp,-region),
  #           aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
  geom_line(aes(date,daily_cases_avg,group=region, color="#2222BB"), size = 1 ) +
  geom_line(aes(date,daily_recovered_avg6,group=region, color="#339922"), size = 1 ) +
  geom_line(aes(date,daily_deaths_avg6,group=region,color="black"), size = 1 ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date) ),
                  aes(date + 13,daily_cases_avg, label=paste(format(daily_cases_avg, nsmall=1, big.mark="."))),
                  color="#2222BB",
                  nudge_x = 0, # adjust the starting y position of the text label
                  size=4,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  # segment.size = 0.1,
                  segment.color="#777777"
  ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date) ),
                  aes(date + 11,daily_recovered_avg6, label=paste(format(daily_recovered_avg6, nsmall=1, big.mark="."))),
                  color="#339922",
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=4,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  # segment.size = 0.1,
                  segment.color="#777777"
  ) +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export$date) ),
                  aes(date + 9,daily_deaths_avg6, label=paste(format(daily_deaths_avg6, nsmall=1, big.mark="."))),
                  color="black",
                  nudge_x = 4, # adjust the starting y position of the text label
                  size=4,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  # segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_color_identity(
    guide = "legend",
    labels = c("Casos", "Recuperados", "Muertos"),
  ) +
  scale_y_log10(
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9)
  ) +
  labs(title = "Nuevos casos, recuperados y fallecidos por día de COVID-19 en España (media 6 días)",
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period, warning),
       y = "casos registrados",
       x = "fecha",
       caption = caption,
       colour = "")
dev.off()

# 9. Set 0 day ------------------
# compare_countries with "umbral" or more deceassed accumulated
umbral <- 10

# Select the date when a region had for the first time had n (umbral = n) or more cases
data_all_export_offset_ncases <- data_all_export %>% filter(deceassed >= umbral) %>% group_by(region) %>% arrange(date) %>% 
  filter( row_number()==1 ) %>%
  select(region,date, deceassed) 
data_all_export_offset_ncases <- data_all_export %>% filter(deceassed >= umbral) %>% group_by(region) %>% arrange(date) %>% 
  filter( row_number()==1 ) %>%
  select(region,date, deceassed) %>% mutate(offset = date - min(data_all_export_offset_ncases$date))

test2 <- merge( data_all_export, select(data_all_export_offset_ncases,region,offset ), by = "region" , all.x = TRUE)
# calculate in numer of days since first evaluated, without date
test2$days_since <- as.numeric(test2$date - min(data_all_export_offset_ncases$date) ) - as.numeric(test2$offset)


png(filename=paste("img/spain/regions/covid19_day0_fallecimientos-por-ccaa-acumulados-", umbral ,"-deceased.png", sep = ""),width = 1200,height = 700)
test2  %>% 
  # ptotal <- test %>% filter( country != "France") %>%
  ggplot() +
  geom_line(data =growth_2x, aes(days_since, value), size= 0.5, color = "#555555", linetype = 2 ) +
  geom_line(data =growth_2x, aes(days_since, value3), size= 0.5, color = "#555555", linetype = 2 ) +
  geom_line(data =growth_2x, aes(days_since, value4), size= 0.5, color = "#555555", linetype = 2 ) +
  # geom_line(data =growth_2x, aes(days_since, value5), size= 0.5, color = "#555555", linetype = 2 ) +
  geom_line(aes(days_since, deceassed, group= region, color= region), size= 2, alpha = 0.6 ) +
  # points for interactive
  # geom_point( aes(days_since, deceassed, color= country,
  #                 text = paste0("<b>", region, " (", country, ")</b><br>", format( round(deceassed, digits = 0), big.mark="."), " total deaths" ,"<br>",date, " (", days_since, ")")),
  #             size= 0.6, alpha = 0.6  ) +
  geom_point(
    data = filter(test2,  date == max(test2$date)   ),
             aes(days_since, deceassed, color= region,
                    text = paste0("<b>", region, " (", country, ")</b><br>", format( round(deceassed, digits = 0), big.mark="."), " total deaths" ,"<br>",date, " (", days_since, ")")), 
             size= 2, alpha = 0.6  ) +
  # labels
  geom_text_repel(data = filter(test2,  date == max(test2$date) ),
          aes(days_since, deceassed, label=paste(format( round(deceassed, digits = 0), big.mark="."), region), color= region,),
          # color= "#000000",
          nudge_x = 20, # adjust the starting y position of the text label
          size=6,
          hjust=1,
          # bg.color = "red", bg.r = 0.15,
          family = "Roboto Condensed",
          direction="y",
          segment.size = 0.1,
          segment.color="#333333"
  ) +
  scale_color_manual(values = colors ) +
  coord_cartesian( 
    ylim=c(umbral-1, max(test2[!is.na(test2$deceassed),]$deceassed)*1.3 ),
    xlim=c(0,  65 ) #max(test[!is.na(test$deceassed),]$days_since) + 3
  ) +
  scale_y_log10(
    breaks = c(5,10,20,50,100,200,500,1000,2000,5000),
    # limits = c( umbral,max(test$deceassed)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(  seq(1 , 10, 2), seq(10 , 100, 20), seq(100 , 1000, 200), seq(1000, 10000, 2000) ) ) +
  scale_x_continuous(
    breaks = c(0,5,10,15,20,25,30,35,40,45,50)
    # limits=c( 0, max(test$days_since + 5))
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 20) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    plot.caption = element_text( color="#777777",size = 14, hjust = 1),
    legend.position = "none"
  ) +
  labs(title = paste0("Número de fallecimientos acumulados de COVID-19 registrados (24.04.2020)"),
       subtitle = paste0("Días desde ",umbral ," o más muertes (escala log).",warning),
       y = "fallecimientos registrados (escala log.)",
       x = paste0("días desde ", umbral , " o más fallecimientos"),
       caption = caption ) +
  geom_text(data = growth_2x[1,], aes(20,12000, label=paste0("pendiente: muertes doblan cada 2 días")),
            size = 4, family = "Roboto Condensed", hjust = 1, color = "#555555") +
  geom_text(data = growth_2x[1,], aes(35,12000, label=paste0("... doblan cada 3 días")),
            size = 4, family = "Roboto Condensed", hjust = 1, color = "#555555") +
  geom_text(data = growth_2x[1,], aes(40,12000, label=paste0("... doblan cada 4 días")),
            size = 4, family = "Roboto Condensed", hjust = 0, color = "#555555") 
  # geom_text(data = growth_2x[1,], aes(50,4000, label=paste0("...doblan cada 5 días")),
  #           size = 4, family = "Roboto Condensed", hjust = 0, color = "#555555")
dev.off()


png(filename=paste("img/spain/regions/covid19_day0_fallecimientos-por-ccaa-acumulados-", umbral ,"-deceased_per-cienmil.png", sep = ""),width = 1200,height = 700)
test2  %>%  
  ggplot() +
  geom_line(aes(days_since, deceassed_per_100000, group= region, color= region), size= 2, alpha = 0.6 ) +
  geom_point(data = filter(test2,  date == max(test2$date) ),
             aes(days_since, deceassed_per_100000, color= region,
                 text = paste0("<b>", region, " (", country, ")</b><br>", format( round(deceassed_per_100000, digits = 0), big.mark="."), " total deaths" ,"<br>",date, " (", days_since, ")")), 
             size= 4, alpha = 0.6  ) +
  # labels
  geom_text_repel(data = filter(test2,  date == max(test2$date)  ),
                  aes(days_since, deceassed_per_100000, label=paste(format( round(deceassed_per_100000, digits = 1), big.mark=".", decimal.mark = ","), region), color= region,),
                  # color= "#000000",
                  nudge_x = 15, # adjust the starting y position of the text label
                  size=6,
                  hjust=1,
                  # bg.color = "red", bg.r = 0.15,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_color_manual(values = colors ) +
  coord_cartesian( 
    ylim=c(0.1, max(test2[!is.na(test2$deceassed_per_100000),]$deceassed_per_100000)*1.3 ),
    xlim=c(0,  65 ) #max(test[!is.na(test$deceassed),]$days_since) + 3
  ) +
  scale_x_continuous(
    breaks = c(0,5,10,15,20,25,30,35,40,45,50)
    # limits=c( 0, max(test$days_since + 5))
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 20) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    plot.caption = element_text( color="#777777",size = 14, hjust = 1),
    legend.position = "none"
  ) +
  labs(title = paste0("Número de fallecimientos de COVID-19 acumulados por 100.000 habitantes  (24.04.2020)"),
       subtitle = paste0("Días desde ",umbral ," o más muertes (escala log).",warning),
       y = "fallecimientos registrados (escala log.)",
       x = paste0("días desde ", umbral , " o más fallecimientos"),
       caption = caption ) +
  scale_y_log10(
  # breaks = c(5,10,20,50,100,200,500,1000,2000,5000),
  # limits = c( umbral,max(test$deceassed)),
  labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
  minor_breaks = c(  seq(1 , 10, 2), seq(10 , 100, 20), seq(100 , 1000, 200), seq(1000, 10000, 2000) )
  ) 
dev.off()


png(filename=paste0("img/spain/regions/covid19_muertes-dia-por-ccaa-superpuesto-offset-lineal_since-", umbral ,"deceased.png"), width = 1300,height = 700)
test2  %>% 
  ggplot() +
  geom_line(aes(days_since, daily_deaths_avg6, group= region, color= region), size= 2, alpha = 0.6, se = FALSE ) +
  geom_point(data = filter(test2,  date == max(test2$date)  ),
             aes(days_since, daily_deaths_avg6, color= region, 
                 text = paste0("<b>", region, " (", country, ")</b><br>", format( round(daily_deaths_avg6), big.mark="."), " average daily deaths" ,"<br>",date, " (day ", days_since, ")")), 
             size= 3 ) +
  geom_text_repel(data = filter(test2,  date == max(test2$date)  ),
  aes(days_since, daily_deaths_avg6, label=paste(region), color= region),
  # color= "#000000",
  nudge_x = 7, # adjust the starting y position of the text label
  size=7,
  hjust=1,
  # bg.color = "red", bg.r = 0.15,
  family = "Roboto Condensed",
  direction="y",
  segment.size = 0.3,
  segment.color="#333333"
  ) +
  scale_color_manual(values = colors ) +
  coord_cartesian( 
    ylim=c(1, max(test2[!is.na(test2$daily_deaths_avg6),]$daily_deaths_avg6)*1.15 ),
    xlim=c(0,  60) #max(test[!is.na(test$deceassed),]$days_since) + 3
  ) +
  scale_y_continuous(
    # breaks = c(1,2,5,10,20,50,100,200,500,1000,2000,5000),
    # limits = c( umbral,max(test$deceassed)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    # minor_breaks = c(  seq(1 , 10, 2), seq(10 , 100, 20), seq(100 , 1000, 200), seq(1000, 10000, 2000) ) 
    ) +
  scale_x_continuous(
    breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),
    expand = c(0,0)
    # limits=c( 0, max(test$days_since + 5))
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 20) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    plot.caption = element_text( color="#777777",size = 14, hjust = 1),
    legend.position = "none"
  ) + 
  labs(title = paste0("Media de muertes por día en los 6 días anteriores por COVID-19 (2020.04.24)"),
       subtitle = paste0("Por comunidad autónoma en España. Días desde ",umbral ," o más muertes", warning),
       y = "fallecimientos por día registrados",
       x = paste0("días desde ", umbral , " o más fallecimientos"),
       caption = caption)
dev.off()

png(filename=paste0("img/spain/regions/covid19_muertes-dia-por-ccaa-superpuesto-offset-log_since-", umbral ,"deceased.png"), width = 1300,height = 700)
test2  %>% 
  ggplot() +
  geom_line(aes(days_since, daily_deaths_avg6, group= region, color= region), size= 2, alpha = 0.6, se = FALSE ) +
  geom_point(data = filter(test2,  date == max(test2$date)   ),
             aes(days_since, daily_deaths_avg6, color= region, 
              text = paste0("<b>", region, " (", country, ")</b><br>", format( round(daily_deaths_avg6), big.mark="."), " average daily deaths" ,"<br>",date, " (day ", days_since, ")")), 
             size= 3 ) +
  geom_text_repel(data = filter(test2,  date == max(test2$date)   ),
                          aes(days_since, daily_deaths_avg6, label=paste(region), color= region),
                          # color= "#000000",
                          nudge_x = 7, # adjust the starting y position of the text label
                          size=7,
                          hjust=1,
                          # bg.color = "red", bg.r = 0.15,
                          family = "Roboto Condensed",
                          direction="y",
                          segment.size = 0.3,
                          segment.color="#333333"
  ) +
  scale_color_manual(values = colors ) +
  coord_cartesian( 
    ylim=c(1, max(test2[!is.na(test2$daily_deaths_avg6),]$daily_deaths_avg6)*1.15 ),
    xlim=c(0,  60) #max(test[!is.na(test$deceassed),]$days_since) + 3
  ) +
  scale_y_log10(
    breaks = c(1,2,5,10,20,50,100,200,500,1000,2000,5000),
    # limits = c( umbral,max(test$deceassed)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(  seq(1 , 10, 2), seq(10 , 100, 20), seq(100 , 1000, 200), seq(1000, 10000, 2000) ) ) +
  scale_x_continuous(
    breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),
    expand = c(0,0)
    # limits=c( 0, max(test$days_since + 5))
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 20) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    plot.caption = element_text( color="#777777",size = 14, hjust = 1),
    legend.position = "none"
  ) + 
  labs(title = paste0("Media de muertes por día en los 6 días anteriores por COVID-19 (2020.04.24)"),
                                  subtitle = paste0("Por comunidad autónoma en España. Días desde ",umbral ," o más muertes (escala log).", warning),
                                  y = "fallecimientos por día registrados (escala log.)",
                                  x = paste0("días desde ", umbral , " o más fallecimientos"),
                                  caption = caption)
dev.off()
  
# 10. Scatter polots ------------

# --------- Relaciones --------
png(filename=paste("img/spain/regions/covid19_muertes-vs-casos-provincia-relativo.png", sep = ""),width = 1200,height = 700)
data_all_export %>% filter ( date == max(date) ) %>%
ggplot() +
  # geom_line( aes(cases_per_cienmil,deceassed_per_100000, group=region, color=region), size= 1 ) +
  geom_point(aes(cases_per_100000,deceassed_per_100000*10, color=region), size= 4 ) +
  # lines(x = c(0,0), y = c(20,1000)) +
  # geom_abline(slope = 0.25) +
  # Annotations
  # geom_text(aes(cases_per_cienmil,deceassed_per_100000+0.5, color=deceassed,label=paste( substr(date,7,10 ))), size= 3, color="#000000") +
  geom_text_repel(data=filter( data_all_export, date==max(data_all_export[!is.na(data_all_export$date),]$date)),
                  aes(cases_per_100000,deceassed_per_100000*10, color=region, label=region),
                  nudge_y = 5, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_continuous( 
    breaks = c(200,400,600,800,1000,1200)
    ) +
  scale_x_continuous( 
    # breaks = c(50,100,150,200,250,300,350)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Fallecimientos y casos acumulados COVID-19 en España",
       subtitle = paste0("Por comunidad autónoma",period),
       y = "fallecimientos por 1.000.000 habitantes",
       x = "casos acumulados por 100.000 habitantes",
       caption = caption)
dev.off()