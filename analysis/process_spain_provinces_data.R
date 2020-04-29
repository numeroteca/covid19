# Procesar datos de Coronavirus COVID-19 en España por provincia

# Crea un dataframe que es utilizado en charts_spain_provinces.R para generar los gráficos que pueden verse en lab.montera34.com/covid19
# El data set resultante se publicaa en https://github.com/montera34/escovid19data

# Load libraries -----------
library(tidyverse)
library(reshape2)
  
# Load Data ---------
# / Population -------------
provincias_poblacion <-  read.delim("data/original/spain/provincias-poblacion.csv",sep = ",")

# / COVID-19 in Spain -----------
# / By province -----------
data_cases_sp_provinces <- read.delim("data/original/spain/covid19_spain_provincias.csv",sep = ",")
write.csv(read.delim("data/original/spain/covid19_spain_provincias.csv",sep = ","), file = "../escovid19data/data/original/covid19_spain_provincias.csv", row.names = FALSE)

# Download Andalucía data from https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/operaciones/consulta/anual/38228?CodOper=b3_2314&codConsulta=38228
# that is uploaded manually to our own spreadsheet in google spreadsheet 
andalucia_original <- read.delim("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=andalucia", sep=",")
# TODO: make it work from direct source
# andalucia_original <- read.delim("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=4dcace2a-394f-4e86-9d7b-fede695f0c92&type=3&foto=si&ejecutaDesde=&codConsulta=38228&consTipoVisua=JP", sep=";")

# Process data ------
# Create date variable
data_cases_sp_provinces$date  <- as.Date(data_cases_sp_provinces$date)

# Agreggate Canary islands -------
canarias <- data_cases_sp_provinces %>% filter( ccaa == "Canarias")
names(canarias)
# Group by province
tenerife <- canarias %>% filter(province == "La Gomera" | province =="La Palma" | province == "Tenerife" | province == "El Hierro") %>% group_by(date) %>% summarise(
  province = "Santa, Cruz de Tenerife",
  ccaa = "Canarias",
  new_cases = sum(new_cases),
  activos = sum(activos),
  hospitalized = sum(hospitalized),
  intensive_care = sum(intensive_care),
  deceased = sum(deceased),
  cases_accumulated = sum(cases_accumulated),
  recovered = sum(recovered),
  source_name = "Gobierno de Canarias",
  source = paste(source, collapse = ";"),
  comments = paste(comments, collapse = ";")
)
palmas <- canarias %>% filter(province == "Fuerteventura" | province =="Lanzarote" | province == "Gran Canaria") %>% group_by(date) %>% summarise(
  province = "Palmas, Las",
  ccaa = "Canarias",
  new_cases = sum(new_cases),
  activos = sum(activos),
  hospitalized = sum(hospitalized),
  intensive_care = sum(intensive_care),
  deceased = sum(deceased),
  cases_accumulated = sum(cases_accumulated),
  recovered = sum(recovered),
  source_name = "Gobierno de Canarias",
  source = paste(source, collapse = ";"),
  comments = paste(comments, collapse = ";")
)

# bind Palmas and Tenerife
canarias_bind <- rbind(tenerife,palmas)

# Remove Canarias and adds it as provinces
data_cases_sp_provinces <-  data_cases_sp_provinces %>% filter( ccaa != "Canarias")
# Add Canarias
data_cases_sp_provinces <- rbind(data_cases_sp_provinces,canarias_bind)

# Remove last -usually incomplete- day
data_cases_sp_provinces <- filter(data_cases_sp_provinces, !is.na(date))
data_cases_sp_provinces <- data_cases_sp_provinces %>% filter( date != filter_date) %>% arrange(date)

# Remove existing Andalucia data and add new one from new source ---------------------

# Remove existing Andalucia data
data_cases_sp_provinces <- data_cases_sp_provinces %>% filter( ccaa != "Andalucía")

# prepare format for new Andalucía data
andalucia <- andalucia_original %>% filter( Territorio != "Andalucía" ) %>%
  mutate(
  date = as.Date(Fecha,"%d/%m/%Y"),
  ccaa = "Andalucía"
  ) %>% rename(
    province = Territorio,
    cases_accumulated = Confirmados ,
    hospitalized = Hospitalizados ,
    intensive_care = Total.UCI ,
    deceased = Fallecimientos ,
    recovered = Curados,
    new_cases = Nuevos.casos
  )  %>% mutate (
    activos = NA,
    source_name = "Junta de Andalucía",
    source="https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/operaciones/consulta/anual/38228?CodOper=b3_2314&codConsulta=38228",
    comments=""
  ) %>% select( -Fecha) %>% select( date, province, ccaa, new_cases, activos, hospitalized, intensive_care, deceased, cases_accumulated, recovered, source_name, source, comments) 

# Add new Andalucía data
data_cases_sp_provinces <- rbind(data_cases_sp_provinces,andalucia)

# Remove and add uniprovinciales -----

# Remove existing Uniprovinciales data
data_cases_sp_provinces <- data_cases_sp_provinces %>% 
  filter( !(province == "Melilla" | province == "Asturias" | province == "Balears, Illes" | province == "Cantabria" |
            province == "Ceuta" | province == "Murcia" | province == "Navarra" | province == "Madrid" |
            province == "Rioja, La") )

# import Instituto de Salud CIII 
ciii_original <- read.delim("https://covid19.isciii.es/resources/serie_historica_acumulados.csv",sep = ",")  
write.csv(ciii_original, file = "data/original/spain/iscii_data.csv", row.names = FALSE)

ciii <- ciii_original %>% head(nrow(ciii_original) - 6) %>% #TODO: Cambia el número en función de las notas que incluya el csv original
  ungroup() %>% mutate(
  date = as.Date(FECHA, "%d/%m/%Y" ),
  CCAA = CCAA %>% str_replace_all("AN", "Andalucía"),
  CCAA = CCAA %>% str_replace_all("AR", "Aragón"),
  CCAA = CCAA %>% str_replace_all("AS", "Asturias"),
  CCAA = CCAA %>% str_replace_all("CB", "Cantabria"),
  CCAA = CCAA %>% str_replace_all("CE", "Ceuta"),
  CCAA = CCAA %>% str_replace_all("CL", "Castilla y León"),
  CCAA = CCAA %>% str_replace_all("CM", "Castilla-La Mancha"),
  CCAA = CCAA %>% str_replace_all("CN", "Canarias"),
  CCAA = CCAA %>% str_replace_all("CT", "Cataluña"),
  CCAA = CCAA %>% str_replace_all("EX", "Extremadura"),
  CCAA = CCAA %>% str_replace_all("GA", "Galicia"),
  CCAA = CCAA %>% str_replace_all("IB", "Baleares"),
  CCAA = CCAA %>% str_replace_all("MC", "Murcia"),
  CCAA = CCAA %>% str_replace_all("MD", "Madrid"),
  CCAA = CCAA %>% str_replace_all("ML", "Melilla"),
  CCAA = CCAA %>% str_replace_all("NC", "Navarra"),
  CCAA = CCAA %>% str_replace_all("PV", "País Vasco"),
  CCAA = CCAA %>% str_replace_all("RI", "Rioja, La"),
  CCAA = CCAA %>% str_replace_all("VC", "C. Valenciana")
    ) %>% rename(
    region = CCAA,
    fecha = FECHA,
    cases_registered = CASOS,
    PCR = PCR.,
    TestAc =TestAc.,
    hospitalized = Hospitalizados,
    intensive_care = UCI,
    deceassed = Fallecidos,
    recovered = Recuperados
  )

# Filters and get only uniprovinciales
uniprovinciales <- ciii %>% 
  filter( region == "Melilla" | region == "Asturias" | region == "Baleares" | region == "Cantabria" |
            region == "Ceuta" | region == "Murcia" | region == "Navarra" | region == "Madrid" |
            region == "Rioja, La" ) %>% mutate(
              ccaa = region,
              activos = NA,
              source_name = "Ministerio de Sanidad (Datadista)",
              source="Datadista (Ministerio de Sanidad) https://github.com/datadista/datasets/tree/master/COVID%2019",
              comments= "",
              new_cases = NA,
              deceased = deceassed,
              cases_accumulated = ifelse( is.na(cases_registered), PCR + TestAc, cases_registered)
              ) %>% rename(
              province = region,
              # cases_accumulated = cases_registered,
            ) %>% select( date, province, ccaa, new_cases, activos, hospitalized, intensive_care, deceased, cases_accumulated, recovered, source_name, source,comments) %>%
          mutate(
            ccaa = ccaa %>% str_replace_all("La Rioja", "Rioja, La"),
            province = province %>% str_replace_all("La Rioja", "Rioja, La"),
            ccaa = ccaa %>% str_replace_all("Asturias", "Asturias, Principado de"),
            ccaa = ccaa %>% str_replace_all("Baleares", "Balears, Illes"),
            province = province %>% str_replace_all("Baleares", "Balears, Illes"),
            ccaa = ccaa %>% str_replace_all("Madrid", "Madrid, Comunidad de"),
            ccaa = ccaa %>% str_replace_all("Murcia", "Murcia, Región de"),
            ccaa = ccaa %>% str_replace_all("Navarra", "Navarra, Comunidad Foral de"),
              )

# Add uniprovinciales data
data_cases_sp_provinces <- rbind(data_cases_sp_provinces,uniprovinciales)

# Overwrite Catalunya provinces death data ------------------
powerbi <-  read.delim("data/original/spain/catalunya/powerbi.csv",sep = ",")
powerbi$date <- as.Date(powerbi$fecha, "%d/%m/%y")

data_cases_sp_provinces$dunique <- paste0(data_cases_sp_provinces$date,data_cases_sp_provinces$province)
powerbi$dunique <- paste0(powerbi$date,powerbi$province)

data_cases_sp_provinces <- merge(data_cases_sp_provinces, powerbi %>% select(-date) %>% select(-province) %>% rename(deceased_cat = deceased ) , 
                                 by.x="dunique", by.y="dunique", all.x = TRUE)

data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
  deceased = ifelse( ccaa == "Cataluña", deceased_cat, deceased),
  # add source of cataluña death data
  source_name = ifelse( ccaa == "Cataluña","Generalitat de Catalunya", as.character(source_name) ),
  source = ifelse( ccaa == "Cataluña", paste0(source,";https://app.powerbi.com/view?r=eyJrIjoiZTkyNTcwNjgtNTQ4Yi00ZTg0LTk1OTctNzM3ZGEzNWE4OTIxIiwidCI6IjNiOTQyN2RjLWQzMGUtNDNiYy04YzA2LWZmNzI1MzY3NmZlYyIsImMiOjh9" ), as.character(source) ),
)
# Add missing Barcelona data -------- 
# # Do not use as data have been hasd coded in the original data
# #  select all the cataluña provinces (Barcelona is not available) to calculate how many deaths
# prov_cat <- data_cases_sp_provinces %>% filter( ccaa == "Cataluña" & province != "Barcelona"  & date > as.Date("2020-03-08") ) %>%
#   group_by(date ) %>% summarise( tot_menos_bc = sum(deceased, na.rm = TRUE) )
# prov_cat$date
# # selects dates of DAtadista database for Cataluña region
# catalunya_datadista <- data_all_export %>% filter( region == "Cataluña" & date > as.Date("2020-03-08") &  date < as.Date("2020-04-13") ) %>%
#   select(date,deceassed)
# catalunya_datadista$date
# # insert Cataluña deaths bu Barcelona
# catalunya_datadista$tot_menos_bcn <- prov_cat$tot_menos_bc
# catalunya_datadista <- as.data.frame(catalunya_datadista)
# # calculates Barcelona deaths
# catalunya_datadista$deaths_bcn <- catalunya_datadista$deceassed - catalunya_datadista$tot_menos_bc
# catalunya_datadista$province <- "Barcelona"
# 
# # compare dates to see they are the same
# catalunya_datadista[catalunya_datadista$date > as.Date("2020-03-05") &
#                       catalunya_datadista$date < as.Date("2020-04-13") ,]$date
# data_cases_sp_provinces[  ( data_cases_sp_provinces$date > min(catalunya_datadista$date +2 ) ) &
#                             ( data_cases_sp_provinces$date < as.Date("2020-04-13") ) &
#                             data_cases_sp_provinces$province == "Barcelona", ]$date
# 
# # inserts Barcelona deaths
# data_cases_sp_provinces[  ( data_cases_sp_provinces$date > min(catalunya_datadista$date +2 ) ) &
#                             ( data_cases_sp_provinces$date < as.Date("2020-04-13") ) &
#                             data_cases_sp_provinces$province == "Barcelona", ]$deceased <- catalunya_datadista[catalunya_datadista$date > as.Date("2020-03-05") &
#                                                                                                                  catalunya_datadista$date < as.Date("2020-04-13") ,]$deaths_bcn

# add population data -----
data_cases_sp_provinces <- merge( data_cases_sp_provinces, select(provincias_poblacion,provincia,poblacion,ine_code), by.x = "province", by.y = "provincia", all = TRUE   )

# calculate values per 
data_cases_sp_provinces$cases_per_cienmil <- round( data_cases_sp_provinces$cases_accumulated / data_cases_sp_provinces$poblacion * 100000, digits = 2)
data_cases_sp_provinces$intensive_care_per_1000000 <- round( data_cases_sp_provinces$intensive_care / data_cases_sp_provinces$poblacion * 100000, digits = 2)
data_cases_sp_provinces$deceassed_per_100000 <- round( data_cases_sp_provinces$deceased / data_cases_sp_provinces$poblacion * 100000, digits = 2)
data_cases_sp_provinces$hospitalized_per_100000 <- round( data_cases_sp_provinces$hospitalized / data_cases_sp_provinces$poblacion * 100000, digits = 2)

# Calculates daily deaths
data_cases_sp_provinces <- data_cases_sp_provinces %>% 
  group_by(province) %>% arrange(date) %>% 
  mutate( daily_deaths = deceased - lag(deceased),
          daily_deaths_inc = round((deceased - lag(deceased)) /lag(deceased) * 100, digits = 1),
          daily_deaths_avg3 =  round( ( daily_deaths + lag(daily_deaths,1)+lag(daily_deaths,2) ) / 3, digits = 1 ), # average of daily deaths of 3 last days
          daily_deaths_avg6 =  round( ( daily_deaths + lag(daily_deaths,1)+lag(daily_deaths,2)+
                                          lag(daily_deaths,3)+lag(daily_deaths,4)+lag(daily_deaths,5)+lag(daily_deaths,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
          deaths_cum_last_week = ( deceased + lag(deceased,1) + lag(deceased,2) + lag(deceased,3) + lag(deceased,4) + lag(deceased,5) + lag(deceased,6) ) / 7,  
          deaths_last_week =  daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) + lag(daily_deaths,4) + lag(daily_deaths,5) + lag(daily_deaths,6)  
  )

data_cases_sp_provinces <- data_cases_sp_provinces %>% select(date,province,ine_code,everything()) %>%
                                                  select(-source,-comments,source,comments)

# Re calculates factors to remove things like Andaluc'ia for provinces
data_cases_sp_provinces$province <- factor(data_cases_sp_provinces$province)
data_cases_sp_provinces$ccaa <- factor(data_cases_sp_provinces$ccaa)

write.csv(data_cases_sp_provinces, file = "data/output/spain/covid19-provincias-spain_consolidated.csv", row.names = FALSE)
saveRDS(data_cases_sp_provinces, file = "data/output/spain/covid19-provincias-spain_consolidated.rds")
# saves data in the other repository
write.csv(data_cases_sp_provinces, file = "../escovid19data/data/output/covid19-provincias-spain_consolidated.csv", row.names = FALSE)
saveRDS(data_cases_sp_provinces, file = "../escovid19data/data/output/covid19-provincias-spain_consolidated.rds")