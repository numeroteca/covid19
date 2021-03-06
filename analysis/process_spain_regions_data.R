# Procesa los datos de Coronavirus COVID-19 en España por comunidad autónoma 

# Este script permite importar datosde dos fuentes de datos diferentes: 
# A. Datadista
# B. ISCII
# Produce un dafaframe que es usado en charts_spain_regions.R y otros scripts como evolution_compare.R  

# Load libraries -----------
library(tidyverse)
library(reshape2)

# Load Data ---------
# / Population -------------  
ccaa_poblacion <-  read.delim("data/original/spain/ccaa-poblacion.csv",sep = ";")

# # A Datadista data -----------
# # Data by Ministerio de Sanidad de España (published in PDF format https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm)
# # extracted by Datadista and published in this repository https://github.com/datadista/datasets/tree/master/COVID%2019
# # Spanish data https://github.com/datadista/datasets/tree/master/COVID%2019
# 
# # Load data
# data_cases_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_casos.csv",sep = ",")
# data_uci_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_uci.csv",sep = ",")
# data_death_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_fallecidos.csv",sep = ",")
# data_altas_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_altas.csv",sep = ",")
# data_hosp_original <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_hospitalizados.csv",sep = ",")
# 
# # Process Datadista data ------
# # Change to long format
# # Casos registrados
# data_cases <- melt(data_cases_original, id.vars = c("CCAA", "cod_ine"))
# data_cases$date <- as.Date(substr(data_cases$variable,2,12),"%Y.%m.%d")
# data_cases <- select(data_cases,-variable)
# 
# # add population data
# data_cases <- merge( data_cases, select(ccaa_poblacion,id,poblacion), by.x = "cod_ine", by.y = "id"   )
# # calculate values per
# data_cases$per_cienmil <- round( data_cases$value / data_cases$poblacion * 100000, digits = 2)
# 
# write.csv(data_cases, file = "data/output/covid19-casos-registrados-por-ccaa-espana-por-dia-acumulado.csv", row.names = FALSE)
# 
# # Personas UCI registradas
# data_uci <- melt(data_uci_original, id.vars = c("CCAA","cod_ine"))
# data_uci$date <- as.Date(substr(data_uci$variable,2,12),"%Y.%m.%d")
# data_uci <- select(data_uci,-variable)
# 
# # add population data
# data_uci <- merge( data_uci, data_cases %>% filter (date == as.Date("2020-02-27") ) %>% select(CCAA,poblacion), by.x = "CCAA", by.y = "CCAA" , all.x = TRUE  )
# # calculate values per
# data_uci$per_cienmil <- round( data_uci$value / data_uci$poblacion * 100000, digits = 2)
# 
# write.csv(data_uci, file = "data/output/covid19-ingresos-uci-por-ccaa-espana-por-dia-acumulado.csv", row.names = FALSE)
# 
# # Fallecimientos registrados
# data_death <- melt(data_death_original, id.vars = c("CCAA","cod_ine"))
# data_death$date <- as.Date(substr(data_death$variable,2,12),"%Y.%m.%d")
# data_death <- select(data_death,-variable)
# 
# # add population data
# data_death <- merge( data_death, data_cases %>% filter (date == as.Date("2020-02-27") ) %>% select(CCAA,poblacion), by.x = "CCAA", by.y = "CCAA" , all.x = TRUE  )
# # calculate values per
# data_death$per_cienmil <- round( data_death$value / data_death$poblacion * 100000, digits = 2)
# 
# # Calculates muertes por día
# data_death <- data_death %>% group_by(CCAA) %>%
#   arrange(date) %>% mutate( daily_deaths = value - lag(value),
#                             daily_deaths_inc = round((value - lag(value)) /lag(value) * 100, digits = 1),
#                             daily_deaths_avg6 =  round( ( daily_deaths + lag(daily_deaths,1)+lag(daily_deaths,2)+lag(daily_deaths,3)+lag(daily_deaths,4)+lag(daily_deaths,5) ) / 6, digits = 1 ) # average of dayly deaths of 6 last days
#   )
# 
# write.csv(data_death, file = "data/output/covid19-fallecimientos-por-ccaa-espana-por-dia-acumulado.csv", row.names = FALSE)
# 
# # altas
# data_altas <- melt(data_altas_original, id.vars = c("CCAA","cod_ine"))
# data_altas$date <- as.Date(substr(data_altas$variable,2,12),"%Y.%m.%d")
# data_altas <- select(data_altas,-variable)
# 
# data_altas<- data_altas %>% group_by(CCAA) %>%
#   arrange(date) %>% mutate( daily_altas = value - lag(value),
#                             daily_altas_inc = round((value - lag(value)) /lag(value) * 100, digits = 1),
#                             daily_altas_avg6 =  round( ( daily_altas + lag(daily_altas,1)+lag(daily_altas,2)+lag(daily_altas,3)+lag(daily_altas,4)+lag(daily_altas,5) ) / 6, digits = 1 ) # average of dayly altas of 6 last days
#   )
# 
# 
# # add population data
# data_altas <- merge( data_altas, data_cases %>% filter (date == as.Date("2020-02-27") ) %>% select(CCAA,poblacion), by.x = "CCAA", by.y = "CCAA" , all.x = TRUE  )
# # calculate values per
# data_altas$altas_per_cienmil <- round( data_altas$value / data_altas$poblacion * 100000, digits = 2)
# 
# #  hospitalizados
# data_hosp <- melt(data_hosp_original, id.vars = c("CCAA","cod_ine"))
# data_hosp$date <- as.Date(substr(data_hosp$variable,2,12),"%Y.%m.%d")
# data_hosp <- select(data_hosp,-variable)
# 
# # add population data
# data_hosp <- merge( data_hosp, data_cases %>% filter (date == as.Date("2020-02-27") ) %>% select(CCAA,poblacion), by.x = "CCAA", by.y = "CCAA" , all.x = TRUE  )
# # calculate values per
# data_hosp$hosp_per_cienmil <- round( data_hosp$value / data_hosp$poblacion * 1000000, digits = 2)
# 
# 
# 
# # / join data sets and export --------------
# data_all <- data_cases
# data_all$unique <- paste0(data_all$CCAA,data_all$date)
# colnames(data_all)[3] <- "cases"
# colnames(data_all)[6] <- "cases_per_cienmil"
# 
# data_uci$unique <- paste0(data_uci$CCAA,data_uci$date)
# colnames(data_uci)[3] <- "uci"
# colnames(data_uci)[6] <- "uci_per_cienmil"
# 
# data_all <- merge( data_all, select(data_uci,unique,uci,uci_per_cienmil ), by = "unique", all = TRUE  )
# 
# data_death$unique <- paste0(data_death$CCAA,data_death$date)
# colnames(data_death)[3] <- "death"
# colnames(data_death)[6] <- "death_per_cienmil"
# 
# data_all <- merge( data_all, data_death %>% ungroup() %>% select(unique,death,death_per_cienmil ), by = "unique", all = TRUE  )
# 
# data_altas$unique <- paste0(data_altas$CCAA,data_altas$date)
# colnames(data_altas)[3] <- "altas"
# 
# data_all <- merge( data_all, select(data_altas,unique,altas,altas_per_cienmil ), by = "unique", all = TRUE  )
# 
# data_hosp$unique <- paste0(data_hosp$CCAA,data_hosp$date)
# colnames(data_hosp)[3] <- "hospitalizados"
# 
# data_all <- merge( data_all, select(data_hosp,unique,hospitalizados,hosp_per_cienmil ), by = "unique", all = TRUE  )
# 
# # TODO: fix variables
# data_all_export <- select(data_all,  cod_ine , CCAA, cases, date ,poblacion,
#                           cases_per_cienmil, uci,uci_per_cienmil, death, death_per_cienmil,
#                           altas,altas_per_cienmil, hospitalizados, hosp_per_cienmil )
# 
# names(data_all_export) <- c("region_code" , "region",  "cases_registered" , "date"  ,"population"  , "cases_per_100000", "intensive_care",
#                             "intensive_care_per_100000", "deceassed", "deceassed_per_100000","recovered","recovered_per_100000",
#                             "hospitalized","hospitalized_per_100000")
# 
# data_all_export$country <- "Spain"
# 
# data_all_export <- data_all_export %>% select(date, region_code, region, country, population, cases_registered, cases_per_100000,
#                                               intensive_care, intensive_care_per_100000, deceassed, deceassed_per_100000,
#                                               recovered, recovered_per_100000,hospitalized, hospitalized_per_100000)
# 
# data_all_export <- data_all_export %>% filter(!is.na(region))
# 
# 
# datadista <- data_all_export 
# 
# rm(data_all_export )

# B1. Sustituye ISCII por escovid19data ---------
data_cases_sp_provinces <- readRDS(file = "data/output/spain/covid19-provincias-spain_consolidated.rds")

comaut <-data_cases_sp_provinces %>% group_by(ccaa,date) %>% arrange(date) %>% summarise(
  cases_registered = sum( cases_accumulated ),
  PCR = sum( cases_accumulated_PCR ),
  hospitalized =  sum( hospitalized ),
  intensive_care =  sum( intensive_care ),
  deceassed =  sum( deceased ),
  recovered =  sum( recovered ),
) %>% ungroup() %>% rename(
  CCAA = ccaa
) %>%  mutate(
  CCAA = CCAA %>% str_replace_all("Asturias, Principado de", "Asturias"),
  CCAA = CCAA %>% str_replace_all("Castilla - La Mancha", "Castilla-La Mancha"),
  CCAA = CCAA %>% str_replace_all("Balears, Illes", "Baleares"),
  CCAA = CCAA %>% str_replace_all("Murcia, Región de", "Murcia"),
  CCAA = CCAA %>% str_replace_all("Madrid, Comunidad de", "Madrid"),
  CCAA = CCAA %>% str_replace_all("Navarra, Comunidad Foral de", "Navarra"),
  CCAA = CCAA %>% str_replace_all("Rioja, La", "La Rioja"),
  CCAA = CCAA %>% str_replace_all("Comunitat Valenciana", "C. Valenciana"),
  CCAA = as.factor(CCAA)
) %>% rename(
  region = CCAA
)

ciii <- comaut

# B. ISCiii load Instituto de Salud Carlos III data instead ------------
# import Instituto de Salud CIII 
# ciii_original <- read.delim("https://covid19.isciii.es/resources/serie_historica_acumulados.csv",sep = ",")  

# Comentado porque no se actualiza
# ciii_original <- read.delim("https://cnecovid.isciii.es/covid19/resources/agregados.csv",sep = ",")
# write.csv(ciii_original, file = "data/original/spain/iscii_data.csv", row.names = FALSE)
# 
# # TODO: warning check original footer in ISCII CSV.
# ciii <- ciii_original %>% head(nrow(ciii_original) - 8) %>% #Cambia el número en función de las notas que incluya el csv original
#   ungroup() %>% mutate(
#     date = as.Date(FECHA, "%d/%m/%Y" ),
#     CCAA = CCAA %>% str_replace_all("AN", "Andalucía"),
#     CCAA = CCAA %>% str_replace_all("AR", "Aragón"),
#     CCAA = CCAA %>% str_replace_all("AS", "Asturias"),
#     CCAA = CCAA %>% str_replace_all("CB", "Cantabria"),
#     CCAA = CCAA %>% str_replace_all("CE", "Ceuta"),
#     CCAA = CCAA %>% str_replace_all("CL", "Castilla y León"),
#     CCAA = CCAA %>% str_replace_all("CM", "Castilla-La Mancha"),
#     CCAA = CCAA %>% str_replace_all("CN", "Canarias"),
#     CCAA = CCAA %>% str_replace_all("CT", "Cataluña"),
#     CCAA = CCAA %>% str_replace_all("EX", "Extremadura"),
#     CCAA = CCAA %>% str_replace_all("GA", "Galicia"),
#     CCAA = CCAA %>% str_replace_all("IB", "Baleares"),
#     CCAA = CCAA %>% str_replace_all("MC", "Murcia"),
#     CCAA = CCAA %>% str_replace_all("MD", "Madrid"),
#     CCAA = CCAA %>% str_replace_all("ML", "Melilla"),
#     CCAA = CCAA %>% str_replace_all("NC", "Navarra"),
#     CCAA = CCAA %>% str_replace_all("PV", "País Vasco"),
#     CCAA = CCAA %>% str_replace_all("RI", "La Rioja"),
#     CCAA = CCAA %>% str_replace_all("VC", "C. Valenciana"),
#     Recuperados = NA
#   ) %>% rename(
#     region = CCAA,
#     fecha = FECHA,
#     cases_registered = CASOS,
#     PCR = PCR.,
#     TestAc =TestAc.,
#     hospitalized = Hospitalizados,
#     intensive_care = UCI,
#     deceassed = Fallecidos,
#     recovered = Recuperados
#   ) %>% mutate (
#     cases_registered = ifelse( is.na(cases_registered), PCR + ifelse(is.na(TestAc),0,TestAc ), cases_registered)
#   ) %>% filter ( !is.na(region)  )

# write.csv(ciii, file = "data/output/spain/iscii_processed_data.csv", row.names = FALSE)
# 
# ciii$region <- factor(ciii$region)

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

ciii <- ciii %>% rename( region_code = id, population = poblacion) %>%
  arrange(region, date) %>% select(date, region_code, region, country, population, cases_registered, PCR, TestAc, cases_per_100000,
                                              intensive_care, intensive_care_per_100000, deceassed, deceassed_per_100000,
                                              recovered, recovered_per_100000,hospitalized, hospitalized_per_100000) 

# Export data ------

# write.csv(data_all_export, file = "data/output/covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated.csv", row.names = FALSE)
write.csv(ciii, file = "data/output/covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated_isciii.csv", row.names = FALSE)
  
# Use this to switch to ISCIII data -----
data_all_export <- ciii

# # Add missing deaths previous to March 8 from Datadista database -----------
# data_all_export <- data_all_export %>% mutate (
#   dunique = paste0(region,date)
# )
# 
# # remove date previos to March 8. ISCIII does not have detahs before that day
# datadista <- datadista %>% mutate (
#     date = date-1, # minus one day to correct the date
#     dunique = paste0(region,date)
#   )
# 
# #  merge existing ISCII data with previous data from Datadista
# data_all_export <- merge( data_all_export, 
#                      datadista %>% select(dunique, deceassed, date) %>% filter ( date < as.Date("2020-03-08") ) %>% rename(deceassed_datadista = deceassed) %>% select(-date),
#                      by.x = "dunique", by.y = "dunique", all.x = TRUE   )
# 
# # Fill data if is empty
# data_all_export <- data_all_export %>% mutate(
#   deceassed = ifelse( !is.na(deceassed_datadista), deceassed_datadista, deceassed),
#   source_name =  ifelse( !is.na(deceassed), paste0("Instituto de Salud Carlos III;Ministerio de Sanidad (Datadista)"), "Instituto de Salud Carlos III" ),
#   source =  ifelse( !is.na(deceassed_datadista), paste0("https://cnecovid.isciii.es/covid19/resources/agregados.csv;https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_fallecidos.csv"), "https://cnecovid.isciii.es/covid19/resources/agregados.csv"),
# ) %>% select(-dunique,-deceassed_datadista)
# 
# 
# # Add missing recovered from Datadista database -----------
# data_all_export <- data_all_export %>% mutate (
#   dunique = paste0(region,date)
# )
# 
# datadista <- datadista %>% mutate (
#   date = date-1, # minus one day to correct the date
#   region = region %>% str_replace_all("Castilla La Mancha", "Castilla-La Mancha"),
#   dunique = paste0(region,date)
# )
# 
# #  merge existing ISCII data with previous data from Datadista
# data_all_export <- merge( data_all_export, 
#                           datadista  %>% select(dunique, recovered, date) %>% 
#                             rename(recovered_datadista = recovered) %>% select(-date),
#                           by.x = "dunique", by.y = "dunique", all.x = TRUE   )
# 
# # Fill data if is empty
# data_all_export <- data_all_export %>% mutate(
#   recovered = ifelse( is.na(recovered), recovered_datadista, recovered),
#   source_name =  ifelse( !is.na(recovered_datadista), paste0("Instituto de Salud Carlos III;Instituto de Salud Carlos III;Ministerio de Sanidad (Datadista)"), "Instituto de Salud Carlos III" ),
#   source =  ifelse( !is.na(recovered_datadista), paste0("https://cnecovid.isciii.es/covid19/resources/agregados.csv;https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_fallecidos.csv"), "https://cnecovid.isciii.es/covid19/resources/agregados.csv"),
# ) %>% select(-dunique,-recovered_datadista) %>% mutate(
#   cases_per_100000 = round( cases_registered / population * 100000, digits = 2),
#   deceassed_per_100000 = round( deceassed / population * 100000, digits = 2),
#   recovered_per_100000 = round( recovered / population * 100000, digits = 2),
#   intensive_care_per_100000 = round( intensive_care / population * 100000, digits = 2),
#   hospitalized_per_100000 = round( hospitalized / population * 100000, digits = 2),
# )

# check
# data_all_export %>% select (deceassed,deceassed_datadista,date,region ) %>% filter ( (date < as.Date("2020-03-10")) & (region == "Madrid") )


# Create new variables per day----------------
data_all_export <- data_all_export %>% group_by(region) %>%
    arrange(date) %>% mutate(
                            daily_cases = cases_registered - lag(cases_registered),
                            daily_cases_avg =  round( ( daily_cases + lag(daily_cases,1) + lag(daily_cases,2) + lag(daily_cases,3) + lag(daily_cases,4) + lag(daily_cases,5) ) / 6, digits = 1 ), # average of dayly deaths of 6 last days
                            daily_deaths = deceassed - lag(deceassed),
                            daily_deaths_inc = round((deceassed - lag(deceassed)) / lag(deceassed) * 100, digits = 1),
                            daily_deaths_avg6 =  round( ( daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) + lag(daily_deaths,4) + lag(daily_deaths,5) ) / 6, digits = 1 ), # average of dayly deaths of 6 last days
                            daily_recovered = recovered - lag(recovered),
                            daily_recovered_inc = round((recovered - lag(recovered)) /lag(recovered) * 100, digits = 1),
                            daily_recovered_avg6 =  round( ( daily_recovered + lag(daily_recovered,1)+lag(daily_recovered,2)+
                                                               lag(daily_recovered,3)+lag(daily_recovered,4)+lag(daily_recovered,5)+lag(daily_recovered,6) ) / 7, digits = 1 ), # average of dayly recovered of 6 last days
                            # deaths_cum_last_week = ( deceassed + lag(deceassed,1) + lag(deceassed,2) + lag(deceassed,3) + lag(deceassed,4) + lag(deceassed,5) + lag(deceassed,6) ) / 7,
                            deaths_last_week =  daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) + lag(daily_deaths,4) + lag(daily_deaths,5) + lag(daily_deaths,6),
                            deceassed_per_100000 = round( deceassed / poblacion * 100000, digits = 2) #recalculates relative deaths
                             ) %>% filter( !is.na(date))

saveRDS(data_all_export, file = "data/output/covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated_isciii.rds")

# Export uniprovincial data regions------
# export_uniprovinciales <- data_all_export %>% select(date,region,hospitalized,intensive_care,deceassed,cases_registered, recovered) %>%
#   filter( region == "Melilla" | region == "Asturias" | region == "Baleares" | region == "Cantabria" |
#             region == "Ceuta" |region == "Madride" | region == "Murcia" | region == "Navarra" | region == "Madrid" |
#             region == "La Rioja" )
# 
# write.csv(export_uniprovinciales, file = "data/output/spain/covid19-data-uniprovinciales.csv", row.names = FALSE)

export_uniprovinciales_ciii <- ciii %>% select(date,region,hospitalized,intensive_care,deceassed,cases_registered, recovered) %>%
  filter( region == "Melilla" | region == "Asturias" | region == "Baleares" | region == "Cantabria" |
            region == "Ceuta" |region == "Madride" | region == "Murcia" | region == "Navarra" | region == "Madrid" |
            region == "La Rioja" )

write.csv(export_uniprovinciales_ciii, file = "data/output/spain/covid19-data-uniprovinciales_isciii.csv", row.names = FALSE)

rm(datadista,data_all,data_altas,data_altas_original,data_cases,data_cases_original,data_death, 
   data_death_original, data_hosp, data_hosp_original, data_uci, data_uci_original)
rm(export_uniprovinciales_ciii,ccaa_poblacion)
