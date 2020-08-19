# Procesar datos de Coronavirus COVID-19 en España por provincia

# Crea un dataframe que es utilizado en charts_spain_provinces.R para generar los gráficos que pueden verse en lab.montera34.com/covid19
# El data set resultante se publicaa en https://github.com/montera34/escovid19data

# A. Load libraries -----------
library(tidyverse)
library(reshape2)
library(readxl)
library(openxlsx)
library(zoo)

# B. Load Data ---------
# / Population INE-------------
provincias_poblacion <-  read.delim("data/original/spain/provincias-poblacion.csv",sep = ",")

# Manually download:
# Andalucía, Asturias, Cantabria data. 

# COVID-19 in Spain
# / By province -----------
# donwload provincias data from googgle spreadsheet 
download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=provincias", 
            "data/original/spain/covid19_spain_provincias.csv")
# save in another repository # save file in another repository (comment this line!)
write.csv(read.delim("data/original/spain/covid19_spain_provincias.csv",sep = ","), file = "../escovid19data/data/original/covid19_spain_provincias.csv", row.names = FALSE)
# load data
data_cases_sp_provinces <- read.delim("data/original/spain/covid19_spain_provincias.csv",sep = ",")

# C. Process data ------
# Create date variable
data_cases_sp_provinces$date  <- as.Date(data_cases_sp_provinces$date)
names_original <- names(data_cases_sp_provinces)

# Canarias: Agreggate Canary islands -------
# save all the islands data
write.csv(data_cases_sp_provinces %>% filter( ccaa == "Canarias"), file = "data/original/spain/canarias/canarias.csv", row.names = FALSE)

# Group by province
tenerife <- data_cases_sp_provinces %>% filter(province == "La Gomera" | province =="La Palma" | province == "Tenerife" | province == "El Hierro") %>% group_by(date) %>% summarise(
  province = "Santa Cruz de Tenerife",
  ccaa = "Canarias",
  new_cases = sum(new_cases),
  PCR = sum(PCR),
  TestAc = sum(TestAc),
  activos = sum(activos),
  hospitalized = sum(hospitalized),
  intensive_care = sum(intensive_care),
  deceased = sum(deceased),
  cases_accumulated = sum(cases_accumulated),
  cases_accumulated_PCR = sum(cases_accumulated), # porque todos los casos son PCR+
  recovered = sum(recovered),
  source_name = "Gobierno de Canarias",
  source = paste(source, collapse = ";"), #TODO eavoir repetition
  comments = paste(comments, collapse = ";")
)
palmas <- data_cases_sp_provinces %>% filter(province == "Fuerteventura" | province =="Lanzarote" | province == "Gran Canaria") %>% group_by(date) %>% summarise(
  province = "Palmas, Las",
  ccaa = "Canarias",
  new_cases = sum(new_cases),
  PCR = sum(PCR),
  TestAc = sum(TestAc),
  activos = sum(activos),
  hospitalized = sum(hospitalized),
  intensive_care = sum(intensive_care),
  deceased = sum(deceased),
  cases_accumulated = sum(cases_accumulated),
  cases_accumulated_PCR = sum(cases_accumulated), # porque todos los casos son PCR+
  recovered = sum(recovered),
  source_name = "Gobierno de Canarias",
  source = paste(source, collapse = ";"), #TODO eavoir repetition
  comments = paste(comments, collapse = ";")
)

# bind Palmas and Tenerife
canarias_bind <- rbind(tenerife,palmas)

# Remove Canarias and adds it as provinces  
data_cases_sp_provinces <-  data_cases_sp_provinces %>% filter( ccaa != "Canarias")
# Add Canarias
data_cases_sp_provinces <- rbind(data_cases_sp_provinces,canarias_bind)

rm(tenerife,palmas,canarias_bind)

# Remove last -usually incomplete- day
data_cases_sp_provinces <- filter(data_cases_sp_provinces, !is.na(date))

# Castilla y León: Remove existing Castilla y León data and add new one from new source ---------------------
download.file("https://analisis.datosabiertos.jcyl.es/explore/dataset/situacion-epidemiologica-coronavirus-en-castilla-y-leon/download/?format=csv&timezone=Europe/Madrid&lang=en&use_labels_for_header=true&csv_separator=%3B", 
              "data/original/spain/cyl/covid19_cyl_a.csv")
download.file("https://analisis.datosabiertos.jcyl.es/explore/dataset/situacion-de-hospitalizados-por-coronavirus-en-castilla-y-leon/download/?format=csv&timezone=Europe/Madrid&lang=en&use_labels_for_header=true&csv_separator=%3B", 
              "data/original/spain/cyl/covid19_cyl_b.csv")
download.file("https://analisis.datosabiertos.jcyl.es/explore/dataset/pruebas-realizados-coronavirus/download/?format=csv&timezone=Europe/Madrid&lang=en&use_labels_for_header=true&csv_separator=%3B", 
              "data/original/spain/cyl/covid19_cyl_pruebas-realizados-coronavirus.csv")
# TODO PCR+: Desde el 18 de mayo 2020, los nuevos casos confirmados solo reflejan resultados de PCR. 
# Fuente https://datosabiertos.jcyl.es/web/jcyl/set/es/salud/situacion-epidemiologica-coronavirus/1284940407131
download.file("https://datosabiertos.jcyl.es/web/jcyl/risp/es/salud/situacion-epidemiologica-coronavirus/1284940407131.csv", 
              "data/original/spain/cyl/situacion-epidemiologica.csv")


cyla_original <- read.delim("data/original/spain/cyl/covid19_cyl_a.csv", sep=";")
cylb_original <- read.delim("data/original/spain/cyl/covid19_cyl_b.csv", sep=";")

# TODO PCR+
cylc_original <- read.delim("data/original/spain/cyl/covid19_cyl_pruebas-realizados-coronavirus.csv", sep=";")
cyld_original <- read.delim("data/original/spain/cyl/situacion-epidemiologica.csv", sep=";")

# Remove existing CyL data
data_cases_sp_provinces <- data_cases_sp_provinces %>% filter( ccaa != "Castilla y León" )

cyla <- cyla_original %>%
  mutate(
    date = as.Date(fecha,"%Y-%m-%d"),
    ccaa = "Castilla y León"
  ) %>% rename(
    province = provincia,
    cases_temp = casos_confirmados,
    deceased = fallecimientos ,
    recovered = altas,
    new_cases = nuevos_positivos,
  )  %>% mutate (
    activos = NA,
    intensive_care = NA,
    hospitalized = NA,
    source_name = "Junta de Castilla y León",
    source = "https://analisis.datosabiertos.jcyl.es/explore/dataset/situacion-epidemiologica-coronavirus-en-castilla-y-leon/download/?format=csv&timezone=Europe/Madrid&lang=en&use_labels_for_header=true&csv_separator=%3B",
    PCR = NA,
    TestAc = NA,
    cases_accumulated = ifelse( date > as.Date("2020-05-17"), NA, cases_temp),
    cases_accumulated_PCR = ifelse( date > as.Date("2020-05-17"), cases_temp, NA),
    PCR_14days = NA,
    comments=""
  ) %>% select( date, province, ccaa, new_cases, PCR, TestAc, activos, hospitalized, intensive_care, deceased, cases_accumulated, 
                cases_accumulated_PCR, recovered, source_name, source, comments)  %>% 
  mutate(
    dunique = paste0(date,province)
  )

cylb <- cylb_original %>% 
  mutate(
    date = as.Date(fecha,"%Y-%m-%d")
  ) %>% group_by(date,provincia) %>% 
  summarise(
    nuevos_hospitalizados_planta = sum(nuevos_hospitalizados_planta),
    hospitalizados_planta = sum(hospitalizados_planta),
    nuevos_hospitalizados_uci = sum(nuevos_hospitalizados_uci),
    hospitalizados_uci = sum(hospitalizados_uci),
    nuevas_altas = sum(nuevas_altas),
    altas = sum(altas),
    nuevos_fallecimientos = sum(nuevos_fallecimientos),
    fallecimientos = sum(fallecimientos)
  ) %>% rename(
    province = provincia,
    deceased_hospital = fallecimientos ,
    recovered = altas,
    hospitalized = hospitalizados_planta,
    intensive_care = hospitalizados_uci
  )  %>% mutate (
    ccaa = "Castilla y León",
    activos = NA,
    cases_accumulated = NA,
    new_cases = NA,
    source_name = "Junta de Castilla y León",
    source = "https://analisis.datosabiertos.jcyl.es/explore/dataset/situacion-de-hospitalizados-por-coronavirus-en-castilla-y-leon/download/?format=csv&timezone=Europe/Madrid&lang=en&use_labels_for_header=true&csv_separator=%3B",
    PCR = NA,
    TestAc = NA,
    cases_accumulated_PCR = NA,
    PCR_14days = NA,
    comments=""
  ) %>% select( date, province, ccaa, new_cases, PCR, TestAc, activos, hospitalized, intensive_care, deceased_hospital, 
                cases_accumulated, cases_accumulated_PCR, recovered, source_name, source, comments) %>% 
  mutate(
    dunique = paste0(date,province)
  )

# Parece que los daos de fallecidos y altas son iguales en ambas bases de datos

# Merge cyla y cylb
cyl <-  merge(cyla %>% select(-hospitalized,-intensive_care), 
              cylb %>% ungroup() %>%  select(dunique,hospitalized,intensive_care,source) %>% rename(source_b = source) , 
              by.x="dunique", by.y="dunique", all = TRUE) %>% select( -dunique) %>% mutate (
                  source = paste0(source,";",source_b) 
                  ) %>% select(-source_b)
                
# Add new CyL data
data_cases_sp_provinces <- rbind(data_cases_sp_provinces, 
                                 cyl)

rm(cyl,cyla,cylb,cyla_original,cylb_original, cylc_original)

# Comunidad Valenciana ------------------
# Download C. Valenciana  data from https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/operaciones/consulta/anual/38228?CodOper=b3_2314&codConsulta=38228
# that is uploaded manually to our own spreadsheet in google spreadsheet 
download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=cvalenciana", 
              "data/original/spain/valencia/valencia.csv")
valencia <- read.delim("data/original/spain/valencia/valencia.csv", sep=",")

data_cases_sp_provinces <- data_cases_sp_provinces %>% filter( ccaa != "Comunitat Valenciana")
data_cases_sp_provinces <- rbind(
  data_cases_sp_provinces,
  valencia
)

# Andalucía: Remove existing Andalucia data and add new one from new source ---------------------

# Download Andalucía data from https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/operaciones/consulta/anual/38228?CodOper=b3_2314&codConsulta=38228
# that is uploaded manually to our own spreadsheet in google spreadsheet 
download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=andalucia", 
              "data/original/spain/andalucia/andalucia.csv")
andalucia_original <- read.delim("data/original/spain/andalucia/andalucia.csv", sep=",")

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
    new_cases = Nuevos.casos,
    cases_accumulated_PCR = Confirmados.PCR,
    PCR_14days = Confirmados.PCR..14dias,
  )  %>% mutate (
    activos = NA,
    source_name = "Junta de Andalucía",
    PCR = NA,
    TestAc = NA,
    comments=""
  ) %>% select( -Fecha) %>% select( date, province, ccaa, new_cases, PCR, TestAc, activos, hospitalized, intensive_care, deceased, cases_accumulated, cases_accumulated_PCR, recovered, source_name, source, comments) 


# Andalucía 2 ------------
# TODO: hasta que funcione usar el nuevom da problemas de 'Peer certificate cannot be authenticated with given CA certificates'
# Initial file
# download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=0b200bc6-3e8a-4991-a9ad-848e31c6cc69&type=3&foto=si&ejecutaDesde=&codConsulta=39464&consTipoVisua=JP",
# then it changed to this one:
download.file("https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=e31f8668-049c-4c17-a879-e097e9b3dfc8&type=3&foto=si&ejecutaDesde=&codConsulta=38228&consTipoVisua=JP",
              "data/original/spain/andalucia/andalucia-instituto-estadistica-cartografia.csv")

andalucia_original2 <- read.delim("data/original/spain/andalucia/andalucia-instituto-estadistica-cartografia.csv", sep=";") %>% select(-X) %>% filter( !(Territorio  == "") )
andalucia2 <- spread(andalucia_original2, Medida, Valor) #from long to wide format

andalucia2 <- andalucia2 %>% filter( Territorio != "Andalucía" ) %>%
  mutate(
    date = as.Date(Fecha,"%d/%m/%Y"),
    ccaa = "Andalucía"
  ) %>% rename(
    province = Territorio,
    # cases_accumulated = Confirmados ,
    hospitalized = Hospitalizados ,
    intensive_care = "Total UCI" ,
    deceased = Fallecidos,
    recovered = Curados,
    # new_cases = Nuevos.casos,
    cases_accumulated_PCR = "Confirmados PCR",
    # PCR_14days = Confirmados.PCR.14.dias
  )  %>% mutate (
    cases_accumulated = NA,
    new_cases = NA,
    activos = NA,
    source_name = "Junta de Andalucía",
    PCR = NA,
    TestAc = NA,
    comments="",
    source = "https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=0b200bc6-3e8a-4991-a9ad-848e31c6cc69&type=3&foto=si&ejecutaDesde=&codConsulta=39464&consTipoVisua=JP"
  ) %>% select( -Fecha) %>% select( date, province, ccaa, new_cases, PCR, TestAc, activos, hospitalized, 
                                    intensive_care, deceased, cases_accumulated, cases_accumulated_PCR, 
                                    recovered, source_name, source, comments) %>% filter(date > as.Date("2020-07-24"))

# Add new Andalucía data
data_cases_sp_provinces <- rbind(data_cases_sp_provinces,andalucia,andalucia2) 

rm(andalucia,andalucia_original)


# Andalucia hospitalizados --------
download.file("https://github.com/montera34/escovid19data/raw/master/data/original/andalucia-hospitalizados.csv", 
              "data/original/spain/andalucia/andalucia-hospitalizados.csv")
andalucia_hosp <- read.delim("data/original/spain/andalucia/andalucia-hospitalizados.csv", sep=",")

andalucia_hosp <- andalucia_hosp %>% 
  mutate(
    date_and = as.Date(Fecha.report,"%d/%m/%Y"),
    ccaa_and = "Andalucía"
  ) %>% rename(
    province_and = provincia,
    hospitalized_and = Hospitalizados ,
    intensive_care_and = UCI,
  )  %>% mutate (
    source_name_and  = "Junta de Andalucía",
    comments_and="Usa este script para hospitalizados https://github.com/montera34/escovid19data/blob/master/analysis/descarga_andalucia.py",
    source_and = "https://github.com/montera34/escovid19data/raw/master/data/original/andalucia-hospitalizados.csv"
  )

data_cases_sp_provinces <- merge( data_cases_sp_provinces %>% mutate ( dunique = paste0( date, province) ) %>% ungroup(),
# zzz <- merge( data_cases_sp_provinces %>% select( names(read.delim("data/original/spain/covid19_spain_provincias.csv",sep = ",")) ) %>% mutate ( dunique = paste0( date, province) ) %>% ungroup(),
              andalucia_hosp  %>% mutate ( dunique = paste0( date_and, province_and) ) %>% ungroup() %>% 
                                    select(dunique, date_and, ccaa_and, province_and, hospitalized_and, intensive_care_and, source_name_and, comments_and, source_and) , 
                                  by.x="dunique", by.y="dunique", all = TRUE) %>% select(-dunique) %>% mutate(
                                    # add hospitalized data for existing dates and provinces
                                    hospitalized = ifelse(ccaa=="Andalucía", hospitalized_and ,hospitalized),
                                    intensive_care = ifelse(ccaa=="Andalucía",intensive_care_and, intensive_care),
                                    source = as.character(source),
                                    source = ifelse( ccaa_and == "Andalucía", paste0(source,";",source_and), source),
                                    source_name = as.character(source_name),
                                    source_name = ifelse( ccaa_and == "Andalucía", paste0(source_name,";",source_name_and), source_name),
                                    comments = as.character(comments),
                                    comments = ifelse( ccaa_and == "Andalucía", 
                                                       paste0( ifelse(is.na(comments), paste0(comments,";"),""),comments_and), 
                                             comments),
                                    # add hospitalized data for non existing dates and provinces (on weekends)
                                    date= as.character(date),
                                    date = ifelse( ccaa_and == "Andalucía" & is.na(ccaa), as.character(date_and), date ),
                                    date = as.Date(date,"%Y-%m-%d"),
                                    hospitalized = ifelse( ccaa_and == "Andalucía" & is.na(ccaa), hospitalized_and, hospitalized),
                                    intensive_care = ifelse( ccaa_and == "Andalucía" & is.na(ccaa), intensive_care_and, intensive_care),
                                    ccaa = as.character(ccaa),
                                    ccaa = ifelse( ccaa_and == "Andalucía" & is.na(ccaa), as.character(ccaa_and), ccaa ),
                                    ccaa = as.factor(ccaa),
                                    province = as.character(province),
                                    province = ifelse( ccaa_and == "Andalucía" & is.na(province), as.character(province_and), province ),
                                    province = as.factor(province)
                                  ) %>% select( names(read.delim("data/original/spain/covid19_spain_provincias.csv",sep = ",")) )

# País Vasco --------------
download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=pais-vasco", 
              "data/original/spain/euskadi/pais-vasco.csv")
euskadi <- read.delim("data/original/spain/euskadi/pais-vasco.csv", sep=",") %>% select(-PCR_diario_residencia_fuera_de_euskadi)

data_cases_sp_provinces <- rbind(data_cases_sp_provinces,euskadi) 

# Galicia: Remove existing Galicia data and add new one from new source ---------------------
# Remove existing Galicia data
data_cases_sp_provinces <- data_cases_sp_provinces %>% filter( ccaa != "Galicia")

download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=galicia",
              "data/original/spain/galicia/galicia-provincias-data.csv")
galicia_original <- read.delim("data/original/spain/galicia/galicia-provincias-data.csv", sep=",")

galicia <- galicia_original %>%
  mutate(
    date = as.Date(date)
  )

# Add old Galicia data cumulative cases 
# # TODO: add sources of added data
data_cases_sp_provinces <- rbind(data_cases_sp_provinces,galicia %>% select(-X,-casos_acumul_old,-recovered_old,-deceased_old))
rm(galicia,galicia_original)

galicia_cumulative <- read.delim("data/original/spain/covid19_spain_provincias.csv",sep = ",") %>% filter( province == "Coruña, A"  | province =="Lugo" |province =="Pontevedra") %>%
  mutate(
    dunique = paste0(date,province)
  ) %>%
  select( dunique, cases_accumulated ) %>% 
  rename( cases_accumulated_gal = cases_accumulated )

data_cases_sp_provinces <- merge(data_cases_sp_provinces %>% mutate( dunique = paste0(date,province) ), 
                                 galicia_cumulative, 
                                 by.x="dunique", by.y="dunique", all = TRUE) %>% 
  mutate(
    cases_accumulated = ifelse( (province == "Coruña, A"  | province =="Lugo" |province =="Pontevedra"),cases_accumulated_gal ,cases_accumulated )
  ) %>% select(-dunique,-cases_accumulated_gal)

# Galicia: Ourense data --------
download.file("https://github.com/lipido/galicia-covid19/raw/master/ourense.csv",
              "data/original/spain/galicia/ourense.csv")
ourense_original <- read.delim("data/original/spain/galicia/ourense.csv", sep=",")
download.file("https://github.com/lipido/galicia-covid19/raw/master/ourense.ext.csv",
              "data/original/spain/galicia/ourense.ext.csv")
ourense_hosp <- read.delim("data/original/spain/galicia/ourense.ext.csv", sep=",")

ourense_a <- ourense_original %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% rename (
    cases_accumulated_our = Ourense.casos.acum,
    recovered_our = Ourense.altas.acum,
    deceased_our = Ourense.fallecidos.acum
  )

ourense_b <- ourense_hosp %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% group_by(date) %>% summarise (
    hospitalized_our = Ourense.hospitalizados_HPCHUO + Ourense.hospitalizados_Carmen + Ourense.hospitalizados_COSAGA +
      Ourense.hospitalizados_HPValdeorras + Ourense.hospitalizados_HPVerin,
    intensive_care_our = Ourense.uci_HPCHUO + Ourense.uci_COSAGA,
    cases_accumulated_test_PCR_our = Ourense.PCR.acum
  )

ourense <- merge( ourense_a,
                  ourense_b,
                  by.x = "date", by = "date") %>% mutate(
                    province = "Ourense"
                  )
write.csv(ourense, file = "data/output/spain/galicia/ourense.csv", row.names = FALSE)

data_cases_sp_provinces <- merge( data_cases_sp_provinces %>% mutate( dunique = paste0(date,province) ),
                                  ourense  %>% mutate( dunique = paste0(date,province) ) %>% rename( date_our = date, province_our = province ) %>% 
                                    select( - cases_accumulated_test_PCR_our, -Fecha ),
                                  by.x = "dunique" , by.y =  "dunique", all = TRUE
)
data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
  # no sé por qué pero he tenido que montar este lío para que las fechas funcionaran
  date_new = ifelse( province_our == "Ourense", date_our, NA ),
  date_new = as.Date(date_our, origin=as.Date("1970-01-01") ),
  date = ifelse( province_our == "Ourense" & !is.na(province_our), date_new, date ),
  date = as.Date(date, origin=as.Date("1970-01-01") ),
  
  province_new = ifelse( province_our == "Ourense" & !is.na(province_our), province_our, as.character(province) ),
  ccaa_new = ifelse(  province_our == "Ourense" & !is.na(province_our), "Galicia", as.character(ccaa) ),
  
  province = province_new,
  ccaa = ccaa_new,
  # date = ifelse( is.na(date) & province_our == "Ourense", as.Date(date_our), NA),
  # province  = ifelse( province_our == "Ourense", "Ourense", province),
  # ccaa  = ifelse(  province_our == "Ourense", "Galicia", ccaa),
  
  deceased =    ifelse( province == "Ourense" , deceased_our, deceased),
  hospitalized = ifelse( province == "Ourense", hospitalized_our, hospitalized),
  intensive_care = ifelse( province == "Ourense", intensive_care_our, intensive_care),
  cases_accumulated = ifelse( province == "Ourense", cases_accumulated_our, cases_accumulated),
  # cases_accumulated_PCR = ifelse( province == "Ourense", cases_accumulated_PCR_our, cases_accumulated_PCR),
  recovered = ifelse( province == "Ourense", recovered_our, recovered),
  source_name = ifelse( province == "Ourense", paste0(as.character(source_name),";Área sanitaria Ourense via @lipido"), as.character(source_name) ),
  source = ifelse(province == "Ourense", 
                  paste0(source,";https://github.com/lipido/galicia-covid19" ), 
                  as.character(source) )
) %>% select(-hospitalized_our, -intensive_care_our, -deceased_our, -date_our, -province_our, -date_new, -cases_accumulated_our,
             -recovered_our, -province_new, -ccaa_new, -dunique)

rm(ourense, ourense_a, ourense_b, ourense_hosp, ourense_original)

# Galicia: las otras provincias ------------
# Lugo
# Ourense
# Santiago + Ferrol + Coruña = A Coruña; 
# Vigo + Pontevedra = Pontevedra; 

# / Lugo ------
download.file("https://github.com/lipido/galicia-covid19/raw/master/lugo.csv",
              "data/original/spain/galicia/lugo.csv")
lugo_original <- read.delim("data/original/spain/galicia/lugo.csv", sep=",")
download.file("https://github.com/lipido/galicia-covid19/raw/master/lugo.ext.csv",
              "data/original/spain/galicia/lugo.ext.csv")
lugo_hosp <- read.delim("data/original/spain/galicia/lugo.ext.csv", sep=",")

lugo_a <- lugo_original %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% rename (
    cases_accumulated = Lugo.casos.acum, #solo para después de 25 de mayo 2020 Casos totales hasta la fecha. Los casos son con PCR positiva a partir del 25 de mayo con total seguridad. Antes de esta fecha se incluían casos diagnosticados clínicamente con posterior test positivo de anticuerpos. Podrían ser unos 200 casos y concentrados al principio de la serie.
    recovered = Lugo.altas.acum,
    deceased = Lugo.fallecidos.acum
  )

lugo_b <- lugo_hosp %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% rename(
    hospitalized = Lugos.hospitalizados, # TODO advertir del error en el nombre de columna
    intensive_care = Lugo.uci,
    cases_accumulated_test_PCR = Lugo.PCR.acum
  )

lugo <- merge( lugo_a %>% select(-Fecha),
               lugo_b %>% select(-Fecha),
                  by.x = "date", by = "date") %>% mutate(
                    province = "Lugo",
                    ccaa = "Galicia",
                    new_cases = NA,
                    PCR = NA,
                    TestAc = NA,
                    activos = NA,
                    cases_accumulated_PCR = NA,
                    recovered = NA,
                    source_name = "Área sanitaria Ourense via @lipido",
                    source = "https://github.com/lipido/galicia-covid19/blob/master/lugo.csv;https://github.com/lipido/galicia-covid19/blob/master/lugo.ext.csv",
                    comments = NA,
                  ) %>% filter ( date > as.Date("2020-06-03") ) %>% # TODO, change date when more data are gathered
  select( date, province, ccaa, new_cases, PCR, TestAc,              
          activos, hospitalized,intensive_care,  deceased, cases_accumulated, cases_accumulated_PCR, recovered, source_name, source, comments )

write.csv( lugo, file = "data/output/spain/galicia/lugo.csv", row.names = FALSE)

data_cases_sp_provinces <- rbind(data_cases_sp_provinces, lugo)


# / Pontevedra = Vigo + Pontevedra  ---------

# Vigo
download.file("https://github.com/lipido/galicia-covid19/raw/master/vigo.csv",
              "data/original/spain/galicia/vigo.csv")
vigo_original <- read.delim("data/original/spain/galicia/vigo.csv", sep=",")
download.file("https://github.com/lipido/galicia-covid19/raw/master/vigo.ext.csv",
              "data/original/spain/galicia/vigo.ext.csv")
vigo_hosp <- read.delim("data/original/spain/galicia/vigo.ext.csv", sep=",")

vigo_a <- vigo_original %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% rename (
    cases_accumulated = Vigo.casos.acum, #solo para después de 25 de mayo 2020 Casos totales hasta la fecha. Los casos son con PCR positiva a partir del 25 de mayo con total seguridad. Antes de esta fecha se incluían casos diagnosticados clínicamente con posterior test positivo de anticuerpos. Podrían ser unos 200 casos y concentrados al principio de la serie.
    recovered = Vigo.altas.acum,
    deceased = Vigo.fallecidos.acum
  )

vigo_b <- vigo_hosp %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% rename(
    hospitalized = Vigo.hospitalizados, # TODO advertir del error en el nombre de columna
    intensive_care = Vigo.uci,
    cases_accumulated_test_PCR = Vigo.PCR.acum
  )

vigo <- merge( vigo_a %>% select(-Fecha),
               vigo_b %>% select(-Fecha),
               by.x = "date", by = "date") %>% mutate(
                 province = "Vigo",
                 ccaa = "Galicia",
                 new_cases = NA,
                 PCR = NA,
                 TestAc = NA,
                 activos = NA,
                 cases_accumulated_PCR = NA,
                 recovered = NA,
                 source_name = "Área sanitaria Ourense via @lipido",
                 source = "https://github.com/lipido/galicia-covid19/blob/master/vigo.csv;https://github.com/lipido/galicia-covid19/blob/master/vigo.ext.csv",
                 comments = NA,
               ) %>% filter ( date > as.Date("2020-06-03") ) %>% # TODO, change date when more data are gathered
  select( date, province, ccaa, new_cases,   PCR, TestAc,              
          activos, hospitalized,intensive_care,  deceased, cases_accumulated, cases_accumulated_PCR, recovered, source_name, source, comments )

write.csv( vigo, file = "data/output/spain/galicia/vigo_area.csv", row.names = FALSE)

# Pontevedra
download.file("https://github.com/lipido/galicia-covid19/raw/master/pontevedra.csv",
              "data/original/spain/galicia/pontevedra.csv")
pontevedra_original <- read.delim("data/original/spain/galicia/pontevedra.csv", sep=",")
download.file("https://github.com/lipido/galicia-covid19/raw/master/pontevedra.ext.csv",
              "data/original/spain/galicia/pontevedra.ext.csv")
pontevedra_hosp <- read.delim("data/original/spain/galicia/pontevedra.ext.csv", sep=",")

pontevedra_a <- pontevedra_original %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% rename (
    cases_accumulated = Pontevedra.casos.acum, #solo para después de 25 de mayo 2020 Casos totales hasta la fecha. Los casos son con PCR positiva a partir del 25 de mayo con total seguridad. Antes de esta fecha se incluían casos diagnosticados clínicamente con posterior test positivo de anticuerpos. Podrían ser unos 200 casos y concentrados al principio de la serie.
    recovered = Pontevedra.altas.acum,
    deceased = Pontevedra.fallecidos.acum
  )

pontevedra_b <- pontevedra_hosp %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% rename(
    hospitalized = Pontevedra.hospitalizados, # TODO advertir del error en el nombre de columna
    intensive_care = Pontevedra.uci,
    cases_accumulated_test_PCR = Pontevedra.PCR.acum
  )

pontevedra <- merge( pontevedra_a %>% select(-Fecha),
               pontevedra_b %>% select(-Fecha),
               by.x = "date", by = "date") %>% mutate(
                 province = "Pontevedra",
                 ccaa = "Galicia",
                 new_cases = NA,
                 PCR = NA,
                 TestAc = NA,
                 activos = NA,
                 cases_accumulated_PCR = NA,
                 recovered = NA,
                 source_name = "Área sanitaria Ourense via @lipido",
                 source = "https://github.com/lipido/galicia-covid19/blob/master/pontevedra.csv;https://github.com/lipido/galicia-covid19/blob/master/pontevedra.ext.csv",
                 comments = NA,
               ) %>% filter ( date > as.Date("2020-06-03") ) %>% # TODO, change date when more data are gathered
  select( date, province, ccaa, new_cases,   PCR, TestAc,              
          activos, hospitalized,intensive_care,  deceased, cases_accumulated, cases_accumulated_PCR, recovered, source_name, source, comments )

write.csv( pontevedra, file = "data/output/spain/galicia/pontevedra_area.csv", row.names = FALSE)

pontevedra_prov <- rbind(vigo, pontevedra) %>%  group_by(date) %>% summarise (
  hospitalized = sum(hospitalized),
  intensive_care = sum(intensive_care),
  deceased = sum(deceased),
  cases_accumulated = sum(cases_accumulated),
  province = "Pontevedra",
  ccaa = "Galicia",
  new_cases = NA,
  PCR = NA,
  TestAc = NA,
  activos = NA,
  cases_accumulated_PCR = NA,
  recovered = NA,
  source_name = "Áreas sanitaria Pontevedra y Vigo via @lipido",
  source = "https://github.com/lipido/galicia-covid19/blob/master/pontevedra.csv;https://github.com/lipido/galicia-covid19/blob/master/pontevedra.ext.csv;https://github.com/lipido/galicia-covid19/blob/master/vigo.csv;https://github.com/lipido/galicia-covid19/blob/master/vigo.ext.csv",
  comments = NA
) %>% select( date, province, ccaa, new_cases,   PCR, TestAc,              
          activos, hospitalized,intensive_care,  deceased, cases_accumulated, cases_accumulated_PCR, recovered, source_name, source, comments )

write.csv( pontevedra_prov, file = "data/output/spain/galicia/pontevedra_provincia.csv", row.names = FALSE)

data_cases_sp_provinces <- rbind(data_cases_sp_provinces, pontevedra_prov)


# / A Coruña = Santiago + Ferrol + Coruña -------------
# // Santiago -----
download.file("https://github.com/lipido/galicia-covid19/raw/master/santiago.csv",
              "data/original/spain/galicia/santiago.csv")
santiago_original <- read.delim("data/original/spain/galicia/santiago.csv", sep=",")
download.file("https://github.com/lipido/galicia-covid19/raw/master/santiago.ext.csv",
              "data/original/spain/galicia/santiago.ext.csv")
santiago_hosp <- read.delim("data/original/spain/galicia/santiago.ext.csv", sep=",")

santiago_a <- santiago_original %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% rename (
    cases_accumulated = Santiago.casos.acum, #solo para después de 25 de mayo 2020 Casos totales hasta la fecha. Los casos son con PCR positiva a partir del 25 de mayo con total seguridad. Antes de esta fecha se incluían casos diagnosticados clínicamente con posterior test positivo de anticuerpos. Podrían ser unos 200 casos y concentrados al principio de la serie.
    recovered = Santiago.altas.acum,
    deceased = Santiago.fallecidos.acum
  )

santiago_b <- santiago_hosp %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% rename(
    hospitalized = Santiago.hospitalizados, # TODO advertir del error en el nombre de columna
    intensive_care = Santiago.uci,
    cases_accumulated_test = Santiago.PCR.acum
  )

santiago <- merge( santiago_a %>% select(-Fecha),
                     santiago_b %>% select(-Fecha),
                     by.x = "date", by = "date") %>% mutate(
                       province = "Santiago",
                       ccaa = "Galicia",
                       new_cases = NA,
                       PCR = NA,
                       TestAc = NA,
                       activos = NA,
                       cases_accumulated_PCR = NA,
                       recovered = NA,
                       source_name = "Área sanitaria Ourense via @lipido",
                       source = "https://github.com/lipido/galicia-covid19/blob/master/santiago.csv;https://github.com/lipido/galicia-covid19/blob/master/santiago.ext.csv",
                       comments = NA
                     ) %>% filter ( date > as.Date("2020-06-03") ) %>% # TODO, change date when more data are gathered
  select( date, province, ccaa, new_cases,   PCR, TestAc,              
          activos, hospitalized,intensive_care,  deceased, cases_accumulated, cases_accumulated_PCR, recovered, source_name, source, comments )

write.csv( santiago, file = "data/output/spain/galicia/santiago_area.csv", row.names = FALSE)

# // Ferrol  -----
download.file("https://github.com/lipido/galicia-covid19/raw/master/ferrol.csv",
              "data/original/spain/galicia/ferrol.csv")
ferrol_original <- read.delim("data/original/spain/galicia/ferrol.csv", sep=",")
download.file("https://github.com/lipido/galicia-covid19/raw/master/ferrol.ext.csv",
              "data/original/spain/galicia/ferrol.ext.csv")
ferrol_hosp <- read.delim("data/original/spain/galicia/ferrol.ext.csv", sep=",")

ferrol_a <- ferrol_original %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% rename (
    cases_accumulated = Ferrol.casos.acum, #solo para después de 25 de mayo 2020 Casos totales hasta la fecha. Los casos son con PCR positiva a partir del 25 de mayo con total seguridad. Antes de esta fecha se incluían casos diagnosticados clínicamente con posterior test positivo de anticuerpos. Podrían ser unos 200 casos y concentrados al principio de la serie.
    recovered = Ferrol.altas.acum,
    deceased = Ferrol.fallecidos.acum
  )

ferrol_b <- ferrol_hosp %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% rename(
    hospitalized = Ferrol.hospitalizados, # TODO advertir del error en el nombre de columna
    intensive_care = Ferrol.uci,
    cases_accumulated_test_PCR = Ferrol.PCR.acum
  )

ferrol <- merge( ferrol_a %>% select(-Fecha),
                   ferrol_b %>% select(-Fecha),
                   by.x = "date", by = "date") %>% mutate(
                     province = "Ferrol",
                     ccaa = "Galicia",
                     new_cases = NA,
                     PCR = NA,
                     TestAc = NA,
                     activos = NA,
                     cases_accumulated_PCR = NA,
                     recovered = NA,
                     source_name = "Área sanitaria Ourense via @lipido",
                     source = "https://github.com/lipido/galicia-covid19/blob/master/ferrol.csv;https://github.com/lipido/galicia-covid19/blob/master/ferrol.ext.csv",
                     comments = NA
                   ) %>% filter ( date > as.Date("2020-06-03") ) %>% # TODO, change date when more data are gathered
  select( date, province, ccaa, new_cases,   PCR, TestAc,              
          activos, hospitalized,intensive_care,  deceased, cases_accumulated, cases_accumulated_PCR, recovered, source_name, source, comments )

write.csv( ferrol, file = "data/output/spain/galicia/ferrol_area.csv", row.names = FALSE)

# // Coruña  -----
download.file("https://github.com/lipido/galicia-covid19/raw/master/coruna.csv",
              "data/original/spain/galicia/coruna.csv")
coruna_original <- read.delim("data/original/spain/galicia/coruna.csv", sep=",")
download.file("https://github.com/lipido/galicia-covid19/raw/master/coruna.ext.csv",
              "data/original/spain/galicia/coruna.ext.csv")
coruna_hosp <- read.delim("data/original/spain/galicia/coruna.ext.csv", sep=",")

coruna_a <- coruna_original %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% rename (
    cases_accumulated = A_Coruña.casos.acum, #solo para después de 25 de mayo 2020 Casos totales hasta la fecha. Los casos son con PCR positiva a partir del 25 de mayo con total seguridad. Antes de esta fecha se incluían casos diagnosticados clínicamente con posterior test positivo de anticuerpos. Podrían ser unos 200 casos y concentrados al principio de la serie.
    recovered = A_Coruña.altas.acum,
    deceased = A_Coruña.fallecidos.acum
  )

coruna_b <- coruna_hosp %>%
  mutate(
    date = as.Date(Fecha),
  ) %>% rename(
    hospitalized = A_Coruña.hospitalizados, # TODO advertir del error en el nombre de columna
    intensive_care = A_Coruña.uci,
    cases_accumulated_test = A_Coruña.PCR.acum
  )

coruna <- merge( coruna_a %>% select(-Fecha),
                 coruna_b %>% select(-Fecha),
                 by.x = "date", by = "date") %>% mutate(
                   province = "A Coruña",
                   ccaa = "Galicia",
                   new_cases = NA,
                   PCR = NA,
                   TestAc = NA,
                   activos = NA,
                   cases_accumulated_PCR = NA,
                   recovered = NA,
                   source_name = "Área sanitarias Ourense via @lipido",
                   source = "https://github.com/lipido/galicia-covid19/blob/master/coruna.csv;https://github.com/lipido/galicia-covid19/blob/master/coruna.ext.csv",
                   comments = NA
                 ) %>% filter ( date > as.Date("2020-06-03") ) %>% # TODO, change date when more data are gathered
  select( date, province, ccaa, new_cases,   PCR, TestAc,              
          activos, hospitalized,intensive_care,  deceased, cases_accumulated, cases_accumulated_PCR, recovered, source_name, source, comments )

write.csv( coruna, file = "data/output/spain/galicia/coruna_area.csv", row.names = FALSE)

# // Une las areas de A Coruña --------
coruna_prov <- rbind(santiago, ferrol, coruna) %>%  group_by(date) %>% summarise (
  hospitalized = sum(hospitalized),
  intensive_care = sum(intensive_care),
  deceased = sum(deceased),
  cases_accumulated = sum(cases_accumulated),
  province = "Coruña, A",
  ccaa = "Galicia",
  new_cases = NA,
  PCR = NA,
  TestAc = NA,
  activos = NA,
  cases_accumulated_PCR = NA,
  recovered = NA,
  source_name = "Áreas sanitarias Santiago, El Ferrol y A Coruña via @lipido",
  source = "https://github.com/lipido/galicia-covid19/blob/master/santiago.csv;https://github.com/lipido/galicia-covid19/blob/master/santiago.ext.csv;
  https://github.com/lipido/galicia-covid19/blob/master/ferrol.csv;https://github.com/lipido/galicia-covid19/blob/master/ferrol.ext.csv;
  https://github.com/lipido/galicia-covid19/blob/master/coruna.csv;https://github.com/lipido/galicia-covid19/blob/master/coruna.ext.csv",
  comments = NA
) %>% select( date, province, ccaa, new_cases,   PCR, TestAc,              
              activos, hospitalized,intensive_care,  deceased, cases_accumulated, cases_accumulated_PCR, recovered, source_name, source, comments )

write.csv( coruna_prov, file = "data/output/spain/galicia/coruna_provincia.csv", row.names = FALSE)

data_cases_sp_provinces <- rbind(data_cases_sp_provinces, coruna_prov)

rm(lugo,lugo_a,lugo_b, lugo_hosp, lugo_original, 
   pontevedra, pontevedra_a,pontevedra_b, pontevedra_hosp, pontevedra_original, pontevedra_prov,
   santiago, santiago_a,santiago_b, santiago_hosp, santiago_original, 
   vigo, vigo_a, vigo_b, vigo_hosp, vigo_original, 
   ferrol, ferrol_a, ferrol_b, ferrol_hosp, ferrol_original, 
   coruna, coruna_a, coruna_b, coruna_hosp, coruna_original, coruna_prov )

# Uniprovinciales: Remove and add uniprovinciales from ISCIII -----

# Remove existing Uniprovinciales data
data_cases_sp_provinces <- data_cases_sp_provinces %>% 
  filter( !(province == "Melilla" | province == "Asturias" | province == "Balears, Illes" | province == "Cantabria" |
              province == "Ceuta" | province == "Murcia" | province == "Navarra" | province == "Madrid" |
              province == "Rioja, La") )

# import Instituto de Salud CIII 
download.file("https://cnecovid.isciii.es/covid19/resources/agregados.csv", 
              "data/original/spain/iscii_data.csv")
ciii_original <- read.delim("data/original/spain/iscii_data.csv",sep = ",")

ciii <- ciii_original %>% head(nrow(ciii_original) - 9) %>% #TODO: Cambia el número en función de las notas que incluya el csv original
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
    CCAA = CCAA %>% str_replace_all("VC", "C. Valenciana"),
    recovered = NA
  ) %>% rename(
    region = CCAA,
    fecha = FECHA,
    cases_registered = CASOS,
    cases_accumulated_PCR = PCR.,
    TestAc =TestAc.,
    hospitalized = Hospitalizados,
    intensive_care = UCI,
    deceassed = Fallecidos
    # recovered = Recuperados
  ) %>% filter ( !is.na(region)  )

# Filters and get only uniprovinciales
uniprovinciales <- ciii %>% 
  filter( region == "Melilla" | region == "Asturias" | region == "Baleares" | region == "Cantabria" |
            region == "Ceuta" | region == "Murcia" | region == "Navarra" | region == "Madrid" |
            region == "Rioja, La" ) %>% mutate(
              ccaa = region,
              activos = NA,
              source_name = "Instituto de Salud Carlos III",
              source="https://cnecovid.isciii.es/covid19/resources/agregados.csv",
              comments= "",
              new_cases = NA,
              deceased = deceassed,
              cases_accumulated = ifelse( is.na(cases_registered), cases_accumulated_PCR + ifelse(is.na(TestAc),0,TestAc ), cases_registered),
              PCR = NA
            ) %>% rename(
              province = region,
              # cases_accumulated = cases_registered,
            ) %>% select( date, province, ccaa, new_cases, PCR, TestAc, activos, hospitalized, intensive_care, deceased, cases_accumulated, cases_accumulated_PCR, recovered, source_name, source,comments) %>%
  mutate(
    ccaa = ccaa %>% str_replace_all("La Rioja", "Rioja, La"),
    province = province %>% str_replace_all("La Rioja", "Rioja, La"),
    ccaa = ccaa %>% str_replace_all("Asturias", "Asturias, Principado de"),
    ccaa = ccaa %>% str_replace_all("Baleares", "Balears, Illes"),
    province = province %>% str_replace_all("Baleares", "Balears, Illes"),
    ccaa = ccaa %>% str_replace_all("Madrid", "Madrid, Comunidad de"),
    ccaa = ccaa %>% str_replace_all("Murcia", "Murcia, Región de"),
    ccaa = ccaa %>% str_replace_all("Navarra", "Navarra, Comunidad Foral de")
  )
# Add uniprovinciales data 
data_cases_sp_provinces <- rbind(data_cases_sp_provinces,uniprovinciales)


# Catalunya: Overwrite Catalunya provinces cases  data ------------------
  # Download data from: https://analisi.transparenciacatalunya.cat/Salut/Registre-de-casos-de-COVID-19-realitzats-a-Catalun/jj6z-iyrp/data
download.file("https://analisi.transparenciacatalunya.cat/api/views/jj6z-iyrp/rows.csv?accessType=DOWNLOAD&sorting=true", 
              "data/original/spain/catalunya/Registre_de_casos_de_COVID-19_realitzats_a_Catalunya._Segregaci__per_sexe_i_municipi.csv")
catalunya <-  read.delim("data/original/spain/catalunya/Registre_de_casos_de_COVID-19_realitzats_a_Catalunya._Segregaci__per_sexe_i_municipi.csv",sep = ",")

# creates date variable
catalunya$date <- as.Date(catalunya$TipusCasData, "%d/%m/%Y")
# extracts first charatcter of zipcode to select province code
catalunya$provincia_code <- ifelse(catalunya$MunicipiCodi < 10000, paste0(substr(as.character(catalunya$MunicipiCodi),1,1)), substr(as.character(catalunya$MunicipiCodi),1,2))     

# iterates through data to count positives
catalunya_new <- catalunya %>% group_by(date,provincia_code,TipusCasDescripcio) %>% arrange(date) %>% 
  # filter( TipusCasDescripcio == "Positiu PCR" | TipusCasDescripcio == "Positiu per Test Ràpid" ) %>%
  # catalunya_new <- catalunya %>% group_by(date,provincia_code) %>% arrange(date) %>% filter( ResultatCovidDescripcio == "Positiu" |  ResultatCovidDescripcio == "Probable" ) %>%
  summarise ( 
    by_day = sum(NumCasos)
  )

# converts to wide format the different variables of positive or suspect cases with spread
cattotal <- catalunya_new %>% spread(TipusCasDescripcio,by_day) %>% 
  rename(PCR_day = "Positiu PCR", TestAc_day = "Positiu per Test Ràpid", sospechosos_day = "Sospitós") %>% 
  # groups by province to calculate
  group_by(provincia_code) %>% arrange(date) %>%
  # calculates de accumulated values
  mutate ( 
    PCR_cum = cumsum(replace_na(PCR_day, 0)), #replace NA values with 0 to make cumsum work
    TestAc_cum = cumsum(replace_na(TestAc_day, 0)),
    sospechosos_cum = cumsum(replace_na(sospechosos_day, 0))
  )

# Creates provinica factor
cattotal$province <- as.factor(cattotal$provincia_code)
# Gives names 
levels(cattotal$province) <- c("Girona","Lleida","Tarragona","Barcelona")

write.csv(cattotal, file = "data/output/spain/catalunya/catalunya-cases-evolution-by-province.csv", row.names = FALSE)
saveRDS(cattotal, file = "data/output/spain/catalunya/catalunya-cases-evolution-by-province.rds")

cattotal$ccaa <- "Cataluña"

# creates unique date-province id to merge
cattotal$dunique <- paste0(cattotal$date,cattotal$province)
data_cases_sp_provinces$dunique <- paste0(data_cases_sp_provinces$date,data_cases_sp_provinces$province)

# TODO: no usamos los sospechosos o TestAc acumulados, pero se podría
data_cases_sp_provinces <- merge(data_cases_sp_provinces, 
                                 cattotal %>%
                                   filter (!is.na(province)) %>%
                                   select(date,dunique,PCR_day,PCR_cum,TestAc_day,TestAc_cum,province,ccaa) %>% 
                                   mutate(
                                     cases_accumulated_cat = PCR_cum + TestAc_cum,
                                   ) %>% rename(
                                     PCR_cat = PCR_day,
                                     PCR_cum_cat = PCR_cum,
                                     TestAc_cat = TestAc_day,
                                     date_cat = date,
                                     province_cat = province,
                                     ccaa_cat = ccaa ) , 
                                 by.x="dunique", by.y="dunique", all = TRUE) 
# %>% select(-dunique) 

data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
  # add source of cataluña cases data
  province = ifelse( is.na(province), as.character(province_cat), as.character(province)),
  ccaa = ifelse( is.na(ccaa), as.character(ccaa_cat), as.character(ccaa)),
  date = ifelse( is.na(date), date_cat, date),
  date = as.Date(date, origin = "1970-01-01"),
  cases_accumulated = ifelse( ccaa == "Cataluña", cases_accumulated_cat, cases_accumulated),
  cases_accumulated_PCR = ifelse( ccaa == "Cataluña", PCR_cum_cat, cases_accumulated_PCR),
  PCR = ifelse( ccaa == "Cataluña", PCR_cat, PCR),
  TestAc = ifelse( ccaa == "Cataluña", TestAc_cat, TestAc),
  source_name = ifelse( ccaa == "Cataluña",paste0("Transparencia Catalunya;",as.character(source_name)), as.character(source_name) ), 
  source = ifelse( ccaa == "Cataluña", 
                   paste0(source,";https://analisi.transparenciacatalunya.cat/Salut/Registre-de-test-de-COVID-19-realitzats-a-Cataluny/jj6z-iyrp/data;https://code.montera34.com:4443/numeroteca/covid19/-/blob/master/data/output/spain/catalunya-cases-evolution-by-province.csv" ), 
                   as.character(source) )
) %>% select(-cases_accumulated_cat) %>% select(-province_cat) %>% select(-ccaa_cat)  %>% select(-dunique)  %>%  
  select(-date_cat, -PCR_cum_cat, -PCR_cat, -TestAc_cat, -TestAc_cum, -provincia_code)

# Catalunya: Overwrite Catalunya provinces death data ------------------
# This is calculated at analysis/count_catalunya.R
# donwload provincias data from googgle spreadsheet 
download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=cat_", 
              "data/original/spain/catalunya/powerbi.csv")
powerbi <-  read.delim("data/original/spain/catalunya/powerbi.csv",sep = ",") %>% 
  select(Fecha,Territorio,Fallecimientos_positivos,Fallecimientos_todos) %>% 
  filter( !Territorio =="Cataluña")
powerbi$date <- as.Date(powerbi$Fecha, "%d/%m/%y")
powerbi$ccaa <- "Cataluña"

data_cases_sp_provinces$dunique <- paste0(data_cases_sp_provinces$date,data_cases_sp_provinces$province)
powerbi$dunique <- paste0(powerbi$date,powerbi$Territorio)

data_cases_sp_provinces <- merge(data_cases_sp_provinces,
                                 powerbi %>% select(date,dunique,Fallecimientos_positivos,Territorio,ccaa) %>% 
                                   rename(
                                     date_cat = date,
                                     deceased_cat = Fallecimientos_positivos,
                                     ccaa_cat = ccaa ) , 
                                 by.x="dunique", by.y="dunique", all = TRUE) %>% select(-dunique)

data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
  # add source of cataluña death data
  date = ifelse( is.na(date), date_cat, date),
  date = as.Date(date, origin = "1970-01-01"),
  province = ifelse( is.na(province),   as.character(Territorio), province ),
  ccaa = ifelse( is.na(ccaa),   as.character(ccaa_cat), ccaa ),
  deceased = ifelse( ccaa == "Cataluña", deceased_cat, deceased),
  source_name = ifelse( ccaa == "Cataluña",paste0(as.character(source_name),";Generalitat de Catalunya"), as.character(source_name) ),
  source = ifelse( ccaa == "Cataluña", 
                   paste0(source,";https://app.powerbi.com/view?r=eyJrIjoiZTkyNTcwNjgtNTQ4Yi00ZTg0LTk1OTctNzM3ZGEzNWE4OTIxIiwidCI6IjNiOTQyN2RjLWQzMGUtNDNiYy04YzA2LWZmNzI1MzY3NmZlYyIsImMiOjh9" ), 
                   as.character(source) )
) %>% select(-deceased_cat) %>% select(-date_cat) %>% select(-Territorio) %>% select(-ccaa_cat)


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

# Euskadi hospitalizados. Overwrite hospitalized  data --------------
download.file("https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/datos-asistenciales.xlsx",
              "data/original/spain/euskadi/datos-asistenciales.xlsx")
#TODO: volver a activar cuando arreglen als fechas, mietnras trabajar con los CSV del .zip
download.file("https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/datos-asistenciales.zip",
              "data/original/spain/euskadi/datos-asistenciales.zip")
unzip(zipfile = "data/original/spain/euskadi/datos-asistenciales.zip", exdir = "data/original/spain/euskadi/")

euskadi_original <- read_excel("data/original/spain/euskadi/datos-asistenciales.xlsx", skip = 2, col_names = TRUE, sheet = "01")
# euskadi_original <- read.delim("data/original/spain/euskadi/01.csv",sep = ";", skip = 2)

euskadi_a <- euskadi_original %>% rename( date = ...1 ) %>% 
  mutate( date = as.Date(date,"%d/%m/%Y")) %>% select( -`Ingresados en Planta`)  %>% melt(
# euskadi_a <- euskadi_original %>% rename( date = X ) %>%
#   mutate( date = as.Date(date,"%d/%m/%Y")) %>% select( -`Ingresados.en.Planta`)  %>% melt(
    id.vars = c("date")
  ) %>% mutate(
    province = ifelse(variable=="01 Araba", "Araba/Álava" ,NA),
    province = ifelse(variable=="02 Cruces", "Bizkaia"  ,province),
    province = ifelse(variable=="03 Donosti", "Gipuzkoa" ,province),
    province = ifelse(variable=="04 Basurto", "Bizkaia"  ,province),
    province = ifelse(variable=="05 Galdakao", "Bizkaia" ,province),
    province = ifelse(variable=="06 Zumarraga", "Gipuzkoa" ,province),
    province = ifelse(variable=="07 Bidasoa", "Gipuzkoa" ,province),
    province = ifelse(variable=="08 Mendaro", "Gipuzkoa" ,province),
    province = ifelse(variable=="09 Alto Deba", "Gipuzkoa" ,province),
    province = ifelse(variable=="10 San Eloy", "Bizkaia" ,province),
    province = ifelse(variable=="11 Urduliz", "Bizkaia" ,province),
    province = ifelse(variable=="12 Eibar", "Gipuzkoa" ,province),
    province = ifelse(variable=="13 Leza", "Araba/Álava" ,province),
    province = ifelse(variable=="14 Sta Marina", "Bizkaia" ,province),
    province = ifelse(variable=="15 Gorliz", "Bizkaia" ,province),
    province = ifelse(variable=="BERMEO H.", "Bizkaia" ,province),
    province = ifelse(variable=="ZALDIBAR H.", "Bizkaia" ,province),
    province = ifelse(variable=="ZAMUDIO H.", "Bizkaia" ,province),
    province = ifelse(variable=="ÁLAVA PSIQUIÁTRICO H.", "Araba/Álava" , province)
    # for CSV option
    # province = ifelse(variable=="X01.Araba", "Araba/Álava" ,NA),
    # province = ifelse(variable=="X02.Cruces", "Bizkaia"  ,province),
    # province = ifelse(variable=="X03.Donosti", "Gipuzkoa" ,province),
    # province = ifelse(variable=="X04.Basurto", "Bizkaia"  ,province),
    # province = ifelse(variable=="X05.Galdakao", "Bizkaia" ,province),
    # province = ifelse(variable=="X06.Zumarraga", "Gipuzkoa" ,province),
    # province = ifelse(variable=="X07.Bidasoa", "Gipuzkoa" ,province),
    # province = ifelse(variable=="X08.Mendaro", "Gipuzkoa" ,province),
    # province = ifelse(variable=="X09.Alto.Deba", "Gipuzkoa" ,province),
    # province = ifelse(variable=="X10.San.Eloy", "Bizkaia" ,province),
    # province = ifelse(variable=="X11.Urduliz", "Bizkaia" ,province),
    # province = ifelse(variable=="X12.Eibar", "Gipuzkoa" ,province),
    # province = ifelse(variable=="X13.Leza", "Araba/Álava" ,province),
    # province = ifelse(variable=="X14.Sta.Marina", "Bizkaia" ,province),
    # province = ifelse(variable=="X15.Gorliz", "Bizkaia" ,province),
    # province = ifelse(variable=="BERMEO.H.", "Bizkaia" ,province),
    # province = ifelse(variable=="ZALDIBAR.H.", "Bizkaia" ,province),
    # province = ifelse(variable=="ZAMUDIO.H.", "Bizkaia" ,province),
    # province = ifelse(variable=="ALAVA.PSIQUIATRICO.H.", "Araba/Álava" , province)
  ) %>% group_by(province,date) %>%
  mutate(
    value = ifelse(is.na(value),0,value)) %>%
  summarise(
    hospitalized = sum(value)
  ) %>% filter( !is.na(province) )

data_cases_sp_provinces$dunique <- paste0(data_cases_sp_provinces$date,data_cases_sp_provinces$province)
euskadi_a$dunique <- paste0(euskadi_a$date,euskadi_a$province)

# TODO: mirar los hospitalizados por día: no coinciden los datos con lo que se publicó en las notas de prensa de Irekia
data_cases_sp_provinces <- merge(data_cases_sp_provinces,
                                 euskadi_a %>% ungroup() %>% select(dunique,hospitalized) %>% 
                                   rename(
                                     hospitalized_eus = hospitalized
                                   ) , 
                                 by.x="dunique", by.y="dunique", all = TRUE) %>% select(-dunique)
saveRDS(data_cases_sp_provinces, file = "data/output/spain/euskadi/compare_hospitalized_irekia-vs-opendata.rds")

data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
  hospitalized = ifelse( ccaa == "País Vasco", hospitalized_eus, hospitalized),
  source_name = ifelse( ccaa == "País Vasco", paste0(as.character(source_name),";Open Data Euskadi"), as.character(source_name) ),
  source = ifelse( ccaa == "País Vasco", 
                   paste0(source,";https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/datos-asistenciales.xlsx" ), 
                   as.character(source) )
) %>% select(-hospitalized_eus)

rm(euskadi_a,euskadi_original)

# Euskadi UCI. Overwrite UCI  data --------------
euskadi_original <- read_excel("data/original/spain/euskadi/datos-asistenciales.xlsx", skip = 2, col_names = TRUE, sheet = "04")

euskadi_a <- euskadi_original %>% rename( date = ...1 ) %>% 
  mutate( date = as.Date(date,"%d/%m/%Y")) %>% select( -`Ingresados UCI`)  %>% melt(
    # euskadi_a <- euskadi_original %>% rename( date = X ) %>%
    #   mutate( date = as.Date(date,"%d/%m/%Y")) %>% select( -`Ingresados.en.Planta`)  %>% melt(
    id.vars = c("date")
  ) %>% mutate(
    province = ifelse(variable=="01 Araba", "Araba/Álava" ,NA),
    province = ifelse(variable=="02 Cruces", "Bizkaia"  ,province),
    province = ifelse(variable=="03 Donosti", "Gipuzkoa" ,province),
    province = ifelse(variable=="04 Basurto", "Bizkaia"  ,province),
    province = ifelse(variable=="05 Galdakao", "Bizkaia" ,province),
    province = ifelse(variable=="06 Zumarraga", "Gipuzkoa" ,province),
    province = ifelse(variable=="07 Bidasoa", "Gipuzkoa" ,province),
    province = ifelse(variable=="08 Mendaro", "Gipuzkoa" ,province),
    province = ifelse(variable=="09 Alto Deba", "Gipuzkoa" ,province),
    province = ifelse(variable=="10 San Eloy", "Bizkaia" ,province),
    province = ifelse(variable=="11 Urduliz", "Bizkaia" ,province),
    province = ifelse(variable=="12 Eibar", "Gipuzkoa" ,province),
    province = ifelse(variable=="13 Leza", "Araba/Álava" ,province),
    province = ifelse(variable=="14 Sta Marina", "Bizkaia" ,province),
    province = ifelse(variable=="15 Gorliz", "Bizkaia" ,province),
    province = ifelse(variable=="BERMEO H.", "Bizkaia" ,province),
    province = ifelse(variable=="ZALDIBAR H.", "Bizkaia" ,province),
    province = ifelse(variable=="ZAMUDIO H.", "Bizkaia" ,province),
    province = ifelse(variable=="ÁLAVA PSIQUIÁTRICO H.", "Araba/Álava" , province)
    # for CSV option
    # province = ifelse(variable=="X01.Araba", "Araba/Álava" ,NA),
    # province = ifelse(variable=="X02.Cruces", "Bizkaia"  ,province),
    # province = ifelse(variable=="X03.Donosti", "Gipuzkoa" ,province),
    # province = ifelse(variable=="X04.Basurto", "Bizkaia"  ,province),
    # province = ifelse(variable=="X05.Galdakao", "Bizkaia" ,province),
    # province = ifelse(variable=="X06.Zumarraga", "Gipuzkoa" ,province),
    # province = ifelse(variable=="X07.Bidasoa", "Gipuzkoa" ,province),
    # province = ifelse(variable=="X08.Mendaro", "Gipuzkoa" ,province),
    # province = ifelse(variable=="X09.Alto.Deba", "Gipuzkoa" ,province),
    # province = ifelse(variable=="X10.San.Eloy", "Bizkaia" ,province),
    # province = ifelse(variable=="X11.Urduliz", "Bizkaia" ,province),
    # province = ifelse(variable=="X12.Eibar", "Gipuzkoa" ,province),
    # province = ifelse(variable=="X13.Leza", "Araba/Álava" ,province),
    # province = ifelse(variable=="X14.Sta.Marina", "Bizkaia" ,province),
    # province = ifelse(variable=="X15.Gorliz", "Bizkaia" ,province),
    # province = ifelse(variable=="BERMEO.H.", "Bizkaia" ,province),
    # province = ifelse(variable=="ZALDIBAR.H.", "Bizkaia" ,province),
    # province = ifelse(variable=="ZAMUDIO.H.", "Bizkaia" ,province),
    # province = ifelse(variable=="ALAVA.PSIQUIATRICO.H.", "Araba/Álava" , province)
  ) %>% group_by(province,date) %>%
  mutate(
    value = ifelse(is.na(value),0,value)) %>%
  summarise(
    intensive_care = sum(value)
  ) %>% filter( !is.na(province) )

data_cases_sp_provinces$dunique <- paste0(data_cases_sp_provinces$date,data_cases_sp_provinces$province)
euskadi_a$dunique <- paste0(euskadi_a$date,euskadi_a$province)

data_cases_sp_provinces <- merge(data_cases_sp_provinces,
                                 euskadi_a %>% ungroup() %>% select(dunique,intensive_care) %>% 
                                   rename(
                                     intensive_care_eus = intensive_care
                                   ) , 
                                 by.x="dunique", by.y="dunique", all = TRUE) %>% select(-dunique)

data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
  intensive_care = ifelse( ccaa == "País Vasco", intensive_care_eus, intensive_care),
  source_name = ifelse( ccaa == "País Vasco", paste0(as.character(source_name),";Open Data Euskadi"), as.character(source_name) ),
  source = ifelse( ccaa == "País Vasco", 
                   paste0(source,";https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/datos-asistenciales.xlsx" ), 
                   as.character(source) )
) %>% select(-intensive_care_eus)

rm(euskadi_a,euskadi_original)


# Euskadi fallecidos en hospitales. -------------- 
# TODO: no se usa porque los fallecidos notificados 
# son solamente los de hospitales desde el 15 de mayo 2020
# euskadi_original <- read_excel("data/original/spain/euskadi/datos-asistenciales.xlsx", skip = 2, col_names = TRUE, sheet = "07")
# 
# euskadi_b <- euskadi_original %>% rename( date = ...1 ) %>% filter( date != "Suma Total" ) %>%
#   mutate( date = as.Date(date,"%d/%m/%Y")) %>% select( -`Exitus`) %>% melt(
#     id.vars = c("date")
#   ) %>% mutate(
#     province = ifelse(variable=="01 Araba", "Araba/Álava" ,NA),
#     province = ifelse(variable=="02 Cruces", "Bizkaia"  ,province),
#     province = ifelse(variable=="03 Donosti", "Gipuzkoa" ,province),
#     province = ifelse(variable=="04 Basurto", "Bizkaia"  ,province),
#     province = ifelse(variable=="05 Galdakao", "Bizkaia" ,province),
#     province = ifelse(variable=="06 Zumarraga", "Gipuzkoa" ,province),
#     province = ifelse(variable=="07 Bidasoa", "Gipuzkoa" ,province),
#     province = ifelse(variable=="08 Mendaro", "Gipuzkoa" ,province),
#     province = ifelse(variable=="09 Alto Deba", "Gipuzkoa" ,province),
#     province = ifelse(variable=="10 San Eloy", "Bizkaia" ,province),
#     province = ifelse(variable=="11 Urduliz", "Bizkaia" ,province),
#     province = ifelse(variable=="12 Eibar", "Gipuzkoa" ,province),
#     province = ifelse(variable=="13 Leza", "Araba/Álava" ,province),
#     province = ifelse(variable=="14 Sta Marina", "Bizkaia" ,province),
#     province = ifelse(variable=="15 Gorliz", "Bizkaia" ,province),
#     province = ifelse(variable=="BERMEO H.", "Bizkaia" ,province),
#     province = ifelse(variable=="ZALDIBAR H.", "Bizkaia" ,province),
#     province = ifelse(variable=="ZAMUDIO H.", "Bizkaia" ,province),
#     province = ifelse(variable=="ÁLAVA PSIQUIÁTRICO H.", "Araba/Álava" , province)
#   ) %>% group_by(province,date) %>%
#   mutate(
#     value = ifelse(is.na(value),0,value)) %>%
#   summarise(
#     deceased_per_day = sum(value)
#   ) %>% filter ( !is.na(province) ) %>% group_by(province) %>% arrange(date) %>% mutate(
#     deceased = cumsum(deceased_per_day)
#   )
# 
# data_cases_sp_provinces$dunique <- paste0(data_cases_sp_provinces$date,data_cases_sp_provinces$province)
# euskadi_b$dunique <- paste0(euskadi_b$date,euskadi_b$province)
# 
# data_cases_sp_provinces <- merge(data_cases_sp_provinces,
#                                  euskadi_b %>% ungroup() %>% select(dunique,deceased_per_day, deceased) %>%
#                                    rename(
#                                      deceased_per_day_eus = deceased_per_day,
#                                      deceased_eus = deceased
#                                    ) ,
#                                  by.x="dunique", by.y="dunique", all = TRUE) %>% select(-dunique)
# 
# data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
#   deceased = ifelse( (ccaa == "País Vasco") & (date > as.Date("2020-05-20")), deceased_eus, deceased)
# ) # %>% select(-deceased_eus,deceased_per_day_eus)
# 
# 
# data_cases_sp_provinces <- deceased_per_day_eus %>% select(-deceased_per_day_eus ,-deceased_eus )
# 
# rm(euskadi_b,euskadi_original)

# Aragón hospitalizados.  Overwrite hospitalized  data -------------- -----
download.file("https://www.aragon.es/documents/20127/38742837/casos_coronavirus_hospitales.xlsx", # TODO, ya no da datos por provincia
              "data/original/spain/aragon/casos_coronavirus_hospitales.xlsx")

aragon_original <- read_excel("data/original/spain/aragon/casos_coronavirus_hospitales.xlsx", col_names = TRUE)

aragon_a <- aragon_original %>%
  rename(
    date = fecha,
    province = provincia
  ) %>% 
  group_by(province,date) %>% 
  summarise(
    hospitalized = sum(replace_na(camas_ocupadas_total,0)),
    intensive_care = sum( replace_na(camas_uci_ocupadas,0))
  )

data_cases_sp_provinces$dunique <- paste0(data_cases_sp_provinces$date,data_cases_sp_provinces$province)
aragon_a$dunique <- paste0(aragon_a$date,aragon_a$province)

data_cases_sp_provinces <- merge(data_cases_sp_provinces,
                                 aragon_a %>% ungroup() %>% select(dunique,hospitalized,intensive_care) %>% 
                                   rename(
                                     hospitalized_ara = hospitalized,
                                     intensive_care_ara = intensive_care
                                   ) , 
                                 by.x="dunique", by.y="dunique", all = TRUE) %>% select(-dunique)

data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
  hospitalized = ifelse( ccaa == "Aragón", hospitalized_ara, hospitalized),
  intensive_care = ifelse( ccaa == "Aragón", intensive_care_ara, intensive_care),
  source_name = ifelse( ccaa == "Aragón", paste0(as.character(source_name),";aragon.es"), as.character(source_name) ),
  source = ifelse( ccaa == "Aragón" & !is.na(hospitalized), 
                   paste0(source,";https://www.aragon.es/documents/20127/38742837/casos_coronavirus_hospitales.xlsx" ), 
                   as.character(source) )
) %>% select(-hospitalized_ara, -intensive_care_ara)

rm(aragon_a,aragon_original)

# Aragon 2 new hospitalized data ------
# https://transparencia.aragon.es/sites/default/files/documents/20200731_casos_confirmados_zona_basica_salud.xlsx
# https://transparencia.aragon.es/sites/default/files/documents/20200802_casos_confirmados_zbs.xlsx

# Datos de https://opendata.aragon.es/datos/catalogo/dataset/publicaciones-y-anuncios-relacionados-con-el-coronavirus-en-aragon
download.file("https://www.aragon.es/documents/20127/38742837/casos_coronavirus_hospitales.csv", 
              "data/original/spain/aragon/casos_coronavirus_hospitales.csv")

# aragon_original <- read.delim("data/original/spain/aragon/casos_coronavirus_hospitales.csv",sep = ";") 
aragon_b <- read.delim("data/original/spain/aragon/casos_coronavirus_hospitales.csv",sep = ";") %>% mutate(
  date_ara = as.Date(fecha, "%Y-%m-%d"),
  camas_ocupadas_total = ifelse(is.na(camas_ocupadas_total),0,camas_ocupadas_total),
  camas_uci_ocupadas = ifelse(is.na(camas_uci_ocupadas),0,camas_uci_ocupadas)
) %>% group_by(provincia,date_ara) %>% summarise(
  hosp_ara = sum( camas_ocupadas_total),
  uci_ara = sum( camas_uci_ocupadas)
)

# add data
data_cases_sp_provinces <- merge(
  data_cases_sp_provinces %>% mutate ( dunique = paste0( date, province) ) %>% ungroup(),
  aragon_b %>% mutate ( dunique = paste0( date_ara, provincia) ) %>% ungroup() %>% select (dunique, hosp_ara, uci_ara,provincia,date_ara ) ,
  by.x = "dunique", by.y = "dunique", all = TRUE
) %>% mutate (
 province = ifelse( is.na(provincia), as.character(province), as.character(provincia ) ),
 ccaa = ifelse( !is.na(provincia), "Aragón", ccaa),
 date = as.Date( ifelse( is.na(date), date_ara, date), origin=as.Date("1970-01-01") ),
 # 
 hospitalized = ifelse( ccaa == "Aragón", hosp_ara, hospitalized),
 intensive_care = ifelse( ccaa == "Aragón", uci_ara, intensive_care)
) %>% select(-dunique, -hosp_ara,- uci_ara, -provincia, -date_ara)


# Madrid hospitalizados.  Overwrite hospitalized  data -------------------
# download data from https://github.com/alfonsotwr/snippets/blob/master/covidia-cam/madrid-series.csv
download.file("https://github.com/alfonsotwr/snippets/raw/master/covidia-cam/madrid-series.csv", 
              "data/original/spain/madrid/madrid-series.csv")
download.file("https://github.com/alfonsotwr/snippets/raw/master/covidia-cam/madrid-historico.csv", 
              "data/original/spain/madrid/madrid-historico.csv")

madrid_original <- read.delim("data/original/spain/madrid/madrid-series.csv",sep = ",")
madrid_original2 <- read.delim("data/original/spain/madrid/madrid-historico.csv",sep = ",")

madrid_a <- madrid_original %>%
  mutate(
    date = as.Date(as.character(Fecha)),
    province = "Madrid",
    ccaa = "Madrid, Comunidad de",
    ine_code = 28
  ) %>% rename(
    hospitalized = hospitalizados_dia,
    intensive_care = uci_dia,
    deceased = Fallecidos,
    # TODO terminarinsertar los casos
    cases_accumulated_PCR = CASOS_PCR
  )

madrid_b <- madrid_original2 %>%
  mutate(
    date = as.Date(as.character(FECHA)),
    province = "Madrid",
    ccaa = "Madrid, Comunidad de",
    ine_code = 28
  ) %>% rename(
    hospitalized = hospitalizados_dia,
    intensive_care = uci_dia
  )

data_cases_sp_provinces$dunique <- paste0(data_cases_sp_provinces$date,data_cases_sp_provinces$province)
madrid_a$dunique <- paste0(madrid_a$date,madrid_a$province)

# Datos de los PDF de la C. Madrid en ausenciade datos de ISCIII
data_cases_sp_provinces <- merge(data_cases_sp_provinces,
                                 madrid_a %>% ungroup() %>% 
                                   select(dunique,deceased,hospitalized,intensive_care,date,province,ccaa) %>% 
                                   rename(
                                     hospitalized_mad = hospitalized,
                                     intensive_care_mad = intensive_care,
                                     deceased_mad = deceased,
                                     date_mad = date,
                                     province_mad = province,
                                     ccaa_mad = ccaa
                                   ) , 
                                 by.x="dunique", by.y="dunique", all = TRUE) %>% select(-dunique)

data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
# zzz <- data_cases_sp_provinces %>% mutate(
  # no sé por qué per ohe tenido que montar este lío para que las fechas funcionaran
  date_new = ifelse( ccaa_mad == "Madrid, Comunidad de" & !is.na(province_mad), date_mad, NA ),
  date_new = as.Date(date_mad, origin=as.Date("1970-01-01") ),
  date = ifelse( ccaa_mad == "Madrid, Comunidad de" & !is.na(province_mad), date_new, date ),
  date = as.Date(date, origin=as.Date("1970-01-01") ),
  province = ifelse( ccaa_mad == "Madrid, Comunidad de" & !is.na(province_mad), province_mad, province),
  ccaa = ifelse( ccaa_mad == "Madrid, Comunidad de" & !is.na(province_mad), ccaa_mad, ccaa),
  
  deceased =    ifelse( ccaa == "Madrid, Comunidad de" & (date > as.Date("2020-05-20") ), deceased_mad, deceased),
  hospitalized = ifelse( ccaa == "Madrid, Comunidad de", hospitalized_mad, hospitalized),
  intensive_care = ifelse( ccaa == "Madrid, Comunidad de", intensive_care_mad, intensive_care),
  source_name = ifelse( ccaa == "Madrid, Comunidad de", paste0(as.character(source_name),";Consejería de Salud de la Comunidad de Madrid"), as.character(source_name) ),
  source = ifelse( ccaa == "Madrid, Comunidad de" & !is.na(hospitalized), 
                   paste0(source,";https://github.com/alfonsotwr/snippets/blob/master/covidia-cam/madrid-series.csv;https://www.comunidad.madrid/servicios/salud/2019-nuevo-coronavirus#situacion-epidemiologica-actual" ), 
                   as.character(source) )
) %>% select(-hospitalized_mad, -intensive_care_mad, -deceased_mad, -date_mad, -province_mad, -ccaa_mad, -date_new)

data_cases_sp_provinces$dunique <- paste0(data_cases_sp_provinces$date,data_cases_sp_provinces$province)
madrid_b$dunique <- paste0(madrid_b$date,madrid_b$province)

# Datos de ISCII seleccionados para meter solamente los prevalentes
data_cases_sp_provinces <- merge( data_cases_sp_provinces,
                                  madrid_b %>% ungroup() %>% 
                                    select(dunique, hospitalized, intensive_care, date, province, ccaa) %>% 
                                    filter ( date > as.Date("2020-03-12") & date < as.Date("2020-04-22") ) %>%
                                    rename(
                                      hospitalized_mad = hospitalized,
                                      intensive_care_mad = intensive_care,
                                      date_mad = date,
                                      province_mad = province,
                                      ccaa_mad = ccaa
                                    ) , 
                                  by.x="dunique", by.y="dunique", all = TRUE) %>% select(-dunique)


data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
  # no sé por qué per ohe tenido que montar este lío para que las fechas funcionaran
  date_new = ifelse( ccaa_mad == "Madrid, Comunidad de" & !is.na(province_mad), date_mad, NA ),
  date_new = as.Date(date_mad, origin=as.Date("1970-01-01") ),
  date = ifelse( ccaa_mad == "Madrid, Comunidad de" & !is.na(province_mad), date_new, date ),
  date = as.Date(date, origin=as.Date("1970-01-01") ),
  province = ifelse( ccaa_mad == "Madrid, Comunidad de" & !is.na(province_mad), province_mad, province),
  
  ccaa = ifelse( ccaa_mad == "Madrid, Comunidad de" & !is.na(province_mad), ccaa_mad, ccaa),
  hospitalized = ifelse( is.na(hospitalized_mad), hospitalized, hospitalized_mad),
  intensive_care = ifelse( is.na(intensive_care_mad), intensive_care, intensive_care_mad),
  
  source_name = ifelse( ccaa == "Madrid, Comunidad de", paste0(as.character(source_name),";Instituto de Salud Carlos III via @alfonsotwr"), as.character(source_name) ),
  source = ifelse( ccaa == "Madrid, Comunidad de" & !is.na(hospitalized), 
                   paste0(source,";https://github.com/alfonsotwr/snippets/blob/master/covidia-cam/madrid-historico.csv" ), 
                   as.character(source) )
) %>% select(-hospitalized_mad, -intensive_care_mad, -date_mad, -province_mad, -ccaa_mad, -date_new)

rm(madrid_a, madrid_b,madrid_original)


# Uniprovinciales @danielegrasso --------------------
download.file("https://gitlab.com/elpais/datos/-/raw/master/20_Covid-19/covid-provincias/data_uniprovs.csv?inline=false",
              "data/original/spain/uniprovinciales/data_uniprovs.csv") #TODO

uniprovinciales_d <- read.delim("data/original/spain/uniprovinciales/data_uniprovs.csv",sep = ",") %>% mutate (
  date_uni = as.Date(DATE, "%d/%m/%y") - 1 # one day less as the date is the report date!
) %>% rename (
  ccaa_uni = CCAA,
  cases_accumulated_PCR_uni = PCR_acumuado,
  deceased_uni = MUERTOS_acumulado
) %>% mutate(
  ccaa_uni = ccaa_uni %>% str_replace_all("La Rioja", "Rioja, La"),
  ccaa_uni = ccaa_uni %>% str_replace_all("Baleares", "Balears, Illes"),
  province_uni = ccaa_uni,
  ccaa_uni = ccaa_uni %>% str_replace_all("Asturias", "Asturias, Principado de"),
  ccaa_uni = ccaa_uni %>% str_replace_all("Madrid", "Madrid, Comunidad de"),
  ccaa_uni = ccaa_uni %>% str_replace_all("Murcia", "Murcia, Región de"),
  ccaa_uni = ccaa_uni %>% str_replace_all("Navarra", "Navarra, Comunidad Foral de")
  # province = province %>% str_replace_all("Baleares", "Balears, Illes"),
) %>% select (-DATE)

data_cases_sp_provinces <- merge ( data_cases_sp_provinces  %>% mutate (dunique = paste0(date,province)),
                                   uniprovinciales_d %>% mutate (dunique = paste0(date_uni,province_uni)) ,
                                   by.x = "dunique", by.y ="dunique", all = TRUE
)

data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
  # no sé por qué pero he tenido que montar este lío para que las fechas funcionaran: porque no puede evaluar NA!!
  date_new = ifelse( !is.na(province_uni), date_uni, NA ),
  date_new = as.Date(date_uni, origin=as.Date("1970-01-01") ),
  date = ifelse( !is.na(province_uni), date_new, date ),
  date = as.Date(date, origin=as.Date("1970-01-01") ),
  
  province = ifelse( !is.na(province_uni), as.character(province_uni), province),
  ccaa = ifelse( !is.na(province_uni), ccaa_uni, ccaa),
  
  cases_accumulated_PCR = ifelse( is.na(cases_accumulated_PCR_uni), cases_accumulated_PCR, cases_accumulated_PCR_uni),
  deceased = ifelse( is.na(deceased_uni), deceased, deceased_uni),
  
  # source_name = ifelse(!is.na(province_uni), paste0(as.character(source_name),";Web de Comunidad autónoma por @danielegrasso"), as.character(source_name) ),
  # source = ifelse( !is.na(province_uni), 
  #                  paste0(source,";https://github.com/alfonsotwr/snippets/blob/master/covidia-cam/madrid-historico.csv" ), 
  #                  as.character(source) )
  source_name = ifelse( province_uni == "Madrid" & !is.na(province_uni), paste0(as.character(source_name),";Comunidad de Madrid vía @danielegrasso"), as.character(source_name) ),
  source = ifelse(  province_uni == "Madrid" & !is.na(province_uni),
                    paste0(source,";https://www.comunidad.madrid/servicios/salud/2019-nuevo-coronavirus;https://gitlab.com/elpais/datos/-/raw/master/20_Covid-19/covid-provincias/data_uniprovs.csv?inline=false" ),
                    as.character(source) ),
  source_name = ifelse( province_uni == "Asturias" & !is.na(province_uni), paste0(as.character(source_name),";Gobierno del Principado de Asturias vía @danielegrasso"), as.character(source_name) ),
  source = ifelse(  province_uni == "Asturias" & !is.na(province_uni),
                    paste0(source,";https://app.transparenciaendatos.es/v/#!/5eb4344e16b9fc465933d217;https://coronavirus.asturias.es;https://gitlab.com/elpais/datos/-/raw/master/20_Covid-19/covid-provincias/data_uniprovs.csv?inline=false" ),
                    as.character(source) ),
  source_name = ifelse( province_uni == "Balears, Illes" & !is.na(province_uni), paste0(as.character(source_name),";Govern Illes Balears vía @danielegrasso"), as.character(source_name) ),
  source = ifelse(  province_uni =="Balears, Illes" & !is.na(province_uni),
                    paste0(source,";http://www.caib.es/sites/coronavirus/es/l/noticias_sobre_el_coronavirus_covid-19/;https://gitlab.com/elpais/datos/-/raw/master/20_Covid-19/covid-provincias/data_uniprovs.csv?inline=false" ),
                    as.character(source) ),
  source_name = ifelse( province_uni == "Cantabria" & !is.na(province_uni), paste0(as.character(source_name),";Servicio Cántabro de Salud vía @danielegrasso"), as.character(source_name) ),
  source = ifelse(  province_uni =="Cantabria" & !is.na(province_uni),
                    paste0(source,";https://www.scsalud.es/coronavirus;https://gitlab.com/elpais/datos/-/raw/master/20_Covid-19/covid-provincias/data_uniprovs.csv?inline=false" ),
                    as.character(source) ),
  source_name = ifelse( province_uni == "Rioja, La" & !is.na(province_uni), paste0(as.character(source_name),";Gobierno de La Rioja vía @danielegrasso"), as.character(source_name) ),
  source = ifelse(  province_uni =="Rioja, La" & !is.na(province_uni),
                    paste0(source,";https://actualidad.larioja.org/coronavirus;https://gitlab.com/elpais/datos/-/raw/master/20_Covid-19/covid-provincias/data_uniprovs.csv?inline=false" ),
                    as.character(source) ),
  source_name = ifelse( province_uni == "Navarra" & !is.na(province_uni), paste0(as.character(source_name),";Gobierno Abierto de Navarra vía @danielegrasso"), as.character(source_name) ),
  source = ifelse(  province_uni =="Navarra" & !is.na(province_uni),
                    paste0(source,";https://gobiernoabierto.navarra.es/es/coronavirus/impacto-situacion;https://gitlab.com/elpais/datos/-/raw/master/20_Covid-19/covid-provincias/data_uniprovs.csv?inline=false" ),
                    as.character(source) ),
  source_name = ifelse( province_uni == "Murcia" & !is.na(province_uni), 
                        paste0(as.character(source_name),";Murcia Salud vía @danielegrasso"),
                        as.character(source_name) ),
  source = ifelse(  province_uni =="Murcia" & !is.na(province_uni),
                    paste0(source,";https://www.murciasalud.es/pagina.php?id=458869&idsec=6575;https://gitlab.com/elpais/datos/-/raw/master/20_Covid-19/covid-provincias/data_uniprovs.csv?inline=false" ),
                    as.character(source) )
) %>% select( -date_uni, -province_uni, -ccaa_uni, -date_new ,-cases_accumulated_PCR_uni, -deceased_uni, -dunique)

rm(uniprovinciales_d)

# Baleares --------------
download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=Baleares", 
              "data/original/spain/baleares/baleares.csv")
baleares <- read.delim("data/original/spain/baleares/baleares.csv",sep = ",")

# Remove Balerares  
data_cases_sp_provinces <-  data_cases_sp_provinces %>% filter( ccaa != "Balears, Illes")

# Add Baleares data
data_cases_sp_provinces <- rbind(data_cases_sp_provinces,
                                 baleares)

# Add missing data deaths previous 2020.03.08 --------------

# remove date previos to March 8. ISCIII does not have deaths before that day
datadista <- read.delim("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_fallecidos.csv",sep = ",") %>% melt(
  id.vars = c("CCAA","cod_ine")) %>% mutate (
    date = as.Date(substr(variable,2,12),"%Y.%m.%d"),
    date = date-1 # minus one day to correct the date
  ) %>% select(-variable) %>% filter( date < as.Date("2020-03-08") ) %>% filter (
    (CCAA == "Melilla" | CCAA == "Asturias" | CCAA == "Balears, Illes" | CCAA == "Cantabria" |
       CCAA == "Ceuta" | CCAA == "Murcia" | CCAA == "Navarra" | CCAA == "Madrid" |
       CCAA == "Rioja, La") ) %>% mutate (
         CCAA = CCAA %>% str_replace_all("La Rioja", "Rioja, La"),
         CCAA = CCAA %>% str_replace_all("Asturias", "Asturias, Principado de"),
         CCAA = CCAA %>% str_replace_all("Baleares", "Balears, Illes"),
         CCAA = CCAA %>% str_replace_all("Madrid", "Madrid, Comunidad de"),
         CCAA = CCAA %>% str_replace_all("Murcia", "Murcia, Región de"),
         CCAA = CCAA %>% str_replace_all("Navarra", "Navarra, Comunidad Foral de"),
         dunique = paste0(CCAA,date)
       ) %>% select(dunique, value, date)

data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate (
  dunique = paste0(ccaa,date)
)
# merge existing province data with previous data from Datadista, only for uniprovinciales
data_cases_sp_provinces <- merge( data_cases_sp_provinces,
                                  datadista %>% rename(
                                    deceassed_datadista = value) %>% select(-date),
                                  by.x = "dunique", by.y = "dunique", all.x = TRUE   )

# check
# data_all_export %>% select (deceassed,deceassed_datadista,date,region ) %>% filter ( (date < as.Date("2020-03-10")) & (region == "Madrid") )

# Fill data if is empty
data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
  deceased = ifelse( !is.na(deceassed_datadista), deceassed_datadista, deceased),
  source_name =  ifelse( !is.na(deceassed_datadista), paste0(source_name,";Ministerio de Sanidad (Datadista)"), source_name),
  source =  ifelse( !is.na(deceassed_datadista), paste0(source,";https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_fallecidos.csv"), source),
) %>% select(-dunique,-deceassed_datadista)

# Madrid substitute list of PCR+ by Comunidad de Madrid data --------
download.file("https://raw.githubusercontent.com/alfonsotwr/snippets/master/covidia-cam/madrid-pcr.csv", 
              "data/original/spain/madrid/madrid-pcr.csv")
madrid_pcr <- read.delim("data/original/spain/madrid/madrid-pcr.csv",sep = ",") %>% mutate(
  date = as.Date(as.character(Fecha), "%Y-%m-%d")
)

# add Madrid PCR+ data 
data_cases_sp_provinces <- merge( data_cases_sp_provinces %>% mutate ( dunique = paste0(province,date) ),
# zzz <- merge( data_cases_sp_provinces %>% mutate ( dunique = paste0(province,date) ),
                                  madrid_pcr %>% mutate( dunique = paste0("Madrid",date)) %>% select(-Fecha) %>% rename(date_mad = date),
                                  by.x = "dunique", by.y = "dunique", all = TRUE ) %>% mutate(
                                    cases_accumulated_PCR = ifelse( !is.na(PCR.), PCR., cases_accumulated_PCR)
                                  ) %>% select(-PCR.) %>% mutate (
                                    province = ifelse(is.na(province),"Madrid",province ), #TODO: repasar qué es lo que fallaba
                                    ccaa = ifelse(is.na(ccaa),"Madrid, Comunidad de",ccaa ),
                                    # dunique = paste0("Madrid",date),
                                    date = ifelse(is.na(date),date_mad,date ),
                                    # date = ifelse(is.na("NANA"),substr(dunique,7,16),date ),
                                    date = as.Date(date,  origin=as.Date("1970-01-01") ),
                                    source_name =  ifelse( province == "Madrid", paste0(source_name,";Comunidad de Madrid"), source_name),
                                    source =  ifelse(province == "Madrid", paste0(source,";https://github.com/alfonsotwr/snippets/blob/master/covidia-cam/madrid-pcr.csv"), source),
                                  ) %>% select( -date_mad, -dunique) %>% filter ( !is.na(date))

# test <- madrid_pcr %>% mutate( dunique = paste0("Madrid",date)) %>% select(-date, -Fecha)


# Add manually the day that do not exist, therefore merge does not work TODO
# data_cases_sp_provinces <- rbind(
#   data_cases_sp_provinces,
#   madrid_pcr %>% filter ( (date == as.Date("2020-07-26") )) %>% mutate(
#     hospitalized = NA,
#     intensive_care = NA,
#     cases_accumulated_PCR = PCR.,
#     PCR = NA,
#     cases_accumulated = NA,
#     deceased = NA,
#     recovered = NA,
#     province = "Madrid",
#     ccaa = "Madrid, Comunidad de",
#     new_cases = NA,
#     TestAc = NA,
#     activos = NA,
#     recovered = NA,
#     source_name = "Comunidad de Madrid",
#     source = "https://github.com/alfonsotwr/snippets/blob/master/covidia-cam/madrid-pcr.csv",
#     comments = NA
#   ) %>% select(names(data_cases_sp_provinces) ) 
#   )

# Cantabria --------
# web https://www.scsalud.es/coronavirus download CSV at the bottom
# file https://www.scsalud.es/documents/2162705/9255280/2020_covid19_historico.csv 
# TODO: to avoid problem with CA certificate I downoad manually the file
# download.file("https://serviweb.scsalud.es:10443/ficheros/COVID19_historico.csv", 
#               "data/original/spain/cantabria/COVID19_historico.csv")

cantabria <- read.delim("data/original/spain/cantabria/COVID19_historico.csv",sep = ";") %>% mutate(
  date = as.Date(as.character(FECHA), "%d/%m/%Y"),
  cases_accumulated_PCR_can = cumsum(CASOS.NUEVOS.PCR.) # TODO: calculates cumulative value, it's not original data
) %>% rename (
  cases_accumulated_can = TOTAL.CASOS,
  PCR_can = CASOS.NUEVOS.PCR.,
  hospitalized_can = TOTAL.HOSPITALIZADOS,
  deceased_can = FALLECIDOS,
  recovered_can = RECUPERADOS
) %>% select(cases_accumulated_PCR_can,everything() )

# add Cantabria data
data_cases_sp_provinces <- merge( data_cases_sp_provinces %>% mutate ( dunique = paste0(province,date) ),
                cantabria %>% mutate( dunique = paste0("Cantabria",date)) %>% select( dunique, cases_accumulated_can, PCR_can,
                                                                                      hospitalized_can, deceased_can, recovered_can,cases_accumulated_PCR_can),
                by.x = "dunique", by.y = "dunique", all.x = TRUE ) %>% mutate(
                  cases_accumulated = ifelse( !is.na(cases_accumulated_can), cases_accumulated_can, cases_accumulated),
                  cases_accumulated_PCR = ifelse( !is.na(cases_accumulated_PCR_can), cases_accumulated_PCR_can, cases_accumulated_PCR),
                  PCR = ifelse( !is.na(PCR_can), PCR_can, PCR),
                  hospitalized = ifelse( !is.na(hospitalized_can), hospitalized_can, hospitalized),
                  deceased = ifelse( !is.na(deceased_can), deceased_can, deceased),
                  recovered = ifelse( !is.na(recovered_can), recovered_can, recovered)
                ) %>% select(-cases_accumulated_can,-cases_accumulated_PCR_can, -PCR_can, -hospitalized_can, -deceased_can, -recovered_can, -dunique) %>% mutate (
                  source_name =  ifelse( province == "Cantabria", paste0(source_name,";Servicio Cántabro de Salud"), source_name),
                  source =  ifelse(province == "Cantabria", paste0(source,";https://serviweb.scsalud.es:10443/ficheros/COVID19_historico.csv;https://www.scsalud.es/coronavirus"), source),
                )

cantabria <- cantabria %>% filter (date > as.Date("2020-07-19")) %>% rename(
  PCR = PCR_can,
  cases_accumulated = cases_accumulated_can,
  deceased = deceased_can,
  recovered = recovered_can,
  hospitalized = hospitalized_can
) %>% mutate(
  province = "Cantabria",
  ccaa = "Cantabria",
  new_cases = NA,
  TestAc = NA,
  activos = NA,
  cases_accumulated_PCR = NA,
  intensive_care = NA,
  recovered = NA,
  source_name = "Servicio Cántabro de Salud",
  source = "https://github.com/lipido/galicia-covid19/blob/master/ferrol.csv;https://github.com/lipido/galicia-covid19/blob/master/ferrol.ext.csv",
  comments = NA
) %>% select(names(data_cases_sp_provinces)  )

# Add rows that don-t have an existing previous date created for cantabria
data_cases_sp_provinces <- rbind(
  data_cases_sp_provinces,
  cantabria
)

# Murcia -------
# donwload provincias data from googgle spreadsheet 
download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=Murcia", 
              "data/original/spain/murcia/murcia.csv")
# load data
murcia <- read.delim("data/original/spain/murcia/murcia.csv",sep = ",") %>% mutate(
  date = as.Date(as.character(date), "%d/%m/%Y"),
) %>% select(-Pruebas.diagnósticas.totales, -recovered_PCR ) %>% mutate(
  PCR = NA
) %>% select( names(data_cases_sp_provinces) )

# Add rows
data_cases_sp_provinces <- rbind(
  data_cases_sp_provinces %>% filter( !( date > as.Date("2020-07-16") & province == "Murcia" ) ), #remove unwanted Murcia rows
  murcia %>% filter( date > as.Date("2020-07-16") )
)

# Navarra -------
download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=Navarra", 
              "data/original/spain/navarra/navarra.csv")

# load data
navarra <- read.delim("data/original/spain/navarra/navarra.csv",sep = ",") %>% mutate(
  date = as.Date( date )
)

data_cases_sp_provinces <- rbind(
  data_cases_sp_provinces %>% filter( !( date > as.Date("2020-07-18") & province == "Navarra" ) ), #remove unwanted Navarra rows
  navarra %>% filter( date > as.Date("2020-07-18") )
)

# Asturias --------------
# TODO download manually from https://app.transparenciaendatos.es/v/#!/5eb4344e16b9fc465933d217. Table at the bottom
# No uso este método: de momento usamos la pestaña con datos a mano de la hoja de cálculo
# asturias <- read.delim("data/original/spain/asturias/historico-datos-asturias.csv",sep = ",") %>% mutate(
#   date = as.Date( FECHA, "%d/%m/%Y" )
# ) %>% rename(
#   hospitalized = Personas.hospitalizadas.actualmente.en.planta,
#   intensive_care = Personas.hospitalizadas.actualmente.en.UCI,
#   cases_accumulated_PCR = Casos.confirmados.por.PCR,
# ) %>% mutate(
#   PCR = NA,
#   cases_accumulated = NA,
#   deceased = NA,
#   recovered = NA,
#   province = "Asturias",
#   ccaa = "Asturias, Principado de",
#   new_cases = NA,
#   TestAc = NA,
#   activos = NA,
#   recovered = NA,
#   source_name = "Gobierno del Principado de Asturias",
#   source = "https://app.transparenciaendatos.es/v/#!/5eb4344e16b9fc465933d217",
#   comments = NA
# ) %>% select(names(data_cases_sp_provinces)  )
# 
# data_cases_sp_provinces <- rbind(
#   data_cases_sp_provinces %>% filter( !( date > as.Date("2020-07-19") & province == "Asturias" ) ), #remove unwanted Asturias rows
#   asturias %>% filter( date > as.Date("2020-07-19") )
# )

# download manual data from spreadsheet
download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=Asturias", 
              "data/original/spain/asturias/asturias.csv")

# load data
asturias2 <- read.delim("data/original/spain/asturias/asturias.csv",sep = ",") %>% mutate(
  date = as.Date( date )
)
# add data after 
data_cases_sp_provinces <- rbind(
  data_cases_sp_provinces %>% filter( !( date > as.Date("2020-07-19") & province == "Asturias" ) ), #remove unwanted Asturias rows
  asturias2 %>% filter( date > as.Date("2020-07-19") ) %>% select(names(data_cases_sp_provinces))
)

# substitute all the hospitalized and intensive care data
data_cases_sp_provinces <- merge(
  data_cases_sp_provinces %>% mutate ( dunique = paste0( date, province) ) %>% ungroup(),
  asturias2 %>% mutate ( dunique = paste0( date, province) ) %>% ungroup() %>% select(dunique,hospitalized, intensive_care, source, source_name, comments) %>%
    rename(
    hosp_ast = hospitalized,
    uci_ast = intensive_care,
    source_ast = source,
    source_name_ast = source_name,
    com_ast = comments
  ),
  by.x = "dunique", by.y = "dunique", all.x = TRUE
)

data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
# zzz <- data_cases_sp_provinces %>% mutate(
  hospitalized = ifelse(province == "Asturias" & date < as.Date("2020-07-19"), hosp_ast, hospitalized ),
  intensive_care = ifelse(province == "Asturias" & date < as.Date("2020-07-19"), uci_ast, intensive_care ),
  source = ifelse(
    province == "Asturias" & date < as.Date("2020-07-19"), 
    paste0(source,";",source_ast), 
    source ),
  source_name = ifelse(
    province == "Asturias" & date < as.Date("2020-07-19"),
    paste0(source_name,";",source_name_ast), 
    source_name ),
  comments = ifelse(
    province == "Asturias" & date < as.Date("2020-07-19"),
    paste0(comments,";",com_ast), 
    comments )
) %>% select (-dunique, -hosp_ast, -uci_ast, -source_ast, -source_name_ast, -com_ast )

# Catalunya deaths---------
# de la web https://analisi.transparenciacatalunya.cat/es/Salut/Dades-di-ries-de-COVID-19-per-rees-de-gesti-assist/dmzh-fz47 y 
# a su vez de https://dadescovid.cat/descarregues?drop_es_residencia=1
download.file("https://analisi.transparenciacatalunya.cat/api/views/dmzh-fz47/rows.csv?accessType=DOWNLOAD&bom=true&format=true&delimiter=%3B&sorting=true", 
              "data/original/spain/catalunya/Dades_di_ries_de_COVID-19_per__rees_de_gesti__assistencials__AGA_.csv")
# informaci'on de coorespondenica AGA con provincias http://www.aceba.cat/files/doc386/aga-arees-gestio-assistencial.pdf
catalunya <- read.delim("data/original/spain/catalunya/Dades_di_ries_de_COVID-19_per__rees_de_gesti__assistencials__AGA_.csv",sep = ";") 
aga_prov <-  read.delim("data/original/spain/catalunya/aga-provincias-catalunya.csv",sep = ",") 

# creates date format and fix characters with accents
catalunya <- catalunya %>% mutate (date = as.Date(DATA, "%d/%m/%Y") ) %>% select(date, everything()) %>% mutate(
  NOM = as.character(NOM),
  NOM = NOM %>% str_replace_all("Ã\u0080", "À"),
  NOM = NOM %>% str_replace_all("Ã\u0088", "È"),
  NOM = NOM %>% str_replace_all("Ã\u0087", "Ç"),
  NOM = as.factor(NOM)
)

# sum(catalunya$EXITUS)
# levels(catalunya$NOM)
# levels(aga_prov$aga)

# add province information to AGA
catalunya <- merge( catalunya,
                    aga_prov,
                    by.x = "NOM", by.y= "aga" )


# iterates through data to count cases and other measures
catalunya_process <- catalunya %>% group_by(date,province) %>% arrange(date) %>% 
  summarise ( 
    pcr_by_day = sum(PCR),
    cases_by_day = sum(CASOS_CONFIRMAT),
    ingresos_by_day = sum(INGRESSOS_TOTAL),
    ingresos_critic_by_day = sum(INGRESSOS_CRITIC),
    ingresados = sum(INGRESSATS_TOTAL),
    UCI = sum(INGRESSATS_CRITIC),
    deceased_by_day = sum(EXITUS)
  )

catalunya_process <- catalunya_process %>% group_by(province) %>% mutate (
  deceased_cum = cumsum(deceased_by_day)
)

data_cases_sp_provinces <- merge(
  data_cases_sp_provinces %>% mutate ( dunique = paste0( date, province) ) %>% ungroup(),
  catalunya_process %>% mutate ( dunique = paste0( date, province) ) %>% ungroup() %>% select (dunique, deceased_cum, ingresados, UCI ) ,
  by.x = "dunique", by.y = "dunique", all = TRUE
)

data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
 deceased = ifelse(ccaa == "Cataluña", deceased_cum, deceased ),
 hospitalized = ifelse(ccaa == "Cataluña"  & date > as.Date("2020-04-30"), ingresados, hospitalized ), # insert data after 2020.05.01 because earlier values are 0
 intensive_care = ifelse(ccaa == "Cataluña"  & date > as.Date("2020-04-30"), UCI, intensive_care ),
 source = ifelse(
   !is.na(deceased_cum) | !is.na(ingresados) | !is.na(UCI), 
   paste0(source,";https://analisi.transparenciacatalunya.cat/Salut/Dades-di-ries-de-COVID-19-per-rees-de-gesti-assist/dmzh-fz47"), 
   source ),
 source_name = ifelse(
   !is.na(deceased_cum) | !is.na(ingresados) | !is.na(UCI), 
   paste0(source_name,";Transparencia Catalunya"), 
   source_name )
) %>% select (-deceased_cum, -dunique, -ingresados, -UCI)

# La Rioja --------------
download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=Rioja", 
              "data/original/spain/rioja/rioja.csv")
rioja <- read.delim("data/original/spain/rioja/rioja.csv",sep = ",") %>% mutate (
  # deceased_cum = cumsum(deceased),
  # deceased = deceased_cum,
  date = as.Date( as.character(date))
) %>% select(names(data_cases_sp_provinces))

# Remove Rioja 
# TODO: try to not lose data taht are not in this new data source
# Add Baleares data
data_cases_sp_provinces <- rbind(data_cases_sp_provinces,
                                 rioja %>% filter( date > as.Date("2020-07-19") )
                                                   )
# Melilla --------------
download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=Melilla", 
              "data/original/spain/melilla/melilla.csv")
melilla <- read.delim("data/original/spain/melilla/melilla.csv",sep = ",") %>% mutate (
  date = as.Date( as.character(date))
) %>% select(names(data_cases_sp_provinces))

# Remove melilla 
# TODO: try to not lose data taht are not in this new data source
# Add Melilla data
data_cases_sp_provinces <- rbind(data_cases_sp_provinces,
                                 melilla %>% filter( date > as.Date("2020-07-19") )
)

# Ceuta --------------
download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=Ceuta", 
              "data/original/spain/ceuta/ceuta.csv")
ceuta <- read.delim("data/original/spain/ceuta/ceuta.csv",sep = ",") %>% mutate (
  date = as.Date( as.character(date))
) %>% select(names(data_cases_sp_provinces))

# Remove Ceuta 
# TODO: try to not lose data taht are not in this new data source
# Add Melilla data
data_cases_sp_provinces <- rbind(data_cases_sp_provinces,
                                 ceuta %>% filter( date > as.Date("2020-07-19") )
)


# D. Add province ISCIII RENAVE data -----
download.file("https://cnecovid.isciii.es/covid19/resources/datos_provincias.csv", 
              "data/original/spain/iscii_casos_renave.csv")
ciii_renave <- read.delim("data/original/spain/iscii_casos_renave.csv",sep = ",") %>% 
  mutate ( 
    date = as.Date(as.character(fecha)),
    provincia_iso = ifelse ( is.na(provincia_iso), "NA", as.character(provincia_iso) )
  )

ciii_renave <- ciii_renave %>%
  mutate(
    province =  "",
    province = ifelse( provincia_iso =="C", "Coruña, A", province ),
    province = ifelse( provincia_iso =="VI", "Araba/Álava", province ),
    province = ifelse( provincia_iso =="AB", "Albacete", province ),
    province = ifelse( provincia_iso =="A", "Alicante/Alacant", province ),
    province = ifelse( provincia_iso =="AL", "Almería", province ),
    province = ifelse( provincia_iso =="O", "Asturias", province ),
    province = ifelse( provincia_iso =="AV", "Ávila", province ),
    province = ifelse( provincia_iso =="BA", "Badajoz", province ),
    province = ifelse( provincia_iso =="PM", "Balears, Illes", province ),
    province = ifelse( provincia_iso =="B", "Barcelona", province ),
    province = ifelse( provincia_iso =="BI", "Bizkaia", province ),
    province = ifelse( provincia_iso =="BU", "Burgos", province ),
    province = ifelse( provincia_iso =="CC", "Cáceres", province ),
    province = ifelse( provincia_iso =="CA", "Cádiz", province ),
    province = ifelse( provincia_iso =="S", "Cantabria", province ),
    province = ifelse( provincia_iso =="CS", "Castellón/Castelló", province ),
    province = ifelse( provincia_iso =="CR", "Ciudad Real", province ),
    province = ifelse( provincia_iso =="CO", "Córdoba", province ),
    province = ifelse( provincia_iso =="CU", "Cuenca", province ),
    province = ifelse( provincia_iso =="SS", "Gipuzkoa", province ),
    province = ifelse( provincia_iso =="GI", "Girona", province ),
    province = ifelse( provincia_iso =="GR", "Granada", province ),
    province = ifelse( provincia_iso =="GU", "Guadalajara", province ),
    province = ifelse( provincia_iso =="H", "Huelva", province ),
    province = ifelse( provincia_iso =="HU", "Huesca", province ),
    province = ifelse( provincia_iso =="J", "Jaén", province ),
    province = ifelse( provincia_iso =="LO", "Rioja, La", province ),
    province = ifelse( provincia_iso =="GC", "Palmas, Las", province ),
    province = ifelse( provincia_iso =="LE", "León", province ),
    province = ifelse( provincia_iso =="L", "Lleida", province ),
    province = ifelse( provincia_iso =="LU", "Lugo", province ),
    province = ifelse( provincia_iso =="M", "Madrid", province ),
    province = ifelse( provincia_iso =="MA", "Málaga", province ),
    province = ifelse( provincia_iso =="MU", "Murcia", province ),
    province = ifelse( provincia_iso =="OR", "Ourense", province ),
    province = ifelse( provincia_iso =="P", "Palencia", province ),
    province = ifelse( provincia_iso =="PO", "Pontevedra", province ),
    province = ifelse( provincia_iso =="SA", "Salamanca", province ),
    province = ifelse( provincia_iso =="TF", "Santa Cruz de Tenerife", province ),
    province = ifelse( provincia_iso =="SE", "Sevilla", province ),
    province = ifelse( provincia_iso =="SG", "Segovia", province ),
    province = ifelse( provincia_iso =="SO", "Soria", province ),
    province = ifelse( provincia_iso =="T", "Tarragona", province ),
    province = ifelse( provincia_iso =="TE", "Teruel", province ),
    province = ifelse( provincia_iso =="TO", "Toledo", province ),
    province = ifelse( provincia_iso =="V", "Valencia/València", province ),
    province = ifelse( provincia_iso =="VA", "Valladolid", province ),
    province = ifelse( provincia_iso =="ZA", "Zamora", province ),
    province = ifelse( provincia_iso =="Z", "Zaragoza", province ),
    province = ifelse( provincia_iso =="CE", "Ceuta", province ),
    province = ifelse( provincia_iso =="ME", "Melilla", province ),
    province = ifelse( provincia_iso =="NA", "Navarra", province ),
    dunique = paste0( date, province)
  )

data_cases_sp_provinces <- merge(
  data_cases_sp_provinces %>% mutate ( dunique = paste0( date, province) ),
  ciii_renave %>% select (-provincia_iso, -fecha) %>% rename ( date_ren = date, province_ren = province ),
  by.x = "dunique", by.y = "dunique", all= TRUE
)

data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
  # no sé por qué pero he tenido que montar este lío para que las fechas funcionaran
  date_new = ifelse( is.na(date) , date_ren, NA ),
  date_new = as.Date(date_ren, origin=as.Date("1970-01-01") ),
  date = ifelse( is.na(date) & !is.na(date_ren), date_new, date ),
  date = as.Date(date, origin=as.Date("1970-01-01") ),
  
  ccaa = "",
  province = ifelse ( is.na(province), province_ren, province),
  ccaa = ifelse( province == "Coruña, A", "Galicia", ccaa ),
  ccaa = ifelse( province =="Araba/Álava", "País Vasco", ccaa ),
  ccaa = ifelse( province =="Albacete", "Castilla - La Mancha", ccaa ),
  ccaa = ifelse( province =="Alicante/Alacant", "Comunitat Valenciana", ccaa ),
  ccaa = ifelse( province =="Almería", "Andalucía", ccaa ),
  ccaa = ifelse( province =="Asturias", "Asturias, Principado de", ccaa ),
  ccaa = ifelse( province =="Ávila", "Castilla y León", ccaa ),
  ccaa = ifelse( province =="Badajoz", "Extremadura", ccaa ),
  ccaa = ifelse( province =="Balears, Illes", "Balears, Illes", ccaa ),
  ccaa = ifelse( province =="Barcelona", "Cataluña", ccaa ),
  ccaa = ifelse( province =="Bizkaia", "País Vasco", ccaa ),
  ccaa = ifelse( province =="Burgos", "Castilla y León", ccaa ),
  ccaa = ifelse( province == "Cáceres", "Extremadura", ccaa ),
  ccaa = ifelse( province == "Cádiz", "Andalucía", ccaa ),
  ccaa = ifelse( province == "Cantabria", "Cantabria", ccaa ),
  ccaa = ifelse( province == "Castellón/Castelló", "Comunitat Valenciana", ccaa ),
  ccaa = ifelse( province == "Ciudad Real", "Castilla - La Mancha", ccaa ),
  ccaa = ifelse( province == "Córdoba", "Andalucía", ccaa ),
  ccaa = ifelse( province == "Cuenca", "Castilla - La Mancha", ccaa ),
  ccaa = ifelse( province == "Gipuzkoa", "País Vasco", ccaa ),
  ccaa = ifelse( province == "Girona", "Cataluña", ccaa ),
  ccaa = ifelse( province == "Granada", "Andalucía", ccaa ),
  ccaa = ifelse( province == "Guadalajara", "Castilla - La Mancha", ccaa ),
  ccaa = ifelse( province == "Huelva", "Andalucía", ccaa ),
  ccaa = ifelse( province == "Huesca", "Aragón", ccaa ),
  ccaa = ifelse( province == "Jaén", "Andalucía", ccaa ),
  ccaa = ifelse( province == "Rioja, La", "Rioja, La", ccaa ),
  ccaa = ifelse( province == "Palmas, Las", "Canarias", ccaa ),
  ccaa = ifelse( province == "León", "Castilla y León", ccaa ),
  ccaa = ifelse( province == "Lleida", "Cataluña", ccaa ),
  ccaa = ifelse( province == "Lugo", "Galicia", ccaa ),
  ccaa = ifelse( province == "Madrid", "Madrid, Comunidad de", ccaa ),
  ccaa = ifelse( province == "Málaga", "Andalucía", ccaa ),
  ccaa = ifelse( province == "Murcia", "Murcia, Región de", ccaa ),
  ccaa = ifelse( province == "Ourense", "Galicia", ccaa ),
  ccaa = ifelse( province == "Palencia", "Castilla y León", ccaa ),
  ccaa = ifelse( province == "Pontevedra", "Galicia", ccaa ),
  ccaa = ifelse( province == "Salamanca", "Castilla y León", ccaa ),
  ccaa = ifelse( province == "Santa Cruz de Tenerife", "Canarias", ccaa ),
  ccaa = ifelse( province == "Sevilla", "Andalucía", ccaa ),
  ccaa = ifelse( province == "Segovia", "Castilla y León", ccaa ),
  ccaa = ifelse( province == "Soria", "Castilla y León", ccaa ),
  ccaa = ifelse( province == "Tarragona", "Cataluña", ccaa ),
  ccaa = ifelse( province == "Teruel", "Aragón", ccaa ),
  ccaa = ifelse( province == "Toledo", "Castilla - La Mancha", ccaa ),
  ccaa = ifelse( province == "Valencia/València", "Comunitat Valenciana", ccaa ),
  ccaa = ifelse( province == "Valladolid", "Castilla y León", ccaa ),
  ccaa = ifelse( province == "Zamora", "Castilla y León", ccaa ),
  ccaa = ifelse( province == "Zaragoza", "Aragón", ccaa ),
  ccaa = ifelse( province == "Ceuta", "Ceuta", ccaa ),
  ccaa = ifelse( province == "Melilla", "Melilla", ccaa ),
  ccaa = ifelse( province == "Navarra", "Navarra, Comunidad Foral de", ccaa )
) %>% mutate(
  source = ifelse( is.na(province_ren), 
                   source, 
                   paste0( ifelse( 
                     is.na(source),
                     "", 
                     source), ifelse( is.na(source),"",";"), "https://cnecovid.isciii.es/covid19/resources/datos_provincias.csv")),
  source_name = ifelse( is.na(province_ren), 
                        source_name ,
                        paste0( ifelse( is.na(source_name),"", source_name), ifelse( is.na(source_name),"",";") ,"ISCIII RENAVE") )
) %>% select (- province_ren, -date_new, -date_ren, -dunique)

# Add population data -----
data_cases_sp_provinces <- merge( data_cases_sp_provinces, select(provincias_poblacion,provincia,poblacion,ine_code), by.x = "province", by.y = "provincia", all = TRUE   )

# Calculate values per inhab. -------------- 
data_cases_sp_provinces$cases_per_cienmil <- round( data_cases_sp_provinces$cases_accumulated / data_cases_sp_provinces$poblacion * 100000, digits = 2)
data_cases_sp_provinces$intensive_care_per_1000000 <- round( data_cases_sp_provinces$intensive_care / data_cases_sp_provinces$poblacion * 100000, digits = 2)
data_cases_sp_provinces$deceassed_per_100000 <- round( data_cases_sp_provinces$deceased / data_cases_sp_provinces$poblacion * 100000, digits = 2)
data_cases_sp_provinces$hospitalized_per_100000 <- round( data_cases_sp_provinces$hospitalized / data_cases_sp_provinces$poblacion * 100000, digits = 2)

# E. Calculates daily data and averages ----------------
data_cases_sp_provinces <- data_cases_sp_provinces %>% 
    group_by(province) %>% arrange(date) %>% 
  mutate( 
    cases_14days = cases_accumulated - lag(cases_accumulated,13),
    cases_7days = cases_accumulated - lag(cases_accumulated,6),
    cases_PCR_14days = cases_accumulated_PCR - lag(cases_accumulated_PCR,13),
    cases_PCR_7days = cases_accumulated_PCR - lag(cases_accumulated_PCR,6),
    daily_cases = cases_accumulated - lag(cases_accumulated),
    daily_cases_avg7 =  round( ( daily_cases + lag(daily_cases,1)+lag(daily_cases,2)+
                                   lag(daily_cases,3)+lag(daily_cases,4)+lag(daily_cases,5)+lag(daily_cases,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
    daily_cases_PCR = cases_accumulated_PCR - lag(cases_accumulated_PCR),
    daily_cases_PCR = ifelse( is.na(daily_cases_PCR), PCR, daily_cases_PCR), # inserta datos originales de PCR diarios si la diferencia del acumulado no se puede calcular
    daily_cases_PCR_avg7 =  round( ( daily_cases_PCR + lag(daily_cases_PCR,1)+lag(daily_cases_PCR,2)+
                                       lag(daily_cases_PCR,3)+lag(daily_cases_PCR,4) +lag(daily_cases_PCR,5) +lag(daily_cases_PCR,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
    daily_deaths = deceased - lag(deceased),
    daily_deaths_inc = round((deceased - lag(deceased)) /lag(deceased) * 100, digits = 1),

    daily_deaths_avg3 =  round( ( daily_deaths + lag(daily_deaths,1)+lag(daily_deaths,2) ) / 3, digits = 1 ), # average of daily deaths of 3 last days
    # daily_deaths_avg3_zoo =  round( zoo::rollmean( daily_deaths,3 , na.pad = TRUE  ), digits = 1), #TODO calculate roll mean with this function. Check why dates are shifted one day
    # daily_deaths_avg3_zoo_fix =   lag(daily_deaths_avg3_zoo,1),

    daily_deaths_avg7 =  round( ( daily_deaths + lag(daily_deaths,1)+lag(daily_deaths,2)+
                                    lag(daily_deaths,3)+lag(daily_deaths,4)+lag(daily_deaths,5)+lag(daily_deaths,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
    # deaths_cum_last_week = deceased - lag(deceased,6),
    deaths_last_week =  daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) + lag(daily_deaths,4) + lag(daily_deaths,5) + lag(daily_deaths,6)
    # hospitalized_avg7 =  round( ( hospitalized + lag(hospitalized,1)+lag(hospitalized,2)+
    #                                 lag(hospitalized,3)+lag(hospitalized,4)+lag(hospitalized,5)+lag(hospitalized,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
    
  )

# zzz <- data_cases_sp_provinces %>% group_by(province) %>%
#   mutate(dif_casos = c(NA,diff(cases_accumulated_PCR))) %>%
#   filter(dif_casos >= 0 | is.na(dif_casos)) %>% arrange(date) %>%
#   mutate(
#     daily_cases_PCR_avg7_zoo = round( zoo::rollmeanr(dif_casos,7,na.pad=T), digits = 1 ),
#     fechas_dif = c(NA, diff(date))
#     ) %>%
#    select(date, province, daily_cases_PCR, dif_casos, daily_cases_PCR_avg7, daily_cases_PCR_avg7_zoo, daily_cases_PCR,fechas_dif )

# Calculates averages when no enough data available-----------
# Code provided by @picanumeros
# Caclulates average when no values are available. Usually when in the weekends no data are available, daily_cases_PCR_avg7 can't be calculated
# Lo que propongo es que cuando haya un hueco de varios días sin datos, el dato del primer día en el 
# que vuelva a haber se promedie entre los días 
# que se ha estado sin datos. Digamos que se "reparten" los casos a lo largo de los días.
pcr_avg7 <- data_cases_sp_provinces %>% # filter(date >= as.Date("2020-05-11")) %>%  
  group_by(province) %>%
  mutate(
    dif_casos = c(NA,diff(cases_accumulated_PCR)),
    dif_casos = ifelse( is.na(dif_casos),daily_cases_PCR,dif_casos)
    ) %>%
  filter(dif_casos >= 0 | !is.na(dif_casos)) %>% # filter(dif_casos < 60) %>%
  # filter(province == "Alicante/Alacant") %>%
  arrange(date) %>%
  mutate(
          fechas_dif = c(NA, diff(date)),
         serie = dif_casos/fechas_dif,
         daily_cases_PCR_avg7_complete = round( zoo::rollmeanr(serie, 7, na.pad = T), digits = 1))  %>%
  select(date, ccaa, province, daily_cases_PCR, dif_casos, daily_cases_PCR_avg7, daily_cases_PCR,fechas_dif, serie, daily_cases_PCR_avg7_complete )
  # select(date, ccaa, province, PCR, cases_accumulated_PCR,daily_cases_PCR, dif_casos, daily_cases_PCR_avg7, daily_cases_PCR )

# Check plot
# pcr_avg7 %>% filter(province == "Alicante/Alacant") %>%
#   ggplot(aes(x = date, y = serie)) +
#   geom_point() +
#   geom_line(aes(x = date, y = daily_cases_PCR_avg7_complete), col = "blue", size = 1.05) +
#   geom_point(aes(x = date, y = daily_cases_PCR_avg7_complete), col = "green", size = 2)

# Add data to the source
  # data_cases_sp_provinces <- merge(
data_cases_sp_provinces <- merge(
    data_cases_sp_provinces %>% mutate ( dunique = paste0( date, province) ),
    pcr_avg7 %>% mutate ( dunique = paste0( date, province) ) %>% ungroup() %>% select (dunique,daily_cases_PCR_avg7_complete),
    by.x = "dunique", by.y = "dunique", all= TRUE
  ) %>% mutate (
    daily_cases_PCR_avg7 = daily_cases_PCR_avg7_complete
  ) %>% select (-daily_cases_PCR_avg7_complete,-dunique)
  # select(date, province,daily_cases_PCR,daily_cases_PCR_avg7,daily_cases_PCR_avg7_complete)

# calculate average 7 days deaths
deaths_avg7 <- data_cases_sp_provinces %>% # filter(date >= as.Date("2020-05-11")) %>%  
  group_by(province) %>%
  mutate(
    dif_deaths = c(NA,diff(deceased))
    # dif_deaths = ifelse( is.na(dif_deaths),daily_deaths,dif_deaths)
  ) %>%  #select(date, ccaa, province, deceased, dif_deaths, daily_deaths) %>%
  filter(dif_deaths >= 0 | !is.na(dif_deaths)) %>% # filter(dif_casos < 60) %>%
  # filter(province == "Alicante/Alacant") %>%
  arrange(date) %>%
  mutate(
    fechas_dif = c(NA, diff(date)),
    serie = dif_deaths/fechas_dif,
    daily_deaths_avg7_complete = round( zoo::rollmeanr(serie, 7, na.pad = T), digits = 1))  %>%
  select(date, ccaa, province, deceased, serie, fechas_dif, dif_deaths, daily_deaths, daily_deaths_avg7_complete, daily_deaths_avg7 )

# Add data to the source
data_cases_sp_provinces <- merge(
  data_cases_sp_provinces %>% mutate ( dunique = paste0( date, province) ),
  deaths_avg7 %>% mutate ( dunique = paste0( date, province) ) %>% ungroup() %>% select (dunique,daily_deaths_avg7_complete,),
  by.x = "dunique", by.y = "dunique", all= TRUE
) %>% mutate (
  daily_deaths_avg7 = daily_deaths_avg7_complete,
) %>% select (-daily_deaths_avg7_complete,-dunique)


# Reorder columns
data_cases_sp_provinces <- data_cases_sp_provinces %>% select(date,province,ine_code,everything())  
data_cases_sp_provinces <- data_cases_sp_provinces %>% select(-source, -source_name,-comments,source_name,source,comments)

# Re calculates factors to remove things like Andalucía for provinces ---
data_cases_sp_provinces$province <- factor(data_cases_sp_provinces$province)
data_cases_sp_provinces$ccaa <- factor(data_cases_sp_provinces$ccaa)

data_cases_sp_provinces <- data_cases_sp_provinces %>% filter(!is.na(date))

# Cleans unwanted NA in source and source_name
# TODO: averiguar por qu'e salen esos NA
data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
  source = source %>% str_replace_all("NA;", ""),
  source = source %>% str_replace_all(";NA", ""),
  source_name = source_name %>% str_replace_all(";NA", ""),
  source_name = source_name %>% str_replace_all("NA;", ""),
)

# F. Saves data in the other repository -------------
write.csv(data_cases_sp_provinces, file = "../escovid19data/data/output/covid19-provincias-spain_consolidated.csv", row.names = FALSE)
saveRDS(data_cases_sp_provinces, file = "../escovid19data/data/output/covid19-provincias-spain_consolidated.rds")
write.xlsx(data_cases_sp_provinces, "../escovid19data/data/output/covid19-provincias-spain_consolidated.xlsx", colNames = TRUE)

# save date, but first rearrange
data_cases_sp_provinces <- data_cases_sp_provinces %>% arrange(ccaa,province,date)

write.csv(data_cases_sp_provinces, file = "data/output/spain/covid19-provincias-spain_consolidated.csv", row.names = FALSE)
saveRDS(data_cases_sp_provinces, file = "data/output/spain/covid19-provincias-spain_consolidated.rds")

# cleans environment
rm(uniprovinciales, powerbi, catalunya, catalunya_new, cattotal, provincias_poblacion,datadista,ciii, 
   galicia_cumulative, madrid_original2, cantabria, murcia, madrid_pcr,navarra,asturias2, aga_prov, 
   andalucia_original2, andalucia2, aragon_b, baleares, catalunya_process, ceuta, ciii_original, ciii_renave,
   euskadi, melilla,rioja,valencia )

# Summarise by CCAA and Spain -------------

# Summarise by CCAA
spain_ccaa <- data_cases_sp_provinces %>% group_by(date, ccaa) %>% summarise(
  new_cases = sum(new_cases),
  PCR = sum(PCR),
  TestAc = sum(TestAc),
  activos = sum(activos),
  hospitalized = sum(hospitalized),
  intensive_care = sum(intensive_care),
  deceased = sum(deceased),
  cases_accumulated = sum(cases_accumulated),
  cases_accumulated_PCR = sum(cases_accumulated_PCR),
  recovered = sum(recovered)
)

spain <- data_cases_sp_provinces %>% group_by(date) %>% summarise(
  new_cases = sum(replace_na(new_cases, 0)),
  PCR = sum(replace_na(PCR, 0)),
  TestAc = sum(replace_na(TestAc, 0)),
  activos = sum(replace_na(activos, 0)),
  hospitalized = sum(replace_na(hospitalized, 0)),
  intensive_care = sum(replace_na(intensive_care, 0)),
  deceased = sum(replace_na(deceased, 0)),
  cases_accumulated = sum(replace_na(cases_accumulated, 0)),
  cases_accumulated_PCR = sum(replace_na(cases_accumulated_PCR, 0)),
  recovered = sum(replace_na(recovered, 0))
)

# Saves data in the other repository -------------
write.csv(spain_ccaa, file = "../escovid19data/data/output/covid19-ccaa-spain_consolidated.csv", row.names = FALSE)
saveRDS(spain_ccaa, file = "../escovid19data/data/output/covid19-ccaa-spain_consolidated.rds")
write.xlsx(spain_ccaa, "../escovid19data/data/output/covid19-ccaa-spain_consolidated.xlsx", colNames = TRUE)

write.csv(spain, file = "../escovid19data/data/output/covid19-spain_consolidated.csv", row.names = FALSE)
saveRDS(spain, file = "../escovid19data/data/output/covid19-spain_consolidated.rds")
write.xlsx(spain, "../escovid19data/data/output/covid19-spain_consolidated.xlsx", colNames = TRUE)
