# Procesar datos de Coronavirus COVID-19 en España por provincia

# Crea un dataframe que es utilizado en charts_spain_provinces.R para generar los gráficos que pueden verse en lab.montera34.com/covid19
# El data set resultante se publicaa en https://github.com/montera34/escovid19data

# Load libraries -----------
library(tidyverse)
library(reshape2)
library(readxl)

# Load Data ---------
# / Population -------------
provincias_poblacion <-  read.delim("data/original/spain/provincias-poblacion.csv",sep = ",")

# / COVID-19 in Spain -----------
# / By province -----------
# donwload provincias data from googgle spreadsheet 
download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=provincias", 
              "data/original/spain/covid19_spain_provincias.csv")
write.csv(read.delim("data/original/spain/covid19_spain_provincias.csv",sep = ","), file = "../escovid19data/data/original/covid19_spain_provincias.csv", row.names = FALSE)
data_cases_sp_provinces <- read.delim("data/original/spain/covid19_spain_provincias.csv",sep = ",")
# save file in another repository (comment this line!)

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

# Group by province
tenerife <- canarias %>% filter(province == "La Gomera" | province =="La Palma" | province == "Tenerife" | province == "El Hierro") %>% group_by(date) %>% summarise(
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
  cases_accumulated_PCR = sum(cases_accumulated_PCR),
  recovered = sum(recovered),
  source_name = "Gobierno de Canarias",
  source = paste(source, collapse = ";"),
  comments = paste(comments, collapse = ";")
)
palmas <- canarias %>% filter(province == "Fuerteventura" | province =="Lanzarote" | province == "Gran Canaria") %>% group_by(date) %>% summarise(
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
  cases_accumulated_PCR = sum(cases_accumulated_PCR),
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

rm(tenerife,palmas,canarias,canarias_bind)

# Remove last -usually incomplete- day
data_cases_sp_provinces <- filter(data_cases_sp_provinces, !is.na(date))

# Castilla y León: Remove existing Andalucia data and add new one from new source ---------------------
download.file("https://analisis.datosabiertos.jcyl.es/explore/dataset/situacion-epidemiologica-coronavirus-en-castilla-y-leon/download/?format=csv&timezone=Europe/Madrid&lang=en&use_labels_for_header=true&csv_separator=%3B", 
              "data/original/spain/cyl/covid19_cyl_a.csv")
download.file("https://analisis.datosabiertos.jcyl.es/explore/dataset/situacion-de-hospitalizados-por-coronavirus-en-castilla-y-leon/download/?format=csv&timezone=Europe/Madrid&lang=en&use_labels_for_header=true&csv_separator=%3B", 
              "data/original/spain/cyl/covid19_cyl_b.csv")

cyla_original <- read.delim("data/original/spain/cyl/covid19_cyl_a.csv", sep=";")
cylb_original <- read.delim("data/original/spain/cyl/covid19_cyl_b.csv", sep=";")

# Remove existing CyL data
data_cases_sp_provinces <- data_cases_sp_provinces %>% filter( ccaa != "Castilla y León" )

cyla <- cyla_original %>%
  mutate(
    date = as.Date(fecha,"%Y-%m-%d"),
    ccaa = "Castilla y León"
  ) %>% rename(
    province = provincia,
    cases_accumulated = casos_confirmados ,
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
    cases_accumulated_PCR = NA,
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
              by.x="dunique", by.y="dunique", all = TRUE) %>% select( -dunique)

# Add new CyL data
data_cases_sp_provinces <- rbind(data_cases_sp_provinces, 
             cyl %>% mutate( source = paste(source,source_b) ) %>% 
               select(-source_b)
        )

rm(cyl,cyla,cylb,cyla_original,cylb_original)

# Andalucía: Remove existing Andalucia data and add new one from new source ---------------------

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

# Add new Andalucía data
data_cases_sp_provinces <- rbind(data_cases_sp_provinces,andalucia)

rm(andalucia,andalucia_original)

# Uniprovinciales: Remove and add uniprovinciales from ISCIII -----

# Remove existing Uniprovinciales data
data_cases_sp_provinces <- data_cases_sp_provinces %>% 
  filter( !(province == "Melilla" | province == "Asturias" | province == "Balears, Illes" | province == "Cantabria" |
            province == "Ceuta" | province == "Murcia" | province == "Navarra" | province == "Madrid" |
            province == "Rioja, La") )

# import Instituto de Salud CIII 
ciii_original <- read.delim("https://cnecovid.isciii.es/covid19/resources/agregados.csv",sep = ",")  
write.csv(ciii_original, file = "data/original/spain/iscii_data.csv", row.names = FALSE)

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
            ccaa = ccaa %>% str_replace_all("Navarra", "Navarra, Comunidad Foral de"),
            PCR = NA
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

# # Makes df for every province
# names(cat8)
# # converts to wide format the different variables of positive or suspect cases
# cat8 <- catalunya_new %>% filter(provincia_code == "8") %>% spread(TipusCasDescripcio,by_day) %>% 
#   rename(PCR_day = "Positiu PCR", TestAc_day = "Positiu per Test Ràpid", sospechosos_day = "Sospitós")
# cat8 <- cat8 %>% group_by( provincia_code) %>% arrange(date) %>%
#   mutate ( 
#     PCR_cum = cumsum(replace_na(PCR_day, 0)), #replace NA values with 0 to make cumsum work
#     TestAc_cum = cumsum(replace_na(TestAc_day, 0)),
#     sospechosos_cum = cumsum(replace_na(sospechosos_day, 0))
#   )
# 
# cat17 <- catalunya_new %>% filter(provincia_code == "17") %>% spread(TipusCasDescripcio,by_day) %>% 
#   rename(PCR_day = "Positiu PCR", TestAc_day = "Positiu per Test Ràpid", sospechosos_day = "Sospitós")
# cat17 <- cat17 %>% group_by( provincia_code) %>% arrange(date) %>%
#   mutate ( 
#     PCR_cum = cumsum(replace_na(PCR_day, 0)), #replace NA values with 0 to make cumsum work
#     TestAc_cum = cumsum(replace_na(TestAc_day, 0)),
#     sospechosos_cum = cumsum(replace_na(sospechosos_day, 0))
#   )
# 
# 
# cat25 <- catalunya_new %>% filter(provincia_code == "25") %>% spread(TipusCasDescripcio,by_day) %>% 
#   rename(PCR_day = "Positiu PCR", TestAc_day = "Positiu per Test Ràpid", sospechosos_day = "Sospitós")
# cat25 <- cat25 %>% group_by( provincia_code) %>% arrange(date) %>%
#   mutate ( 
#     PCR_cum = cumsum(replace_na(PCR_day, 0)), #replace NA values with 0 to make cumsum work
#     TestAc_cum = cumsum(replace_na(TestAc_day, 0)),
#     sospechosos_cum = cumsum(replace_na(sospechosos_day, 0))
#   )
# 
# cat43 <- catalunya_new %>% filter(provincia_code == "43") %>% spread(TipusCasDescripcio,by_day) %>% 
#   rename(PCR_day = "Positiu PCR", TestAc_day = "Positiu per Test Ràpid", sospechosos_day = "Sospitós")
# cat43 <- cat43 %>% group_by( provincia_code) %>% arrange(date) %>%
#   mutate ( 
#     PCR_cum = cumsum(replace_na(PCR_day, 0)), #replace NA values with 0 to make cumsum work
#     TestAc_cum = cumsum(replace_na(TestAc_day, 0)),
#     sospechosos_cum = cumsum(replace_na(sospechosos_day, 0))
#   )
# 
# # Binds all the provinces
# cattotal <- rbind(cat8,cat17,cat25,cat43)
# rm(cat8,cat17,cat25,cat43)

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
  select(-date_cat, -PCR_cum_cat, -PCR_cat, -TestAc_cat)

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
                                 powerbi %>% select(date,dunique,Fallecimientos_positivos,Fallecimientos_todos,Territorio,ccaa) %>% 
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

# Euskadi hospitalizados.  Overwrite hospitalized  data --------------
download.file("https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/datos-asistenciales.xlsx", 
              "data/original/spain/euskadi/datos-asistenciales.xlsx")

euskadi_original <- read_excel("data/original/spain/euskadi/datos-asistenciales.xlsx", skip = 2, col_names = TRUE, sheet = "01")

euskadi_a <- euskadi_original %>% rename( date = ...1 ) %>% 
  mutate( date = as.Date(date,"%d/%m/%Y")) %>% select( -`Ingresados en Planta`)  %>% melt(
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
  ) %>% group_by(province,date) %>% 
  mutate( 
    value = ifelse(is.na(value),0,value)) %>%
  summarise(
    hospitalized = sum(value)
  )



data_cases_sp_provinces$dunique <- paste0(data_cases_sp_provinces$date,data_cases_sp_provinces$province)
euskadi_a$dunique <- paste0(euskadi_a$date,euskadi_a$province)

# TODO: mirar los hospitalizados por día: no coinciden los datos con lo que se publicó en las notas de prensa de Irekia
data_cases_sp_provinces <- merge(data_cases_sp_provinces,
                                 euskadi_a %>% select(dunique,hospitalized) %>% 
                                   ungroup() %>%
                                   rename(
                                     hospitalized_eus = hospitalized
                                     ) , 
                                 by.x="dunique", by.y="dunique", all = TRUE) %>% select(-dunique)

data_cases_sp_provinces <- data_cases_sp_provinces %>% mutate(
  hospitalized = ifelse( ccaa == "País Vasco", hospitalized_eus, hospitalized),
  source_name = ifelse( ccaa == "País Vasco", paste0(as.character(source_name),";Open Data Euskadi"), as.character(source_name) ),
  source = ifelse( ccaa == "País Vasco", 
                   paste0(source,";https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/datos-asistenciales.xlsx" ), 
                   as.character(source) )
) %>% select(-hospitalized_eus)


# Add missing data deaths previous 2020.03.08 --------------

# remove date previos to March 8. ISCIII does not have detahs before that day
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
  mutate( 
          cases_14days = cases_accumulated - lag(cases_accumulated,13),
          daily_cases = cases_accumulated - lag(cases_accumulated),
          daily_cases_avg7 =  round( ( daily_cases + lag(daily_cases,1)+lag(daily_cases,2)+
                                          lag(daily_cases,3)+lag(daily_cases,4)+lag(daily_cases,5)+lag(daily_cases,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
          daily_cases_PCR = cases_accumulated_PCR - lag(cases_accumulated_PCR),
          daily_cases_PCR = ifelse(is.na(daily_cases_PCR),PCR,daily_cases_PCR), # inserta datos originales de PCR diarios si la diferencia del acumulado no se puede calcular
          daily_cases_PCR_avg7 =  round( ( daily_cases_PCR + lag(daily_cases_PCR,1)+lag(daily_cases_PCR,2)+
                                             lag(daily_cases_PCR,3)+lag(daily_cases_PCR,4) +lag(daily_cases_PCR,5) +lag(daily_cases_PCR,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
          daily_deaths = deceased - lag(deceased),
          daily_deaths_inc = round((deceased - lag(deceased)) /lag(deceased) * 100, digits = 1),
          daily_deaths_avg3 =  round( ( daily_deaths + lag(daily_deaths,1)+lag(daily_deaths,2) ) / 3, digits = 1 ), # average of daily deaths of 3 last days
          daily_deaths_avg7 =  round( ( daily_deaths + lag(daily_deaths,1)+lag(daily_deaths,2)+
                                          lag(daily_deaths,3)+lag(daily_deaths,4)+lag(daily_deaths,5)+lag(daily_deaths,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
          # deaths_cum_last_week = deceased - lag(deceased,6),
          deaths_last_week =  daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) + lag(daily_deaths,4) + lag(daily_deaths,5) + lag(daily_deaths,6)
          # hospitalized_avg7 =  round( ( hospitalized + lag(hospitalized,1)+lag(hospitalized,2)+
          #                                 lag(hospitalized,3)+lag(hospitalized,4)+lag(hospitalized,5)+lag(hospitalized,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
          
  )

data_cases_sp_provinces <- data_cases_sp_provinces %>% select(date,province,ine_code,everything())  
data_cases_sp_provinces <- data_cases_sp_provinces %>% select(-source, -source_name,-comments,source_name,source,comments)

# Re calculates factors to remove things like Andaluc'ia for provinces
data_cases_sp_provinces$province <- factor(data_cases_sp_provinces$province)
data_cases_sp_provinces$ccaa <- factor(data_cases_sp_provinces$ccaa)

# saves data in the other repository
write.csv(data_cases_sp_provinces, file = "../escovid19data/data/output/covid19-provincias-spain_consolidated.csv", row.names = FALSE)
saveRDS(data_cases_sp_provinces, file = "../escovid19data/data/output/covid19-provincias-spain_consolidated.rds")

# save date, but first rearrange
data_cases_sp_provinces <- data_cases_sp_provinces %>% arrange(ccaa,province,date)

write.csv(data_cases_sp_provinces, file = "data/output/spain/covid19-provincias-spain_consolidated.csv", row.names = FALSE)
saveRDS(data_cases_sp_provinces, file = "data/output/spain/covid19-provincias-spain_consolidated.rds")

# cleans environment
rm(uniprovinciales, powerbi, catalunya, catalunya_new, cattotal, provincias_poblacion,datadista,ciii)