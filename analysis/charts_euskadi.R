# Euskadi charts

# Load libraries------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping
library(readxl)

# Load data 
download.file("https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/datos-asistenciales.xlsx", 
              "data/original/spain/euskadi/datos-asistenciales.xlsx")
download.file("https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/situacion-epidemiologica.xlsx", 
              "data/original/spain/euskadi/situacion-epidemiologica.xlsx")

# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption_provincia <- "Gráfico: @numeroteca (lab.montera34.com/covid19/euskadi.html) | Datos: Open Data Euskadi"

# Set colors ---------
# extends color paletter
library(RColorBrewer)
# creates extended color palette https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
colourCount <- 17
getPalette <- colorRampPalette(brewer.pal(9, "Dark2"))
colors_prov <- getPalette(colourCount )
# Change yellow to blue
# colors_prov[1] <- "#a60000"

# Hospitalizados----
euskadi_hosp <- read_excel("data/original/spain/euskadi/datos-asistenciales.xlsx", skip = 2, col_names = TRUE, sheet = "01")

euskadi_hosp <- euskadi_hosp %>% rename( date = ...1 ) %>% 
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
  ) %>% group_by(province,date) %>% rename(
    hospital = variable,
    hospitalizados = value,
    provincia = province
  ) %>% mutate (
    dunique = paste0(date,hospital),
    hospitalizados = replace_na(hospitalizados, 0)
  )


# hospitalizados nuevos cada dia
euskadi_hosp_new <- read_excel("data/original/spain/euskadi/datos-asistenciales.xlsx", skip = 2, col_names = TRUE, sheet = "02")

euskadi_hosp_new <- euskadi_hosp_new %>% rename( date = ...1 ) %>% 
  mutate( date = as.Date(date,"%d/%m/%Y")) %>% select( -`Nuevos Ingresos Hospitalarios`)  %>% melt(
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
  ) %>% group_by(province,date) %>% rename(
    hospital = variable,
    hospitalizados_nuevos = value,
    provincia = province
  ) %>% group_by(hospital) %>% arrange(date)  %>% mutate (
    dunique = paste0(date,hospital),
    hospitalizados_nuevos = replace_na(hospitalizados_nuevos, 0),
    hospitalizados_cum = cumsum(hospitalizados_nuevos)
  ) 

euskadi_hosp_new <- euskadi_hosp_new %>% mutate ( # Hay que añadir la cifra de hospitalizados inicial de cada hospital
    hospitalizados_cum = ifelse(hospital=="01 Araba",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "01 Araba" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "01 Araba" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="02 Cruces",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "02 Cruces" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "02 Cruces" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="03 Donosti",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "03 Donosti" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "03 Donosti" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="04 Basurto",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "04 Basurto" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "04 Basurto" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="05 Galdakao",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "05 Galdakao" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "05 Galdakao" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="06 Zumarraga",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "06 Zumarraga" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "06 Zumarraga" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="07 Bidasoa",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "07 Bidasoa" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "07 Bidasoa" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="08 Mendaro",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "08 Mendaro" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "08 Mendaro" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="09 Alto Deba",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "09 Alto Deba" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "09 Alto Deba" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="10 San Eloy",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "10 San Eloy" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "10 San Eloy" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="11 Urduliz",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "11 Urduliz" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "11 Urduliz" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="12 Eibar",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "12 Eibar" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "12 Eibar" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="13 Leza",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "13 Leza" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "13 Leza" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="14 Sta Marina",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "14 Sta Marina" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "14 Sta Marina" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="15 Gorliz",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "15 Gorliz" & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "15 Gorliz" & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="BERMEO H.",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "BERMEO H." & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "BERMEO H." & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="ZALDIBAR H.",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "ZALDIBAR H." & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "ZALDIBAR H." & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="ZAMUDIO H.",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "ZAMUDIO H." & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "ZAMUDIO H." & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum),
    hospitalizados_cum = ifelse(hospital=="ÁLAVA PSIQUIÁTRICO H.",
                                hospitalizados_cum  + euskadi_hosp[euskadi_hosp$hospital == "ÁLAVA PSIQUIÁTRICO H." & euskadi_hosp$date == as.Date("2020-03-01"),  ]$hospitalizados -
                                  euskadi_hosp_new[euskadi_hosp_new$hospital == "ÁLAVA PSIQUIÁTRICO H." & euskadi_hosp_new$date == as.Date("2020-03-01"),  ]$hospitalizados_nuevos,
                                hospitalizados_cum)
   )

# altas ----------
euskadi_altas <- read_excel("data/original/spain/euskadi/datos-asistenciales.xlsx", skip = 2, col_names = TRUE, sheet = "03")
euskadi_altas <- euskadi_altas %>% rename( date = ...1 ) %>% 
  mutate( date = as.Date(date,"%d/%m/%Y")) %>% select( -`Altas Planta`)  %>% melt(
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
  ) %>% group_by(province,date) %>% rename(
    hospital = variable,
    altas = value,
    provincia = province
  ) %>% mutate (
    dunique = paste0(date,hospital)
  ) %>% group_by(hospital) %>% arrange(date) %>% mutate (
    altas_cum = cumsum(replace_na(altas, 0))
  )

# altas UCI ----------
euskadi_altas_uci <- read_excel("data/original/spain/euskadi/datos-asistenciales.xlsx", skip = 2, col_names = TRUE, sheet = "06")
euskadi_altas_uci <- euskadi_altas_uci %>% rename( date = ...1 ) %>% 
  mutate( date = as.Date(date,"%d/%m/%Y")) %>% select( -`Altas UCI`)  %>% melt(
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
  ) %>% group_by(province,date) %>% rename(
    hospital = variable,
    altas_uci = value,
    provincia = province
  ) %>% mutate (
    dunique = paste0(date,hospital)
  ) %>% group_by(hospital) %>% arrange(date) %>% mutate (
    altas_uci_cum = cumsum(replace_na(altas_uci, 0))
  )


#  ingresados en UCI ----------
euskadi_uci <- read_excel("data/original/spain/euskadi/datos-asistenciales.xlsx", skip = 2, col_names = TRUE, sheet = "04")
euskadi_uci <- euskadi_uci %>% rename( date = ...1 ) %>%
  mutate( date = as.Date(date,"%d/%m/%Y")) %>% select( -`Ingresados UCI`)  %>% melt(
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
  ) %>% group_by(province,date) %>% rename(
    hospital = variable,
    uci = value,
    provincia = province
  ) %>% mutate (
    dunique = paste0(date,hospital),
    uci = replace_na(uci, 0)
  )

# uci nuevos cada dia
euskadi_uci_new <- read_excel("data/original/spain/euskadi/datos-asistenciales.xlsx", skip = 2, col_names = TRUE, sheet = "05")

euskadi_uci_new <- euskadi_uci_new %>% rename( date = ...1 ) %>% 
  mutate( date = as.Date(date,"%d/%m/%Y")) %>% select( -`Nuevos Ingresos UCI`)  %>% melt(
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
  ) %>% group_by(province,date) %>% rename(
    hospital = variable,
    uci_nuevos = value,
    provincia = province
  ) %>% group_by(hospital) %>% arrange(date)  %>% mutate (
    dunique = paste0(date,hospital),
    uci_nuevos = replace_na(uci_nuevos, 0),
    uci_cum = cumsum(uci_nuevos)
  )

#  fallecidos ----------
euskadi_fallecidos <- read_excel("data/original/spain/euskadi/datos-asistenciales.xlsx", skip = 2, col_names = TRUE, sheet = "07")
euskadi_fallecidos <- euskadi_fallecidos %>% rename( date = ...1 ) %>% filter( date != "Suma Total") %>%
  mutate( date = as.Date(date,"%d/%m/%Y")) %>% select( -`Exitus`)  %>% melt(
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
  ) %>% group_by(province,date) %>% rename(
    hospital = variable,
    fallecidos = value,
    provincia = province
  ) %>% mutate (
    dunique = paste0(date,hospital),
  ) %>% group_by(hospital) %>% arrange(date) %>% mutate (
    fallecidos_cum = cumsum(replace_na(fallecidos, 0))
  )

# Merge data ---------
euskadi_total <- merge( euskadi_hosp, euskadi_uci %>% ungroup() %>% select(-hospital,-date,-provincia), by.x="dunique", by.y="dunique" )
euskadi_total <- merge( euskadi_total, euskadi_hosp_new %>% ungroup() %>% select(-hospital,-date,-provincia), by.x="dunique", by.y="dunique" )
euskadi_total <- merge( euskadi_total, euskadi_uci_new %>% ungroup() %>% select(-hospital,-date,-provincia), by.x="dunique", by.y="dunique" )
euskadi_total <- merge( euskadi_total, euskadi_fallecidos %>% ungroup() %>% select(-hospital,-date,-provincia), by.x="dunique", by.y="dunique" )
euskadi_total <- merge( euskadi_total, euskadi_altas %>% ungroup() %>% select(-hospital,-date,-provincia), by.x="dunique", by.y="dunique" )
euskadi_total <- merge( euskadi_total, euskadi_altas_uci %>% ungroup() %>% select(-hospital,-date,-provincia), by.x="dunique", by.y="dunique" ) %>% select (-dunique)
euskadi_total <- euskadi_total %>% group_by(hospital) %>% arrange(date) %>% 
  mutate(
  # total_ingresados = hospitalizados_cum
  total_ingresados = hospitalizados+uci+fallecidos_cum+altas_cum
  )  %>% 
  arrange(date)  %>% mutate(
  # daily_totat_ingresados = total_ingresados - lag(total_ingresados),
  daily_deceassed = fallecidos_cum - lag(fallecidos_cum),
  daily_deceassed_avg =  round( ( daily_deceassed + lag(daily_deceassed,1 ) + lag(daily_deceassed,2 ) + lag(daily_deceassed,3 )+ lag(daily_deceassed,4 ) + lag(daily_deceassed,5 ) +
                                    lag(daily_deceassed,6 ) ) / 7, digits=1)
) %>% select( date,provincia,hospital, hospitalizados, hospitalizados_nuevos, hospitalizados_cum, uci, uci_nuevos, fallecidos, fallecidos_cum, altas, altas_cum, everything())

write.csv(euskadi_total, file = "data/output/spain/euskadi/euskadi-hospital-dat.csv", row.names = FALSE)

euskadi_total <- euskadi_total %>% group_by(hospital) %>% arrange(date) %>%  mutate (
  hospitalizados_per = round(hospitalizados/total_ingresados*100, digits=1),
  fallecidos_cum_per = round(fallecidos_cum/total_ingresados*100, digits=1),
  uci_per = round(uci/total_ingresados*100, digits=1),
  altas_cum_per = round(altas_cum/total_ingresados*100, digits=1),
  suma_per = round(hospitalizados_per+fallecidos_cum_per+uci_per+altas_cum_per , digits=1)
)


max_date  <- max(euskadi_total$date)
period_eus <- paste("Última fecha:", max_date)

# Plots ----------------
png(filename=paste0("img/spain/euskadi/covid19_hosp-por-dia-hospital-media-superpuesto-lineal_media-pais-vasco.png", sep = ""),width = 1200,height = 800)
euskadi_hosp %>%
  ggplot() +
  geom_line(aes(date, hospitalizados ,group=hospital, color=provincia), size= 1) +
  geom_point(aes(date, hospitalizados, color=provincia), size= 1) +
  geom_text_repel(
    data = euskadi_hosp %>% group_by(hospital) %>% filter(!is.na(hospitalizados) ) %>% top_n(1, date) %>% filter (hospitalizados > 4 ),
    aes(date, hospitalizados, color=provincia, 
        label=paste(format(hospitalizados, nsmall=0, big.mark=".", decimal.mark = ","), "[" , hospital, "]")),
    nudge_x = 3, # adjust the starting y position of the text label
    size=5,
    hjust=0,
    family = "Roboto Condensed",
    direction="y",
    segment.size = 0.1,
    segment.color="#777777"
  ) +
  # geom_text_repel(
  #   data = euskadi_hosp %>% group_by(hospital) %>% filter( !is.na(hospitalizados) & euskadi_hosp == max(hospitalizados) ) ,
  #   aes(date, hospitalizados, color=provincia, 
  #       label=paste(format(hospitalizados+5, nsmall=0, big.mark=".", decimal.mark = ","), "[" , hospital, "]")),
  #   nudge_x = 3, # adjust the starting y position of the text label
  #   size=5,
  #   hjust=0,
  #   family = "Roboto Condensed",
  #   direction="y",
  #   segment.size = 0.1,
  #   segment.color="#777777"
  # ) +
  # scale_color_manual(values = colors_prov) +
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
              date_breaks = "15 day",
               date_labels = "%d/%m",
               limits=c( min(euskadi_hosp$date), max(euskadi_hosp$date +39)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  c(0.9,0.7)
  ) +
  labs(title = paste0("Hospitalizados por día por COVID-19 en Euskadi" ),
       subtitle = paste0("Por provincia  ",period_eus),
       y = "hospitalizados",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# mix ----------------
png(filename=paste0("img/spain/euskadi/covid19_hosp-mix-free-esc-pais-vasco.png", sep = ""),width = 1200,height = 700)
euskadi_total %>% #filter( hospital == "14 Sta Marina") %>% 
  ggplot() +
  geom_ribbon(aes(date,  ymin=0, ymax=hospitalizados, group=hospital, fill="#5b5bbb")) +
  geom_ribbon(aes(date,   ymin=hospitalizados,ymax=hospitalizados+uci, group=hospital,fill="red") ) +
  geom_ribbon(aes(date,ymin=hospitalizados+uci,ymax=hospitalizados+uci+fallecidos_cum,group=hospital,fill="black") ) +
  geom_ribbon(aes(date,ymin=hospitalizados+uci+fallecidos_cum, ymax=hospitalizados+uci+fallecidos_cum+altas_cum, group=hospital, fill="#7ba934") ) +
  scale_fill_identity(
    guide = "legend",
    labels = c("Hospitalizados","Altas","Fallecidos","UCI"),
  ) +
  facet_wrap( ~hospital, scales = "free_y") + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
                # date_breaks = "3 day", 
               date_labels = "%m",
               # limits=c( min(euskadi_hosp$date), max(euskadi_hosp$date +20)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9),
    legend.position =  "top"
  ) +
  labs(title = paste0("Evolución ingresados por día por COVID-19 en Euskadi" ),
       subtitle = paste0("Por hospitales. ",period_eus),
       y = "hospitalizados",
       x = "fecha (mes) 2020",
       fill = "",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/euskadi/covid19_hosp-mix-pais-vasco.png", sep = ""),width = 1200,height = 700)
euskadi_total %>%
  ggplot() +
  geom_ribbon(aes(date,  ymin=0, ymax=hospitalizados, group=hospital, fill="#5b5bbb")) +
  geom_ribbon(aes(date,   ymin=hospitalizados,ymax=hospitalizados+uci, group=hospital,fill="red") ) +
  geom_ribbon(aes(date,ymin=hospitalizados+uci,ymax=hospitalizados+uci+fallecidos_cum,group=hospital,fill="black") ) +
  geom_ribbon(aes(date,ymin=hospitalizados+uci+fallecidos_cum,ymax=hospitalizados+uci+fallecidos_cum+altas_cum,group=hospital, fill="#7ba934") ) +
  scale_fill_identity(
    guide = "legend",
    labels = c("Hospitalizados","Altas","Fallecidos","UCI"),
  ) +
  facet_wrap( ~hospital) + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    # date_breaks = "3 day", 
    date_labels = "%m",
    # limits=c( min(euskadi_hosp$date), max(euskadi_hosp$date +20)),
    expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "top"
  ) +
  labs(title = paste0("Evolución ingresados por día por COVID-19 en Euskadi" ),
       subtitle = paste0("Por hospitales. ",period_eus),
       y = "hospitalizados",
       x = "fecha (mes) 2020",
       fill = "",
       caption = caption_provincia)
dev.off()


png(filename=paste0("img/spain/euskadi/covid19_hosp-mix-pais-vasco_percent.png", sep = ""),width = 1300,height = 700)
euskadi_total %>%
  ggplot() +
  geom_ribbon( aes(date, ymin=0, ymax=hospitalizados/total_ingresados*100, group=hospital, fill="#5b5bbb")) +
  geom_ribbon( aes(date, ymin= hospitalizados/total_ingresados*100,      ymax= (hospitalizados+uci)/total_ingresados*100, group=hospital, fill="red") ) +
  geom_ribbon( aes(date, ymin=(hospitalizados+uci)/total_ingresados*100, ymax= (hospitalizados+uci+fallecidos_cum)/total_ingresados*100, group=hospital, fill="black") ) +
  geom_ribbon( aes(date, ymin=(hospitalizados+uci+fallecidos_cum)/total_ingresados*100, ymax=(hospitalizados+uci+fallecidos_cum+altas_cum)/total_ingresados*100, group=hospital, fill="#7ba934") ) +
  geom_text(data=filter( euskadi_total, date==max(euskadi_total$date) & hospitalizados > 1 ),
            aes(date + 1, ((hospitalizados)/total_ingresados*100),
                label=paste0( hospitalizados_per,"% (",hospitalizados,")")),
            color="#5b5bbb",
            size=4,
            hjust=0,
            family = "Roboto Condensed"
  ) +
  geom_text(data=filter( euskadi_total, date==max(euskadi_total$date) & fallecidos_cum > 1),
            aes(date + 1, (hospitalizados+fallecidos_cum)/total_ingresados*100,
                label=paste0(fallecidos_cum_per,"% (",format(fallecidos_cum, big.mark="."),")")),
            color="#000000",
            size=4,
            hjust=0,
            family = "Roboto Condensed"
  ) +
  geom_text(data=filter( euskadi_total, date==max(euskadi_total$date) ),
            aes(date + 1, (hospitalizados+uci+fallecidos_cum)/total_ingresados*100 + ((hospitalizados+uci+fallecidos_cum+altas_cum)/total_ingresados*100)/2,
                label=paste0(altas_cum_per,"% (",format(altas_cum, big.mark="."),")")),
            color="#7ba934",
            size=4,
            hjust=0,
            family = "Roboto Condensed"
  ) +
  scale_fill_identity(
    guide = "legend",
    labels = c("Hospitalizados","Altas","Fallecidos","UCI"),
  ) +
  facet_wrap( ~hospital) + #, scales = "free_x"
  scale_y_continuous( 
    limits = c(0,100),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    # date_breaks = "3 day", 
    date_labels = "%m",
    limits=c( min(euskadi_total$date), max(euskadi_hosp$date +70)),
    expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "top"
  ) +
  labs(title = paste0("Evolución de ingresados en hospitales por COVID-19 en Euskadi" ),
       subtitle = paste0("Por hospitales en porcentaje sobre total de ingresados hasta la fecha (total personas entre paréntesis). ",period_eus),
       y = "hospitalizados",
       x = "fecha (mes) 2020",
       fill = "",
       caption = caption_provincia)
dev.off()

# daily data ---------
# hospitalizados
png(filename=paste0("img/spain/euskadi/covid19_hospitalizados-dia-pais-vasco.png", sep = ""),width = 1300,height = 700)
euskadi_total %>%
  ggplot() +
  geom_col( aes(date,hospitalizados), width=1, color="#5b5bbb") +
  facet_wrap( ~hospital) + #, scales = "free_x"
  scale_y_continuous( 
    # limits = c(0,100),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    # date_breaks = "3 day", 
    date_labels = "%m",
    limits=c( min(euskadi_total$date), max(euskadi_hosp$date +30)),
    expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "top"
  ) +
  labs(title = paste0("Pacientes hospitalizados en plana diarios en hospitales por COVID-19 en Euskadi" ),
       subtitle = paste0("",period_eus),
       y = "falecidos",
       x = "fecha (mes) 2020",
       fill = "",
       caption = caption_provincia)
dev.off()


# UCI
png(filename=paste0("img/spain/euskadi/covid19_uci-dia-pais-vasco.png", sep = ""),width = 1300,height = 700)
euskadi_total %>%
  ggplot() +
  geom_col( aes(date,uci), width=1,fill="red") +
  facet_wrap( ~hospital) + #, scales = "free_x"
  scale_y_continuous( 
    # limits = c(0,100),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    # date_breaks = "3 day", 
    date_labels = "%m",
    limits=c( min(euskadi_total$date), max(euskadi_hosp$date +30)),
    expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "top"
  ) +
  labs(title = paste0("Pacientes UCI diarios en hospitales por COVID-19 en Euskadi" ),
       subtitle = paste0("",period_eus),
       y = "falecidos",
       x = "fecha (mes) 2020",
       fill = "",
       caption = caption_provincia)
dev.off()


# fallecidos
png(filename=paste0("img/spain/euskadi/covid19_muertes-dia-pais-vasco.png", sep = ""),width = 1300,height = 700)
euskadi_total %>%
  ggplot() +
  geom_col( aes(date,daily_deceassed), width=1, fill="grey") +
  geom_line( aes(date,daily_deceassed_avg, group=hospital), size=1) +
  facet_wrap( ~hospital) + #, scales = "free_x"
  scale_y_continuous( 
    # limits = c(0,100),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    # date_breaks = "3 day", 
    date_labels = "%m",
    limits=c( min(euskadi_total$date), max(euskadi_hosp$date +30)),
    expand = c(0,0)
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "top"
  ) +
  labs(title = paste0("Media de fallecidos diarios (ventana 7 días) en hospitales por COVID-19 en Euskadi" ),
       subtitle = paste0("",period_eus),
       y = "falecidos",
       x = "fecha (mes) 2020",
       fill = "",
       caption = caption_provincia)
dev.off()


# 2. Zonas de salud ------------

# period_eus <- paste("Actualizado:", "2020-07-29")

# load zonas de salud 
# made once to store the data and rename zonas de salud
# require(rgdal)
# shape <- readOGR(dsn = "data/original/spain/euskadi/shapes/OsasunEremuak_ZonasSalud_A_2018.shp")
# zonassalud <- shape@data
# write.csv(zonassalud, file = "data/original/spain/euskadi/zonas-de-salud.csv", row.names = FALSE)
zonassalud <- read.delim("data/output/spain/euskadi/zonas-de-salud_renamed.csv",sep = ",")

# process data -------
municipios <- read_excel("data/original/spain/euskadi/situacion-epidemiologica.xlsx", skip = 1, col_names = TRUE, sheet = "05") %>% rename(
  name = "OSASUN-EREMUAK/ZONAS DE SALUD"
) %>% melt(  id.vars = c("name")) %>% rename ( date = variable) %>% 
  group_by(name) %>% arrange(date) %>% 
  mutate(
    date =  as.Date( paste0(date,"/2020"), "%d/%m/%y"),
    daily_cases_avg7 =  round( ( value + lag(value,1)+lag(value,2)+
                                 lag(value,3)+lag(value,4)+lag(value,5)+lag(value,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
  ) %>% filter( !is.na(date))


municipios <- merge(municipios,
                    zonassalud %>% select (ZONA_Nom1, OSI_NOM_eu, OSI_NOM_es),
                    by.x = "name", by.y = "ZONA_Nom1", all.x = TRUE) %>% mutate (
                      OSI_NOM_eu = as.character(OSI_NOM_eu),
                      OSI_NOM_eu = ifelse( is.na(OSI_NOM_eu),"Otros", as.character(OSI_NOM_eu) ),
                      OSI_NOM_eu = as.factor(OSI_NOM_eu),
                      value = ifelse(is.na(value), 0 ,value )
                    )

osi <- municipios %>% group_by(OSI_NOM_eu,date)  %>% summarise(
  value = sum(value),
  # total = cumsum(value),
  ) %>% arrange(date ) %>% mutate(
    daily_cases_avg7 =  round( ( value + lag(value,1)+lag(value,2)+
                                   lag(value,3)+lag(value,4)+lag(value,5)+lag(value,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
  ) %>% rename(
    name = OSI_NOM_eu
  )

saveRDS(municipios, file = "data/output/spain/euskadi/zonas-de-salud-casos-diarios.rds")
write.csv(municipios, file = "data/output/spain/euskadi/zonas-de-salud-casos-diarios.csv", row.names = FALSE)

# municipios with average more than 1 per day
municipios_top <- municipios %>% top_n(1, date) %>% filter (daily_cases_avg7 > 2 ) %>% select (name)


# zonas superpuestas ---------
png(filename=paste0("img/spain/euskadi/covid19_casos-municipios-pais-vasco.png", sep = ""),width = 1200,height = 800)
municipios %>% # filter( name %in% municipios_top$name) %>%
  ggplot() +
  geom_line(aes(date, daily_cases_avg7, group=name, color = name), size= 0.5) +
  # geom_point(aes(date, value, color = name), size= 1) +
  geom_text_repel(
    data = municipios %>% group_by(name) %>% filter(!is.na(daily_cases_avg7) ) %>% top_n(1, date) %>% filter (daily_cases_avg7 > 6 ),
    aes(date, daily_cases_avg7,  color = name,
        label=paste(format(daily_cases_avg7, nsmall=0, big.mark=".", decimal.mark = ","), name)),
    nudge_x = 3, # adjust the starting y position of the text label
    size=5,
    hjust=0,
    family = "Roboto Condensed",
    direction="y",
    segment.size = 0.1,
    segment.color="#777777"
  ) +
# scale_color_manual(values = colors_prov) +
# scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+70, max(municipios$date +15)),
    expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "none"
  ) +
  labs(title = paste0("Casos PCR+ por COVID-19 por zonas de salud por día en Euskadi" ),
       subtitle = paste0("Media: ventana de 7 días. ",period_eus),
       y = "casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/euskadi/covid19_casos-municipios-pais-vasco_log.png", sep = ""),width = 1200,height = 800)
municipios %>% # filter( name %in% municipios_top$name) %>%
  ggplot() +
  geom_line(aes(date, daily_cases_avg7, group=name, color = name), size= 0.5) +
  # geom_point(aes(date, value, color = name), size= 1) +
  geom_text_repel(
    data = municipios %>% group_by(name) %>% filter(!is.na(daily_cases_avg7) ) %>% top_n(1, date) %>% filter (daily_cases_avg7 > 6 ),
    aes(date, daily_cases_avg7,  color = name,
        label=paste(format(daily_cases_avg7, nsmall=0, big.mark=".", decimal.mark = ","), name)),
    nudge_x = 3, # adjust the starting y position of the text label
    size=5,
    hjust=0,
    family = "Roboto Condensed",
    direction="y",
    segment.size = 0.1,
    segment.color="#777777"
  ) +
  # scale_color_manual(values = colors_prov) +
  # scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+70, max(municipios$date +15)),
    expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "none"
  ) +
  labs(title = paste0("Casos PCR+ por COVID-19 por zonas de salud por día en Euskadi" ),
       subtitle = paste0("Media: ventana de 7 días. Escala logarítmica ",period_eus),
       y = "casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/euskadi/covid19_casos-zs-pais-vasco_rejilla-osi.png", sep = ""),width = 1200,height = 800)
municipios %>% # filter( name %in% municipios_top$name) %>%
  ggplot() +
  geom_line(aes(date, daily_cases_avg7, group=name, color = name), size= 0.5) +
  # geom_point(aes(date, value, color = name), size= 1, alpha=0.4) +
  geom_text_repel(
    data = municipios %>% group_by(name) %>% filter(!is.na(daily_cases_avg7) ) %>% top_n(1, date) %>% filter (daily_cases_avg7 > 2 ),
    aes(date, daily_cases_avg7,  color = name,
        label=paste(format(daily_cases_avg7, nsmall=0, big.mark=".", decimal.mark = ","), substr(name,1,7))),
    nudge_x = 3, # adjust the starting y position of the text label
    size=3,
    hjust=0,
    family = "Roboto Condensed",
    direction="y",
    segment.size = 0.1,
    segment.color="#777777"
  ) +
  # scale_color_manual(values = colors_prov) +
  # scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  facet_wrap( ~OSI_NOM_eu, scales = "free_y") +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "15 days",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+70, max(municipios$date +18)),
    expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "none"
  ) +
  labs(title = paste0("Casos PCR+ por día de COVID-19 por zonas de salud y OSI en Euskadi" ),
       subtitle = paste0("Media: ventana de 7 días. ",period_eus),
       y = "casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/euskadi/covid19_casos-zs-pais-vasco_rejilla-osi_not-free.png", sep = ""),width = 1200,height = 800)
municipios %>% # filter( name %in% municipios_top$name) %>%
  ggplot() +
  geom_line(aes(date, daily_cases_avg7, group=name, color = name), size= 0.5) +
  # geom_point(aes(date, value, color = name), size= 1) +
  geom_text_repel(
    data = municipios %>% group_by(name) %>% filter(!is.na(daily_cases_avg7) ) %>% top_n(1, date) %>% filter (daily_cases_avg7 > 2 ),
    aes(date, daily_cases_avg7,  color = name,
        label=paste(format(daily_cases_avg7, nsmall=0, big.mark=".", decimal.mark = ","), substr(name,1,7))),
    nudge_x = 3, # adjust the starting y position of the text label
    size=3,
    hjust=0,
    family = "Roboto Condensed",
    direction="y",
    segment.size = 0.1,
    segment.color="#777777"
  ) +
  # scale_color_manual(values = colors_prov) +
  # scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  facet_wrap( ~OSI_NOM_eu) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "15 days",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+70, max(municipios$date +18)),
    expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "none"
  ) +
  labs(title = paste0("Casos PCR+ por día de COVID-19 por zonas de salud y OSI en Euskadi" ),
       subtitle = paste0("Media: ventana de 7 días. ",period_eus),
       y = "casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

# rejilla top zonas --------------
png(filename=paste0("img/spain/euskadi/covid19_casos-municipios-pais-vasco_rejilla.png", sep = ""),width = 1200,height = 800)
municipios %>% filter( name %in% municipios_top$name) %>%
  ggplot() +
  geom_col(aes(date, value, fill = ""), width= 1 ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_avg7, group=name, color= ""), size= 1.1 ) +
  scale_color_manual(values=c("#565656")  )+
  facet_wrap( ~name, scales = "free_y") + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "15 days",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+70, max(municipios$date)+1),
    expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position =  "top"
  ) +
  labs(title = paste0("Casos PCR+ por COVID-19 por zonas de salud por día en Euskadi" ),
       subtitle = paste0("Media: ventana de 7 días. Zonas de salud con media mayor que 2. ",period_eus),
       y = "casos por día",
       x = "fecha",
       fill = "casos por dia",
       colour = "media",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/euskadi/covid19_casos-municipios-pais-vasco_rejilla_not-free.png", sep = ""),width = 1200,height = 800)
municipios %>% filter( name %in% municipios_top$name) %>%
  ggplot() +
  geom_col(aes(date, value, fill = ""), width= 1 ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_avg7, group=name, color= ""), size= 1.1 ) +
  scale_color_manual(values=c("#565656")  )+
  facet_wrap( ~name) + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "15 days",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+70, max(municipios$date)+1),
    expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position =  "top"
  ) +
  labs(title = paste0("Casos PCR+ por COVID-19 por zonas de salud por día en Euskadi" ),
       subtitle = paste0("Media: ventana de 7 días. Zonas de salud con media mayor que 2. ",period_eus),
       y = "casos por día",
       x = "fecha",
       fill = "casos por dia",
       colour = "media",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/euskadi/covid19_casos-pais-vasco_osi-bilbao_rejilla.png", sep = ""),width = 1200,height = 800)
municipios %>% filter( OSI_NOM_eu == "Bilbao - Basurto" ) %>% 
  ggplot() +
  geom_col(aes(date, value, fill = ""), width= 1 ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_avg7, group=name, color= ""), size= 1.1 ) +
  scale_color_manual(values=c("#565656")  )+
  facet_wrap( ~name, scales = "free_y") + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "15 days",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+70, max(municipios$date)+1),
    expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 20) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position =  "top"
  ) +
  labs(title = paste0("Casos PCR+ por COVID-19 por zonas de salud por día en Bilbao-Basurto." ),
       subtitle = paste0("Media: ventana de 7 días. ",period_eus),
       y = "casos por día",
       x = "fecha",
       fill = "casos por dia",
       colour = "media",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/euskadi/covid19_casos-pais-vasco_osi-bilbao_rejilla_not-free.png", sep = ""),width = 1200,height = 800)
municipios %>% filter( OSI_NOM_eu == "Bilbao - Basurto" ) %>% 
  ggplot() +
  geom_col(aes(date, value, fill = ""), width= 1 ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_avg7, group=name, color= ""), size= 1.1 ) +
  scale_color_manual(values=c("#565656")  )+
  facet_wrap( ~name) + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "15 days",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+70, max(municipios$date)+1),
    expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 20) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position =  "top"
  ) +
  labs(title = paste0("Casos PCR+ por COVID-19 por zonas de salud por día en Bilbao-Basurto." ),
       subtitle = paste0("Media: ventana de 7 días. ",period_eus),
       y = "casos por día",
       x = "fecha",
       fill = "casos por dia",
       colour = "media",
       caption = caption_provincia)
dev.off()

# rejilla osi ------
png(filename=paste0("img/spain/euskadi/covid19_casos-osi-pais-vasco_rejilla.png", sep = ""),width = 1200,height = 800)
osi %>% # filter( name %in% municipios_top$name) %>%
  ggplot() +
  geom_col(aes(date, value, fill = ""), width= 1 ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_avg7, group=name, color= ""), size= 1.1 ) +
  scale_color_manual(values=c("#565656")  )+
  facet_wrap( ~name, scales = "free_y") + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "15 days",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+70, max(municipios$date)+1),
    expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "top"
  ) +
  labs(title = paste0("Casos PCR+ por COVID-19 por OSI por día en Euskadi" ),
       subtitle = paste0("Media: ventana de 7 días. ",period_eus),
       y = "casos por día",
       x = "fecha 2020",
       fill = "casos por dia",
       colour = "media",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/euskadi/covid19_casos-osi-pais-vasco_rejilla_not-free.png", sep = ""),width = 1200,height = 800)
osi %>% # filter( name %in% municipios_top$name) %>%
  ggplot() +
  geom_col(aes(date, value, fill = ""), width= 1 ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_avg7, group=name, color= ""), size= 1.1 ) +
  scale_color_manual(values=c("#565656")  )+
  facet_wrap( ~name) + 
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "15 days",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+70, max(municipios$date) +1),
    expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "top"
  ) +
  labs(title = paste0("Casos PCR+ por COVID-19 por OSI por día en Euskadi" ),
       subtitle = paste0("Media: ventana de 7 días. ",period_eus),
       y = "casos por día",
       x = "fecha 2020",
       fill = "casos por dia",
       colour = "media",
       caption = caption_provincia)
dev.off()


# Casos de no residentes en Euskadi ----------
# Set colors 
# extends color paletter
library(RColorBrewer)
# creates extended color palette https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
colourCount <- length(unique(data_cases_sp_provinces$ccaa))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
colors_prov <- getPalette(colourCount )
# Change yellow to blue
colors_prov[1] <- "#a60000"
colors_prov[12] <- "#84d3e7"

download.file("https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/gviz/tq?tqx=out:csv&sheet=EUS", 
              "data/original/spain/euskadi/pais-vasco.csv")
euskadi <- read.delim("data/original/spain/euskadi/pais-vasco.csv", sep=",")
otros <- euskadi %>% filter( province == "Araba/Álava" & !is.na(PCR_diario_residencia_fuera_de_euskadi)) %>% 
  select(date,PCR_diario_residencia_fuera_de_euskadi) %>% mutate(
    province = "Residentes fuera de Euskadi",
    ccaa = "País Vasco"
  ) %>% rename(
    PCR = PCR_diario_residencia_fuera_de_euskadi
  ) %>% mutate(
    new_cases = NA,
    TestAc = NA,
    activos = NA,
    hospitalized = NA,
    intensive_care = NA,
    deceased = NA,
    cases_accumulated = NA,
    cases_accumulated_PCR = NA,
    recovered = NA,
    source_name = NA,
    source = NA,
    comments = NA
    ) %>% select( names(euskadi %>% select(-PCR_diario_residencia_fuera_de_euskadi)) )

euskadi <- rbind (euskadi %>% select(-PCR_diario_residencia_fuera_de_euskadi),
                  otros)

# calculateaverags
euskadi <- euskadi %>% 
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
    # daily_deaths = deceased - lag(deceased),
    # daily_deaths_inc = round((deceased - lag(deceased)) /lag(deceased) * 100, digits = 1),
    # daily_deaths_avg7 =  round( ( daily_deaths + lag(daily_deaths,1)+lag(daily_deaths,2)+
    #                                 lag(daily_deaths,3)+lag(daily_deaths,4)+lag(daily_deaths,5)+lag(daily_deaths,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
    # deaths_last_week =  daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) + lag(daily_deaths,4) + lag(daily_deaths,5) + lag(daily_deaths,6)
  )

# Calculates averages when no enough data available -/////////////
# Code provided by @picanumeros
# Caclulates average when no values are available. Usually when in the weekends no data are available, daily_cases_PCR_avg7 can't be calculated
# Lo que propongo es que cuando haya un hueco de varios días sin datos, el dato del primer día en el 
# que vuelva a haber se promedie entre los días 
# que se ha estado sin datos. Digamos que se "reparten" los casos a lo largo de los días.
pcr_avg7 <- euskadi %>% # filter(date >= as.Date("2020-05-11")) %>%  
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

# Add data to the source
euskadi <- merge(
  euskadi %>% mutate ( dunique = paste0( date, province) ),
  pcr_avg7 %>% mutate ( dunique = paste0( date, province) ) %>% ungroup() %>% select (dunique,daily_cases_PCR_avg7_complete),
  by.x = "dunique", by.y = "dunique", all= TRUE
) %>% mutate (
  daily_cases_PCR_avg7 = daily_cases_PCR_avg7_complete
) %>% select (-daily_cases_PCR_avg7_complete,-dunique)
# select(date, province,daily_cases_PCR,daily_cases_PCR_avg7,daily_cases_PCR_avg7_complete)

euskadi <- euskadi %>% mutate(
  date = as.character(date),
  date = as.Date(date, "%Y-%m-%d")
)

# Loop 50 days Lin 
prov <- levels(data_cases_sp_provinces$ccaa)[18]
  
png(filename=paste0("img/spain/provincias/covid19_casos-por-dia-provincia-media-superpuesto-lineal-last50-media-pais.png", sep = ""),width = 1200,height = 800)
euskadi %>% filter ( (ccaa == prov) & (date > filter_date - 50) ) %>%
    ggplot() +
    geom_line(aes(date, daily_cases_avg7,group=province, color=province), size= 1.5, se = FALSE, span = 0.6 ) +
    geom_point(aes(date, daily_cases, color=province), size= 1.2, alpha = 0.5 ) +
    geom_line(aes(date, daily_cases, color=province, group=province), size= 0.2, alpha = 0.5 ) +
  scale_color_manual(values = colors_prov) +
    scale_y_continuous( 
      labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
      # expand = c(0,0.2)
    ) +
    scale_x_date(date_breaks = "1 week", 
                 date_labels = "%d/%m",
                 limits=c( filter_date - 50, max(euskadi$date +16)),
                 expand = c(0,0) 
    ) + 
    theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      # panel.grid.minor.y = element_blank(),
      axis.ticks.x = element_line(color = "#000000"),
      legend.position =  "none"
    ) +
    labs(title = paste0("Media de casos por día (media 7 días) por COVID-19 en ",prov, " ", updated ),
         subtitle = paste0("Línea de puntos: casos PCR+, media de 7 días. Últimos 50 días. Por provincia. ",period),
         y = "casos por día (media 7 días)",
         x = "fecha",
         caption = caption_provincia) + 
      geom_line( aes(date, daily_cases_PCR_avg7, group=province, color=province), size= 1.4, linetype = "11")  +
      geom_point(aes(date, daily_cases_PCR, color=province), size= 1.4, alpha = 0.7, shape= 21 ) +
      geom_line(aes(date, daily_cases_PCR, color=province, group=province), size= 0.3, alpha = 0.5, linetype="11" ) + 
  geom_text_repel(
      data = euskadi %>% filter( ccaa == prov ) %>% group_by(province) %>% filter(!is.na(daily_cases_PCR_avg7) ) %>% top_n(1, date),
      aes(date, daily_cases_PCR_avg7, color=province,
          label=paste(format(daily_cases_PCR_avg7, nsmall=1, big.mark=".", decimal.mark = ","), "(PCR+)", province)),
      nudge_x = 1, # adjust the starting y position of the text label
      size=5,
      hjust=0,
      family = "Roboto Condensed",
      direction="y",
      segment.size = 0.1,
      segment.color="#777777"
    )
  dev.off()
  
  