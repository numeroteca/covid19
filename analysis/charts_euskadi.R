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
period_eus <- paste("Actualizado:", max_date)

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
               limits=c( min(euskadi_hosp$date), max(euskadi_hosp$date +33)),
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
    limits=c( min(euskadi_total$date), max(euskadi_hosp$date +57)),
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


# Municipio ------------

period_eus <- paste("Actualizado:", "2020-07-29")

municipios <- read_excel("data/original/spain/euskadi/situacion-epidemiologica.xlsx", skip = 1, col_names = TRUE, sheet = "05") %>% rename(
  name = "OSASUN-EREMUAK/ZONAS DE SALUD"
) %>% melt(  id.vars = c("name")) %>% rename ( date = variable) %>% 
  group_by(name) %>% arrange(date) %>% 
  mutate(
  date =  as.Date( paste0(date,"/2020"), "%d/%m/%y"),
  daily_cases_avg7 =  round( ( value + lag(value,1)+lag(value,2)+
                                 lag(value,3)+lag(value,4)+lag(value,5)+lag(value,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
  ) %>% filter( !is.na(date))

municipios_top <- municipios %>% top_n(1, date) %>% filter (daily_cases_avg7 > 1 ) %>% select (name)

png(filename=paste0("img/spain/euskadi/covid19_casos-municipios-pais-vasco.png", sep = ""),width = 1200,height = 800)
municipios %>% # filter( name %in% municipios_top$name) %>%
  ggplot() +
  geom_line(aes(date, daily_cases_avg7, group=name, color = name), size= 0.5) +
  geom_point(aes(date, value, color = name), size= 1) +
  geom_text_repel(
    data = municipios %>% group_by(name) %>% filter(!is.na(daily_cases_avg7) ) %>% top_n(1, date) %>% filter (daily_cases_avg7 > 1.8 ),
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
    limits=c( min(municipios$date)+70, max(municipios$date +9)),
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
  labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Euskadi" ),
       subtitle = paste0("Media: ventana de 7 días. ",period_eus),
       y = "casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/euskadi/covid19_casos-municipios-pais-vasco_rejilla.png", sep = ""),width = 1200,height = 800)
municipios %>% filter( name %in% municipios_top$name) %>%
  ggplot() +
  geom_col(aes(date, value), width= 1, fill = "#AAAAAA") +
  geom_line(aes(date, daily_cases_avg7, group=name), size= 1.1, color= "#565656") +
  facet_wrap( ~name, scales = "free_y") + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%m",
    limits=c( min(municipios$date)+70, max(municipios$date +9)),
    expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "none"
  ) +
  labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Euskadi" ),
       subtitle = paste0("Media: ventana de 7 días. ",period_eus),
       y = "casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/euskadi/covid19_casos-municipios-pais-vasco_rejilla_not-free.png", sep = ""),width = 1200,height = 800)
municipios %>% filter( name %in% municipios_top$name) %>%
  ggplot() +
  geom_col(aes(date, value), width= 1, fill = "#AAAAAA") +
  geom_line(aes(date, daily_cases_avg7, group=name), size= 1.1, color= "#565656") +
  facet_wrap( ~name) + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%m",
    limits=c( min(municipios$date)+70, max(municipios$date +9)),
    expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position =  "none"
  ) +
  labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Euskadi" ),
       subtitle = paste0("Media: ventana de 7 días. ",period_eus),
       y = "casos por día",
       x = "fecha",
       caption = caption_provincia)
dev.off()

