# Script para calcular casos positivos en Cataluña y comparar con otras bases de datos
# También 

library(tidyverse)
library(reshape2)

# Donwloaded from 
# https://analisi.transparenciacatalunya.cat/Salut/Registre-de-test-de-COVID-19-realitzats-a-Cataluny/jj6z-iyrp/data
# catalunya_original_test <-  read.delim("data/original/spain/catalunya/Registre_de_test_de_COVID-19_realitzats_a_Catalunya._Segregaci__per_sexe_i_municipi.csv",sep = ",")
# https://analisi.transparenciacatalunya.cat/Salut/Registre-de-casos-de-COVID-19-realitzats-a-Catalun/jj6z-iyrp/data
catalunya_original <-  read.delim("data/original/spain/catalunya/Registre_de_casos_de_COVID-19_realitzats_a_Catalunya._Segregaci__per_sexe_i_municipi.csv",sep = ",")
# Datos del panel de control (https://app.powerbi.com/view?r=eyJrIjoiZTkyNTcwNjgtNTQ4Yi00ZTg0LTk1OTctNzM3ZGEzNWE4OTIxIiwidCI6IjNiOTQyN2RjLWQzMGUtNDNiYy04YzA2LWZmNzI1MzY3NmZlYyIsImMiOjh9)
# de Salut de Catalunya descargados a mano en la hoja de cálculo y descargados en CSV
powerbi <-  read.delim("data/original/spain/catalunya/powerbi.csv",sep = ",",skip = 1)

catalunya <- catalunya_original

# cases -------------
# creates date variable
catalunya$date <- as.Date(catalunya$TipusCasData, "%d/%m/%Y")
# extracts first charatcter of zipcode to select province code
catalunya$provincia_code <- ifelse(catalunya$MunicipiCodi < 10000, paste0(substr(as.character(catalunya$MunicipiCodi),1,1)), substr(as.character(catalunya$MunicipiCodi),1,2))     

# iterates through data to count positives
catalunya_new <- catalunya %>% group_by(date,provincia_code) %>% arrange(date) %>% filter( TipusCasDescripcio == "Positiu" ) %>%
# catalunya_new <- catalunya %>% group_by(date,provincia_code) %>% arrange(date) %>% filter( ResultatCovidDescripcio == "Positiu" |  ResultatCovidDescripcio == "Probable" ) %>%
  summarise ( 
    by_day = sum(NumCasos)
    )

# Makes df for every province
cat8 <- catalunya_new %>% filter(provincia_code == "8")
cat8 <- cat8 %>% group_by( provincia_code) %>% arrange(date) %>%
  mutate ( 
    cases_accumulated = cumsum(by_day)
  )

cat17 <- catalunya_new %>% filter(provincia_code == "17")
cat17 <- cat17 %>% group_by( provincia_code) %>% arrange(date) %>%
  mutate ( 
    cases_accumulated = cumsum(by_day)
  )

cat25 <- catalunya_new %>% filter(provincia_code == "25")
cat25 <- cat25 %>% group_by( provincia_code) %>% arrange(date) %>%
  mutate ( 
    cases_accumulated = cumsum(by_day)
  )

cat43 <- catalunya_new %>% filter(provincia_code == "43")
cat43 <- cat43 %>% group_by( provincia_code) %>% arrange(date) %>%
  mutate ( 
    cases_accumulated = cumsum(by_day)
  )

# Binds all the provinces
cattotal <- rbind(cat8,cat17,cat25,cat43)
rm(cat8,cat17,cat25,cat43)
# Creates provinica factor
cattotal$province <- as.factor(cattotal$provincia_code)
# Gives names 
levels(cattotal$province) <- c("Girona","Lleida","Tarragona","Barcelona")

write.csv(cattotal, file = "data/output/spain/catalunya/catalunya-cases-evolution-by-province.csv", row.names = FALSE)
saveRDS(cattotal, file = "data/output/spain/catalunya/catalunya-cases-evolution-by-province.rds")

# Plots -----
ggplot(cattotal) +
  geom_line(aes(date,cases_accumulated, group = provincia_code), color = "red") +
  # uses data from evolution_spain_provinces.R script. Rund that first!
  geom_line(data = filter (data_cases_sp_provinces, ccaa =="Cataluña" ) , aes(date,cases_accumulated, group = province),size = 0.5) +
  scale_y_log10()

ggplot(cattotal) +
  geom_line(data = filter (data_cases_sp_provinces, ccaa =="Cataluña" ) , aes(date,cases_accumulated, group = province, color ="#000000"),size = 1) +
  geom_point(data = filter (data_cases_sp_provinces, ccaa =="Cataluña" ) , aes(date,cases_accumulated ),size = 1) +
  geom_line(aes(date,cases_accumulated, group = provincia_code, color = "#DD0000")) +
  geom_point(aes(date,cases_accumulated), color = "red", size = 0.5) +
  scale_color_identity(
    guide = "legend",
    labels = c("esCovid19data", "transparenciacatalunya.cat"),
  ) +
  scale_y_log10() +
  facet_grid(~province)


# ggplot(cattotal) +
#   geom_line(aes(date,cases_accumulated, group = provincia_code)) +
#   facet_grid(~province)+
#   scale_y_log10()
# 
# ggplot(cattotal) +
#   geom_line(aes(date,cases_accumulated, group = provincia_code)) +
#   facet_grid(~province) 


# Fallecimientos ------

powerbi$date <- as.Date(powerbi$Fecha, "%d/%m/%y")

powerbi %>% rename ( 
  province = Territorio,
  deceased = Fallecimientos
  ) %>% 
ggplot() +
  geom_line(data = filter (data_cases_sp_provinces, ccaa =="Cataluña" ) , aes(date, deceased, group = province, color ="#000000"),size = 1) +
  geom_point(data = filter (data_cases_sp_provinces, ccaa =="Cataluña" ) , aes(date,deceased ),size = 1) +
  geom_line(aes(date,deceased, group = province, color = "#DD0000")) +
  geom_point(aes(date,deceased), color = "red", size = 0.5) +
  scale_color_identity(
    guide = "legend",
    labels = c("esCovid19data", "Gencat (Salut-powerbi)"),
  ) +
  scale_y_log10() +
  coord_cartesian(
    xlim = c( min(data_cases_sp_provinces$date + 20), max(data_cases_sp_provinces$date +1)),
  ) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0)
  ) + 
  facet_grid(~province) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    # axis.ticks.x = element_line(color = "#000000"),
    axis.text.x = element_text(size = 9),
    legend.position = "top"
  ) +
  labs(
    title = "Fallecidos acumulados por provinicas en Cataluña (28.04.2020)",
       subtitle = paste0("Comparativa de bases de datos. Gencat - esCOVID19data "),
       y = "fallecidos (escala lineal)",
       x = "fecha",
    color = "Fuente",
       caption = caption_provincia
    )

rm(catalunya_new,catalunya,catalunya_original,powerbi)
