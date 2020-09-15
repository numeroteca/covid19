# Comparativa de bases de datos -----------

# Este script toma datos generados en evolution_spain.R y evolution_spain_provinces.R, debes ejecutarlos antes

# Load libraries -----------
library(tidyverse)

# import Instituto de Salud CIII ----
ciii_original <- read.delim("https://covid19.isciii.es/resources/serie_historica_acumulados.csv",sep = ",")  
write.csv(ciii_original, file = "data/original/spain/iscii_data.csv", row.names = FALSE)

ciii <- ciii_original %>% head(nrow(ciii_original) - 9) %>% #Cambia el número en función de las notas que incluya el csv original
  ungroup() %>% mutate(
    date = as.Date(FECHA, "%d/%m/%Y" ),
    CCAA = CCAA %>% str_replace_all("AN", "Andalucía"),
    CCAA = CCAA %>% str_replace_all("AR", "Aragón"),
    CCAA = CCAA %>% str_replace_all("AS", "Asturias, Principado de"),
    CCAA = CCAA %>% str_replace_all("CB", "Cantabria"),
    CCAA = CCAA %>% str_replace_all("CE", "Ceuta"),
    CCAA = CCAA %>% str_replace_all("CL", "Castilla y León"),
    CCAA = CCAA %>% str_replace_all("CM", "Castilla - La Mancha"),
    CCAA = CCAA %>% str_replace_all("CN", "Canarias"),
    CCAA = CCAA %>% str_replace_all("CT", "Cataluña"),
    CCAA = CCAA %>% str_replace_all("EX", "Extremadura"),
    CCAA = CCAA %>% str_replace_all("GA", "Galicia"),
    CCAA = CCAA %>% str_replace_all("IB", "Balears, Illes"),
    CCAA = CCAA %>% str_replace_all("MC", "Murcia, Región de"),
    CCAA = CCAA %>% str_replace_all("MD", "Madrid, Comunidad de"),
    CCAA = CCAA %>% str_replace_all("ML", "Melilla"),
    CCAA = CCAA %>% str_replace_all("NC", "Navarra, Comunidad Foral de"),
    CCAA = CCAA %>% str_replace_all("PV", "País Vasco"),
    CCAA = CCAA %>% str_replace_all("RI", "Rioja, La"),
    CCAA = CCAA %>% str_replace_all("VC", "Comunitat Valenciana"),
  ) %>% rename(
    region = CCAA,
    fecha = FECHA,
    cases_registered = CASOS,
    PCR = PCR.,
    TestAc =TestAc.,
    hospitalized = Hospitalizados,
    intensive_care = UCI,
    deceassed = Fallecidos
    # recovered = Recuperados
  ) %>% mutate (
    cases_accumulated = ifelse( is.na(cases_registered), replace_na(PCR, 0) + replace_na(TestAc, 0), cases_registered),
    recovered = NA
  )

# write.csv(ciii, file = "data/output/spain/iscii_processed_data.csv", row.names = FALSE)


# summarise Escovid19data data by ccaa ------
data_cases_sp_provinces <- readRDS(file = "data/output/spain/covid19-provincias-spain_consolidated.rds")
resumen_pr_ca <- data_cases_sp_provinces %>% group_by(date,ccaa) %>% 
  summarise( 
    deceassed = sum(deceased), 
    cases_accumulated = sum(cases_accumulated),
    cases_accumulated_PCR = sum(cases_accumulated_PCR),
    ) %>% rename(
 region = ccaa  
)

levels(as.factor(ciii$region))
levels(resumen_pr_ca$region)
# rename comunidades autónomas
# levels(resumen_pr_ca$region) <- c("Andalucía","Aragón", "Asturias", "Baleares", "Canarias", "Cantabria",  "Castilla-La Mancha","Castilla y León", "Cataluña", "Ceuta",
#                                 "C. Valenciana", "Extremadura","Galicia","Madrid", "Melilla", "Murcia", "Navarra",  "País Vasco","La Rioja")      

# resumen_pr_ca$region <- resumen_pr_ca$ccaa
# %>% filter( region== "Cataluña")
# Plots --------------
png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_muertes_bbdd_lineal.png", sep = ""),width = 1400,height = 900)
ggplot(NULL) +
  geom_line( data = resumen_pr_ca , aes( date, deceassed, group=region, color = "#e7298a"), size = 1.5 ) +
  # geom_line( data = data_all_export, aes( date, deceassed, group=region), color = "#66a61e", size = 1 ) +
  geom_line( data = ciii , aes( date, deceassed, group=region, color = "#000000"), size = 0.7 ) +
  geom_text(data = resumen_pr_ca %>% group_by(region) %>% filter(!is.na(deceassed)) %>% top_n(1, date), aes( date+15, deceassed, label=deceassed), color = "#e7298a" ) +
  geom_text(data = ciii %>% group_by(region) %>% filter(!is.na(deceassed))  %>% top_n(1, date), aes( date+40, deceassed, label=deceassed), color = "#000000" ) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_color_identity(
    guide = "legend",
    labels = c("ISCIII","Escovid19data"),
  ) +
  facet_wrap( ~region, scales = "free_y") +
  scale_x_date(
              date_breaks = "1 month",
               date_labels = "%d/%m",
               expand = c(0,20) 
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 23) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =11 ),
    legend.position = "top"
  ) +
  labs(title = "Comparativa de bases de datos. Fallecimientos acumulados COVID-19 por comunidades autónomas. España",
       subtitle = paste0("Instituto de Salud Carlos III (negro). Escovid19data (rosa), agregado por CCAA. 2020-05-27"),
       y = "fallecimientos acumulados",
       x = "fecha",
       color = "",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19  |  Datos: ISCIII y esCOVID19data" )
  )
dev.off()

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_casos_bbdd_lineal.png", sep = ""),width = 1400,height = 900)
ggplot(NULL) +
  geom_line( data = resumen_pr_ca , aes( date, cases_accumulated, group=region, color = "#e7298a"), size = 1.5 ) +
  geom_line( data = resumen_pr_ca , aes( date, cases_accumulated_PCR, group=region, color = "#e7298a"), size = 1.5, linetype="11" ) +
  # geom_line( data = data_all_export, aes( date, deceassed, group=region), color = "#66a61e", size = 1 ) +
  geom_line( data = ciii , aes( date, cases_accumulated, group=region, color = "#000000"), size = 0.7 ) +
  geom_line( data = ciii , aes( date, PCR, group=region, color = "#000000"), size = 0.7, linetype="11" ) +
  geom_text(data = resumen_pr_ca %>% group_by(region) %>% filter(!is.na(cases_accumulated)) %>% top_n(1, date), aes( date+15, cases_accumulated, label=cases_accumulated), color = "#e7298a" ) +
  geom_text(data = ciii %>% group_by(region) %>% filter(!is.na(cases_accumulated))  %>% top_n(1, date), aes( date+40, cases_accumulated, label=cases_accumulated), color = "#000000" ) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_color_identity(
    guide = "legend",
    labels = c("ISCIII","Escovid19data"),
  ) +
  facet_wrap( ~region, scales = "free_y") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%d/%m",
    expand = c(0,20) 
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 23) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =11 ),
    legend.position = "top"
  ) +
  labs(title = "Comparativa de bases de datos. Casos acumulados COVID-19 por comunidades autónomas. España",
       subtitle = paste0("PCR+: Línea de puntos. Instituto de Salud Carlos III (negro). Escovid19data (rosa), agregado por CCAA. 2020-05-27"),
       y = "casos y  casos PCR+ acumulados",
       x = "fecha",
       color = "",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19  |  Datos: ISCIII y esCOVID19data" )
  )
dev.off()

  # 
4+4


png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_muertes_bbdd_log.png", sep = ""), width = 2300, height = 1900)
ggplot(NULL) +
  # geom_line( data = resumen_pr_ca, aes( date, deceassed, group=region), color = "#e7298a", size = 1.5 ) +
  geom_line( data = data_all_export, aes( date, deceassed, group=region), color = "#000000", size = 1 ) +
  geom_line( data = datadista, aes( date, deceassed, group=region), color = "#66a61e", size = 0.7 ) +
  scale_y_log10(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "10 day", 
               date_labels = "%d",
               expand = c(0,0) 
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 19) +
  theme(
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =9 )
    # legend.position = "none"
  ) +
  labs(title = "Comparativa de bases de datos. Fallecimientos acumulados COVID-19 por comunidades autónomas. España",
       subtitle = paste0("Datadista (verde). Instituto de Salud Carlos III (negro). Providencialdata19 (rosa)."),
       y = "fallecimientos acumulados",
       x = "fecha",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19" )
  )
dev.off()

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_casos_bbdd_log.png", sep = ""), width = 2300, height = 1900)
ggplot(NULL) +
  # geom_line( data = resumen_pr_ca, aes( date, cases_accumulated, group=region), color = "#e7298a", size = 1.5 ) +
  geom_line( data = datadista, aes( date, cases_registered, group=region), color = "#66a61e", size = 1 ) +
  geom_line( data = data_all_export, aes( date, cases_registered, group=region), color = "#000000", size = 0.7 ) +
  scale_y_log10(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               expand = c(0,0) 
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 19) +
  theme(
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =9 )
    # legend.position = "none"
  ) +
  labs(title = "Comparativa de bases de datos. Casos acumulados COVID-19 por comunidades autónomas. España",
       subtitle = paste0("Datadista (verde). Instituto de Salud Carlos III (negro). Providencialdata19 (rosa)."),
       y = "casos acumulados",
       x = "fecha",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19" )
  )
dev.off()

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_casos_bbdd_lineal.png", sep = ""), width = 2300, height = 1900)
ggplot(NULL) +
  geom_line( data = resumen_pr_ca, aes( date, cases_accumulated, group=region), color = "#e7298a", size = 1.5 ) +
  geom_line( data = data_all_export, aes( date, cases_registered, group=region), color = "#66a61e", size = 1 ) +
  geom_line( data = ciii, aes( date, cases_registered, group=region), color = "#000000", size = 0.7 ) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               expand = c(0,0) 
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 19) +
  theme(
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =9 )
    # legend.position = "none"
  ) +
  labs(title = "Comparativa de bases de datos. Casos acumulados COVID-19 por comunidades autónomas. España",
       subtitle = paste0("Datadista (verde). Instituto de Salud Carlos III (negro). Providencialdata19 (rosa)."),
       y = "casos acumulados",
       x = "fecha",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19" )
  )
dev.off()


# merge all the datasets in one to calculate diferencs among data bases----------
resumen_pr_ca$dateunique <- paste0(resumen_pr_ca$date,resumen_pr_ca$region)
data_all_export$dateunique <- paste0(data_all_export$date,data_all_export$region)
ciii$dateunique <- paste0(ciii$date,ciii$region)

zmerged <- merge(resumen_pr_ca %>% select(date, deceassed, cases_accumulated, dateunique) %>% rename( dec_pr = deceassed, cas_pr = cases_accumulated),
                 data_all_export %>% select(deceassed, cases_registered, dateunique) %>% rename( dec_dt = deceassed, cas_dt = cases_registered ),
                 by.x = "dateunique", by.y = "dateunique")

zmerged <- merge(zmerged,
                 ciii %>% select(deceassed, cases_registered, dateunique) %>% rename( dec_ci = deceassed, cas_ci = cases_registered ),
                 by.x = "dateunique", by.y = "dateunique") %>% select(-dateunique)

zmerged$dif_pr <- zmerged$dec_pr - zmerged$dec_ci
zmerged$dif_dt <- zmerged$dec_dt - zmerged$dec_ci

png(filename=paste("img/spain/provincias/comparativa/covid19_diferencia-bbdd.png", sep = ""), width = 1200, height = 700)
zmerged %>% filter( date < as.Date("2020-04-15")) %>%
ggplot() +
  geom_line( aes( date, dif_pr, group=region), color = "#e7298a", size = 1.5 ) +
  geom_line(aes( date, dif_dt, group=region), color = "#66a61e", size = 1 ) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0) 
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 19) +
  theme(
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =9 )
    # legend.position = "none"
  ) +
  labs(title = "Comparativa de bases de datos. Muertes acumulados COVID-19 por comunidades autónomas. España",
       subtitle = paste0("Diferencia: base de datos - sciii [Datadista (verde). esCOVID19data (rosa)]"),
       y = "diferencia: base de datos - base de datos isciii",
       x = "fecha",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19" )
  )
dev.off()

# date correction --------
# one day less for the two databases
resumen_pr_ca$date2 <- resumen_pr_ca$date - 1
data_all_export$date2 <- data_all_export$date - 1

resumen_pr_ca$dateunique2 <- paste0(resumen_pr_ca$date2, resumen_pr_ca$region)
data_all_export$dateunique2 <- paste0(data_all_export$date2, data_all_export$region)

zmerged2 <- merge(resumen_pr_ca %>% select(date2, deceassed, cases_accumulated, dateunique2) %>% rename( dec_pr = deceassed, cas_pr = cases_accumulated),
                  data_all_export %>% select(deceassed, cases_registered, dateunique2) %>% rename( dec_dt = deceassed, cas_dt = cases_registered ),
                  by.x = "dateunique2", by.y = "dateunique2")

zmerged2 <- merge(zmerged2,
                  ciii %>% select(deceassed, cases_registered, dateunique) %>% rename( dec_ci = deceassed, cas_ci = cases_registered ),
                  by.x = "dateunique2", by.y = "dateunique") %>% select(-dateunique2)

zmerged2$dif_pr <- zmerged2$dec_pr - zmerged2$dec_ci
zmerged2$dif_dt <- zmerged2$dec_dt - zmerged2$dec_ci

png(filename=paste("img/spain/provincias/comparativa/covid19_diferencia-bbdd-corregido-1-dia.png", sep = ""), width = 1200, height = 700)
zmerged2 %>% filter( date < as.Date("2020-04-15")) %>%
  ggplot() +
  geom_line( aes( date, dif_pr, group=region), color = "#e7298a", size = 1.5 ) +
  geom_line(aes( date, dif_dt, group=region), color = "#66a61e", size = 1 ) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d",
               expand = c(0,0) 
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 19) +
  theme(
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =9 )
    # legend.position = "none"
  ) +
  labs(title = "Comparativa de bases de datos. Muertes acumulados COVID-19 por comunidades autónomas. España",
       subtitle = paste0("Diferencia: base de datos - sciii [Datadista (verde). esCOVID19data (rosa)]"),
       y = "diferencia: base de datos - base de datos isciii",
       x = "fecha (a Datadista y PD19 se resta un día)",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19" )
  )
dev.off()


# Compara con geovoluntarios https://www.datoscovid.es -------
data_cases_sp_provinces <- readRDS(file = "data/output/spain/covid19-provincias-spain_consolidated.rds")

download.file("https://opendata.arcgis.com/datasets/9ec5c536afd643459e5bf40c71124a03_0.csv", 
              "data/original/spain/geovoluntarios-datacovid.csv")
geov <- read.delim("data/original/spain/geovoluntarios-datacovid.csv",sep = ",") %>% rename ( province = Texto ) %>% mutate(
  province = province %>% str_replace_all("Alacant/Alicante", "Alicante/Alacant"),
  province = province %>% str_replace_all("Castelló/Castellón", "Castellón/Castelló"),
  province = province %>% str_replace_all("València/Valencia", "Valencia/València")
)
geov$date <- as.Date(geov$Fecha,"%Y/%m/%d")
geov$CasosConfirmados <- as.numeric(geov$CasosConfirmados)

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_casos_geovoluntarios-escovid19data.png", sep = ""), width = 2400, height = 1900)
ggplot(NULL) +
  geom_line( data = data_cases_sp_provinces %>% filter(!is.na(cases_accumulated)), aes( date, cases_accumulated, group=province, color = "#66a61e"), size = 2.0 ) +
  geom_line( data = geov  %>% filter(!is.na(CasosConfirmados)), aes( date, CasosConfirmados, group=province, color = "#000000"), size = 0.7 ) +
  # scale_y_log10(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~province, scales = "free") + #, scales = "free"
  scale_color_identity(
    guide = "legend",
    labels = c("Geovoluntarios - datoscovid.es","Escovid19data"),
  ) +
  # scale_x_date(date_breaks = "10 day", 
  #              date_labels = "%d"
  #              # expand = c(0,0) 
  # ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 27) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =12 ),
    legend.position = "top"
  ) +
  labs(title = "Comparativa de bases de datos. Casos acumulados COVID-19 por provincias. España",
       subtitle = paste0("Geovoluntarios - datoscovid.es (negro). Escovid19data (verde)"),
       y = "casos acumulados",
       x = "fecha",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19" )
  )
dev.off()

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_fallecidos_geovoluntarios-escovid19data.png", sep = ""), width = 2400, height = 1900)
ggplot(NULL) +
  geom_line( data = data_cases_sp_provinces %>% filter(!is.na(deceased)), aes( date, deceased, group=province, color = "#66a61e"), size = 2.0 ) +
  geom_line( data = geov  %>% filter(!is.na(Fallecidos)), aes( date, Fallecidos, group=province, color = "#000000"), size = 0.7 ) +
  # scale_y_log10(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~province, scales = "free") + #, scales = "free"
  scale_color_identity(
    guide = "legend",
    labels = c("Geovoluntarios - datoscovid.es","Escovid19data"),
  ) +
  # scale_x_date(date_breaks = "10 day", 
  #              date_labels = "%d"
  #              # expand = c(0,0) 
  # ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 27) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =12 ),
    legend.position = "top"
  ) +
  labs(title = "Comparativa de bases de datos. Fallecimientos acumulados COVID-19 por provincias. España",
       subtitle = paste0("Geovoluntarios - datoscovid.es (negro). Escovid19data (verde)"),
       y = "fallecidos acumulados",
       x = "fecha",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19",
       color = ""
       )
  )
dev.off()

# Comparativa datos Euskadi ----------

# Por provincias

# Load data
data_cases_sp_provinces <- readRDS(file = "data/output/spain/covid19-provincias-spain_consolidated.rds") %>% ungroup() %>% mutate(
  province = province %>% str_replace_all("Araba/Álava", "Araba"),
)

# Load Opendataeuskadi data
euskadi_od <- read.delim("data/original/spain/euskadi/06.csv",sep = ";",skip = 1)

euskadi_od <- euskadi_od %>% rename (date = X) %>%
  mutate( date = as.Date(as.character(date), "%d/%m/%y")
  ) %>% select( -Guztira...Total) %>% filter (!is.na(date))

# get total data first line
euskadi_first <- read.delim("data/original/spain/euskadi/06.csv",sep = ";",skip = 1) %>% rename (date = X) %>%
  mutate( date = as.Date(as.character(date), "%d/%m/%y")
  ) %>% select( -Guztira...Total) %>% filter (is.na(date))

# Data in long format
euskadi_long <- melt(euskadi_od, id.vars = c("date")) %>% 
  rename ( province = variable, daily_deceassed = value) %>%
  mutate(
     deceassed = NA
  )

# Añade valor inicial 
euskadi_long[ euskadi_long$province=="Araba" & euskadi_long$date == min(euskadi_long$date),]$deceassed <- euskadi_first$Araba - sum( euskadi_long[euskadi_long$province=="Araba",]$deceassed) + euskadi_long[ euskadi_long$province=="Araba" & euskadi_long$date == min(euskadi_long$date),]$daily_deceassed
euskadi_long[ euskadi_long$province=="Bizkaia" & euskadi_long$date == min(euskadi_long$date),]$deceassed <- euskadi_first$Bizkaia - sum( euskadi_long[euskadi_long$province=="Bizkaia",]$deceassed) + euskadi_long[ euskadi_long$province=="Bizkaia" & euskadi_long$date == min(euskadi_long$date),]$daily_deceassed
euskadi_long[ euskadi_long$province=="Gipuzkoa" & euskadi_long$date == min(euskadi_long$date),]$deceassed <- euskadi_first$Gipuzkoa - sum( euskadi_long[euskadi_long$province=="Gipuzkoa",]$deceassed) + euskadi_long[ euskadi_long$province=="Gipuzkoa" & euskadi_long$date == min(euskadi_long$date),]$daily_deceassed

euskadi_long <- euskadi_long  %>% group_by(province) %>%
  mutate(
    deceassed = cumsum(daily_deceassed)
  )

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_fallecidos_irekia-opendata.png", sep = ""), width = 900, height = 400)
ggplot(NULL) +
  geom_line( data = data_cases_sp_provinces %>% filter(ccaa == "País Vasco" ), aes( date, deceased, group=province, color = "#66a61e"), size = 2.0 ) +
  geom_line( data = euskadi_long, aes( date, deceassed, group=province, color = "#000000"), size = 0.7 ) +
  # scale_y_log10(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~province) + #, scales = "free"
  scale_color_identity(
    guide = "legend",
    labels = c("Opendata Euskadi (2020.05.15)","Irekia-Osakidetza (hasta 2020.05.14)"),
  ) +
  scale_x_date(
      # date_breaks = "10 day",
               date_labels = "%d-%m",
               limits = c(as.Date("2020-03-12"),max(euskadi_long$date)),
               expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size =20) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =10 ),
    legend.position = "top"
  ) +
  labs(title = "Comparativa de bases de datos. Fallecimientos acumulados COVID-19 en Euskadi",
       # subtitle = paste0("Geovoluntarios - datoscovid.es (negro). Escovid19data (verde)"),
       y = "fallecidos acumulados",
       x = "fecha",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19",
                         color = "",
                         colour = ""
       )
  ) +
  guides(color=guide_legend(title="Base de datos"))
dev.off()


# Por ccaa
# ISCIII data
data_all_export <- readRDS(file = "data/output/covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated_isciii.rds")

# esCOVID19data
# aggregate by ccaa
resumen_pr_ca <- data_cases_sp_provinces %>% group_by(date,ccaa) %>% summarise( deceassed = sum(deceased), cases_accumulated = sum(cases_accumulated) ) 

# Opendata Euskadi. aggregate by ccaa
euskadi_ca <- euskadi_long %>% group_by(date) %>% summarise( deceassed = sum(deceassed) ) 

# Plot 
png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_fallecidos_irekia-opendata-ccaa.png", sep = ""), width = 700, height = 400)
ggplot(NULL) +
  geom_line( data = resumen_pr_ca %>% filter(ccaa == "País Vasco" ), aes( date, deceassed, group=ccaa, color = "#66a61e"), size = 2.0 ) +
  geom_line( data = data_all_export %>% filter(region == "País Vasco" ), aes( date, deceassed, group=region, color = "#FF0000" ), size = 0.7 ) +
  geom_line( data = euskadi_ca , aes( date, deceassed, color = "#000000"), size = 0.7 ) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
    scale_color_identity(
    guide = "legend",
    labels = c("Opendata Euskadi (2020.05.15)","ISCIII","Irekia-Osakidetza (hasta 2020.05.14)"),
  ) +
  scale_x_date(
    date_labels = "%d-%m",
    limits = c(as.Date("2020-03-12"),max(euskadi_long$date)),
    expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size =16) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    # axis.text = element_text(size =12 ),
    legend.position = "top"
  ) +
  labs(title = "Comparativa de bases de datos. Fallecimientos acumulados COVID-19 en Euskadi",
       # subtitle = paste0("Geovoluntarios - datoscovid.es (negro). Escovid19data (verde)"),
       y = "fallecidos acumulados",
       x = "fecha",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19",
                         color = "",
                         colour = ""
       )
  ) +
  guides(color=guide_legend(title="Base de datos"))
dev.off()



# Comparativa Euskadi hospitalizados -----------
data_cases_sp_provinces <- readRDS(file = "data/output/spain/euskadi/compare_hospitalized_irekia-vs-opendata.rds")

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_hospitalizados_irekia-opendata-ccaa.png", sep = ""), width = 800, height = 400)
data_cases_sp_provinces %>% filter(ccaa == "País Vasco") %>%
ggplot() +
  geom_line( aes( date, hospitalized, group=province, color = "#66a6CC"), size = 0.8 ) +
  geom_line( aes( date, hospitalized_eus, group=province, color = "#FF0000" ), size = 0.8 ) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_color_identity(
    guide = "legend",
    labels = c("Irekia-Osakidetza (hasta 2020.05.14)","Opendata Euskadi (descargado 2020.05.22)"),
  ) +
  facet_wrap( ~province) + #, scales = "free"
  scale_x_date(
    date_labels = "%d-%m",
    # limits = c(as.Date("2020-03-12"),max(data_cases_sp_provinces$date)),
    expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size =16) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    # axis.text = element_text(size =12 ),
    legend.position = "top"
  ) +
  labs(title = "Comparativa de bases de datos. Hospitalizados COVID-19 en Euskadi",
       # subtitle = paste0("Geovoluntarios - datoscovid.es (negro). Escovid19data (verde)"),
       y = "hospitalizados",
       x = "fecha",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19",
                         color = "",
                         colour = ""
       )
  ) +
  guides(color=guide_legend(title="Base de datos"))
dev.off()


# ISCIII data
data_all_export <- readRDS(file = "data/output/covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated_isciii.rds")

resumen_pr_ca <- data_cases_sp_provinces %>% group_by(date,ccaa) %>% 
  summarise( hospitalized = sum(hospitalized), 
             hospitalized_eus = sum(hospitalized_eus)
             )

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_hospitalizados_irekia-opendata-isciii-ccaa.png", sep = ""), width = 700, height = 400)
ggplot(NULL) +
  geom_line( data = resumen_pr_ca %>% filter(ccaa == "País Vasco" ), aes( date, hospitalized, group=ccaa, color = "#66a61e"), size = 2.0 ) +
  geom_line( data = resumen_pr_ca %>% filter(ccaa == "País Vasco" ), aes( date, hospitalized_eus, group=ccaa, color = "#66a6FF"), size = 2.0 ) +
  geom_line( data = data_all_export %>% filter(region == "País Vasco" ), aes( date, hospitalized, group=region, color = "#FF0000" ), size = 0.7 ) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_color_identity(
    guide = "legend",
    labels = c("Opendata Euskadi (2020.05.15)","ISCIII","Irekia-Osakidetza (hasta 2020.05.14)","dd"),
  ) +
  scale_x_date(
    date_labels = "%d-%m",
    limits = c(as.Date("2020-03-12"),max(data_all_export$date)),
    expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size =16) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    # axis.text = element_text(size =12 ),
    legend.position = "top"
  ) +
  labs(title = "Comparativa de bases de datos. Hospitalizados COVID-19 en Euskadi",
       # subtitle = paste0("Geovoluntarios - datoscovid.es (negro). Escovid19data (verde)"),
       y = "fallecidos acumulados",
       x = "fecha",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19",
                         color = "",
                         colour = ""
       )
  ) +
  guides(color=guide_legend(title="Base de datos"))
dev.off()


# Comparativa casos escovid19data y RENAVE ISCIII -----------
data_cases_sp_provinces <- readRDS(file = "data/output/spain/covid19-provincias-spain_consolidated.rds")

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_renave-iscii_vs_escovid19data.png", sep = ""), width = 2700, height = 1650)
data_cases_sp_provinces %>% # filter( ccaa=="Galicia") %>%
ggplot() +
  # geom_line( aes( date, cases_accumulated, group=province, color = "#66a61e"), size = 2.0 ) +
  geom_line( aes( date, daily_cases_PCR, group=province, color = "#ff4d94"), size = 1.2 ) +
  geom_line( aes( date, num_casos_prueba_pcr, group=province, color = "#000000"), size = 0.7 ) +
  # scale_y_log10(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~province, scales = "free") + #, scales = "free"
  scale_color_identity(
    guide = "legend",
    labels = c("RENAVE","esCOVID19data"),
  ) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%m"
               # expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 33) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =15 ),
    panel.grid = element_line(size=0.4, color="#aaaaaa"),
    legend.position = "top"
  ) +
  labs(title = "Comparativa de bases de datos RENAVE y esCOVID19data. Casos diarios por PCR+",
       subtitle = paste0("COVID-19 por provincias. España"),
       y = "casos diarios PCR+",
       x = "2020",
       colour ="bases de datos",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19" )
  )
dev.off()


png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_renave-iscii_vs_escovid19data_lastdays.png", sep = ""), width = 2700, height = 1650)
data_cases_sp_provinces %>% filter( date > as.Date("2020-07-15") ) %>% # filter( ccaa=="Galicia") %>%
  ggplot() +
  # geom_line( aes( date, cases_accumulated, group=province, color = "#66a61e"), size = 2.0 ) +
  geom_line( aes( date, daily_cases_PCR, group=province, color = "#ff4d94"), size = 1.2 ) +
  geom_line( aes( date, num_casos_prueba_pcr, group=province, color = "#000000"), size = 0.7 ) +
  # scale_y_log10(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~province, scales = "free") + #, scales = "free"
  scale_color_identity(
    guide = "legend",
    labels = c("RENAVE","esCOVID19data"),
  ) +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%d/%m"
               # expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 33) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =15 ),
    panel.grid = element_line(size=0.4, color="#aaaaaa"),
    legend.position = "top"
  ) +
  labs(title = "Comparativa de bases de datos RENAVE y esCOVID19data. Casos diarios por PCR+",
       subtitle = paste0("COVID-19 por provincias. España"),
       y = "casos diarios PCR+",
       x = "2020",
       colour ="bases de datos",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19" )
  )
dev.off()

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_renave-iscii_vs_escovid19data_media.png", sep = ""), width = 2700, height = 1650)
data_cases_sp_provinces %>% # filter( ccaa=="Galicia") %>%
  ggplot() +
  # geom_line( aes( date, cases_accumulated, group=province, color = "#66a61e"), size = 2.0 ) +
  geom_line( aes( date, daily_cases_PCR_avg7, group=province, color = "#ff4d94"), size = 1.2 ) +
  geom_line( aes( date, num_casos_prueba_pcr_avg7, group=province, color = "#000000"), size = 0.7 ) +
  # scale_y_log10(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~province, scales = "free") + #, scales = "free"
  scale_color_identity(
    guide = "legend",
    labels = c("RENAVE","esCOVID19data"),
  ) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%m"
               # expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 33) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =15 ),
    panel.grid = element_line(size=0.4, color="#aaaaaa"),
    legend.position = "top"
  ) +
  labs(title = "Comparativa de bases de datos RENAVE y esCOVID19data. Casos diarios por PCR+",
       subtitle = paste0("Para esCOVID19data se muestra la media con ventana 7 días.  COVID-19 por provincias. España"),
       y = "casos diarios PCR+ media",
       x = "2020",
       colour ="bases de datos",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19" )
  )
dev.off()



png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_renave-iscii_vs_escovid19data_media_lastdays.png", sep = ""), width = 2700, height = 1650)
data_cases_sp_provinces %>% filter ( date > as.Date("2020-07-15") ) %>% # filter( ccaa=="Galicia") %>%
  ggplot() +
  # geom_line( aes( date, cases_accumulated, group=province, color = "#66a61e"), size = 2.0 ) +
  geom_line( aes( date, daily_cases_PCR_avg7, group=province, color = "#ff4d94"), size = 1.2 ) +
  geom_line( aes( date, num_casos_prueba_pcr_avg7, group=province, color = "#000000"), size = 0.7 ) +
  # scale_y_log10(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~province, scales = "free_y") + #, scales = "free"
  scale_color_identity(
    guide = "legend",
    labels = c("RENAVE","esCOVID19data"),
  ) +
  expand_limits(y = 0) +
  scale_x_date(date_breaks = "10 days",
               date_labels = "%d/%m"
               # expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 33) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =16 ),
    panel.grid = element_line(size=0.4, color="#aaaaaa"),
    legend.position = "top"
  ) +
  labs(title = "Comparativa de bases de datos RENAVE y esCOVID19data. Casos diarios por PCR+",
       subtitle = paste0("Media de ventana 7 días para la serie de esCOVID19data (datos de las CCAA). Por provincias. España"),
       y = "casos diarios PCR+ media",
       x = "2020",
       colour ="bases de datos",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19" )
  )
dev.off()

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_renave-iscii_vs_escovid19data_media-diff.png", sep = ""), width = 2700, height = 1650)
data_cases_sp_provinces %>%
  # filter( date > as.Date( ) ) %>%
  # filter( ccaa=="Galicia") %>% filter)
  ggplot() +
  # geom_line( aes( date, cases_accumulated, group=province, color = "#66a61e"), size = 2.0 ) +
  # geom_line( aes( date, daily_cases_PCR_avg7, group=province, color = "#ff4d94"), size = 1.2 ) +
  geom_line( aes( date, num_casos_prueba_pcr_avg7 - daily_cases_PCR_avg7, group=province), size = 1, color = "777777" ) +
  # scale_y_log10(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~province, scales = "free") + #, scales = "free"
  # scale_color_identity(
    # guide = "legend",
    # labels = c("RENAVE - esCOVID19data"),
  # ) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%m"
               # expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 33) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =15 ),
    panel.grid = element_line(size=0.4, color="#aaaaaa"),
    legend.position = "top"
  ) +
  labs(title = "Diferencia de media de casos diarios por PCR+: RENAVE menos esCOVID19data",
       subtitle = paste0("Media calculada con ventana de 7 días.  COVID-19 por provincias. España"),
       y = "casos diarios PCR+ media",
       x = "2020",
       # colour ="base de datos",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19" )
  )
dev.off()


png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_renave-iscii_vs_escovid19data_media-diff_lastdays.png", sep = ""), width = 2700, height = 1650)
data_cases_sp_provinces %>%
  filter( date > as.Date("2020-07-15") ) %>%
  # filter( ccaa=="Galicia") %>% filter)
  ggplot() +
  # geom_line( aes( date, cases_accumulated, group=province, color = "#66a61e"), size = 2.0 ) +
  # geom_line( aes( date, daily_cases_PCR_avg7, group=province, color = "#ff4d94"), size = 1.2 ) +
  geom_line( aes( date, num_casos_prueba_pcr_avg7 - daily_cases_PCR_avg7, group=province), size = 1, color = "777777" ) +
  # scale_y_log10(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~province, scales = "free") + #, scales = "free"
  # scale_color_identity(
  # guide = "legend",
  # labels = c("RENAVE - esCOVID19data"),
  # ) +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%d/%m"
               # expand = c(0,0)
  ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 33) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.text = element_text(size =15 ),
    panel.grid = element_line(size=0.4, color="#aaaaaa"),
    legend.position = "top"
  ) +
  labs(title = "Diferencia de media de casos diarios por PCR+: RENAVE menos esCOVID19data",
       subtitle = paste0("Media calculada con ventana de 7 días.  COVID-19 por provincias. España"),
       y = "casos diarios PCR+ media",
       x = "2020",
       # colour ="base de datos",
       caption = paste0( "@numeroteca Datos: https://github.com/montera34/escovid19data. Gráficos: lab.montera34.com/covid19" )
  )
dev.off()


