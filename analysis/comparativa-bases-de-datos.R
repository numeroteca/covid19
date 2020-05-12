# Comparativa de bases de datos -----------

# Este script toma datos generados en evolution_spain.R y evolution_spain_provinces.R, debes ejecutarlos antes

# Load libraries -----------
library(tidyverse)

# import Instituto de Salud CIII ----
ciii_original <- read.delim("https://covid19.isciii.es/resources/serie_historica_acumulados.csv",sep = ",")  
write.csv(ciii_original, file = "data/original/spain/iscii_data.csv", row.names = FALSE)

ciii <- ciii_original %>% head(nrow(ciii_original) - 6) %>% #Cambia el número en función de las notas que incluya el csv original
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
    CCAA = CCAA %>% str_replace_all("RI", "La Rioja"),
    CCAA = CCAA %>% str_replace_all("VC", "C. Valenciana"),
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
  ) %>% mutate (
    cases_registered = ifelse( is.na(cases_registered), PCR + TestAc, cases_registered)
  )

write.csv(ciii, file = "data/output/spain/iscii_processed_data.csv", row.names = FALSE)


# summarise providencialdata19 data by ccaa ------
resumen_pr_ca <- data_cases_sp_provinces %>% group_by(date,ccaa) %>% summarise( deceassed = sum(deceased), cases_accumulated = sum(cases_accumulated) )
# rename comunidades autónomas
levels(resumen_pr_ca$ccaa) <- c("Andalucía","Aragón", "Asturias", "Baleares", "Canarias", "Cantabria",  "Castilla-La Mancha","Castilla y León", "Cataluña", "Ceuta",
                                "C. Valenciana", "Extremadura","Galicia","Madrid", "Melilla", "Murcia", "Navarra",  "País Vasco","La Rioja")      

resumen_pr_ca$region <- resumen_pr_ca$ccaa

# Plots --------------
png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_casos_bbdd_lineal.png", sep = ""),width = 2300,height = 1900)
ggplot(NULL) +
  geom_line( data = resumen_pr_ca, aes( date, deceassed, group=region), color = "#e7298a", size = 1.5 ) +
  geom_line( data = data_all_export, aes( date, deceassed, group=region), color = "#66a61e", size = 1 ) +
  geom_line( data = ciii, aes( date, deceassed, group=region), color = "#000000", size = 0.7 ) +
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
  labs(title = "Comparativa de bases de datos. Fallecimientos acumulados COVID-19 por comunidades autónomas. España",
       subtitle = paste0("Datadista (verde). Instituto de Salud Carlos III (negro). Providencialdata19 (rosa)."),
       y = "fallecimientos acumulados",
       x = "fecha (Datadista y PD19 se resta un día)",
       caption = paste0( "@numeroteca. lab.montera34.com/covid19" )
  )
dev.off()

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
       caption = paste0( "@numeroteca. lab.montera34.com/covid19" )
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
       caption = paste0( "@numeroteca. lab.montera34.com/covid19" )
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
       caption = paste0( "@numeroteca. lab.montera34.com/covid19" )
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
       caption = paste0( "@numeroteca. lab.montera34.com/covid19" )
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
       caption = paste0( "@numeroteca. lab.montera34.com/covid19" )
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
       caption = paste0( "@numeroteca. lab.montera34.com/covid19" )
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
       caption = paste0( "@numeroteca. lab.montera34.com/covid19",
       color = ""
       )
  )
dev.off()

