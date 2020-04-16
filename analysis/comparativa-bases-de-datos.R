# Comparativa de bases de datos -----------

# Este script toma datos generados en evolution_spain.R y evolution_spain_provinces.R, debes correrlos antes

# Load libraries -----------
library(tidyverse)

# import Instituto de Salud CIII ----
ciii_original <- read.delim("https://covid19.isciii.es/resources/serie_historica_acumulados.csv",sep = ",")  
ciii <- ciii_original %>% head(nrow(ciii_original) - 4) %>% ungroup()
ciii$date <- as.Date(ciii$FECHA, "%d/%m/%Y" )
names(ciii) <- c("region","fecha","cases_registered","hospitalized","UVI","deceassed","recovered","date")
ciii$region <- factor(ciii$region)
# translate iniciales
levels(ciii$region)
# rename comunidades autónomas
# "AN"         "AR"    "AS"       "CB"        "CE"     "CL"               "CM"                 "CN"          "CT"        "EX"           "GA"        "IB"        "MC"          "MD"      "ML"        "NC"                   "PV"       "RI"           "VC"
levels(ciii$region) <- c("Andalucía","Aragón", "Asturias",   "Cantabria","Ceuta", "Castilla y León","Castilla-La Mancha", "Canarias","Cataluña" , "Extremadura", "Galicia", "Baleares",   "Murcia","Madrid", "Melilla", "Navarra",  "País Vasco","La Rioja","C. Valenciana")  

# summarise providencialdata19 data by ccaa ------
resumen_pr_ca <- data_cases_sp_provinces %>% group_by(date,ccaa) %>% summarise( deceassed = sum(deceased), cases_accumulated = sum(cases_accumulated) )
# rename comunidades autónomas
levels(resumen_pr_ca$ccaa) <- c("Andalucía","Aragón", "Asturias", "Baleares", "Canarias", "Cantabria",  "Castilla-La Mancha","Castilla y León", "Cataluña", "Ceuta",
                                "C. Valenciana", "Extremadura","Galicia","Madrid", "Melilla", "Murcia", "Navarra",  "País Vasco","La Rioja")      

resumen_pr_ca$region <- resumen_pr_ca$ccaa

# Plots --------------

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_casos_bbdd_lineal.png", sep = ""),width = 1300,height = 900)
ggplot(NULL) +
  geom_line( data = resumen_pr_ca, aes( date-1, deceassed, group=region), color = "#e7298a", size = 1.5 ) +
  geom_line( data = data_all_export, aes( date-1, deceassed, group=region), color = "#66a61e", size = 1 ) +
  geom_line( data = ciii, aes( date, deceassed, group=region), color = "#000000", size = 0.7 ) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "3 day", 
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

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_muertes_bbdd_log.png", sep = ""), width = 1300, height = 900)
ggplot(NULL) +
  geom_line( data = resumen_pr_ca, aes( date-1, deceassed, group=region), color = "#e7298a", size = 1.5 ) +
  geom_line( data = data_all_export, aes( date-1, deceassed, group=region), color = "#66a61e", size = 1 ) +
  geom_line( data = ciii, aes( date, deceassed, group=region), color = "#000000", size = 0.7 ) +
  scale_y_log10(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "3 day", 
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

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_casos_bbdd_log.png", sep = ""), width = 1300, height = 900)
ggplot(NULL) +
  geom_line( data = resumen_pr_ca, aes( date-1, cases_accumulated, group=region), color = "#e7298a", size = 1.5 ) +
  geom_line( data = data_all_export, aes( date-1, cases_registered, group=region), color = "#66a61e", size = 1 ) +
  geom_line( data = ciii, aes( date, cases_registered, group=region), color = "#000000", size = 0.7 ) +
  scale_y_log10(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "3 day", 
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
       x = "fecha (Datadista y PD19 se resta un día)",
       caption = paste0( "@numeroteca. lab.montera34.com/covid19" )
  )
dev.off()

png(filename=paste("img/spain/provincias/comparativa/covid19_comparativa_casos_bbdd_lineal.png", sep = ""), width = 1300, height = 900)
ggplot(NULL) +
  geom_line( data = resumen_pr_ca, aes( date-1, cases_accumulated, group=region), color = "#e7298a", size = 1.5 ) +
  geom_line( data = data_all_export, aes( date-1, cases_registered, group=region), color = "#66a61e", size = 1 ) +
  geom_line( data = ciii, aes( date, cases_registered, group=region), color = "#000000", size = 0.7 ) +
  scale_y_continuous(labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  facet_wrap( ~region) +
  scale_x_date(date_breaks = "3 day", 
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
       x = "fecha (Datadista y PD19 se resta un día)",
       caption = paste0( "@numeroteca. lab.montera34.com/covid19" )
  )
dev.off()