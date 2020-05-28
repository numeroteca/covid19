# Euskadi charts

# Load libraries------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping
library(readxl)

# Load data
download.file("https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/datos-asistenciales.xlsx", 
              "data/original/spain/euskadi/datos-asistenciales.xlsx")

# Settings -------
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption_provincia <- "Gráfico: @numeroteca (lab.montera34.com/covid19) | Datos: Open Data Euskadi"

# Set colors ---------
# extends color paletter
library(RColorBrewer)
# creates extended color palette https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
colourCount <- length(unique(data_cases_sp_provinces$ccaa))
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
euskadi_total <- merge( euskadi_total, euskadi_fallecidos %>% ungroup() %>% select(-hospital,-date,-provincia), by.x="dunique", by.y="dunique" )
euskadi_total <- merge( euskadi_total, euskadi_altas %>% ungroup() %>% select(-hospital,-date,-provincia), by.x="dunique", by.y="dunique" ) %>% select (-dunique)
euskadi_total <- euskadi_total %>% mutate(
  total_ingresados = hospitalizados+uci+fallecidos_cum+altas_cum
)  %>% arrange(date) %>% group_by(hospital) %>% mutate(
  daily_totat_ingresados = total_ingresados - lag(total_ingresados),
  daily_deceassed = fallecidos_cum - lag(fallecidos_cum),
  daily_deceassed_avg =  round( ( daily_deceassed + lag(daily_deceassed,1 ) + lag(daily_deceassed,2 ) + lag(daily_deceassed,3 )+ lag(daily_deceassed,4 ) + lag(daily_deceassed,5 ) +
                                    lag(daily_deceassed,6 ) ) / 7, digits=1)
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
               limits=c( min(euskadi_hosp$date), max(euskadi_hosp$date +20)),
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
               date_labels = "%d/%m",
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
       x = "fecha",
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
    date_labels = "%d/%m",
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
       x = "fecha",
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
                label=paste0(format( round(hospitalizados/total_ingresados*100/2, digits = 0), nsmall=0, big.mark="."),"% (",hospitalizados,")")),
            color="#5b5bbb",
            size=4,
            hjust=0,
            family = "Roboto Condensed"
  ) +
  geom_text(data=filter( euskadi_total, date==max(euskadi_total$date) & fallecidos_cum > 1),
            aes(date + 1, (hospitalizados+fallecidos_cum)/total_ingresados*100,
                label=paste0(format( round(fallecidos_cum/total_ingresados*100+4, digits = 0), nsmall=0, big.mark="."),"% (",format(fallecidos_cum, big.mark="."),")")),
            color="#000000",
            size=4,
            hjust=0,
            family = "Roboto Condensed"
  ) +
  geom_text(data=filter( euskadi_total, date==max(euskadi_total$date) ),
            aes(date + 1, (hospitalizados+uci+fallecidos_cum)/total_ingresados*100 + ((hospitalizados+uci+fallecidos_cum+altas_cum)/total_ingresados*100)/2,
                label=paste0(format( round(altas_cum/total_ingresados*100, digits = 0), nsmall=0, big.mark="."),"% (",format(altas_cum, big.mark="."),")")),
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
    date_labels = "%d/%m",
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
  labs(title = paste0("Evolución de ingresados en hospitales por COVID-19 en Euskadi" ),
       subtitle = paste0("Por hospitales en porcentaje sobre total de ingresados hasta la fecha (total personas entre paréntesis). ",period_eus),
       y = "hospitalizados",
       x = "fecha",
       fill = "",
       caption = caption_provincia)
dev.off()

# daily data ---------
png(filename=paste0("img/spain/euskadi/covid19_uci-dia-pais-vasco.png", sep = ""),width = 1300,height = 700)
euskadi_total %>%
  ggplot() +
  geom_col( aes(date,uci), size=1,color="red") +
  facet_wrap( ~hospital) + #, scales = "free_x"
  scale_y_continuous( 
    # limits = c(0,100),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    # date_breaks = "3 day", 
    date_labels = "%d/%m",
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
       x = "fecha",
       fill = "",
       caption = caption_provincia)
dev.off()


# fallecidos
png(filename=paste0("img/spain/euskadi/covid19_muertes-dia-pais-vasco.png", sep = ""),width = 1300,height = 700)
euskadi_total %>%
  ggplot() +
  geom_col( aes(date,daily_deceassed), size=0.5, fill="grey") +
  geom_line( aes(date,daily_deceassed_avg, group=hospital), size=1) +
  facet_wrap( ~hospital) + #, scales = "free_x"
  scale_y_continuous( 
    # limits = c(0,100),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    # date_breaks = "3 day", 
    date_labels = "%d/%m",
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
       x = "fecha",
       fill = "",
       caption = caption_provincia)
dev.off()
