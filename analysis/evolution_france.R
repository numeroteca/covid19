# Analizar datos de Coronavirus COVID-19 en Francia por provincia

# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping


# Settings -------  
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption_f <- "Gráfico: montera34.com. Datos: OpenCOVID19-fr"
periodo_f <- "2020.03.04 - 04.12"

# COVID-19 in France-----------

# Load data ----
# data_f_cases_original <- read.delim("data/original/france/covid19.csv",sep = ",")  
# original source
# data_f_cases_original <- read.delim("https://www.data.gouv.fr/fr/datasets/r/fa9b8fc8-35d5-4e24-90eb-9abe586b0fa5",sep = ",")
# new source
# data_f_cases_original <- read.delim("https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv",sep = ",") %>% filter( granularite == "region")
# data_f_cases_original2 <- read.delim("data/original/france/covid_hospit.csv",sep = ";")
data_f2_cases_original <- read.delim("https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv",sep = ",")
# table(data_f_cases_original$source_nom)
# names(data_f_cases_original)
# Population
population_f_region <- read.delim("data/original/france/ccaa-poblacion.csv",sep = ",")

# Process data -------
# data_f_cases<- melt(data_f_cases_original, id.vars = "Date") 
# names(data_f_cases) <- c("date","region","cases_registered")
# data_f_cases$date <-  as.Date(data_f_cases$date)
# data_f_cases$cases_registered <- as.numeric(data_f_cases$cases_registered)
# levels(data_f_cases$region)
# population_f_region$Regio


# add population data ----
# data_f_cases <- merge( data_f_cases, select(population_f_region,Region,X1er.janvier.2020..p.), by.x = "region", by.y = "Region", all.x = TRUE)
# # rename variables
# names(data_f_cases) <- c("region","date","cases_registered","population")
# 
# # calculate values per 100.000
# data_f_cases$cases_per_100000 <- NA
# data_f_cases[!is.na(data_f_cases$population),]$cases_per_100000 <- round( data_f_cases[!is.na(data_f_cases$population),]$cases_registered / data_f_cases[!is.na(data_f_cases$population),]$population * 100000, digits = 2)
# 
# write.csv(data_f_cases, file = "data/output/france/covid19-cases-registered-region-france-by-day-accumulated.csv", row.names = FALSE)

# Process new data-------
# Convert facator to number id
# data_f_cases_original2$id <- as.numeric( data_f_cases_original2$dep )
# add population data
# TODO # data_f_cases_original2 <- merge( data_f_cases_original2, select(population_f_region,id,Region,X1er.janvier.2020..p.), by.x = "id", by.y = "id", all.x = TRUE)
# rename variables
# names(data_f_cases_original2) <- c("region","sexe","date","hosp","rea","rad","deceassed") #"region","population"
# data_f_cases_original2$date <-  as.Date(as.character(data_f_cases_original2$date))
# calculate values per 100.000
# data_f_cases_original2$cases_per_100000 <- NA
# data_f_cases_original2$deceassed_per_100000 <- round( data_f_cases_original2$deceassed / data_f_cases_original2$population * 100000, digits = 2)

# create temp dataframes to be able to plot all the   values in small multiples
# data_f_cases_original2_sm <-data_f_cases_original2
# data_f_cases_original2_sm$region_cp <- data_f_cases_original2$region


# data_f_cases_dc <- read.delim("data/output/france/covid19-deceassed-region-france-by-day-accumulated.csv",sep = ",")
#  new new france data
data_f2_cases <- data_f2_cases_original %>% filter(granularite == "region"& source_nom != "Santé publique France")
data_f2_cases$date <- as.Date(data_f2_cases$date)

# data_f2_cases <- filter(data_f2_cases, source_nom == "OpenCOVID19-fr" & date > "2020-03-26" | source_nom != "OpenCOVID19-fr" & date < "2020-03-27" )
data_f2_cases <- data_f2_cases %>% group_by(maille_nom,date) %>% 
  mutate( deceassed = max(deces),
          cases = max(cas_confirmes) )
names(data_f2_cases)

names(data_f2_cases)  <- c("date","granularite","region_code","region","cases_registered","deces","deces_ehpad","reanimation","hospitalises",
                           "gueris","depistes","source_nom","source_url","source_archive","source_type","deceassed", "cases"  )

# write.csv(data_f2_cases, file = "data/output/france/covid19-cases-registered-region-france-by-day-accumulated.csv", row.names = FALSE)


# Calculate daily deaths
data_f2_cases <- data_f2_cases %>% group_by(region) %>% arrange(date) %>%
  mutate( daily_deaths = deceassed - lag(deceassed),
          daily_deaths_inc = round((deceassed - lag(deceassed)) /lag(deceassed) * 100, digits = 1),
          daily_deaths_avg6 =  round( ( daily_deaths + lag(daily_deaths,1)+lag(daily_deaths,2)+lag(daily_deaths,3)+lag(daily_deaths,4)+lag(daily_deaths,5) ) / 6, digits = 1 ) # average of dayly deaths of 6 last days
          )
data_f2_cases_death <- data_f2_cases %>% group_by(date,region) %>% arrange(date) %>% 
  summarise( 
    # date = date,
    # region = region,
    daily_deaths = max(daily_deaths, na.rm = TRUE)
    ) %>% group_by(region) %>% arrange(date) %>% mutate( 
        daily_deaths_avg6 =  round( ( daily_deaths + lag(daily_deaths,1)+lag(daily_deaths,2)) / 3, digits = 1 ) # average of dayly deaths of 6 last days
    )

# create temp dataframes to be able to plot all the   values in small multiples
data_f2_cases_sm <-data_f2_cases %>% ungroup()
data_f2_cases_sm$region_cp <- data_f2_cases$region

# # 1. Cases ------------
# 
# # ----- Small multiple ------------
# # Escala lineal
# png(filename=paste("img/france/covid19_casos-registrados-por-region-lineal.png", sep = ""),width = 1200,height = 700)
# data_f2_cases %>%
#   ggplot() +
#   geom_line(data = data_f2_cases_sm %>% ungroup() %>% select(date,cases_registered,region_cp,-region),
#             aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
#   geom_line(aes(date,cases_registered,group=region) ) +
#   geom_point(aes(date,cases_registered,group=region), size = 0.5 ) +
#   facet_wrap( ~region) +
#   scale_x_date(date_breaks = "2 day", 
#                date_labels = "%d"
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     axis.text.x = element_text(size = 9)
#     # legend.position = "bottom"
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados en Francia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_f),
#        y = "casos registrados",
#        x = "fecha",
#        caption = caption_f)
# dev.off()
# 
# # Escala logarítmica
# png(filename=paste("img/france/covid19_casos-registrados-por-region-log.png", sep = ""),width = 1200,height = 700)
# data_f_cases %>%
#   ggplot() +
#   geom_line(data = select(data_f_cases_sm,date,cases_registered,region_cp,-region),
#             aes(date,cases_registered,group=region_cp), color="#CACACA" ) +
#   geom_line(aes(date,cases_registered,group=region) ) +
#   geom_point(aes(date,cases_registered,group=region), size = 0.5 ) +
#   scale_y_log10( minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000) ) ) +
#   facet_wrap( ~region) +
#   scale_x_date(date_breaks = "2 day", 
#                date_labels = "%d"
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     axis.text.x = element_text(size = 9)
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados en Francia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_f),
#        y = "casos registrados",
#        x = "fecha",
#        caption = caption_f)
# dev.off()
# 
# png(filename=paste("img/france/covid19_casos-registrados-por-region-per-cienmil-lineal.png", sep = ""),width = 1200,height = 700)
# data_f_cases %>%
#   ggplot() +
#   geom_line(data = select(data_f_cases_sm,date,cases_per_100000,region_cp,-region),
#             aes(date,cases_per_100000,group=region_cp), color="#CACACA" ) +
#   geom_line(aes(date,cases_per_100000,group=region) ) +
#   geom_point(aes(date,cases_per_100000,group=region), size = 0.5 ) +
#   facet_wrap( ~region) +
#   scale_x_date(date_breaks = "2 day",
#                date_labels = "%d"
#   ) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     axis.text.x = element_text(size = 9)
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Francia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_f),
#        y = "casos registrados por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_f)
# dev.off()
# 
# png(filename=paste("img/france/covid19_casos-registrados-por-region-per-cienmil-log.png", sep = ""),width = 1200,height = 700)
# data_f_cases %>% 
#   ggplot() +
#   geom_line(data = select(data_f_cases_sm,date,cases_per_100000,region_cp,-region),
#             aes(date,cases_per_100000,group=region_cp), color="#CACACA" ) +
#   geom_line(aes(date,cases_per_100000,group=region) ) +
#   geom_point(aes(date,cases_per_100000,group=region), size = 0.5 ) +
#   scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 3000, 100) )) +
#   facet_wrap( ~region) +
#   scale_x_date(date_breaks = "2 day",
#                date_labels = "%d"
#   ) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     axis.text.x = element_text(size = 9)
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Francia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_f),
#        y = "casos registrados por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_f)
# dev.off()
# 
# # ---------- Superpuesto ---------------
# png(filename=paste("img/france/covid19_casos-registrados-por-region-superpuesto-lineal.png", sep = ""),width = 1200,height = 700)
# data_f_cases %>%
#   ggplot() +
#   geom_line( aes(date,cases_registered, group=region, color=region), size= 1 ) +
#   geom_point(aes(date,cases_registered, color=region), size = 2 ) +
#   geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date)), 
#                   aes(date,cases_registered, color=region, label=paste(format(cases_registered, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   # hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   scale_x_date(date_breaks = "2 day", 
#                date_labels = "%d",
#                limits=c( min(data_f_cases$date), max(data_f_cases$date + 2.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados en Francia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_f),
#        y = "casos registrados",
#        x = "fecha",
#        caption = caption_f)
# dev.off()
# 
# png(filename=paste("img/france/covid19_casos-registrados-por-region-superpuesto-log.png", sep = ""),width = 1200,height = 700)
# data_f_cases %>%
#   ggplot() +
#   geom_line( aes(date,cases_registered, group=region, color=region), size= 1 ) +
#   geom_point(aes(date,cases_registered, color=region), size = 2 ) +
#   geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date)), 
#                   aes(date,cases_registered, color=region, label=paste(format(cases_registered, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
#                  # limits = c(0.95,12000),
#                  breaks = c(1,10,100,1000,12000),
#                  minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ) ) +
#   scale_x_date(date_breaks = "2 day", 
#                date_labels = "%d",
#                limits=c( min(data_f_cases$date), max(data_f_cases$date + 3.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados en Francia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_f),
#        y = "casos registrados",
#        x = "fecha",
#        caption = caption_f)
# dev.off()
# 
# png(filename=paste("img/france/covid19_casos-registrados-por-region-superpuesto-per-cienmil-lineal.png", sep = ""),width = 1200,height = 700)
# data_f_cases %>% 
#   ggplot() +
#   geom_line(aes(date,cases_per_100000,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,cases_per_100000, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date)),
#                   aes(date,cases_per_100000, color=region, label=paste(format(cases_per_100000, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#333333"
#   ) +
#   scale_x_date(date_breaks = "2 day",
#                date_labels = "%d",
#                limits=c( min(data_f_cases$date), max(data_f_cases$date + 3.5))
#   ) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Francia",
#        subtitle = paste0("Por región (escala lineal). ",periodo_f),
#        y = "casos registrados por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_f)
# dev.off()
# 
# 
# png(filename=paste("img/france/covid19_casos-registrados-por-region-superpuesto-per-cienmil-log.png", sep = ""),width = 1200,height = 700)
# data_f_cases %>% 
#   ggplot() +
#   geom_line(aes(date,cases_per_100000,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,cases_per_100000, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date)),
#                   aes(date,cases_per_100000, color=region, label=paste(format(cases_per_100000, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#333333"
#                   # xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2020-01-4"))
#   ) +
#   # scale_color_brewer(palette = "Dark2", type = "discrete") +
#   scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
#                  minor_breaks = c(  seq(0.1 , 1, 0.1), seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100) ) ) +
#   scale_x_date(date_breaks = "2 day",
#                date_labels = "%d",
#                limits=c( min(data_f_cases$date), max(data_f_cases$date + 3.5))
#   ) +
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de casos acumulados de COVID-19 registrados por 100.000 habitantes en Francia",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_f),
#        y = "casos registrados por 100.000 habitantes",
#        x = "fecha",
#        caption = caption_f)
# dev.off()
# 
# # 3. Deceassed -------------------
# 
# sm_fr <-as.data.frame( select(data_f2_cases_sm,date,deceassed,region_cp,-region, -region_code) )
# # sm_fr <- sm_fr[,2:4]
# 
# # / small multiple ----------
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-lineal.png", sep = ""),width = 1000,height = 700)
# # png(filename=paste("xxxxxxxxxx.png", sep = ""),width = 1000,height = 700)
#   data_f2_cases %>%
#     ggplot() +
#     geom_line(data = sm_fr, aes(date,deceassed,group=region_cp), color="#CACACA" ) +
#     geom_line(aes(date, deceassed, group=region) ) +
#     geom_point(aes(date,deceassed), size= 0.5 ) +
#     facet_wrap( ~region) +
#   scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
#   scale_x_date(date_breaks = "4 day", 
#                date_labels = "%d",
#                limits=c( min(data_f2_cases[!is.na(data_f2_cases$deces) > 0,]$date)+25, max(data_f2_cases$date))
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en France",
#        subtitle = paste0("Por región (escala lineal). ",periodo_f),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption_f)
# dev.off()
# 
# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-log.png", sep = ""),width = 1000,height = 700)
# data_f2_cases %>% 
#   ggplot() +
#   geom_line(data = sm_fr, aes(date,deceassed,group=region_cp), color="#CACACA" ) +
#   geom_line(aes(date, deceassed, group=region) ) +
#   geom_point(aes(date,deceassed), size= 0.5 ) +
#   facet_wrap( ~region) +
#   coord_cartesian( ylim = c(1, max(data_f2_cases[!is.na(data_f2_cases$deceassed),]$deceassed) * 1.05) ) +
#   scale_y_log10( 
#     # limits = c(1,max(data_f_cases_original2$deceassed)),
#     labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
#     minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100)) ) +
#   scale_x_date(date_breaks = "3 day", 
#                date_labels = "%d",
#                limits=c( min(data_f2_cases[!is.na(data_f2_cases$deces) > 0,]$date)+25, max(data_f2_cases$date))
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en France",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_f),
#        y = "fallecidos",
#        x = "fecha",
#        caption = caption_f)
# dev.off()

# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
# data_f_cases_original2 %>% 
#   ggplot() +
#   geom_line(data = select(data_f_cases_original2_sm,date,deceassed_per_100000,region_cp,-region),
#             aes(date,deceassed_per_100000,group=region_cp), color="#CACACA" ) +
#   geom_line(aes(date, deceassed_per_100000, group=region) ) +
#   geom_point(aes(date,deceassed_per_100000), size= 0.5 ) +
#   facet_wrap( ~region) +
#   # coord_cartesian( ylim = c( 0 , max(data_f_cases_original2$deceassed_per_100000))  )+
#   scale_y_log10( 
#     # limits = c( 0 , max(data_i_cases$deceassed_per_100000) ),
#     labels= function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
#     minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100)) ) +
#   scale_x_date(date_breaks = "2 day", 
#                date_labels = "%d",
#                limits=c( min(data_i_cases$date), max(data_i_cases$date)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados  por 100.000 habitantes en France",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_f),
#        y = "fallecidos por 100.000 habitantes ",
#        x = "fecha",
#        caption = caption_f)
# dev.off()

# / Fallecimientos superpuestos ----------
png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-superpuesto-lineal.png", sep = ""),width = 1000,height = 700)
data_f2_cases %>% 
  ggplot() +
  geom_line(aes(date, deceassed, group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_f2_cases, date==max(data_f2_cases$date)), 
                  aes(date, deceassed, color=region, 
                    label=paste0(format(deceassed, nsmall=1, big.mark="."),region, " (+", daily_deaths,", +", daily_deaths_inc ,"%)")),
                  nudge_x = 2, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_f2_cases$date) + 40, max(data_f2_cases$date + 14)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en France",
       subtitle = paste0("Por región (escala lineal). ",periodo_f),
       y = "fallecidos",
       x = "fecha",
       caption = caption_f)
dev.off()

png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-superpuesto-log.png", sep = ""),width = 1000,height = 700)
data_f2_cases %>% # filter( source_type != "opencovid19-fr" | (source_type == "opencovid19-fr" & date > as.Date("2020-03-26")) ) %>%
  ggplot() +
  geom_line(aes(date, deceassed, group=region, color=region), size= 1 ) +
  geom_point(aes(date,deceassed, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_f2_cases, date==max(data_f2_cases$date)), 
                  aes(date, deceassed, color=region, 
                      label=paste0( format( as.integer(deceassed), nsmall=1, big.mark="." ), " ", region, " (+", daily_deaths,", +", daily_deaths_inc ,"%)")),
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  scale_y_log10( 
    limits = c(1,max(data_f2_cases$deces)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)) ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_f2_cases$date) + 40, max(data_f2_cases$date + 14)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de fallecimientos acumulados por COVID-19 registrados en France",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_f),
       y = "fallecidos",
       x = "fecha",
       caption = caption_f)
dev.off()

# png(filename=paste("img/france/covid19_fallecimientos-registrados-por-region-superpuesto-per-cienmil-log.png", sep = ""),width = 1000,height = 700)
# data_i_cases %>% 
#   ggplot() +
#   geom_line(aes(date,deceassed_per_100000,group=region, color=region), size= 1 ) +
#   geom_point(aes(date,deceassed_per_100000, color=region), size= 1.5 ) +
#   geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date),  region != "Total"), 
#                   aes(date,deceassed_per_100000, color=region, label=paste(format(deceassed_per_100000, nsmall=1, big.mark="."),region)),
#                   nudge_x = 3, # adjust the starting y position of the text label
#                   size=5,
#                   hjust=0,
#                   family = "Roboto Condensed",
#                   direction="y",
#                   segment.size = 0.1,
#                   segment.color="#777777"
#   ) +
#   scale_y_log10( 
#     limits = c(0.1,max(data_i_cases$deceassed_per_100000)),
#     labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
#     minor_breaks = c(seq(0.1 , 1, 0.1),seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100)) ) +
#   scale_x_date(date_breaks = "2 day", 
#                date_labels = "%d",
#                limits=c( min(data_i_cases$date), max(data_i_cases$date +3.5)) 
#   ) + 
#   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.x = element_line(color = "#000000"),
#     legend.position = "none"
#   ) +
#   labs(title = "Número de fallecimientos acumulados por COVID-19 registrados por 100.000 habitantes en France",
#        subtitle = paste0("Por región (escala logarítmica). ",periodo_f),
#        y = "fallecidospor 100.000 habitantes",
#        x = "fecha",
#        caption = caption_f)
# dev.off()


# Daily deaths log average --------
png(filename=paste("img/france/covid19_muertes-por-dia-region-superpuesto-log.png", sep = ""),width = 1100,height = 800)
data_f2_cases_death %>% 
  ggplot() +
  geom_line(aes(date, daily_deaths,group=region, color=region), size= 1 ) +
  geom_point(aes(date, daily_deaths, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_f2_cases_death, date==max(data_f2_cases_death$date),  region != "Total"), 
                  aes(date, daily_deaths, color=region, label=paste(format( daily_deaths, nsmall=1, big.mark="."),region)),
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  coord_cartesian(
    ylim=c(1, max(data_f2_cases[!is.na(data_f2_cases$daily_deaths),]$daily_deaths)*1.05 )
  ) +
  scale_y_log10( 
    breaks = c(0,1,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
    expand = c(0,0.1)
  ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_f2_cases$date + 40), max(data_f2_cases$date +9)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de muertes por COVID-19 registradas por día en Francia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_f),
       y = "fallecidos por día",
       x = "fecha",
       caption = caption_f)
dev.off()

png(filename=paste("img/france/covid19_muertes-por-dia-region-superpuesto-log_media.png", sep = ""),width = 1100,height = 800)
data_f2_cases_death %>% 
  ggplot() +
  geom_smooth(aes(date, daily_deaths_avg6,group=region, color=region), size= 1, se = FALSE ) +
  geom_point(aes(date, daily_deaths, color=region), size= 1.5 ) +
  geom_text_repel(data=filter( data_f2_cases_death, date==max(data_f2_cases_death$date),  region != "Total"), 
                  aes(date, daily_deaths_avg6, color=region, label=paste(format( daily_deaths_avg6, nsmall=1, big.mark="."),region)),
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  coord_cartesian(
    ylim=c(1, max(data_f2_cases[!is.na(data_f2_cases$daily_deaths),]$daily_deaths)*1.05 )
  ) +
  scale_y_log10( 
    breaks = c(0,1,5,10,20,50,100,200,500,1000,2000,5000 ),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(seq(1 , 10, 1),seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000 , 10000, 1000)),
    expand = c(0,0.1)
  ) +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d",
               limits=c( min(data_f2_cases$date + 40), max(data_f2_cases$date +9)),
               expand = c(0,0) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Media de muertes por día en los 3 días anteriores (último inclusive) por COVID-19 Francia",
       subtitle = paste0("Por región (escala logarítmica). ",periodo_f),
       y = "fallecidos por día",
       x = "fecha",
       caption = caption_f)
dev.off()
