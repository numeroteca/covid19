download.file("https://analisi.transparenciacatalunya.cat/api/views/jj6z-iyrp/rows.csv?accessType=DOWNLOAD&sorting=true", 
              "data/original/spain/catalunya/Registre_de_casos_de_COVID-19_realitzats_a_Catalunya._Segregaci__per_sexe_i_municipi.csv")
catalunya <-  read.delim("data/original/spain/catalunya/Registre_de_casos_de_COVID-19_realitzats_a_Catalunya._Segregaci__per_sexe_i_municipi.csv",sep = ",")

period_cat <- "Actualizado: 2020-09-03"
caption_provincia <- "Gráfico: @numeroteca (lab.montera34.com/covid19/cat.html) | Datos: Transparencia Catalunya"

# creates date variable
catalunya$date <- as.Date(catalunya$TipusCasData, "%d/%m/%Y")
# extracts first charatcter of zipcode to select province code
# catalunya$provincia_code <- ifelse(catalunya$MunicipiCodi < 10000, paste0(substr(as.character(catalunya$MunicipiCodi),1,1)), substr(as.character(catalunya$MunicipiCodi),1,2))     

# iterates through data to count positives
catalunya_new <- catalunya %>% group_by(date, MunicipiDescripcio,TipusCasDescripcio) %>% arrange(date) %>% 
  # filter( TipusCasDescripcio == "Positiu PCR" | TipusCasDescripcio == "Positiu per Test Ràpid" ) %>%
  # catalunya_new <- catalunya %>% group_by(date,provincia_code) %>% arrange(date) %>% filter( ResultatCovidDescripcio == "Positiu" |  ResultatCovidDescripcio == "Probable" ) %>%
  summarise ( 
    by_day = sum(NumCasos)
  )

# converts to wide format the different variables of positive or suspect cases with spread
municipios <- catalunya_new %>% spread(TipusCasDescripcio,by_day) %>% 
  rename(
    daily_cases_PCR = "Positiu PCR", 
    TestAc_day = "Positiu per Test Ràpid", 
    sospechosos_day = "Sospitós"
    ) %>% 
  # groups by province to calculate
  group_by(MunicipiDescripcio) %>% arrange(date) %>%
  # calculates de accumulated values
  mutate ( 
    cases_accumulated_PCR = cumsum(replace_na(daily_cases_PCR, 0)), #replace NA values with 0 to make cumsum work
    TestAc_cum = cumsum(replace_na(TestAc_day, 0)),
    sospechosos_cum = cumsum(replace_na(sospechosos_day, 0)),
    daily_cases_PCR_avg7 =  round( ( daily_cases_PCR + lag(daily_cases_PCR,1)+lag(daily_cases_PCR,2)+
                                       lag(daily_cases_PCR,3)+lag(daily_cases_PCR,4) +lag(daily_cases_PCR,5) +lag(daily_cases_PCR,6) ) / 7, digits = 1 ),  # average of dayly deaths of 7 last days
    
    
  )

pcr_avg7 <- municipios %>% # filter(date >= as.Date("2020-05-11")) %>%  
  group_by(MunicipiDescripcio) %>%
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
    daily_cases_PCR_avg7_complete = round( zoo::rollmeanr(serie, 7, na.pad = T), digits = 1))  #%>%
  # select(date, ccaa, province, daily_cases_PCR, dif_casos, daily_cases_PCR_avg7, daily_cases_PCR,fechas_dif, serie, daily_cases_PCR_avg7_complete )


municipios <- merge(
  municipios %>% mutate ( dunique = paste0( date, MunicipiDescripcio) ),
  pcr_avg7 %>% mutate ( dunique = paste0( date, MunicipiDescripcio) ) %>% ungroup() %>% select (dunique,daily_cases_PCR_avg7_complete),
  by.x = "dunique", by.y = "dunique", all= TRUE
) %>% mutate (
  daily_cases_PCR_avg7 = daily_cases_PCR_avg7_complete
) %>% select (-daily_cases_PCR_avg7_complete,-dunique)


# Set filter date
filter_date <- as.Date("2020-08-31")
municipios  <- municipios %>% filter( !( date > filter_date-5 ) )

# set number of days
limite <- 6
municipios_top <- municipios %>% top_n(1, date) %>% filter (daily_cases_PCR_avg7 > limite ) %>% select (MunicipiDescripcio)


# rejilla municpios ------
png(filename=paste0("img/spain/catalunya/covid19_municipios-catalunya_rejilla_not-free.png", sep = ""),width = 1200,height = 800)
municipios %>% 
  filter( MunicipiDescripcio %in% municipios_top$MunicipiDescripcio) %>%
  # filter( MunicipiDescripcio == "Barcelona") %>%
  ggplot() +
  geom_col(aes(date, daily_cases_PCR, fill = ""), width= 1 ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_PCR_avg7, group=MunicipiDescripcio, color= ""), size= 1.1 ) +
  scale_color_manual(values=c("#565656")  )+
  facet_wrap( ~MunicipiDescripcio) + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 1), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "20 days",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+80, max(municipios$date)+1),
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
  labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Cataluña" ),
       # labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Barcelona" ),
       subtitle = paste0("Con más de ", limite, " de media casos el último dia. Media: ventana de 7 días. ",period_cat),
       y = "casos por día",
       x = "fecha 2020",
       fill = "casos por día",
       colour = "media",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/catalunya/covid19_municipios-catalunya_rejilla.png", sep = ""),width = 1200,height = 800)
municipios %>%
  filter( MunicipiDescripcio %in% municipios_top$MunicipiDescripcio) %>%
  # filter( MunicipiDescripcio == "Barcelona") %>%
  ggplot() +
  geom_col(aes(date, daily_cases_PCR, fill = ""), width= 1 ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_PCR_avg7, group=MunicipiDescripcio, color= ""), size= 1.1 ) +
  scale_color_manual(values=c("#565656")  )+
  facet_wrap( ~MunicipiDescripcio, scales = "free_y") + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 1), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "20 days",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+80, max(municipios$date)+1),
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
  labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Cataluña" ),
  # labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Barcelona" ),
       subtitle = paste0("Con más de ", limite, " de media casos el último dia. Media: ventana de 7 días. ",period_cat),
       y = "casos por día",
       x = "fecha 2020",
       fill = "casos por día",
       colour = "media",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/catalunya/covid19_municipios-catalunya_rejilla_periodo-completo.png", sep = ""),width = 1200,height = 800)
municipios %>%
  filter( MunicipiDescripcio %in% municipios_top$MunicipiDescripcio) %>%
  # filter( MunicipiDescripcio == "Barcelona") %>%
  ggplot() +
  geom_col(aes(date, daily_cases_PCR, fill = ""), width= 1 ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_PCR_avg7, group=MunicipiDescripcio, color= ""), size= 1.1 ) +
  scale_color_manual(values=c("#565656")  )+
  facet_wrap( ~MunicipiDescripcio, scales = "free_y") + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 1), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%m",
    limits=c( min(municipios$date)+0, max(municipios$date)+1),
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
  labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Cataluña" ),
       # labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Barcelona" ),
       subtitle = paste0("Con más de ", limite, " de media casos el último dia. Media: ventana de 7 días. ",period_cat),
       y = "casos por día",
       x = "fecha 2020",
       fill = "casos por día",
       colour = "media",
       caption = caption_provincia)
dev.off()
  
png(filename=paste0("img/spain/catalunya/covid19_municipios-catalunya_rejilla_periodo-completo_not-free.png", sep = ""),width = 1200,height = 800)
municipios %>%
  filter( MunicipiDescripcio %in% municipios_top$MunicipiDescripcio) %>%
  # filter( MunicipiDescripcio == "Barcelona") %>%
  ggplot() +
  geom_col(aes(date, daily_cases_PCR, fill = ""), width= 1 ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_PCR_avg7, group=MunicipiDescripcio, color= ""), size= 1.1 ) +
  scale_color_manual(values=c("#565656")  )+
  facet_wrap( ~MunicipiDescripcio) + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 1), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%m",
    limits=c( min(municipios$date)+0, max(municipios$date)+1),
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
  labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Cataluña" ),
       # labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Barcelona" ),
       subtitle = paste0("Con más de ", limite, " de media casos el último dia. Media: ventana de 7 días. ",period_cat),
       y = "casos por día",
       x = "fecha 2020",
       fill = "casos por día",
       colour = "media",
       caption = caption_provincia)
dev.off()


png(filename=paste0("img/spain/catalunya/covid19_casos-calafell.png", sep = ""),width = 900,height = 400)
municipios %>%
  # filter( MunicipiDescripcio %in% municipios_top$MunicipiDescripcio) %>%
  filter( MunicipiDescripcio == "Calafell") %>%
  ggplot() +
  geom_col(aes(date, daily_cases_PCR, fill = ""), width= 1 ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_PCR_avg7, group=MunicipiDescripcio, color= ""), size= 1.1 ) +
  scale_color_manual(values=c("#565656")  )+
  facet_wrap( ~MunicipiDescripcio, scales = "free_y") + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 1), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "20 days",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+0, max(municipios$date)+1),
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
  # labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Cataluña" ),
       labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Calafell" ),
       subtitle = paste0("Con más de ", limite, " de media casos el último dia. Media: ventana de 7 días. ",period_cat),
       y = "casos por día",
       x = "fecha 2020",
       fill = "casos por día",
       colour = "media",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/catalunya/covid19_casos-vendrell.png", sep = ""),width = 900,height = 400)
municipios %>%
  # filter( MunicipiDescripcio %in% municipios_top$MunicipiDescripcio) %>%
  filter( MunicipiDescripcio == "El Vendrell") %>%
  ggplot() +
  geom_col(aes(date, daily_cases_PCR, fill = ""), width= 1 ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_PCR_avg7, group=MunicipiDescripcio, color= ""), size= 1.1 ) +
  scale_color_manual(values=c("#565656")  )+
  facet_wrap( ~MunicipiDescripcio, scales = "free_y") + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 1), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "20 days",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+0, max(municipios$date)+1),
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
  # labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Cataluña" ),
  labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en El Vendrell" ),
       subtitle = paste0("Con más de ", limite, " de media casos el último dia. Media: ventana de 7 días. ",period_cat),
       y = "casos por día",
       x = "fecha 2020",
       fill = "casos por día",
       colour = "media",
       caption = caption_provincia)
dev.off()

png(filename=paste0("img/spain/catalunya/covid19_casos-barcelona.png", sep = ""),width = 900,height = 400)
municipios %>%
  # filter( MunicipiDescripcio %in% municipios_top$MunicipiDescripcio) %>%
  filter( MunicipiDescripcio == "Barcelona") %>%
  ggplot() +
  geom_col(aes(date, daily_cases_PCR, fill = ""), width= 1 ) +
  scale_fill_manual(values=c("#AAAAAA")  )+
  geom_line(aes(date, daily_cases_PCR_avg7, group=MunicipiDescripcio, color= ""), size= 1.1 ) +
  scale_color_manual(values=c("#565656")  )+
  facet_wrap( ~MunicipiDescripcio, scales = "free_y") + #, scales = "free_x"
  scale_y_continuous( labels=function(x) format(round(x, digits = 1), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(
    date_breaks = "20 days",
    date_labels = "%d/%m",
    limits=c( min(municipios$date)+0, max(municipios$date)+1),
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
  # labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Cataluña" ),
  labs(title = paste0("Casos PCR+ por COVID-19 por municipio por día en Barcelona" ),
       subtitle = paste0("Con más de ", limite, " de media casos el último dia. Media: ventana de 7 días. ",period_cat),
       y = "casos por día",
       x = "fecha 2020",
       fill = "casos por día",
       colour = "media",
       caption = caption_provincia)
dev.off()
