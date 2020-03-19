# Compare countries

# You need to run the other country scripts to be able to load the data

# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping
library(plotly)

# Bind Spanish and Italian data
compare_countries <- rbind(data_all_export, data_i_cases)

# Create France dataframe

data_f_cases_to_bind <- data_all_export[-c(1:(nrow(data_all_export)-nrow(data_f_cases))),]
data_f_cases_to_bind[1:nrow(data_f_cases),1:13] <- NA
data_f_cases_to_bind$date <- data_f_cases$date
data_f_cases_to_bind$region <- data_f_cases$region
data_f_cases_to_bind$country <- "France"
data_f_cases_to_bind$cases_registered <- data_f_cases$cases_registered
data_f_cases_to_bind$cases_per_100000 <- data_f_cases$cases_per_100000

# Add France
compare_countries <- rbind(compare_countries, data_f_cases_to_bind)

compare_countries$cases_registered[1] + compare_countries$cases_registered[3]



# Cases compare -----------------
p <- compare_countries %>%
# compare_countries %>% # filter(region =="Lazio")  %>% #  filter(country !="Italy") %>%
  ggplot() +
  # geom_point(aes(date, cases_registered, color=country ), size= 0.7) +
  geom_line(aes(date, cases_registered, group=region, color=country ), size= 0.5) +
  # Scales
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "none"
  ) +
  labs(title = "Confirmed accumulated COVID-19 registed cases in Spain, Italy and France by region",
       subtitle = paste0("By region (log scale). ",period),
       y = "registered cases (log scale)",
       x = "date",
       caption = caption_en)

fig <- ggplotly(p)

# save to interactive/spain-italy-france_cases_regions-evolution.jtml
fig %>% 
  layout(annotations = 
           list(x = 1, y = -0.1, text = "lab.montera34.com/covid19 | Data: various official sources. Check website.", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="grey"))
  )


# Cases per 100.000-----------------
q <- compare_countries %>%
  ggplot() +
  # geom_point(aes(date, cases_registered, color=country ), size= 0.7) +
  geom_line(aes(date, cases_per_100000, group=region, color=country ), size= 0.5) +
  # Scales
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ),
                 limits = c(1,max(compare_countries$cases_per_100000))
                 ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "none"
  ) +
  labs(title = "Confirmed accumulated COVID-19 registered cases in Spain, Italy and France by region per 100.000 inhabitants",
       subtitle = paste0("By region ",period),
       y = "cases per 100.000 inhabitants (log scale)",
       x = "date",
       caption = caption_en)

figq <- ggplotly(q)

# save to spain-italy-france_cases_regions-evolution-by-100000-inhabitans.html
figq %>% 
  layout(annotations = 
           list(x = 1, y = -0.1, text = "lab.montera34.com/covid19 | Data: various official sources. Check website.", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="grey"))
  )


# plot cases ----------------
png(filename=paste("img/compare/covid19_casos-registrados-superpuesto-countries-regions-log.png", sep = ""),width = 1500,height = 700)
data_cases %>%
  ggplot() +
  # Spain
  geom_line(aes(date,value,group=CCAA), size= 0.7, color="orange" ) +
  # geom_point(aes(date,value ), size= 0.5, color="orange"  ) +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  value > 100), 
                  aes(date,value, label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
                  color="orange",
                  nudge_x = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # Italy
  geom_line( data= data_i_cases, aes(date,cases_registered, group=region), size= 0.7, color="blue", alpha = 0.5  ) +
  # geom_point( data= data_i_cases,aes(date,totale_casi), size = 0.5, color="blue", alpha = 0.3, opacity = 0.3 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date), cases_registered > 100 ), 
                  aes(date,cases_registered, 
                      label=paste(format(cases_registered, nsmall=1, big.mark="."), region)),
                  color="blue", 
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # France
  geom_line( data = data_f_cases, aes(date,cases_registered, group=region), size= 0.7, color= "darkgreen", alpha = 0.8  ) +
  # geom_point(data = data_f_cases, aes(date,cases), size = 0.5, color= "darkgreen", alpha = 0.8 ) +
  geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date), cases_registered > 100), 
                  aes(date,cases_registered, label=paste(format(cases_registered, nsmall=1, big.mark="."),region)),
                  color= "darkgreen",
                  nudge_x = 11.5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # Scales
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases$date), max(data_cases$date + 14))
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "none"
  ) +
  labs(title = "Confirmed accumulated COVID-19 registed cases in Spain, Italy and France by region",
       subtitle = paste0("By region (log scale). ",period),
       y = "registered cases (log scale)",
       x = "date",
       caption = caption_en)
dev.off()

# Cases. Superpuesto per 100.000 inhabitantes.
png(filename=paste("img/compare/covid19_casos-registrados-superpuesto-countries-regions-per-cienmil-log.png", sep = ""),width = 1500,height = 700)
data_cases %>%
  ggplot() +
  # Spain
  geom_line(aes(date,per_cienmil,group=CCAA), size= 0.7, color="orange" ) +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  per_cienmil > 1), 
                  aes(date,per_cienmil, label=paste(format(per_cienmil, nsmall=1, big.mark=".", decimal.mark = ","),CCAA)),
                  color="orange",
                  nudge_x = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # Italy
  geom_line( data= data_i_cases, aes(date,cases_per_100000, group=region), size= 0.7, color="blue", alpha = 0.5  ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date), cases_per_100000 > 2 ), 
                  aes(date,cases_per_100000, 
                      label=paste(format(cases_per_100000, nsmall=1, big.mark=".", decimal.mark = ","), region)),
                  color="blue", 
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # France
  geom_line( data = data_f_cases, aes(date,cases_per_100000, group=region), size= 0.7, color= "darkgreen", alpha = 0.8  ) +
  geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date), cases_per_100000 > 2), 
                  aes(date,cases_per_100000, label=paste(format(cases_per_100000, nsmall=1, big.mark=".", decimal.mark = ","),region)),
                  color= "darkgreen",
                  nudge_x = 10, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # Scales
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ),
                 limits = c(0.5,max(data_i_cases$cases_per_100000)) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases$date), max(data_cases$date + 15))
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "none"
  ) +
  labs(title = "Confirmed accumulated COVID-19 registered cases in Italy, Spain and France by 100.000 inhabitants by region",
       subtitle = paste0("By region (log scale). ",period),
       y = "registered cases by 100.000 inhabitants (log scale)",
       x = "date",
       caption = caption_en)
dev.off()



# Deceassed plot PNG ---------------------

# Absolute
png(filename=paste("img/compare/covid19_fallecimientos-superpuesto-countries-regions-log.png", sep = ""),width = 1500,height = 700)
data_death %>% filter (CCAA != "Total") %>%
  ggplot() +
  # Spain
  geom_line(aes(date, death ,group=CCAA), size= 0.7, color="orange" ) +
  geom_text_repel(data=filter(data_death, date==max(data_death$date) & CCAA != "Total"), 
                  aes(date, death, label=paste(format(death, nsmall=1, big.mark="."),CCAA)),
                  color="orange",
                  nudge_x = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # Italy
  geom_line( data= data_i_cases, aes(date,deceassed, group=region), size= 0.7, color="blue", alpha = 0.5  ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date) ), 
                  aes(date,deceassed, 
                      label=paste(format(deceassed, nsmall=1, big.mark="."), region)),
                  color="blue", 
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
# Scales
scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
               minor_breaks = c(  seq(0.1 , 1, 0.1),seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ),
               # limits = c(0.1,20)
) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c( min(data_cases$date), max(data_cases$date + 8))
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "none"
  ) +
  labs(title = "Confirmed accumulated deceassed COVID-19 in Spain and Italy by region",
       subtitle = paste0("By region (log scale). ",period),
       y = "deceassed (log scale)",
       x = "date",
       caption = caption_en)
dev.off()

# Per 100.000
png(filename=paste("img/compare/covid19_fallecimientos-superpuesto-countries-regions-per-cienmil-log.png", sep = ""),width = 1500,height = 700)
data_death %>%
  ggplot() +
  # Spain
  geom_line(aes(date, death_per_cienmil ,group=CCAA), size= 0.7, color="orange" ) +
  geom_text_repel(data=filter(data_death, date==max(data_death$date)), 
                  aes(date, death_per_cienmil, label=paste(format(death_per_cienmil, nsmall=1, big.mark="."),CCAA)),
                  color="orange",
                  nudge_x = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # Italy
  geom_line( data= data_i_cases, aes(date,deceassed_per_100000, group=region), size= 0.7, color="blue", alpha = 0.5  ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date) ), 
                  aes(date,deceassed_per_100000, 
                      label=paste(format(deceassed_per_100000, nsmall=1, big.mark="."), region)),
                  color="blue", 
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # France
  # geom_line( data = data_f_cases, aes(date,cases_per_100000, group=region), size= 0.7, color= "darkgreen", alpha = 0.8  ) +
  # geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date), cases_per_100000 > 2), 
  #                 aes(date,cases_per_100000, label=paste(format(cases_per_100000, nsmall=1, big.mark="."),region)),
  #                 color= "darkgreen",
  #                 nudge_x = 10, # adjust the starting y position of the text label
  #                 size=5,
  #                 hjust=0,
  #                 family = "Roboto Condensed",
  #                 direction="y",
  #                 segment.size = 0.1,
  #                 segment.color="#777777"
  # ) +
  # Scales
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 minor_breaks = c(  seq(0.1 , 1, 0.1),seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) )
                 # limits = c(min(data_i_cases$deceassed_per_100000),max(data_i_cases$deceassed_per_100000))
                 ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d",
               limits=c(  min(data_i_cases$date), max(data_cases$date + 8))
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "none"
  ) +
  labs(title = "Confirmed accumulated deceassed COVID-19 in Spain and Italy by 100.000 inhabitants by region",
       subtitle = paste0("By region (log scale). ",period),
       y = "deceassed by 100.000 inhabitants (log scale)",
       x = "date",
       caption = caption_en)
dev.off()

# deceassed total log ---------------------
m <- compare_countries %>%
  ggplot() +
  geom_line(aes(date, deceassed, group=region, color=country ), size= 0.5) +
  # Scales
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 breaks = c(0.1,1,10),
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ),
                 limits = c(1,max(compare_countries$deceassed))
  ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "none"
  ) +
  labs(title = "Confirmed accumulated deceaassed by COVID-19 in Spain and Italy by region",
       subtitle = paste0("By region (log scale). ",period),
       y = "deceassed (log scale)",
       x = "date",
       caption = caption_en)

figm <- ggplotly(m)

# save to interactive/spain-italy-france-deceassed_regions-evolution_absolute_log.html
figm %>% 
  layout(annotations = 
           list(x = 1, y = -0.1, text = "lab.montera34.com/covid19 | Data: various official sources. Check website.", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="grey"))
  )


# Deceassed per 100.000 interactive-----------------
t <- compare_countries %>%
  ggplot() +
  geom_line(aes(date, deceassed_per_100000, group=region, color=country ), size= 0.5) +
  # Scales
  scale_y_continuous( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE)
  ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "none"
  ) +
  labs(title = "Confirmed accumulated deceaassed by COVID-19 by 100.000 inhabitants in Spain and Italy by region",
       subtitle = paste0("By region (lineal scale). ",period),
       y = "deceassed by 100.000 inhabitants (log scale)",
       x = "date",
       caption = caption_en)

figt <- ggplotly(t)

# save to interactive/spain-italy-france-deceassed_regions-evolution_lineal.html
figt %>% 
  layout(annotations = 
           list(x = 1, y = -0.1, text = "lab.montera34.com/covid19 | Data: various official sources. Check website.", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="grey"))
  )

# deceassed per 100.000
r <- compare_countries %>%
  ggplot() +
  geom_line(aes(date, deceassed_per_100000, group=region, color=country ), size= 0.5) +
  # Scales
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 breaks = c(0.1,1,10),
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ),
                 limits = c(0.02,max(compare_countries$deceassed_per_100000))
                 ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "none"
  ) +
  labs(title = "Confirmed accumulated deceaassed by COVID-19 by 100.000 inhabitants in Spain and Italy by region",
       subtitle = paste0("By region (log scale). ",period),
       y = "deceassed by 100.000 inhabitants (log scale)",
       x = "date",
       caption = caption_en)

figr <- ggplotly(r)

# save to spain-italy-france-deceassed_regions-evolution_log.html
figr %>% 
  layout(annotations = 
           list(x = 1, y = -0.1, text = "lab.montera34.com/covid19 | Data: various official sources. Check website.", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="grey"))
  )
