# Compare countries

# You need to run the other country scripts to be able to load the data

# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping

# Bind Spanish and Italian data
compare_countries <- rbind(data_all_export, data_i_cases)

# Create France dataframe

data_f_cases_to_bind <- data_all_export[-c(1:(nrow(data_all_export)-nrow(data_f_cases))),]
data_f_cases_to_bind[1:nrow(data_f_cases),1:13] <- NA
data_f_cases_to_bind$date <- data_f_cases$date
data_f_cases_to_bind$region <- data_f_cases$region
data_f_cases_to_bind$country <- "France"
data_f_cases_to_bind$cases_registered <- data_f_cases$cases_registered

# Add France
compare_countries <- rbind(compare_countries, data_f_cases_to_bind)

compare_countries$cases_registered[1] + compare_countries$cases_registered[3]

# png(filename=paste("img/compare/covid19_casos-registrados-superpuesto-countries-regions-log_2.png", sep = ""),width = 1500,height = 700)
p <- compare_countries %>%
# compare_countries %>% # filter(region =="Lazio")  %>% #  filter(country !="Italy") %>%
  ggplot() +
  # geom_point(aes(date, cases_registered), size= 0.7) +
  geom_line(aes(date, cases_registered, group=region, color=country ), size= 0.7) +
  # geom_text_repel( data=filter( data_cases, date==max(data_cases$date),  value > 100), 
  #                 aes(date,value, color=country, label=paste(format(cases_registered, nsmall=1, big.mark="."),region)),
  #                 color="orange",
  #                 nudge_x = 5, # adjust the starting y position of the text label
  #                 size=5,
  #                 hjust=0,
  #                 family = "Roboto Condensed",
  #                 direction="y",
  #                 segment.size = 0.1,
  #                 segment.color="#777777"
  # ) +
  # Scales
  scale_y_log10( labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ) ) +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d"
               # limits=c( min(data_cases$date), max(data_cases$date + 12)) 
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
# dev.off()
library(plotly)

fig <- ggplotly(p)

fig %>% 
  layout(annotations = 
           list(x = 1, y = -0.1, text = "lab.montera34.com/covid19 | Data: various official sources. Check website.", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="grey"))
  )



png(filename=paste("img/compare/covid19_casos-registrados-superpuesto-countries-regions-log.png", sep = ""),width = 1500,height = 700)
p <- data_cases %>%
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
  geom_line( data= data_i_cases, aes(date,totale_casi, group=denominazione_regione), size= 0.7, color="blue", alpha = 0.5  ) +
  # geom_point( data= data_i_cases,aes(date,totale_casi), size = 0.5, color="blue", alpha = 0.3, opacity = 0.3 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date), totale_casi > 100 ), 
                  aes(date,totale_casi, 
                      label=paste(format(totale_casi, nsmall=1, big.mark="."),denominazione_regione)),
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
  geom_line( data = data_f_cases, aes(date,cases, group=region), size= 0.7, color= "darkgreen", alpha = 0.8  ) +
  # geom_point(data = data_f_cases, aes(date,cases), size = 0.5, color= "darkgreen", alpha = 0.8 ) +
  geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date), cases > 100), 
                  aes(date,cases, label=paste(format(cases, nsmall=1, big.mark="."),region)),
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
               date_labels = "%d"
               # limits=c( min(data_cases$date), max(data_cases$date + 12)) 
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


