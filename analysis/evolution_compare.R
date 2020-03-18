# Compare countries

# You need t orun the other country scripts to be able to load the data


png(filename=paste("img/compare/covid19_casos-registrados-superpuesto-countries-regions-log.png", sep = ""),width = 1500,height = 700)
data_cases %>% 
  ggplot() +
  # Spain
  geom_line(aes(date,value,group=CCAA), size= 0.7, color="#fc8d62" ) +
  # geom_point(aes(date,value ), size= 0.5, color="#fc8d62"  ) +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  value > 100), 
                  aes(date,value, label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
                  color="#fc8d62",
                  nudge_x = 5, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # Italy
  geom_line( data= data_i_cases, aes(date,totale_casi, group=denominazione_regione), size= 0.7, color="#8da0cb", alpha = 0.8  ) +
  # geom_point( data= data_i_cases,aes(date,totale_casi), size = 0.5, color="#8da0cb", alpha = 0.3, opacity = 0.3 ) +
  geom_text_repel(data=filter( data_i_cases, date==max(data_i_cases$date), totale_casi > 100 ), 
                  aes(date,totale_casi, 
                      label=paste(format(totale_casi, nsmall=1, big.mark="."),denominazione_regione)),
                  color="#8da0cb", 
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=5,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.1,
                  segment.color="#777777"
  ) +
  # France
  geom_line( data = data_f_cases, aes(date,cases, group=region), size= 0.7, color= "#66c2a5", alpha = 0.8  ) +
  # geom_point(data = data_f_cases, aes(date,cases), size = 0.5, color= "#66c2a5", alpha = 0.8 ) +
  geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date), cases > 100), 
                  aes(date,cases, label=paste(format(cases, nsmall=1, big.mark="."),region)),
                  color= "#66c2a5",
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
               limits=c( min(data_cases$date), max(data_cases$date + 12)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000")
    # legend.position = "none"
  ) +
  labs(title = "Confirmed accumulated COVID-19 registed cases in Spain, Italy and France",
       subtitle = paste0("By region (log scale). ",period),
       y = "registered cases (log scale)",
       x = "date",
       caption = caption_en)
dev.off()
