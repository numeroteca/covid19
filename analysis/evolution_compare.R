
# Compare countries

# You need to run the other country scripts at /analysis/ to be able to load the data first.

# Load libraries -----------
library(tidyverse)
library(reshape2)
library(ggrepel) # for geom_text_repel to prevent overlapping
library(plotly)

# # load data
# world_original <- read.delim("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",sep = ",")
# 
# # process data
# world <- melt(world_original, id.vars = c("Province.State", "Country.Region"))
# 
# world <- world %>% filter (variable != "Lat")
# world <- world %>% filter (variable != "Long")
# world$date <-  paste0(0,substr(world$variable,2,10))
# world$date <- as.Date(substr(world$date,2,10),"%m.%d.%y")
# world <- world %>% select(-variable)
# hubei <- world %>% filter( Province.State == "Hubei")
# names(hubei) <- c("region","country","deceassed","date")
# 
# hubei <- hubei %>% group_by(region) %>% arrange(date) %>% mutate(
#   daily_deaths = deceassed - lag(deceassed),
#   daily_deaths_avg6 =  round( ( daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) + lag(daily_deaths,4) + lag(daily_deaths,5) ) / 6, digits = 1 ) # average of dayly deaths of 6 last days
# )


# settings

period_comp <- "Updated: 2020-04-19"

# Bind Spanish and Italian data
names(data_all_export)
names(data_i_cases)
compare_countries <- rbind(
  data_all_export %>% select( -hospitalized, -hospitalized_per_100000), 
  data_i_cases %>% select(-daily_deaths)
)

# Create France dataframe
data_f_cases_to_bind <- data_all_export[-c(1:(nrow(data_all_export)-nrow(data_f_cases))),]
data_f_cases_to_bind[1:nrow(data_f_cases),1:13] <- NA
data_f_cases_to_bind$date <- data_f_cases$date
data_f_cases_to_bind$region <- data_f_cases$region
data_f_cases_to_bind$country <- "France"
data_f_cases_to_bind$cases_registered <- data_f_cases$cases_registered
data_f_cases_to_bind$cases_per_100000 <- data_f_cases$cases_per_100000
data_f_cases_to_bind <- data_f_cases %>% filter ( !is.na(date))
data_f_cases <- data_f_cases %>% filter ( !is.na(date))

# Add France
compare_countries <- rbind(compare_countries, data_f_cases_to_bind)
table(compare_countries$country)
write.csv(compare_countries, file = "data/output/covid19-countries-regions-compile.csv", row.names = FALSE)

# compare_countries$cases_registered[1] + compare_countries$cases_registered[3]

# 1. Print plots ---------------
# plot cases ----------------
png(filename=paste("img/compare/covid19_casos-registrados-superpuesto-countries-regions-log.png", sep = ""),width = 1500,height = 700)
data_cases %>%
  ggplot() +
  # Spain
  geom_line(aes(date,value,group=CCAA), size= 0.8, color="orange" ) +
  # geom_point(aes(date,value ), size= 0.5, color="orange"  ) +
  geom_text_repel(data=filter( data_cases, date==max(data_cases$date),  value > 100), 
                  aes(date,value, label=paste(format(value, nsmall=1, big.mark="."),CCAA)),
                  color="orange",
                  nudge_x = 6, # adjust the starting y position of the text label
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
  geom_text_repel(data=filter( data_f_cases, date==max(data_f_cases$date) & cases_registered > 100 & !is.na(cases_registered) ), 
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
               limits=c( min(data_cases$date), max(data_cases$date + 17))
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
               minor_breaks = c(  seq(0.1 , 1, 0.1),seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ),
               limits = c(0.1,max(data_i_cases$deceassed_per_100000))
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


# setting 0 day. Cumulative deaths-------------

# China Wuhan
world2_original <- read.delim("https://raw.githubusercontent.com/yiluheihei/nCoV-2019-Data/master/ncov.csv",sep = ",")
# wuhan <- world2_original %>% filter( cityEnglishName == "Wuhan")
wuhan <- world2_original %>% filter( countryEnglishName == "China")

wuhan$date <- as.Date(wuhan$updateTime)
wuhan <- wuhan %>% select( date,cityEnglishName,countryEnglishName,city_deadCount)
names(wuhan) <- c( "date", "region", "country", "deceassed")

wuhan <- wuhan  %>% group_by(date,region,country) %>% summarise(
  deceassed = max(deceassed)
)

wuhan <- wuhan %>% group_by(region) %>% arrange(date) %>% mutate(
  daily_deaths = deceassed - lag(deceassed),
  daily_deaths_inc = round((deceassed - lag(deceassed)) /lag(deceassed) * 100, digits = 1),
  daily_deaths_avg6 =  round( ( daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) + lag(daily_deaths,4) + lag(daily_deaths,5) ) / 6, digits = 1 ) # average of dayly deaths of 6 last days
  # daily_deaths_avg2 =  round( ( daily_deaths + lag(daily_deaths,1) ) / 2, digits = 1 ),
  # daily_deaths_avg3 =  round( ( daily_deaths + lag(daily_deaths,1)+ lag(daily_deaths,2) ) / 3, digits = 1 ),
  # daily_deaths_avg4 =  round( ( daily_deaths + lag(daily_deaths,1) + lag(daily_deaths,2) + lag(daily_deaths,3) ) / 4, digits = 1 )
)


# Bind Spanish and Italian data
compare_countries <- rbind(
  as.data.frame( data_all_export %>% select( date, region, country, deceassed, daily_deaths, daily_deaths_inc, daily_deaths_avg6 ) ), 
  as.data.frame( data_i_cases %>% select( date, region, country, deceassed, daily_deaths, daily_deaths_inc, daily_deaths_avg6 ) ),
  as.data.frame( wuhan ) %>% filter( date < as.Date("2020-04-15"))
)

# adds FRance
data_f2_cases$country <- "France"

# ddd <- data_f2_cases %>% filter( (maille_nom == "Corse" & date < as.Date("2020-03-17") ) )
compare_countries_deceassed <-rbind(
  compare_countries, 
  as.data.frame( data_f2_cases %>% select( date, region, country, deceassed,  daily_deaths, daily_deaths_inc, daily_deaths_avg6 ) ) 
)


# compare_countries with "umbral" or more deceassed accumulated
umbral <- 5

# Select the date when a region had for the first time had n (umbral = n) or more cases
compare_countries_offset_ncases <- compare_countries_deceassed %>% filter(deceassed >= umbral) %>% group_by(region) %>% arrange(date) %>% 
  filter( row_number()==1 ) %>%
  select(region,date, deceassed) 
compare_countries_offset_ncases <- compare_countries_deceassed %>% filter(deceassed >= umbral) %>% group_by(region) %>% arrange(date) %>% 
  filter( row_number()==1 ) %>%
  select(region,date, deceassed) %>% mutate(offset = date - min(compare_countries_offset_ncases$date))

test <- merge(compare_countries_deceassed, select(compare_countries_offset_ncases,region,offset ), by = "region" , all.x = TRUE)
# calculate in numer of days since first evaluated, without date
test$days_since <- as.numeric(test$date - min(compare_countries_offset_ncases$date) ) - as.numeric(test$offset)

# growth curve --
# Contribution by @lorezmt
x <- 1:50
growth_2x <- vector(length = length(x))
growth_2x <- as.data.frame(growth_2x)
growth_2x$value[1] <- 5
growth_2x$value[2] <- 7
growth_2x$value[3] <- 2 * growth_2x$value[1]
growth_2x$value[4] <- 2 * growth_2x$value[2]
growth_2x$value[5] <- 2 * growth_2x$value[3]

growth_2x$value3[1] <- 5
growth_2x$value3[2] <- 6.2
growth_2x$value3[3] <- 8
growth_2x$value3[4] <- 2 * growth_2x$value3[1]
growth_2x$value3[5] <- 2 * growth_2x$value3[2]

growth_2x$value4[1] <- 5
growth_2x$value4[2] <- 5.9
growth_2x$value4[3] <- 6.9
growth_2x$value4[4] <- 8.4
growth_2x$value4[5] <- 2 * growth_2x$value4[1]

growth_2x$value5[1] <- 5
growth_2x$value5[2] <- 5.59
growth_2x$value5[3] <- 6.4
growth_2x$value5[4] <- 7.65
growth_2x$value5[5] <- 8.55

growth_2x$days_since <- 0
growth_2x$days_since[1] <- 0
growth_2x$days_since[2] <- 1
growth_2x$days_since[3] <- 2
growth_2x$days_since[4] <- 3
growth_2x$days_since[5] <- 4

for (i in 6:length(x)) {
  growth_2x$value[i] <- growth_2x$value[i-2]*2   ## doubling time 2 days
  growth_2x$value3[i] <- growth_2x$value3[i-3]*2   ## doubling time 2 days
  growth_2x$value4[i] <- growth_2x$value4[i-4]*2   ## doubling time 2 days
  growth_2x$value5[i] <- growth_2x$value5[i-5]*2   ## doubling time 2 days
  growth_2x$days_since[i] <- i-1
}

growth_2x %>%
  ggplot() +
  # geom_line(aes(days_since, value), size= 1, color = "#000000" ) +
  # geom_line(aes(days_since, value3), size= 1, color = "#000000" )
  geom_line(aes(days_since, value5), size= 1, color = "#000000" )

for (i in 1:4) {
  if (i == 1 ) {
    png(filename=paste0("img/compare/covid19_fallecimientos-por-region-superpuesto-offset-log_since-", umbral ,"deceased_en.png"), width = 1100,height = 800)
  } else if (i == 2 ) {
    png(filename=paste0("img/compare/covid19_fallecimientos-por-region-superpuesto-offset-log_since-", umbral ,"deceased-facet_en.png"), width = 800,height = 1500)
  } else if (i == 4 ) { 
    png(filename=paste0("img/compare/covid19_fallecimientos-por-region-superpuesto-offset-log_since-", umbral ,"deceased.png"), width = 1100,height = 800)
  }  else if (i == 3 ) { 
    png(filename=paste0("img/compare/covid19_fallecimientos-por-region-superpuesto-offset-log_since-", umbral ,"deceased-facet.png"),width = 800,height = 1500)
  }   
  # png(filename=paste0("img/compare/covid19_fallecimientos-por-region-superpuesto-offset-log_since-", umbral ,"deceased-facet_es.png"),width = 700,height = 1500)
# test %>%
ptotal <- test %>% filter( region != "Area not defined" & date != as.Date("2020-02-09")) %>%
# ptotal <- test %>% filter( country != "France") %>%
ggplot() +
  geom_line(data =growth_2x, aes(days_since, value), size= 0.5, color = "#555555", linetype = 2 ) +
  geom_line(data =growth_2x, aes(days_since, value3), size= 0.5, color = "#555555", linetype = 2 ) +
  geom_line(data =growth_2x, aes(days_since, value4), size= 0.5, color = "#555555", linetype = 2 ) +
  geom_line(data =growth_2x, aes(days_since, value5), size= 0.5, color = "#555555", linetype = 2 ) +
  geom_line(aes(days_since, deceassed, group= region, color= country), size= 0.7, alpha = 0.6 ) +
  # points for interactive
  geom_point( aes(days_since, deceassed, color= country,
            text = paste0("<b>", region, " (", country, ")</b><br>", format( round(deceassed, digits = 0), big.mark="."), " total deaths" ,"<br>",date, " (", days_since, ")")),
             size= 0.6, alpha = 0.6  ) +
  geom_point(data = filter(test, date == max(test$date) ),  aes(days_since, deceassed, color= country,
                  text = paste0("<b>", region, " (", country, ")</b><br>", format( round(deceassed, digits = 0), big.mark="."), " total deaths" ,"<br>",date, " (", days_since, ")")), 
              size= 1, alpha = 0.6  ) +
  # labels
  geom_text_repel(data=filter( test, date== as.Date("2020-05-06") & country == "Spain" |
                                 date==as.Date("2020-05-06") & country == "Italia" | 
                                 date==as.Date("2020-05-05") & country == "France" |
                                 date==as.Date("2020-04-14") & country == "China"
                            ),
                  aes(days_since, deceassed, label=paste(format( round(deceassed, digits = 0), big.mark="."), region)),
                  color= "#000000",
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=4,
                  hjust=1,
                  # bg.color = "red", bg.r = 0.15,
                  family = "Roboto Condensed",
                  # direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  coord_cartesian( 
    ylim=c(umbral-1, max(test[!is.na(test$deceassed),]$deceassed)*1.15 ),
    xlim=c(0,  100 ) #max(test[!is.na(test$deceassed),]$days_since) + 3
    ) +
  scale_y_log10(
    breaks = c(5,10,20,50,100,200,500,1000,2000,5000,10000,20000),
    # limits = c( umbral,max(test$deceassed)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
    minor_breaks = c(  seq(1 , 10, 2), seq(10 , 100, 20), seq(100 , 1000, 200), seq(1000, 10000, 2000) ) ) +
  scale_x_continuous(
    breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
    # limits=c( 0, max(test$days_since + 5))
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 20) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    plot.caption = element_text( color="#777777",size = 14, hjust = 1),
    legend.position = c(0.9,0.3)
  ) 


if ( (i == 2) | (i == 3)  ) {
  ptotal <- ptotal + facet_wrap(~country, ncol = 1) 
}

if ( (i == 1) | (i == 2)  ) { # EN
  ptotal <- ptotal + labs(title = paste0("Coronavirus (COVID-19) deaths in regions of China, Spain, Italy and France"),
     subtitle = paste0("Cumulative number of deaths, by number of days since ",umbral ,"th death. Updated: 2020.05.06"),
     y = "Number of deaths (log. scale)",
     x = paste0("Days since ", umbral , "th or more cumulative deaths"),
     caption ="By: @numeroteca (Montera34). https://lab.montera34.com/covid19 | Data: various official sources. Check website.") +
    # EN
    geom_text(data = growth_2x[1,], aes(20,12000, label=paste0("slope: deaths double every 2 days")),
              size = 4, family = "Roboto Condensed", hjust = 1, color = "#555555") +
    geom_text(data = growth_2x[1,], aes(35,12000, label=paste0("... every 3 days")),
              size = 4, family = "Roboto Condensed", hjust = 1, color = "#555555") +
    geom_text(data = growth_2x[1,], aes(40,12000, label=paste0("... every 4 days")),
              size = 4, family = "Roboto Condensed", hjust = 0, color = "#555555") +
    geom_text(data = growth_2x[1,], aes(50,4000, label=paste0("... every 5 days")),
              size = 4, family = "Roboto Condensed", hjust = 0, color = "#555555")
} else { # ES
  ptotal <- ptotal + labs(title = paste0("Número de fallecimientos de COVID-19 registrados"),
       subtitle = paste0("Por región en China, España, Italia y Francia (06.05.2022). Días desde ",umbral ," o más muertes (escala log)."),
       y = "fallecimientos registrados (escala log.)",
       x = paste0("días desde ", umbral , " o más fallecimientos"),
       caption ="Por: @numeroteca (Montera34). lab.montera34.com/covid19 | Data: various official sources. Check website.") +
    geom_text(data = growth_2x[1,], aes(20,12000, label=paste0("pendiente: muertes doblan cada 2 días")),
              size = 4, family = "Roboto Condensed", hjust = 1, color = "#555555") +
    geom_text(data = growth_2x[1,], aes(35,12000, label=paste0("... doblan cada 3 días")),
              size = 4, family = "Roboto Condensed", hjust = 1, color = "#555555") +
    geom_text(data = growth_2x[1,], aes(40,12000, label=paste0("... doblan cada 4 días")),
              size = 4, family = "Roboto Condensed", hjust = 0, color = "#555555") +
    geom_text(data = growth_2x[1,], aes(50,4000, label=paste0("...doblan cada 5 días")),
              size = 4, family = "Roboto Condensed", hjust = 0, color = "#555555")
}

print(ptotal)
print(paste("plot",i))
dev.off()
}

fig <- ggplotly(ptotal, tooltip = "text") %>% 
  layout(title = list(text = paste0('Coronavirus (COVID-19) deaths in regions of Spain, France, China and Italy',
      '<br>',
      '<sup>',
      'Cumulative number of deaths, by number of days since ',umbral ,'th death. Updated: 2020.05.06',
      '</sup>')))
  
# save to interactive/spain-italy-france_cases_regions-evolution.html
fig <- fig %>% 
  layout(annotations = 
           list(x = 1, y = -0.1, text = "lab.montera34.com/covid19 | Data: Check <a href='https://lab.montera34.com/covid19' style='color:#999999'>lab.montera34.com</a>.", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="grey"))
  )
htmlwidgets::saveWidget(as_widget(fig), paste0(getwd(),"/interactive/covid19-country-regions-compare-deaths.html") )


# Per 100.000 inhabitants -----------------

# compare_countries with "umbral" or more deceassed accumulated
umbral2 <- 0.5 # 0.5 deceassed per 100.00 inhab is 5 deceassed per million

# Bind Spanish and Italian data
compare_countries <- rbind(
  as.data.frame( data_all_export %>% select( date, region, country, deceassed, deceassed_per_100000, daily_deaths, daily_deaths_inc, daily_deaths_avg6 ) ), 
  as.data.frame( data_i_cases %>% select( date, region, country, deceassed, deceassed_per_100000, daily_deaths, daily_deaths_inc, daily_deaths_avg6 ) )
)

# Select the date when a region had for the first time had n (umbral = n) or more cases
compare_countries_offset_ncases_per100 <- compare_countries %>% filter(deceassed_per_100000 >= umbral2) %>% group_by(region) %>% arrange(date) %>% filter( row_number()==1 ) %>%
  select(region,date, deceassed, deceassed_per_100000) 
compare_countries_offset_ncases_per100 <- compare_countries %>% filter(deceassed_per_100000 >= umbral2) %>% group_by(region) %>% arrange(date) %>% filter( row_number()==1 ) %>%
  select(region,date, deceassed, deceassed_per_100000) %>% mutate(offset = date - min(compare_countries_offset_ncases_per100$date))

test2 <- merge(compare_countries, select(compare_countries_offset_ncases_per100,region,offset ), by = "region" , all.x = TRUE)
# calculate in numer of days since first evaluated, without date
test2$days_since <- as.numeric(test2$date - min(compare_countries_offset_ncases_per100$date) ) - as.numeric(test2$offset)


png(filename=paste0("img/compare/covid19_fallecimientos-por-region-superpuesto-offset-per-cienmil-log_since-", umbral ,"deceased.png"),width = 1000,height = 700)
test2 %>% filter( country != "France") %>%
  ggplot() +
  geom_line(aes(days_since, deceassed_per_100000*10, group= region, color= country), size= 1 ) +
  geom_point(data = test2 %>% filter( date==max(as.Date("2020-05-02")) & country == "Spain"  |
                                        date==max(as.Date("2020-05-03")) & country == "Italia" ), 
             aes(days_since, deceassed_per_100000*10, color= country), size= 1.5 ) +
  # Spain
  geom_text_repel(data=filter( test2,   date==max(as.Date("2020-05-02")) & country == "Spain"  |
                                       date==max(as.Date("2020-05-03")) & country == "Italia"  ),
                  aes(days_since, deceassed_per_100000*10, label=paste(format(deceassed_per_100000*10, nsmall=1, big.mark="."), region)),
                  color= "#000000",
                  nudge_x = 3, # adjust the starting y position of the text label
                  size=4,
                  hjust=1,
                  family = "Roboto Condensed",
                  # direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_y_log10( 
    limits = c( umbral, max(test2$deceassed_per_100000*10)),
    labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
    minor_breaks = c(  seq(0.1 , 1, 0.1),  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000), seq(10000, 10000, 10000) ) ) +
  scale_x_continuous(
    limits=c( 0, max(test2$days_since + 7))
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    plot.caption = element_text( color="#777777",size = 14, hjust = 1),
    legend.position = c(0.9,0.3)
  ) +
  labs(title = paste0("Número de fallecimientos de COVID-19 registrados por 1.000.000 habitantes por región en España e Italia"),
       subtitle = paste0("Muertes acumuladas desde ", umbral , " o más fallecimientos por millón de habitantes (03.05.2020). Escala logarítmica"),
       y = "fallecimientos registrados por 1.000.000 habitantes (escala log)",
       x = paste0("días desde ", umbral , " o más fallecimientos por 1.000.000 habitantes"),
       caption ="Por: @numeroteca (Montera34). lab.montera34.com/covid19 | Data: various official sources. Check website.")
dev.off()


# Daily deaths setting 0 day--------------
for (i in 1:4) {
  if (i == 1 ) {
    png(filename=paste0("img/compare/covid19_muertes-dia-por-region-superpuesto-offset-log_since-", umbral ,"deceased_en.png"), width = 1300,height = 700)
  } else if (i == 2 ) {
    png(filename=paste0("img/compare/covid19_muertes-dia-por-region-superpuesto-offset-log_since-", umbral ,"deceased-facet_en.png"), width = 800,height = 1500)
  } else if (i == 4 ) { 
    png(filename=paste0("img/compare/covid19_muertes-dia-por-region-superpuesto-offset-log_since-", umbral ,"deceased.png"), width = 1300,height = 700)
  }  else if (i == 3 ) { 
    png(filename=paste0("img/compare/covid19_muertes-dia-por-region-superpuesto-offset-log_since-", umbral ,"deceased-facet.png"),width = 800,height = 1500)
  }   
  daily_plot <- test %>%  filter( region != "Area not defined" & date != as.Date("2020-02-09")) %>%
    ggplot() +
    geom_line(aes(days_since, daily_deaths_avg6, group= region, color= country), size= 0.9, alpha = 0.6, se = FALSE ) +
    # for interactive
    # geom_point(aes(days_since, daily_deaths_avg6, color= country,
    #            text = paste0("<b>", region, " (", country, ")</b><br>", format( round(daily_deaths_avg6), big.mark="."), " average daily deaths" ,"<br>",date, " (day ", days_since, ")")),
    #            size= 1, alpha = 0.6  ) +
    geom_point(data = filter(test, date == max(test$date) ),  aes(days_since, daily_deaths_avg6, color= country, 
                                                                  text = paste0("<b>", region, " (", country, ")</b><br>", format( round(daily_deaths_avg6), big.mark="."), " average daily deaths" ,"<br>",date, " (day ", days_since, ")")), 
               size= 1, alpha = 0.6  ) +
    geom_text_repel(data=filter( test, date== as.Date("2020-05-02") & country == "Spain" |
                                 date==as.Date("2020-05-03") & country == "Italia" | 
                                 date==as.Date("2020-05-03") & country == "France" |
                                   date==as.Date("2020-04-14") & country == "China"  
                                   ),
                  # aes(days_since, daily_deaths_avg6, label=paste(format( round(daily_deaths_avg6, digits = 1), big.mark="."), region)),
                  aes(days_since, daily_deaths_avg6, label=paste(region)),
                  color= "#000000",
                  nudge_x = 1, # adjust the starting y position of the text label
                  size=4,
                  hjust=1,
                  # bg.color = "red", bg.r = 0.15,
                  family = "Roboto Condensed",
                  # direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
    coord_cartesian( 
      ylim=c(1, max(test[!is.na(test$daily_deaths_avg6),]$daily_deaths_avg6)*1.15 ),
      xlim=c(0,  100 ) #max(test[!is.na(test$deceassed),]$days_since) + 3
    ) +
    scale_y_log10(
      breaks = c(1,2,5,10,20,50,100,200,500,1000,2000,5000),
      # limits = c( umbral,max(test$deceassed)),
      labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
      minor_breaks = c(  seq(1 , 10, 2), seq(10 , 100, 20), seq(100 , 1000, 200), seq(1000, 10000, 2000) ) ) +
    scale_x_continuous(
      breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),
      expand = c(0,0)
      # limits=c( 0, max(test$days_since + 5))
    ) +
    theme_minimal(base_family = "Roboto Condensed", base_size = 20) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      # panel.grid.minor.y = element_blank(),
      axis.ticks.x = element_line(color = "#000000"),
      plot.caption = element_text( color="#777777",size = 14, hjust = 1),
      legend.position = c(0.9,0.3)
    ) 
  if ( (i == 2) | (i == 3)  ) {
    daily_plot <- daily_plot + facet_wrap(~country, ncol = 1) 
  }
  
  if ( (i == 1) | (i == 2)  ) { # EN
    daily_plot <- daily_plot + labs(title = paste0("Daily deaths by coronavirus (COVID-19) in regions of China (2020.04.14), Spain, Italy and France"),
                            subtitle = paste0("Daily deaths, by number of days since ",umbral ,"th death. Updated: 2020.05.03"),
                            y = "Daily deaths (rolling average 6 days)",
                            x = paste0("Days since ", umbral , "th or more cumulative deaths"),
                            caption ="By: @numeroteca (Montera34). https://lab.montera34.com/covid19 | Data: various official sources. Check website.")

  } else { # ES
    daily_plot <- daily_plot + labs(title = paste0("Media de muertes por día en los 6 días anteriores por COVID-19 (2020.05.03)"),
                            subtitle = paste0("Por región en China (2020.04.14), España, Italia y Francia. Días desde ",umbral ," o más muertes (escala log)."),
                            y = "fallecimientos por día registrados (escala log.)",
                            x = paste0("días desde ", umbral , " o más fallecimientos"),
                            caption ="Por: @numeroteca (Montera34). lab.montera34.com/covid19 | Data: various official sources. Check website.")
  }
  
  print(daily_plot)
  print(paste("plot",i))
  dev.off()
}

fig <- ggplotly(daily_plot, tooltip = "text") %>% 
  layout(title = list(text = paste0('Daily deaths by COVID-19 in regions of Spain, Italy and France',
          '<br>',
          '<sup>',
          'Daily deaths (rolling average 6 days)',umbral ,'th death. Updated: 2020.05.03',
          '</sup>')))

# save to interactive/spain-italy-france_cases_regions-evolution.html
fig  <- fig %>% 
  layout(annotations = 
           list(x = 1, y = -0.1, text = "lab.montera34.com/covid19 | Data: various official sources. Check website.", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="grey"))
  )

htmlwidgets::saveWidget(as_widget(fig), paste0(getwd(),"/interactive/covid19-country-regions-compare-daily-deaths.html") )



# 2. interactive -----------------------
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

# save to interactive/spain-italy-france_cases_regions-evolution.html
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
