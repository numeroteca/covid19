# Script to offset the lines and make the m converge when: Ex: days since first time there were more than 5 cases.

# it uses data_all_export generated in evolution_spain.R 

# Cases ---------------------

umbral <- 200

# Select the date when a region had for the first time had n (umbral = n) or more cases
data_s_offset_10cases <- data_all_export %>% filter(cases_registered >= umbral) %>% group_by(region) %>% filter( row_number()==1 ) %>%
  select(region,date, cases_registered) 
data_s_offset_10cases <- data_all_export %>% filter(cases_registered >= umbral) %>% group_by(region) %>% filter( row_number()==1 ) %>%
  select(region,date, cases_registered) %>% mutate(offset = date - min(data_s_offset_10cases$date))

test <- merge(data_all_export, select(data_s_offset_10cases,region,offset ), by = "region" , all.x = TRUE)
# calculate in numer of days since first evaluated, without date
test$days_since <- as.numeric(test$date - min(data_s_offset_10cases$date) ) - as.numeric(test$offset)


png(filename=paste0("img/spain/days-since/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-offset-log_since-", umbral ,"cases.png"),width = 1000,height = 700)
test %>%
  ggplot() +
  geom_line(aes(days_since, cases_registered, group= region, color= region), size= 1 ) +
  geom_point(aes(days_since, cases_registered, color= region), size= 1.5 ) +
  geom_text_repel(data=filter( test, date==max(test$date),  region != "Total"), 
                  aes(days_since, cases_registered, color= region, label=paste(format(cases_registered, nsmall=1, big.mark="."), region)),
                  # nudge_x = 3, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  # direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_y_log10( 
                  limits = c( umbral,max(test$cases_registered)),
                  labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE), 
                   minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ) ) +
  scale_x_continuous(
               limits=c( 0, max(test$days_since + 3.5)) 
  ) + 
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = paste0("Número de casos acumulados de COVID-19 registrados en España. Días desde ",umbral ," o más casos"),
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). "),
       y = "casos registrados",
       x = paste0("días desde ", umbral , " o más casos"),
       caption = caption)
dev.off()


# Deceassed ------------------------

umbral_d <- 40

# Select the date when a region had for the first time had n (umbral = n) or more deceasses
data_s_offset_10deceassed <- data_all_export %>% filter( deceassed >= umbral_d) %>% group_by(region) %>% filter( row_number()==1 ) %>%
  select(region,date, cases_registered, deceassed) 
data_s_offset_10deceassed <- data_all_export %>% filter( deceassed >= umbral_d) %>% group_by(region) %>% filter( row_number()==1 ) %>%
  select(region,date, cases_registered, deceassed) %>% mutate(offset = date - min(data_s_offset_10deceassed$date))

test2 <- merge(data_all_export, select(data_s_offset_10deceassed,region,offset ), by = "region" , all.x = TRUE)
# calculate in numer of days since first evaluated, without date
test2$days_since <- as.numeric(test2$date - min(data_s_offset_10deceassed$date) ) - as.numeric(test2$offset)

# create growth curve-------

data_all_test2 <- data_all_export %>% filter(date >= min(data_s_offset_10deceassed$date)) # sets starting day

x <- seq_along(unique(data_all_test2$date))
# creates empty vectors
ynn <- vector(length=length(x))
# fill firs value
ynn[[1]] <- umbral_d # sets starting value
# create data series with certain % of growth
for (i in 2:length(x)) {
  ynn[[i]] <- ynn[[i-1]] + ynn[[i-1]]*0.35 # grows 35%
}
# creates the data fame
data_unique_test2 <- arrange(data_all_test2, date) %>% select(date) %>% unique()
crec_test2 <- data.frame(x = data_unique_test2, ynn = ynn)
crec_test2$days_since <- as.numeric(crec_test2$date - min(data_s_offset_10deceassed$date) )

png(filename=paste0("img/spain/days-since/covid19_fallecimientos-por-comunidad-autonoma-superpuesto-offset-log_since-", umbral_d, "dec.png", sep = ""),width = 1000,height = 700)
test2 %>%
  ggplot() +
  geom_line(aes(days_since, deceassed, group= region, color= region), size= 1 ) +
  geom_point(aes(days_since, deceassed, color= region), size= 1.5 ) +
  geom_line(data = crec_test2, aes(x = days_since, y = ynn), linetype = 2, size = 1, color ="#444444") +
  geom_text(data = crec_test2[1,],aes(8,200,label="un 35% más de fallecimientos cada día"), 
            size = 5, base_family = "Roboto Condensed") +
  geom_text_repel(data=filter( test2, date==max(test2$date),  region != "Total"), 
                  aes(days_since, deceassed, color= region, label=paste(format(deceassed, nsmall=1, big.mark="."), region)),
                  # nudge_x = 4, # adjust the starting y position of the text label
                  size=5,
                  # hjust=0,
                  family = "Roboto Condensed",
                  # direction="y",
                  segment.size = 0.1,
                  segment.color="#333333"
  ) +
  scale_y_log10(
                limits = c(umbral_d,max(test2$deceassed) ),
                labels=function(x) format(round(x, digits = 0), big.mark = ".", scientific = FALSE),
                 minor_breaks = c(  seq(1 , 10, 1), seq(10 , 100, 10), seq(100 , 1000, 100), seq(1000, 10000, 1000) ) ) +
  scale_x_continuous(
    # date_breaks = "1 day", 
    #  date_labels = "%d",
    limits=c(0,max(test2$days_since)+10)
  ) + 
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = paste0("Número de fallecimientos de COVID-19 registrados en España. Día 0: desde ", umbral_d ," o más fallecimientos acumulados"),
       subtitle = paste0("Por comunidad autónoma (escala logarítmica). ",period),
       y = "casos registrados",
       x = paste0("días desde que hubo ", umbral_d, " o más fallecimientos"),
       caption = caption)
dev.off()

