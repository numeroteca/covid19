# cataluña
# Donwloaded from https://analisi.transparenciacatalunya.cat/Salut/Registre-de-test-de-COVID-19-realitzats-a-Cataluny/jj6z-iyrp/data
catalunya_original <-  read.delim("data/original/spain/catalunya/Registre_de_test_de_COVID-19_realitzats_a_Catalunya._Segregaci__per_sexe_i_municipi.csv",sep = ",")
catalunya <- catalunya_original

# creates date variable
catalunya$date <- as.Date(catalunya$TipusCasData, "%d/%m/%Y")
# extracts first charatcter of zipcode to select province code
catalunya$provincia_code <- ifelse(catalunya$MunicipiCodi < 10000, paste0(substr(as.character(catalunya$MunicipiCodi),1,1)), substr(as.character(catalunya$MunicipiCodi),1,2))     

# iterates through data to count positives
catalunya_new <- catalunya %>% group_by(date,provincia_code) %>% arrange(date) %>% filter( TipusCasDescripcio == "Positiu" ) %>%
# catalunya_new <- catalunya %>% group_by(date,provincia_code) %>% arrange(date) %>% filter( ResultatCovidDescripcio == "Positiu" |  ResultatCovidDescripcio == "Probable" ) %>%
  summarise ( 
    by_day = sum(NumCasos)
    )

# Makes df for every province
cat8 <- catalunya_new %>% filter(provincia_code == "8")
cat8 <- cat8 %>% group_by( provincia_code) %>% arrange(date) %>%
  mutate ( 
    cases_accumulated = cumsum(by_day)
  )

cat17 <- catalunya_new %>% filter(provincia_code == "17")
cat17 <- cat17 %>% group_by( provincia_code) %>% arrange(date) %>%
  mutate ( 
    cases_accumulated = cumsum(by_day)
  )

cat25 <- catalunya_new %>% filter(provincia_code == "25")
cat25 <- cat25 %>% group_by( provincia_code) %>% arrange(date) %>%
  mutate ( 
    cases_accumulated = cumsum(by_day)
  )

cat43 <- catalunya_new %>% filter(provincia_code == "43")
cat43 <- cat43 %>% group_by( provincia_code) %>% arrange(date) %>%
  mutate ( 
    cases_accumulated = cumsum(by_day)
  )

# Binds all the provinces
cattotal <- rbind(cat8,cat17,cat25,cat43)
# Creates provinica factor
cattotal$province <- as.factor(cattotal$provincia_code)
# Gives names 
levels(cattotal$province) <- c("Girona","Lleida","Tarragona","Barcelona")

write.csv(cattotal, file = "data/output/spain/catalunya-cases-evolution-by-province.csv", row.names = FALSE)


# Plots -----

ggplot(cattotal) +
  geom_line(aes(date,cases_accumulated, group = provincia_code), color = "red") +
  # uses data from evolution_spain_provinces.R script. Rund that first!
  geom_line(data = filter (data_cases_sp_provinces, ccaa =="Cataluña" ) , aes(date,cases_accumulated, group = province),size = 0.5) +
  scale_y_log10()

ggplot(cattotal) +
  geom_line(aes(date,cases_accumulated, group = provincia_code), color = "red") +
  geom_point(aes(date,cases_accumulated), color = "red", size = 0.5) +
  geom_line(data = filter (data_cases_sp_provinces, ccaa =="Cataluña" ) , aes(date,cases_accumulated, group = province),size = 0.5) +
  geom_point(data = filter (data_cases_sp_provinces, ccaa =="Cataluña" ) , aes(date,cases_accumulated ),size = 0.5) +
  scale_y_log10()+
  facet_grid(~province)


ggplot(cattotal) +
  geom_line(aes(date,cases_accumulated, group = provincia_code)) +
  facet_grid(~province)+
  scale_y_log10()

ggplot(cattotal) +
  geom_line(aes(date,cases_accumulated, group = provincia_code)) +
  facet_grid(~province) 