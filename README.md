COVID19 en España e Italia / CODVID-19 in Spain and Italy
=================

R script to anlyze COVID19 in Spain and Italy. WEbsite with upated plots: https://lab.montera34.com/covid19

Script de R para facilitar el análisis del COVID19 en España e Italia. Puedes ver los gráficos producidos en la web que hemos montado en montera34: https://lab.montera34.com/covid19

![Casos registrados por comunidad autónoma. España. COVIC-19](https://lab.montera34.com/covid19-r/img/covid19_casos-registrados-por-comunidad-autonoma-superpuesto-per-cienmil-log.png "Plot example")


Licence: [GNU GPLv3](https://code.montera34.com:4443/numeroteca/covid19/-/blob/master/LICENSE.md)

## Cómo usarlo / how to use it

Si vas a publicar visualizaciones, antes lee este artículo [Ten Considerations Before You Create Another Chart About COVID-19](https://medium.com/nightingale/ten-considerations-before-you-create-another-chart-about-covid-19-27d3bd691be8). Todo poder conlleva una gran responsabilidad.

When you go to any of the scripts you can run it and it will produce all the visualizations. Sometimes data directly takes them from the source some others are stored locally.

## File structure

```
├── coronavirus.Rproj                             # R project
├── analysis
│   ├── evolution_france.R                # R script: process and create plots France  
│   ├── evolution_italia.R                # R script: process and create plots Italia  
│   ├── evolution_spain.R                 # R script: process and create plots Spain
├── data
│   ├── original                                  # original data
│   │   └── Spain
│   │       ├── ccaa-poblacion.csv            # population per region
│   │       └── covid10_spain_provincias.csv  # covid19 data by province. 
│   └── output                                    # processed data: by date and comunidad autónoma in Spain
│       ├── covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated.csv     # merge all cases
│       ├── covid19-casos-registrados-por-ccaa-espana-por-dia-acumulado.csv   # registered cases accumulated
│       ├── covid19-fallecimientos-por-ccaa-espana-por-dia-acumulado.csv      # deaths
│       └── covid19-ingresos-uci-por-ccaa-espana-por-dia-acumulado.csv        # intensive care
├── img
│   ├── france    # France plots
│   ├── italia    # Ftaly plots
│   ├── 20200312                                  # previous days plots
│   ├── 20200313                                  # previous days plots
│   ├── covid19_*.png                             # last plots generated
├── LICENSE.md
└── README.md
```

## Datos / Data 

### De COVID-19 

#### España (Spain)

##### Comunidades autónomas

Del respositorio de Datadista (https://github.com/datadista/datasets/tree/master/COVID%2019) que los extrae a su vez de las tablas de la situación diaria de la enfermedad por el coronavirus (COVID-19) en España que publica el Ministerio de Sanidad, Consumo y Bienestar Social (https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm) en incómodos PDF. Cita a Datadista como fuente de los datos extraídos. 

Los datos procesados listos para usarse (formato largo) están disponibles en el directorio [data/output](https://code.montera34.com:4443/numeroteca/covid19/-/tree/master/data/output).

Processed data are avilable (long format) in this directory [data/output](https://code.montera34.com:4443/numeroteca/covid19/-/tree/master/data/output).

There is a file with all the data (registered cases, intensive care patients and deaths): [/data/output/covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated.csv](https://code.montera34.com:4443/numeroteca/covid19/-/blob/master/data/output/covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated.csv)

Data structure:

* `code_ine` INE code numeber for comunidad autónoma (region)
* `comunidad_autonoma` Spanish region
* `cases_registered` Number of registered cases
* `date` Day
* `population` population
* `cases_per_100000` Number of registered cases per 100.000 people
* `intensive_care` Number of intensive care patients (UCI in Spanish)
* `intensive_care_per_1000000` Number of intensive care patients per 100.000 people (UCI in Spanish)
* `deceassed` Number of deceassed
* `deceassed_per_100000` Number of deceassed per 100.000 people
* `altas` Number of recovered
* `altas_per_100000` Number of recovered per 100.000 people

Example of observations:

```
13,"Madrid",2940,2020-03-14,6663394,44.12,NA,NA,86,12.91,NA,NA
13,"Madrid",3544,2020-03-15,6663394,53.19,NA,NA,213,31.97,NA,NA
13,"Madrid",4165,2020-03-16,6663394,62.51,253,3.8,213,31.97,474,71.13
```
When no data is available `NA` is indicated. Intensive care patients data have not been published since March 13th.

##### Provincias

Working spreadsheet: https://docs.google.com/spreadsheets/d/1qxbKnU39yn6yYcNkBqQ0mKnIXmKfPQ4lgpNglpJ9frE/edit#gid=0
Ayúdanos a completarla. Pide acceso.

Data are published in this file: [/data/original/spain/covid19_spain_provincias.csv](https://code.montera34.com:4443/numeroteca/covid19/-/blob/master/data/original/spain/covid19_spain_provincias.csv)

Format:

|            |                  |           |                |          |                   |           |                                                                                                   |                                                                                                     | 
|------------|------------------|-----------|----------------|----------|-------------------|-----------|---------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------| 
| date       | province         | new_cases | intensive_care | deceased | cases_accumulated | recovered | source                                                                                            | comments                                                                                            | 
| 2020-03-13 | Alicante/Alacant |           |                |          | 39                |           |                                                                                                   | More info about this initiative at https://code.montera34.com:4443/numeroteca/covid19/-/tree/master | 
| 2020-03-14 | Alicante/Alacant | 18        |                |          | 57                |           | http://www.san.gva.es/documents/151311/8476524/200314+NOTA+DE+PRENSA+CORONAVIRUS.pdf              |                                                                             | 
| 2020-03-15 | Alicante/Alacant | 94        |                |          | 151               |           | http://www.san.gva.es/documents/151311/8477533/20200315+NOTA+CORONAVIRUS                          |                                         | 
| 2020-03-13 | Almería          | 0         | 0              | 0        | 9                 | 0         | https://www.juntadeandalucia.es/organismos/saludyfamilias/actualidad/noticias/detalle/233232.html |                                                                                                     | 



#### Italia

Italian data: https://github.com/pcm-dpc/COVID-19


### Población por comunidades autónomas (2019)

Población por comunidades autónomas del INE: https://www.ine.es/jaxiT3/Datos.htm?t=2853#!tabs-tabla
Población por provincias del INE:  https://www.ine.es/jaxiT3/Datos.htm?t=2852#!tabs-tabla

## Autoría

Pablo Rey Mazón ([@numeroteca](https://twitter.com/numeroteca)) y Alfonso Sánchez Uzábal ([@skotperez](https://twitter.com/skotperez))  desde [montera34.com](https://montera34.com).

En este post [Análisis de propagación de COVID-19 por comunidades autónomas en España](http://numeroteca.org/2020/03/12/covid19-comunidades-autonomas-espana/) recopilamos algunos resultados y reflexiones.


# This repository

This repository lives at https://code.montera34.com:4443/numeroteca/covid19 and is mirrored in [Github](https://github.com/numeroteca/covid19).