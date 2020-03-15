COVID19 en España e Italia / CODVID-19 in Spain and Italy
=================

Script de R para facilitar el análisis del COVID19 en España e Italia. Puedes ver los gráficos producidos en la web que hemos montado en montera34: https://lab.montera34.com/covid19

R script to anlyze COVID19 in Spain and Italy. WEbsite with upated plots: https://lab.montera34.com/covid19

Licence: [GNU GPLv3](https://code.montera34.com:4443/numeroteca/covid19/-/blob/master/LICENSE.md)

## Cómo usarlo / how to use it

Si vas a publicar visualizaciones, antes lee este artículo [Ten Considerations Before You Create Another Chart About COVID-19](https://medium.com/nightingale/ten-considerations-before-you-create-another-chart-about-covid-19-27d3bd691be8). Todo poder conlleva una gran responsabilidad.


## File structure

```
├── coronavirus.Rproj                             # R project
├── covid19_italia.R                              # R script: process and create plots Italia
├── covid19.R                                     # R script: process and create plots Spain
├── data
│   ├── original                                  # original data
│   │   └── ccaa-poblacion.csv 
│   └── output                                    # processed data: by date and comunidad autónoma in Spain
│       ├── covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated.csv     # merge all cases
│       ├── covid19-casos-registrados-por-ccaa-espana-por-dia-acumulado.csv   # registered cases accumulated
│       ├── covid19-fallecimientos-por-ccaa-espana-por-dia-acumulado.csv      # deaths
│       └── covid19-ingresos-uci-por-ccaa-espana-por-dia-acumulado.csv        # intensive care
├── img
│   ├── 20200312
│   ├── 20200313                                  #previous plots
│   ├── covid19_*.png                             # last plots
├── LICENSE.md
└── README.md
```

## Datos / Data 

### De COVID-19 

#### España (Spain)

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
* `intensive_care` Number of intensive care patients
* `intensive_care_per_1000000` Number of intensive care patients per 100.000 people
* `deceassed` Number of deceassed
* `deceassed_per_100000` Number of deceassed per 100.000 people

Example of observations:

```
13,"Madrid",1990,2020-03-13,6663394,29.9,180,2.7,81,12.16
15,"Navarra",73,2020-03-12,654214,11.2,3,0.46,0,0
13,"Madrid",2940,2020-03-14,6663394,44.1,NA,NA,86,12.91
```

When no data is available `NA` is indicated.

#### Italia

Italian data: https://github.com/pcm-dpc/COVID-19


### Población por comunidades autónomas (2019)

Población por comunidades autónomas del INE: https://www.ine.es/jaxiT3/Datos.htm?t=2853#!tabs-tabla

## Autoría

Pablo Rey Mazón ([@numeroteca](https://twitter.com/numeroteca)) y Alfonso Sánchez Uzábal ([@skotperez](https://twitter.com/skotperez))  desde [montera34.com](https://montera34.com).

En este post [Análisis de propagación de COVID-19 por comunidades autónomas en España](http://numeroteca.org/2020/03/12/covid19-comunidades-autonomas-espana/) recopilamos algunos resultados y reflexiones.


# This repository

This repository lives at https://code.montera34.com:4443/numeroteca/covid19 and is mirrored in [Github](https://github.com/numeroteca/covid19).