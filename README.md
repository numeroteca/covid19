COVID19 en España e Italia / CODVID-19 in Spain and Italy
=================

**EN**

This is a repository that hosts two different projects:

 * R script to analyze COVID-19 in Spain, Italy and France. Website with upated charts: https://lab.montera34.com/covid19
 * Data collection of COVID-19 by province in spain (collected from various data sources). [More info](https://github.com/montera34/escovid19data). It is currently moving as an independent project.

**ES**

Este repositorio contiene dos proyectos diferentes:

  * Script de R para facilitar el análisis del COVID19 en España, Italia y Francia. Puedes ver los gráficos producidos en la web que hemos montado en montera34: https://lab.montera34.com/covid19
  * Los datos de COVID-19 por provincias. Una iniciativa ciudadana para recolectarlos de diferentes fuentes, en vista de que el gobierno no lo facilita. [Más información](https://github.com/montera34/escovid19data). Se está mudando actualmente a un repositorio independiente.

## Licencia / License

[GNU GENERAL PUBLIC LICENSE. V3 (GNU GPLv3)](https://code.montera34.com:4443/numeroteca/covid19/-/blob/master/LICENSE.md).

## Cómo usarlo / how to use it

Si vas a publicar visualizaciones, antes lee este artículo [Ten Considerations Before You Create Another Chart About COVID-19](https://medium.com/nightingale/ten-considerations-before-you-create-another-chart-about-covid-19-27d3bd691be8). Todo poder conlleva una gran responsabilidad.

When you go to any of the scripts you can run it and it will produce all the visualizations. Sometimes data directly takes them from the source some others are stored locally.

## File structure

```
├── coronavirus.Rproj                     # R project
├── analysis                              # scripts to process data and generate charts
│   ├── comparativa-bases-de-datos.R      # R script: to compare databases: ISCII, datadista and esCOVID19data
│   ├── count_catalunya.R                 # R script: to process Catalunya data
│   ├── evolution_compare.R               # R script: process and create plots compare countries
│   ├── evolution_france.R                # R script: process and create plots France  
│   ├── evolution_italia.R                # R script: process and create plots Italia  
│   ├── evolution_spain.R                 # R script: process and create plots Spain by comunidades autonomas
│   ├── evolution_spain_provinces_maps.R                 # R script: generate map by provinces to make animated gif
│   └── evolution_spain_provinces.R                 # R script: process and create plots Spain by provinces
├── data
│   ├── original                                  # original data
│   │   └── spain
│   │       ├── ccaa-poblacion.csv            # population per region
│   │       ├── provincias-poblacion.csv      # population per province
│   │       └── covid10_spain_provincias.csv  # covid19 data by province downloaded from spreasheer 
│   └── output                                # processed data: by date and comunidad autónoma in Spain
│       ├── spain
│       │    ├── covid19-provincias-spain_consolidated.csv  # exported province data
│       ├── covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated.csv     # merge all variables
│       ├── covid19-casos-registrados-por-ccaa-espana-por-dia-acumulado.csv   # registered cases accumulated
│       ├── covid19-fallecimientos-por-ccaa-espana-por-dia-acumulado.csv      # deceassed
│       └── covid19-ingresos-uci-por-ccaa-espana-por-dia-acumulado.csv        # intensive care
├── img
│   ├── france    # France plots
│   ├── italia    # Ftaly plots
│   └── spain
│       ├── regions                       # Comunidades autónomas (regions) charts
│       │    ├── 20200312                 # previous days plots
│       │    ├── 20200313                 # previous days plots
│       │    ├── covid19_*.png            # last plots generated
│       └── provinces                     # Provinces charts
├── LICENSE.md
└── README.md
```

## Datos / Data 

### España (Spain)

<img src="https://lab.montera34.com/covid19-r/img/spain/regions/covid19_muertes-por-dia-comunidad-autonoma-superpuesto-log_media.png"  width="450" alt="Media de muertes por día en los 6 días anteriores por comunidad autónoma. España. COVID-19]">

<img src="https://lab.montera34.com/covid19-r/img/compare/covid19_fallecimientos-por-region-superpuesto-offset-log_since-5deceased.png"  width="450" alt="Fallecimientos acumulados por día desde el primer día que hubo 5 fallecimientos o más en regiones de España (CCAA), Italia y Francia. Escala logarítmica. España. COVID-19">

#### Provincias

Más información sobre la recogida de datos por provincias en [EsCOVID19data](https://github.com/montera34/escovid19data).

Read more information at [EsCOVID19data](https://github.com/montera34/escovid19data).

#### Comunidades autónomas

Data are extracted from official PDF sources by Datadista [in this repository](https://github.com/datadista/datasets/tree/master/COVID%2019).

Del respositorio de Datadista (https://github.com/datadista/datasets/tree/master/COVID%2019) que los extrae a su vez de las tablas de la situación diaria de la enfermedad por el coronavirus (COVID-19) en España que publica el Ministerio de Sanidad, Consumo y Bienestar Social (https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm) en incómodos PDF. Cita a Datadista como fuente de los datos extraídos. 

Los datos procesados listos para usarse (formato largo) están disponibles en el directorio [data/output](https://code.montera34.com:4443/numeroteca/covid19/-/tree/master/data/output).

Processed data are avilable (long format) in this directory.  [data/output](https://code.montera34.com:4443/numeroteca/covid19/-/tree/master/data/output).

There is a file with all the data (registered cases, intensive care patients and deaths), see data structure below: [/data/output/covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated.csv](https://code.montera34.com:4443/numeroteca/covid19/-/blob/master/data/output/covid19-cases-uci-deaths-by-ccaa-spain-by-day-accumulated.csv)

Data structure:

* `date` Day
* `region_code` Region code (INE code number for comunidad autónoma)
* `region` Spanish region (comunidad autónoma)
* `country` COuntry the region belongs to
* `population` population of the region
* `cases_registered` Number of registered cases
* `cases_per_100000` Number of registered cases per 100.000 people
* `intensive_care` Number of intensive care patients (UCI in Spanish). [Read Datadista README for exceptions](https://github.com/datadista/datasets/blob/master/COVID%2019/readme.md)
* `intensive_care_per_1000000` Number of intensive care patients per 100.000 people (UCI in Spanish)
* `deceassed` Number of deceassed
* `deceassed_per_100000` Number of deceassed per 100.000 people
* `recovered` Number of recovered
* `recovered_per_100000` Number of recovered per 100.000 people
* `hospitalized` Number of hospitalized
* `hospitalized_per_100000` Number of hospitalized per 100.000 people

Example of observations:

| "date"       | "region\_code" | "region"    | "country" | "population" | "cases\_registered" | "cases\_per\_100000" | "intensive\_care" | "intensive\_care\_per\_1000000" | "deceassed" | "deceassed\_per\_100000" | "recovered" | "recovered\_per\_100000" |
|--------------|----------------|-------------|-----------|--------------|---------------------|----------------------|-------------------|---------------------------------|-------------|--------------------------|-------------|--------------------------|
| 2020\-03\-14 | 1              | "Andalucía" | "Spain"   | 8414240      | 269                 | 3\.2                 | NA                | NA                              | 2           | 0\.24                    | NA          | NA                       |
| 2020\-03\-15 | 1              | "Andalucía" | "Spain"   | 8414240      | 437                 | 5\.19                | NA                | NA                              | 6           | 0\.71                    | NA          | NA                       |
| 2020\-03\-16 | 1              | "Andalucía" | "Spain"   | 8414240      | 554                 | 6\.58                | 11                | 0\.13                           | 7           | 0\.83                    | 0           | 0                        |
| 2020\-03\-17 | 1              | "Andalucía" | "Spain"   | 8414240      | 683                 | 8\.12                | 13                | 0\.15                           | 11          | 1\.31                    | 0           | 0                        |
| 2020\-03\-18 | 1              | "Andalucía" | "Spain"   | 8414240      | 859                 | 10\.21               | 21                | 0\.25                           | 19          | 2\.26                    | 38          | 4\.52                    |


When no data is available `NA` is indicated. Intensive care patients data have not been published since March 13th.

#### Población por comunidades autónomas (2019)

Población por comunidades autónomas del INE: https://www.ine.es/jaxiT3/Datos.htm?t=2853#!tabs-tabla


### Italia

Italian data: https://github.com/pcm-dpc/COVID-19

### France

Warning: this section needs update.

Working on it. Meanwhile: data/original/france/covid19-france.csv

Official data from national government: https://www.data.gouv.fr/fr/datasets/cas-confirmes-dinfection-au-covid-19-par-region/

The main source for french data is this repo: https://github.com/opencovid19-fr/data

The repo gathers data from national and regional administrations and unifies it. All the structured data is disponible in two formats:

 * CSV. Direct link to CSV file: https://github.com/opencovid19-fr/data/blob/master/dist/chiffres-cles.csv
 * JSON. Direct link to JSON file: https://github.com/opencovid19-fr/data/blob/master/dist/chiffres-cles.json

This is the structure:

| Columns       | Description                                                |
|---------------|------------------------------------------------------------|
| date          | date                                                       |
| granularite   | disaggregation level                                       |
| maille_code   | code of the state, region or country (just FRA for France) |
| maille_nom    | name of the state, region or country (just France)         |
| cas_confirmes | registered cases                                           |
| deces         | deceassed                                                  |
| reanimation   | intensive care hospitalizations                            |
| hospitalises  | hospitalizations                                           |
| gueris        | recovered                                                  |
| depistes      | number of discovered cases\*                               |
| source_nom    | name of the source                                         |
| source_url    | URL of the source                                          |
| source_type   | type of source                                             |
\* don't know the different with registered cases. this column is empty except for Polynésie et Nouvelle Calédonie 

#### Regiones (Régions)

Regional data is in the main exit file linked above.

#### Provincias (Départements)

The data for départements is also in the main file. There is also (https://github.com/opencovid19-fr/data/tree/master/data-sources/sante-publique-france)[this dataset] to gather the data before to push it in the exit file.

This is the structure:

| Input | Output       | Description                        |                                                    |
|-------|--------------|------------------------------------|----------------------------------------------------|
| dep   | code & nom   | Code et nom du département (code and name of the state)                                 |
| sexe  | --           | Non repris en sortie (this data is not used in exit file)                               |
| jour  | date         | Date de la donnée (date)                                                                |
| hosp  | hospitalises | Nombre de personnes hospitalisées  (number of people hospitalized)                      |
| rea   | reanimation  | Nombre de personnes en réanimation (number of people hospitalized with intensive care)  |
| rad   | gueris       | Nombre de personnes guéries (number of recovered people)                                |
| dc    | deces        | Nombre de personnes décédées (number of deceassed people)                               |

Direct link to this dataset: https://github.com/opencovid19-fr/data/blob/master/data-sources/sante-publique-france/covid_hospit.csv

## Autoría | Authorship

Pablo Rey Mazón ([@numeroteca](https://twitter.com/numeroteca)) y Alfonso Sánchez Uzábal ([@skotperez](https://twitter.com/skotperez))desde [montera34.com](https://montera34.com).

Contact: covid19@montera34.com

En este post [Análisis de propagación de COVID-19 por comunidades autónomas en España](http://numeroteca.org/2020/03/12/covid19-comunidades-autonomas-espana/) recopilamos algunos resultados y reflexiones.


# This repository

This repository lives at https://code.montera34.com:4443/numeroteca/covid19 and is mirrored in [Github](https://github.com/numeroteca/covid19).

If you are using these scripts or data, please let us know at info@montera34.com
