# Telefonía fija vs movil

![](https://images.unsplash.com/photo-1525598912003-663126343e1f?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1350&q=80)

# Análisis de la implemetación de la telefonía fija vs mobil

Esta información del TT proviene de [OurWorldInData.org](https://ourworldindata.org/technology-adoption).

> Hannah Ritchie (2017) - "Technology Adoption". Publicado online en OurWorldInData.org. Obtenido de: 'https://ourworldindata.org/technology-adoption' [Online Resource]

[Articulo relacionado](https://www.pewresearch.org/global/2019/02/05/smartphone-ownership-is-growing-rapidly-around-the-world-but-not-always-equally/)

### La data se puede encontrar acá

```{r}
# se puede leer con el paquete de tidytuesdayR  
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2020-11-10')
tuesdata <- tidytuesdayR::tt_load(2020, week = 46)

mobile <- tuesdata$mobile

# O se puede cargar manualmente

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

```
### Data Dictionary

# `mobile.csv`

|variable                           |class     |description |
|:----------------------------------|:---------|:-----------|
|entity                             |character |Country         |
|code                               |character | Country code |
|year                               |double    | Year |
|total_pop |double    | Gapminder total population |
|gdp_per_cap                        |double    | GDP per capita, PPP (constant 2011 international $) |
|mobile_subs                      |double    | Fixed mobile subscriptions (per 100 people)|
|continent                          |character | Continent |

# `landline.csv`

|variable                           |class     |description |
|:----------------------------------|:---------|:-----------|
|entity                             |character |Country         |
|code                               |character | Country code |
|year                               |double    | Year |
|total_pop |double    | Gapminder total population |
|gdp_per_cap                        |double    | GDP per capita, PPP (constant 2011 international $) |
|landline_subs                      |double    | Fixed telephone subscriptions (per 100 people)|
|continent                          |character | Continent |

