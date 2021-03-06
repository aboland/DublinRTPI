DublinRTPI
================

<!-- This is created using README.Rmd, edit that file -->

[![Build
Status](https://travis-ci.org/aboland/DublinRTPI.svg?branch=master)](https://travis-ci.org/aboland/DublinRTPI)
[![codecov](https://codecov.io/gh/aboland/DublinRTPI/branch/master/graph/badge.svg)](https://codecov.io/gh/aboland/DublinRTPI)

Real-time Passenger Information for Dublin city’s public transport.

Code to retrieve real time information about Dublin Bus, Luad, and Dart
services. Allows the aggregation of multiple bus stops.

-----

## Shiny App

The code located in [ShinyApp](/ShinyApp) is an interactive app which
will retrieve and display information about the bus and train services
in Dublin.

A live version of the app should be running at
<https://aboland.shinyapps.io/DublinTransport/>

#### Custom URL

A custom url can be used to pre-load the application with your choices
of stops and buses. This can be bookmarked to save time.  
Example:
<http://aboland.shinyapps.io:/DublinTransport/?stops=334,336&routes=14,140>

### Run Locally

The Shiny app can be run locally from within R or R Studio. *The
[shiny](https://shiny.rstudio.com/) library must be installed.*

``` r
# install.packages("shiny")
shiny::runGitHub("aboland/DublinRTPI", subdir = "ShinyApp")
```

#### Other Dependencies

The app also relies on the following packages,
[`dplyr`](https://dplyr.tidyverse.org/),
[`stringr`](https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html),
[`XML`](https://cran.r-project.org/web/packages/XML/index.html), and
[`rvest`](https://cran.r-project.org/web/packages/rvest/).

-----

## R Package

The code located in [Rlib](/Rlib) contains an R package named
`dublinRTPI`. This can be installed using the `devtools` package.

The package contains functions to retrieve live info for Dart and Dublin
Bus. A light version of the main shiny app is also included in the
package.

``` r
# install.packages("devtools")
devtools::install_github("aboland/DublinRTPI", subdir = "Rlib")

 # Get info about bus stop number 334
dublinRTPI::db_info(334)

 # Run shiny app
dublinRTPI::runShiny()
```
