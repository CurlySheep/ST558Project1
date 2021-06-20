ST558 Project 1
================

# Github Pages

[Click here\!](https://curlysheep.github.io/ST558Project1/)

# Packages Required to Get the API

``` r
library(httr)
library(jsonlite)
library(tidyverse)
```

# Create Functions

``` r
base_url <- 'https://records.nhl.com/site/api'
username <- 'jzhao43@ncsu.edu'
password <- 'OjRlMTZjNDEyNTJjZDViZWU1NmJmODJhY2E1ZTA2YWIx'
```

## Basic Function to Get a List from API

``` r
getlist <- function(uname = username, pa = password, ep){
  full_url <- paste0(base_url, '/', ep)
  temp_get <- GET(full_url, authenticate(username, password, type = 'basic'))
  temp_json <- content(temp_get, 'text')
  temp_list <- fromJSON(temp_json, flatten = T)
  return(temp_list)
}
```

## Function to Get Franchise

``` r
getid <- function(un = username, pas = password){
  temp <- getlist(un,pas,ep = 'franchise')
  dat <- temp[[1]] %>%
    select(id, firstSeasonId, fullName)
  return(dat)
}
franchise <- getid()
franchise
```

    ##    id firstSeasonId              fullName
    ## 1   1      19171918    Montréal Canadiens
    ## 2   2      19171918    Montreal Wanderers
    ## 3   3      19171918      St. Louis Eagles
    ## 4   4      19191920       Hamilton Tigers
    ## 5   5      19171918   Toronto Maple Leafs
    ## 6   6      19241925         Boston Bruins
    ## 7   7      19241925      Montreal Maroons
    ## 8   8      19251926    Brooklyn Americans
    ## 9   9      19251926  Philadelphia Quakers
    ## 10 10      19261927      New York Rangers
    ## 11 11      19261927    Chicago Blackhawks
    ## 12 12      19261927     Detroit Red Wings
    ## 13 13      19671968      Cleveland Barons
    ## 14 14      19671968     Los Angeles Kings
    ## 15 15      19671968          Dallas Stars
    ## 16 16      19671968   Philadelphia Flyers
    ## 17 17      19671968   Pittsburgh Penguins
    ## 18 18      19671968       St. Louis Blues
    ## 19 19      19701971        Buffalo Sabres
    ## 20 20      19701971     Vancouver Canucks
    ## 21 21      19721973        Calgary Flames
    ## 22 22      19721973    New York Islanders
    ## 23 23      19741975     New Jersey Devils
    ## 24 24      19741975   Washington Capitals
    ## 25 25      19791980       Edmonton Oilers
    ## 26 26      19791980   Carolina Hurricanes
    ## 27 27      19791980    Colorado Avalanche
    ## 28 28      19791980       Arizona Coyotes
    ## 29 29      19911992       San Jose Sharks
    ## 30 30      19921993       Ottawa Senators
    ## 31 31      19921993   Tampa Bay Lightning
    ## 32 32      19931994         Anaheim Ducks
    ## 33 33      19931994      Florida Panthers
    ## 34 34      19981999   Nashville Predators
    ## 35 35      19992000         Winnipeg Jets
    ## 36 36      20002001 Columbus Blue Jackets
    ## 37 37      20002001        Minnesota Wild
    ## 38 38      20172018  Vegas Golden Knights
    ## 39 39      20212022        Seattle Kraken

## Function to Get Franchise-team-totals

``` r
getstat <- function(un = username, pas = password){
  temp <- getlist(un,pas,ep = 'franchise-team-totals')
  dat <- temp[[1]] %>%
  select(!c(id,activeFranchise,gameTypeId,lastSeasonId,teamId, teamName, triCode)) %>%
  group_by(firstSeasonId) %>%
  summarise_each(funs(sum(., na.rm = T)))
  return(dat)
}

franstat <- getstat()
franstat
```

    ## # A tibble: 31 x 23
    ##    firstSeasonId franchiseId gamesPlayed goalsAgainst goalsFor homeLosses
    ##            <int>       <int>       <int>        <int>    <int>      <int>
    ##  1      19171918          20        8196        21902    25864       1111
    ##  2      19191920          14         265          970      838         49
    ##  3      19201921           4         126          475      414         30
    ##  4      19241925          26        7973        22528    24616       1230
    ##  5      19251926          34         970         2580     1926        213
    ##  6      19261927          66       14364        43210    42903       2522
    ##  7      19271928          10        7061        21444    21378       1202
    ##  8      19301931          33         138          400      274         27
    ##  9      19321933          24        6911        20446    21295       1066
    ## 10      19341935           3          48          144       86         14
    ## # ... with 21 more rows, and 17 more variables: homeOvertimeLosses <int>,
    ## #   homeTies <int>, homeWins <int>, losses <int>, overtimeLosses <int>,
    ## #   penaltyMinutes <int>, pointPctg <dbl>, points <int>, roadLosses <int>,
    ## #   roadOvertimeLosses <int>, roadTies <int>, roadWins <int>,
    ## #   shootoutLosses <int>, shootoutWins <int>, shutouts <int>, ties <int>,
    ## #   wins <int>

# Output to README.md

``` r
# rmarkdown::render('temp.md', output_file = 'README.md')
```
