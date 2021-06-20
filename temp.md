ST558 Project 1
================

  - [Packages Required](#packages-required)
  - [Create Functions for Listed
    Endpoint](#create-functions-for-listed-endpoint)
      - [Basic Function for a List from
        API](#basic-function-for-a-list-from-api)
      - [Functions for Listed
        endpoints](#functions-for-listed-endpoints)
          - [Function for Franchise ID](#function-for-franchise-id)
          - [Function for
            Franchise-team-totals](#function-for-franchise-team-totals)
          - [Function to Season Records](#function-to-season-records)
          - [Function for Goalie Records](#function-for-goalie-records)
          - [Function for Skater](#function-for-skater)
          - [Function for Admin History](#function-for-admin-history)
      - [Function for team.stats
        Modifier](#function-for-team.stats-modifier)
      - [Wrapper function](#wrapper-function)
  - [Basic Exploratory Data Analysis
    (EDA)](#basic-exploratory-data-analysis-eda)
      - [Get all the Data](#get-all-the-data)
      - [General Idea about All Franchises (Bar
        plot)](#general-idea-about-all-franchises-bar-plot)
  - [Output to README.md](#output-to-readme.md)

# Packages Required

``` r
library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
```

# Create Functions for Listed Endpoint

``` r
base_url <- 'https://records.nhl.com/site/api'
username <- 'jzhao43@ncsu.edu'
password <- 'OjRlMTZjNDEyNTJjZDViZWU1NmJmODJhY2E1ZTA2YWIx'
```

## Basic Function for a List from API

``` r
getlist <- function(uname = username, pa = password, ep){
  full_url <- paste0(base_url, '/', ep)
  temp_get <- GET(full_url, authenticate(username, password, type = 'basic'))
  temp_json <- content(temp_get, 'text')
  temp_list <- fromJSON(temp_json, flatten = T)
  return(temp_list)
}
```

## Functions for Listed endpoints

### Function for Franchise ID

``` r
getid <- function(un = username, pas = password){
  temp <- getlist(un,pas,ep = 'franchise')
  dat <- temp[[1]] %>%
    select(id, firstSeasonId, fullName)
  return(dat)
}
```

### Function for Franchise-team-totals

``` r
getstat <- function(un = username, pas = password){
  temp <- getlist(un,pas,ep = 'franchise-team-totals')
  dat <- temp[[1]] %>%
  select(!c(id,activeFranchise,gameTypeId,lastSeasonId,teamId, teamName, triCode)) %>%
  group_by(firstSeasonId) %>%
  summarise_each(funs(sum(., na.rm = T)))
  return(dat)
}
```

### Function to Season Records

``` r
getseason <- function(frid){
  temp <- getlist(ep = paste0('franchise-season-records?cayenneExp=franchiseId=',frid))
  return(temp[[1]])
}
```

### Function for Goalie Records

``` r
getGoalie <- function(frid){
  temp <- getlist(ep = paste0('franchise-goalie-records?cayenneExp=franchiseId=',frid))
  return(temp[[1]])
}
```

### Function for Skater

``` r
getSkater <- function(frid){
  temp <- getlist(ep = paste0('franchise-skater-records?cayenneExp=franchiseId=',frid))
  return(temp[[1]])
}
```

### Function for Admin History

``` r
getHis <- function(frid){
  temp <- getlist(ep = paste0('franchise-detail?cayenneExp=mostRecentTeamId=',frid))
  return(temp[[1]])
}
```

## Function for team.stats Modifier

``` r
stat_url <- 'https://statsapi.web.nhl.com/api/v1/teams/'
getteamstat <- function(frid=NA){
  if (is.na(frid)){
    full_url <- stat_url
  } else {
    full_url <- paste0(stat_url,frid, '/?expand=team.stats')
  }
  temp_get <- GET(full_url, authenticate(username, password, type = 'basic'))
  temp_json <- content(temp_get, 'text')
  temp_list <- fromJSON(temp_json, flatten = T)
  return(temp_list[[2]])
}
```

## Wrapper function

``` r
getany <- function(type, frid=NA){
  if (is.na(type)){
    stop('Please input a type from id/stat/season/Goalie/Skater/His/teamstat.')
  }
  tempname <- c('id','stat','season','Goalie','Skater','His','teamstat')
  if (!type %in% tempname) {stop('Wrong type!')}
  if (is.na(frid)){
    if (type=='id'){return(getid())}
    if (type=='stat'){return(getstat())}
    if (type=='teamstat'){return(getteamstat())}
  } else {
    if (type=='season'){return(getseason(frid))}
    if (type=='Goalie'){return(getGoalie(frid))}
    if (type=='Skater'){return(getSkater(frid))}
    if (type=='His'){return(getHis(frid))}
    if (type=='teamstat'){return(getteamstat(frid))}
  }
}
```

# Basic Exploratory Data Analysis (EDA)

## Get all the Data

``` r
allid <- getany('id')
allstat <- getany('stat')

# Write a loop to collect season records for all franchises
allseason <- NA
for (i in allstat$franchiseId){
  temp <- getany('season',i)
  allseason <- rbind(allseason, temp)
}
allseason <- allseason[-1,]

## Write a loop to collect Goalie records for all franchises
allGoalie <- NA
for (i in allstat$franchiseId){
  temp <- getany('Goalie',i)
  allGoalie <- rbind(allGoalie, temp)
}
allGoalie <- allGoalie[-1,]

# Get team-stat
allteam <- getteamstat()
```

## General Idea about All Franchises (Bar plot)

I would like to have a view about every franchises’ performance at home
city. A bar plot should be intuitive enough.

``` r
# Join the Data
Join <- left_join(allid, allstat, by='firstSeasonId') %>%
  distinct(firstSeasonId, .keep_all = T)

# Minus the NA
Join <- Join[!is.na(Join$franchiseId),]

# Reshape the data
bar.plot <- Join %>%
  select(fullName, homeLosses, homeTies, homeWins) %>%
  gather(key = 'result', value = 'value', 2:4)

# Bar plot
ggplot(data = bar.plot) + geom_bar(aes(x=fullName,y=value,fill=result),stat="identity",width=0.5, position = 'dodge') + theme_bw() + coord_flip() + 
  labs(x='', y='Count', title = 'Home Results for all Franchises', fill='Result') + scale_fill_discrete(labels=c('Losses','Ties','Wins'))
```

![](temp_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

From this plot we know that home field advantage does exist. Nearly all
the franchises (except those only participated in a few games) have won
more games than they lost.

# Output to README.md

``` r
# rmarkdown::render('temp.md', output_file = 'README.md')
```
