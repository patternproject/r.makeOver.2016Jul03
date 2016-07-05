## `````````````````````````````````````````````
#### Read Me ####
## `````````````````````````````````````````````
# Make Over Monday 
# 2016 Jul 03 (Wk 27 Submission)
# US Children Well Being across states
## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Load Libraries ####
## `````````````````````````````````````````````
# install devtools
install.packages('devtools')

# dev version of ggplot
# main branch of development
devtools::install_github("hadley/ggplot2")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,ggplot2,tidyr,scales,grid,stringr,rvest)
pacman::p_load(readr)
pacman::p_load(ggalt)
pacman::p_load(rjson)
pacman::p_load(XML)
pacman::p_load(htmltab)
pacman::p_load(extrafont)

## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Constants ####
## `````````````````````````````````````````````
## color codes ####
col.maroon = "#802239"
col.beige = "#C4B7A0"
col.green = "#7C6C54"
col.pink = "#D5A1A2"
col.rust = "#876B42"

f.applyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(x, probs = seq(0, 1, by = 0.20))), 
      labels=c("L","F","T","E","D"), include.lowest=TRUE)
}


## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Read Data ####
## `````````````````````````````````````````````
setwd("D:/2. Bianca/1. Perso/14. MakeoverMonday/25. 2016 Jul 03/r.makeOver.2016Jul03")

## df.master ####
df.master = read.csv(
  "2. Data/US Children Well-Being.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

# setting col names
names(df.master) = df.master %>% names() %>% tolower
# tolower(names(df.master))

#shortening the names
names(df.master) = c("state","overall","economic","education","health","family")
## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Manipulate Data ####
## `````````````````````````````````````````````

df.1 = df.master

## Find Quantiles of all ranks

# for overall
df.1$overall.q = 
  df.1$overall %>% f.applyQuintiles()

# for economic
df.1$economic.q = 
  df.1$economic %>% f.applyQuintiles()

# for education
df.1$education.q = 
  df.1$education %>% f.applyQuintiles()

# for health
df.1$health.q = 
  df.1$health %>% f.applyQuintiles()

# for family
df.1$family.q = 
  df.1$family %>% f.applyQuintiles()


# re arrange col
df.1 = 
  df.1 %>%
  select(
    overall,
    overall.q,
    economic,
    economic.q,
    education,
    education.q,
    health,
    health.q,
    family,
    family.q
  )

## `````````````````````````````````````````````


## `````````````````````````````````````````````
#### Visulaize Data ####
## `````````````````````````````````````````````
## `````````````````````````````````````````````
