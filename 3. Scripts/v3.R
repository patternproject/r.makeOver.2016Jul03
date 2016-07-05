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

## g.1 ####
g.1 = ggplot() + theme_minimal()

g.1 = g.1 + geom_point(data=df.1,aes(x=economic.q,y=education.q,col=overall.q))

# g.1 = g.1 + scale_color_manual(values = c(
#   "L" = col.maroon,
#   "F" = col.beige,
#   "T" = col.green,
#   "E" = col.pink,
#   "D" = col.rust
# ))

# http://is-r.tumblr.com/post/34821021257/ggtutorial-day-5-gradient-colors-and-brewer
# g.1 = g.1 + scale_color_gradientn(colours = terrain.colors(5))
g.1 = g.1 + scale_colour_brewer(palette="RdBu") 

g.1


## g.2 ####
g.2 = ggplot() + theme_minimal()

#g.2 = g.2 + geom_jitter(data=df.1,aes(x=economic.q,y=education.q,col=overall.q),position=position_jitter(0.2))
#g.2 = g.2 + geom_jitter(data=df.1,aes(x=economic.q,y=education.q,col=overall.q),position=position_jitter(0.5))
g.2 = g.2 + geom_jitter(data=df.1,aes(x=economic.q,y=education.q,col=overall.q))


g.2 = g.2 + scale_colour_brewer(palette="RdBu",name = "Overall Index",labels = c("Leaders","Fast Transitioners","Transitioners","Emerging","Discoverers")) 

#g.2 = g.2 + theme(panel.background = element_rect(fill = "#929AAF"))
#g.2 = g.2 + theme(panel.background = element_rect(fill = "#969696"))


g.2 = g.2 + theme (
  #panel.background = element_blank(),
  #panel.background = element_rect(fill = "#969696"),
  #panel.background = element_rect(fill = "#BCBCBC"),
  #panel.background = element_rect(fill = "#C9C9C9"),
  panel.background = element_rect(fill = "#8C8C8C"),
  
  # http://docs.ggplot2.org/dev/vignettes/themes.html
  panel.border = element_rect(fill = NA, colour = "#8C8C8C", size = 1),
  
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  
  #plot.caption=element_text(size=8, margin=margin(t=24),
  
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  #axis.ticks.margin = element_blank(),
  
  axis.text.x = element_text(margin=margin(-10,0,-10,0)),
  axis.text.y = element_text(margin=margin(0,-10,0,-10)), # perfect
  
  
  #axis.title.x = element_text(margin=margin(t=-5),vjust=1),
  #axis.title.x = element_text(margin=margin(0,0,-5,0), vjust=-5),
  #axis.title.x = element_text(margin=margin(t=-20)),
  #axis.title.x = element_text(vjust=1),
  #axis.title.x = element_text(hjust=1),
  #axis.title.x = element_text(hjust=0),
  #axis.title.x = element_text(margin=margin(0,0,0,0)),
  #axis.title.x = element_text(margin=margin(-5,0,-5,0), vjust=-5),
  #axis.title.x = element_text(margin=margin(-5,0,-5,0), vjust=-1),
  #axis.title.x = element_text(margin=margin(-8,0,0,0)),
  axis.title.x = element_blank(),
  
  #axis.title.y = element_text(margin=margin(l=-5), hjust=1),
  #axis.title.y = element_text(size = 8),
  #axis.title.y = element_text(hjust=1),
  #axis.title.y = element_text(vjust=1),
  #axis.title.y = element_text(margin=margin(0,-8,0,0)),
  axis.title.y = element_blank(),
  
  # src:
  # https://rud.is/b/2016/06/16/your-data-vis-spidey-sense-the-need-for-a-robust-utility-belt/
  plot.subtitle=element_text(size=9.5, margin=margin(b=10)),
  plot.caption=element_text(size=7, margin=margin(t=10)),
  # margin around entire plot ('unit' with the sizes of the top, right, bottom, and left
  # margins)
  plot.margin=margin(10,10,10,10),
  
  
  
  #legend.background = element_rect(fill = "white"),
  legend.position = c(0.10, 0.9),
  legend.key = element_rect(fill = "black"),
  legend.key.width = unit(0.1, "in"),
  legend.key.height = unit(0.1, "in"),
  legend.direction = "vertical"
  
  
)



g.2


## `````````````````````````````````````````````
