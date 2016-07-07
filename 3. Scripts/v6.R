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
#col.grey = "#8C8C8C"
col.grey = "grey80"

f.applyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(x, probs = seq(0, 1, by = 0.20))), 
      labels=c("L","F","T","E","D"), include.lowest=TRUE)
}
# setting font ####
# src: http://www.r-bloggers.com/change-fonts-in-ggplot2-and-create-xkcd-style-graphs/
font_import(pattern="[C/c]omic")
fonts()
fonttable()
loadfonts(device="win")
f.family="Comic Sans MS"
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
### prep for g.3 ####

# tidy up date before plotting
df.2 = df.1

# removing education, health and family cols
df.2 = 
  df.2 %>%
  select(-education, -health, -family)

# gather the last three value to make facets
df.3 = 
  df.2 %>%
  gather(metric,value,education.q:family.q)

df.3$value = as.factor(df.3$value)

# reorder factor levels
# src: http://www.r-bloggers.com/reorder-factor-levels-2/
df.3$value = factor(df.3$value,levels(df.3$value)[c(4,3,5,2,1)])

# facet labels
# src: http://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels
facet_names <- c(
  `education.q` = "Education",
  `family.q` = "Family",
  `health.q` = "Health"
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


#g.2 = g.2 + scale_colour_brewer(palette="RdBu",name = "Overall Index",labels = c("Leaders : L","Fast Transitioners : F","Transitioners : T","Emerging : E","Discoverers : D")) 
g.2 = g.2 + scale_colour_brewer(palette="RdYlBu",name = "Overall Index",labels = c("Leaders : L","Fast Transitioners : F","Transitioners : T","Emerging : E","Discoverers : D")) 

#g.2 = g.2 + theme(panel.background = element_rect(fill = "#929AAF"))
#g.2 = g.2 + theme(panel.background = element_rect(fill = "#969696"))


g.2 = g.2 + theme (
  #panel.background = element_blank(),
  #panel.background = element_rect(fill = "#969696"),
  #panel.background = element_rect(fill = "#BCBCBC"),
  #panel.background = element_rect(fill = "#C9C9C9"),
  #panel.background = element_rect(fill = "#8C8C8C"),
  panel.background = element_rect(fill = col.grey),
  
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
  
  axis.text.x = element_text(margin=margin(-10,0,-10,0),family=f.family),
  axis.text.y = element_text(margin=margin(0,-10,0,-10),family=f.family), # perfect
  
  
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
  plot.title=element_text(family=f.family),
  plot.subtitle=element_text(size=9.5, margin=margin(b=10),family=f.family),
  plot.caption=element_text(size=7, margin=margin(t=10),family=f.family),
  # margin around entire plot ('unit' with the sizes of the top, right, bottom, and left
  # margins)
  plot.margin=margin(10,10,10,10),
  
  
  
  #legend.background = element_rect(fill = "white"),
  legend.position = c(0.10, 0.85),
  legend.key = element_rect(fill = "black"),
  legend.key.width = unit(0.1, "in"),
  legend.key.height = unit(0.1, "in"),
  legend.direction = "vertical",
  # Title appearance
  legend.title = element_text(family=f.family),
  # Label appearance
  legend.text = element_text(family=f.family)
  
)

# vertical grid line
#g.2 = g.2 + geom_vline(xintercept=which(df.1$economic.q == 'T'))
#g.2 = g.2 + geom_vline(xintercept=median(which(df.1$economic.q == 'T')),col="white")
# http://stackoverflow.com/questions/1831245/vertical-gridlines-in-ggplot-with-discrete-x-axis
g.2 = g.2 + geom_vline(xintercept=which(levels(df.1$economic.q) %in% c("T")),col="black",linetype="dotted")

# horizontal grid line
g.2 = g.2 + geom_hline(yintercept=which(levels(df.1$education.q) %in% c("T")),col="black",linetype="dotted")

# annotations
# df.1$economicq=as.numeric()

g.2 <-
  g.2 + geom_label(
    data = data.frame(),
    hjust = 0,
    label.size = 0,
    size = 3,
    family=f.family,
    #fill = col.grey
    #aes(x = 2.5, y = 1, label = "Leaders on all three scales \nEconomic on X-Axis \nEducation on Y-Axis \nOverall () \nare concentrated on lower left.\nExact opposite for Discoverers \n")
    aes(x = 2.5, y = 1, label = "LEADERs on all 3 scales are concentrated on lower left. \n1) Quantiles of Economic Rank on X-Axis, 2) Education on Y-Axis and \n3) Overall Rank is color coded in RED. Exact opposite for DISCOVERERS, \nor those on lowest end of scale - color coded in BLUE")
  )


# for red dots
g.2 <- g.2 + geom_curve(data=data.frame(), aes(x=2.5, xend=1, y=1, yend=1.25),
                      curvature=-0.25, arrow=arrow(length=unit(0.03, "npc")))



# for blue dots
g.2 <- g.2 + geom_curve(data=data.frame(), aes(x=4.25, xend=4, y=1.65, yend=4.75),
                        curvature=-0.25, arrow=arrow(length=unit(0.03, "npc")))



g.2 <- g.2 + labs(title="Does Education power Economic Outcome ?",
                subtitle="Quantiles of Education Rank, on Y-axis VERSUS Quantiles of Economic Rank, on X-axis",
                caption="Source: cnn.com")


g.2

### plotting g.3 ####

## g.3 ####

g.3 = ggplot() + theme_minimal()

g.3 = g.3 + geom_jitter(data=df.3,aes(x=economic.q,y=value,col=overall.q))

#g.3 = g.3 + scale_colour_brewer(palette="RdYlBu",name = "Overall Index",labels = c("Leaders : L","Fast Transitioners : F","Transitioners : T","Emerging : E","Discoverers : D")) 

g.3 = g.3 + scale_colour_brewer(palette="RdYlBu") 

g.3 = g.3 + facet_grid(.~metric, labeller = as_labeller(facet_names))

g.3 = g.3 + theme (
  panel.background = element_rect(fill = col.grey),
  
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
  
  axis.text.x = element_text(margin=margin(-11,0,-11,0),family=f.family),
  axis.text.y = element_text(margin=margin(0,-10,0,-10),family=f.family), # perfect
  
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  
  # src:
  # https://rud.is/b/2016/06/16/your-data-vis-spidey-sense-the-need-for-a-robust-utility-belt/
  plot.title=element_text(family=f.family),
  plot.subtitle=element_text(size=9.5, margin=margin(b=10),family=f.family),
  plot.caption=element_text(size=7, margin=margin(t=12),family=f.family),
  # margin around entire plot ('unit' with the sizes of the top, right, bottom, and left
  # margins)
  plot.margin=margin(10,10,10,10),
  
  legend.title = element_blank() # removing legend
  
)

# vertical grid line
g.3 = g.3 + geom_vline(xintercept=which(levels(df.3$economic.q) %in% c("T")),col="black",linetype="dotted")

# horizontal grid line
g.3 = g.3 + geom_hline(yintercept=which(levels(df.3$value) %in% c("T")),col="black",linetype="dotted")

g.3 <- g.3 + labs(title="Besides Education, Family and Health power Economic Outcome too",
                  subtitle="Quantiles Ranks of Various Metrics, on Y-axis VERSUS Quantiles of Economic Rank, on X-axis",
                  caption="Source: cnn.com")


g.3


## `````````````````````````````````````````````

# TODO: Unable to add Parenthesis to Labels. 