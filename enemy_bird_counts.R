###Mexico diversity effects experiment
###data analysis for Bird Diversity observations in diversity plots

library(dplyr)
library(vegan)

setwd("/Users/colleennell/Documents/R/enemies/")
birds<-read.csv("/Users/colleennell/Documents/R/enemies/enemy_bird_visit.csv")

drops<-c("Grand.Total","X.blank.")##clean up df
birds=birds[,!(names(birds) %in% drops)]
View(birds)
birds[is.na(birds)]<-0

cols<-names(birds)
drops<-c("DIVERSITY","PLOT","VISIT")
sps<-cols[-c(1:3)]##this is a list of the names of the birds only


##summarize by plot diversity, mean bird abundance by plot, and n_obs
n_visit<-birds%>%
  filter(DIVERSITY=='P')%>%
  group_by(PLOT)%>%
  summarize(n_visit=length(VISIT))%>%
  select(PLOT,n_visit)


poly<-birds%>%
  filter(DIVERSITY=='P')%>%
  group_by(DIVERSITY,PLOT)%>%
  summarize_each(funs(sum))%>%
  mutate(abun = rowSums(.[sps]))%>%
  left_join(poly,n_obs)
drops<-c("VISIT")
poly=poly[,!(names(poly)%in%drops)]
View(poly)

##same for monoculture plots
mono<-birds%>%
  filter(DIVERSITY=='M')%>%
  group_by(DIVERSITY,PLOT)%>%
  summarize_each(funs(mean))
drops<-c("VISIT")
mono=mono[,!(names(mono)%in%drops)]
View(mono)

###What is the mean 


