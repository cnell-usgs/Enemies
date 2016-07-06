library(ggplot2)
library(reshape2)
library(dplyr)
library(shiny)

birds<-read.csv("/Users/colleennell/Documents/R/enemies/enemy_bird_visit.csv")
sp.list<-read.csv("/Users/colleennell/Documents/R/enemies/enemy_bird_list.csv")
std <- function(x) sd(x)/sqrt(length(x))

##clean data 
drops<-c("Grand.Total","X.blank.")##clean up df
birds=birds[,!(names(birds) %in% drops)]
birds[is.na(birds)]<-0
cols<-names(birds)
drops<-c("DIVERSITY","PLOT","VISIT")
sps<-cols[-c(1:3)]##this is a list of the names of the birds only
birds$VISIT<-as.character(birds$VISIT)
birds$total_abun<-rowSums(birds[,sps])
##summarize data for plots/analyses
#by plot
#abundance
bird.plot<-birds%>%
  mutate(birds_plot_visit=rowSums(.[sps]))%>%
  group_by(DIVERSITY,PLOT)%>%
  summarize(total_birds_plot=sum(birds_plot_visit),
            n_visit=length(VISIT),
            mean_birds_visit=mean(birds_plot_visit),
            se_birds_visit=std(birds_plot_visit),
            birds_30=mean_birds_visit*3,
            se_30=std(birds_plot_visit)*3)



#by diversity

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot(function() {
    bun_by_plot<-ggplot(bird.plot,aes(x=reorder(PLOT,mean_birds_visit),y=mean_birds_visit,fill=DIVERSITY))+
      geom_bar(stat="identity")+labs(x="Plot",y="Birds/30/plot")

    
    bun_by_plot
  })
})
