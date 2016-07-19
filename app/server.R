library(shiny)
library(ggplot2)
library(cowplot)
library(reshape2)
library(plotly)
library(dplyr)
library(labdsv)

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
bird.sp<-birds[,sps]#matrix plot x sp


##summarize data for plots/analyses
#by plot
#abundance
bird.plot<-birds%>%
  mutate(birds_plot_visit=rowSums(.[sps]))%>%
  group_by(DIVERSITY,PLOT)%>%
  summarize(total_birds_plot=sum(birds_plot_visit),
            n_visit=length(VISIT),
            mean_birds_visit=mean(birds_plot_visit),
            se_birds_visit=std(birds_plot_visit))

bird.div<-bird.plot%>%
  group_by(DIVERSITY)%>%
  summarize(total_birds_div=sum(total_birds_plot),
            n_plots=length(PLOT),
            mean_birds_plot_visit=sum(total_birds_plot)/sum(n_visit),
            se_birds_plot_visit=std(total_birds_plot))

birds.melt<-melt(birds,id.vars=c("DIVERSITY","PLOT","VISIT","total_abun"),variable.name="ID")%>%
  left_join(sp.list[,c("ID","feeding.guild","Family","Order")],by="ID")

insect.birds<-birds.melt%>%
  filter(feeding.guild %in% c("IN","FL","OM"))

ins.plot<-insect.birds%>%
  group_by(DIVERSITY,PLOT,VISIT)%>%
  summarize(total_birds_plot_visit=sum(value))%>%
  group_by(DIVERSITY,PLOT)%>%
  summarize(total_birds_plot = sum(total_birds_plot_visit),
            mean_birds_visit = mean(total_birds_plot_visit),
            n_visit=as.numeric(max(VISIT)),
            se_birds_visit = std(total_birds_plot_visit))

div.guild<-birds.melt%>%###this is the total number of birds~diversity+feeding guild
  group_by(DIVERSITY,feeding.guild)%>%
  summarize(total_birds = sum(value))

ins.guild<-insect.birds%>%###this is the total number of insecitvorous birds~diversity+feeding guild
  group_by(DIVERSITY,feeding.guild)%>%
  summarize(total_birds = sum(value))


shinyServer(function(input, output) {
  output$guildplot<-renderPlot({
    data<-reactive({switch(input$guild)})
    guilddiv<-ggplot(data,aes(y=data[,"total_birds"],x=reorder(data[,"feeding.guild"],data[,"total_birds"]),fill=data[,"DIVERSITY"]))+
      geom_bar(stat="identity")+
      scale_fill_brewer("Plot Diversity",labels=c("Monoculture","Polyculture"),palette="Paired")+
      theme_minimal()+theme(legend.position="top")+labs(x="Feeding Guild",y="Total Birds Observed")
    abun_by_plot<-ggplot(bird.plot,aes(x=reorder(PLOT,mean_birds_visit),y=mean_birds_visit,fill=DIVERSITY))+
      geom_bar(stat="identity")+
      geom_errorbar(aes(ymin=mean_birds_visit-se_birds_visit,ymax=mean_birds_visit+se_birds_visit))+
      scale_fill_brewer("Plot Diversity",labels=c("Monoculture","Polyculture"),palette="Paired")+
      theme_minimal()+
      theme(legend.position="none")+
      labs(x="Plot",y="Birds Observed/Visit")
    plot_grid(guilddiv,abun_by_plot,nrow=2)
  })
})
