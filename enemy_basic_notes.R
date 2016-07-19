##other got messy

library(shiny)
library(ggplot2)
library(cowplot)
library(reshape2)
library(plotly)
library(dplyr)
library(labdsv)
###enter data
birds<-read.csv("enemy_bird_visit.csv")
sp.list<-read.csv("enemy_bird_list.csv")

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

#Bird Observations  
###All Birds  
div.guild<-birds.melt%>%###this is the total number of birds~diversity+feeding guild
  group_by(DIVERSITY,feeding.guild)%>%
  summarize(total_birds = sum(value))

guilddiv<-ggplot(div.guild,aes(y=total_birds,x=reorder(feeding.guild,total_birds),fill=DIVERSITY))+
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

plot_grid(guilddiv,abun_by_plot)


#Duplicate these figures for species richness?  

**`r length(unique(birds.melt$PLOT))` plots** visited 4x each   
**`r sum(birds.melt$value)` bird observations** of **`r length(unique(birds.melt$ID))` species** from **`r length(unique(birds.melt$Family))` families**(`r sort(unique(birds.melt$Family))`) and **`r length(unique(birds.melt$Order))` orders**(`r sort(unique(birds.melt$Order))`)  

Guilds: Granivorous (GR), Carnivorous (CA), Nectivorous (NE), Frugivorous (FR), Insectivorous (FL = flycatching, IN= gleaning), Omnivorous (OM), NA = unidentified  

###Insectivorous Birds   
#(including gleaners, flycatchers, and omnivores that consume arthropods)  


ins.guild<-insect.birds%>%###this is the total number of insecitvorous birds~diversity+feeding guild
  group_by(DIVERSITY,feeding.guild)%>%
  summarize(total_birds = sum(value))

guilddiv<-ggplot(ins.guild,aes(y=total_birds,x=reorder(feeding.guild,total_birds),fill=DIVERSITY))+
  geom_bar(stat="identity")+
  scale_fill_brewer("Plot Diversity",labels=c("Monoculture","Polyculture"),palette="Paired")+
  theme_minimal()+theme(legend.position="top")+labs(x="Feeding Guild",y="Total Birds Observed")

abun_by_plot<-ggplot(ins.plot,aes(x=reorder(PLOT,mean_birds_visit),y=mean_birds_visit,fill=DIVERSITY))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean_birds_visit-se_birds_visit,ymax=mean_birds_visit+se_birds_visit))+
  scale_fill_brewer("Plot Diversity",labels=c("Monoculture","Polyculture"),palette="Paired")+
  theme_minimal()+
  theme(legend.position="none")+
  labs(x="Plot",y="Birds Observed/Visit")

plot_grid(guilddiv,abun_by_plot)


**`r sum(insect.birds$value)` observations** of **`r length(unique(insect.birds$ID))` species** from **`r length(unique(insect.birds$Family))` families** (`r sort(unique(insect.birds$Family))`) and **`r length(unique(insect.birds$Order))` orders**(`r sort(unique(insect.birds$Order))`)  

compareins<-left_join(bird.plot,ins.plot,by="PLOT")
View(compareins)
ins.by.total<-lm(mean_birds_visit.x~mean_birds_visit.y,data=compareins)
p<-summary(ins.by.total)$coefficients[,4]
r2<-summary(ins.by.total)$r.squared

typeplot<-ggplot(compareins,aes(x=mean_birds_visit.x,y=mean_birds_visit.y,color=DIVERSITY.x))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=mean_birds_visit.y-se_birds_visit.y,ymax=mean_birds_visit.y+se_birds_visit.y))+
  geom_errorbarh(aes(xmin=mean_birds_visit.x-se_birds_visit.x,xmax=mean_birds_visit.x+se_birds_visit.x))+
  theme_minimal()+
  scale_color_brewer(palette="Paired")+
  theme(legend.position="none")+
  labs(x="Birds/Visit",y="Insectivores/Visit")
typeplot


###Exploratory Analysis  
The following figures look at characteristics of the bird observation data, and the relationships between species abundance, occurance, and sampling effort.  
```{r,EDA}
spc_pres<-apply(bird.sp>0,2,sum)
par(mfrow=c(2,2))
occur<-plot(sort(spc_pres),log='y',main="Species by plot",xlab='Cumulative Count of Species',ylab='Number of Plots')#cumulative plot of species(x) by plot(y)
##cumulative distribution of species occuranes
histo<-hist(log(spc_pres),10,main="Species Occurrance Histogram",xlab="Species Occurrances (log)")#histogram

tmp<-apply(bird.sp,2,sum)
spc_mean<-tmp/spc_pres #avg cover for each species
cumsp<-plot(sort(spc_mean),main="Cumulative Species Abundance",
            xlab="Cumulative Number of Species", ylab="Mean Abundance")

##is  mean abun of sp correlated with # of plots occur in?
abunplotcorr<-plot(spc_pres,spc_mean,main="Abundance vs Occurance",xlab="Occurrance",ylab="Mean Abundance")
#yes

#is total abundance correlated with number of species?
plt_pres<-apply(bird.sp>0,1,sum)
abuncorsp<-plot(sort(plt_pres),main="Richness by effort",xlab="Plots Visited",ylab="Number of Species/Plot")
#yes

##total abun on each plot
plt_sum<-apply(bird.sp,1,sum)##calculate total abundance for each plot
abunbyrich<-plot(sort(plt_sum),main="Abundance by effort",xlab="Plots Visited",ylab="Abundance/Plot")

##relationship between number of species/plot and total abundance
spabund<-plot(plt_pres,plt_sum,main="Abundance & Richness",xlab="Number of Species/Plot",ylab="Total Abundance")##number of species/plot, total abundance
```

The average species is found in `r round(mean(tmp/spc_pres),2)` plots  
The mean # of species per plot is `r round(mean(plt_pres),2)`    
The mean abundance per plot is `r round(mean(plt_sum),2)`  

### Diversity Effects  
Does plot diversity mediate (plot is replicate)  
<br>  
  **Bird abundance ** 
  ```{r,abunbydiv}
abun.by.div<-aov(lm(mean_birds_visit~DIVERSITY,data=bird.plot))
summary(abun.by.div)
```

There is something wrong with this figure- the stats and the plot are using different data- redo  

```{r,fig.height=4, fig.width=3}
div_abun_plot<-ggplot(bird.div,aes(x=DIVERSITY,y=mean_birds_plot_visit,color=DIVERSITY))+
  geom_point(size=10,shape=1)+
  geom_errorbar(aes(ymin=mean_birds_plot_visit-se_birds_plot_visit,ymax=mean_birds_plot_visit+se_birds_plot_visit),color="black",width=.1)+
  labs(x="Tree Diversity",y="Mean birds/visit")+
  theme_minimal()+
  theme(legend.position="none")
div_abun_plot+geom_point(data=bird.plot,aes(x=DIVERSITY,y=mean_birds_visit),size=2)