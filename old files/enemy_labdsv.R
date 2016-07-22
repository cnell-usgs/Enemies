library(shiny)
library(ggplot2)
library(reshape2)
library(plotly)
library(dplyr)
library(flexdashboard)
###enter data
setwd("/Users/colleennell/Documents/R/enemies")
birds<-read.csv("enemy_bird_visit.csv")
sp.list<-read.csv("enemy_bird_list.csv")

#functions
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
View(bird.plot)
birds.melt<-melt(birds,id.vars=c("DIVERSITY","PLOT","VISIT","total_abun"),variable.name="ID")%>%
  left_join(sp.list[,c("ID","feeding.guild","Family","Order")],by="ID")
insect.birds<-birds.melt%>%
  filter(feeding.guild %in% c("IN","FL","OM"))
View(insect.birds)
ins.plot<-insect.birds%>%
  group_by(DIVERSITY,PLOT,VISIT)%>%
  summarize(total_birds_plot_visit=sum(value))%>%
  group_by(DIVERSITY,PLOT)%>%
  summarize(total_birds_plot = sum(total_birds_plot_visit),
            mean_birds_plot = mean(total_birds_plot_visit),
            n_visit=as.numeric(max(VISIT)),
            se_birds_plot = std(total_birds_plot_visit))
View(ins.plot)
str(ins.plot)

bird.div<-bird.plot%>%
  group_by(DIVERSITY)%>%
  summarize_each(funs(sum))
bird.div<-bird.plot%>%
  group_by(DIVERSITY)%>%
  summarize(total_birds_div=sum(total_birds_plot),
            n_plots=length(PLOT),
            mean_birds_plot_visit=sum(total_birds_plot)/sum(n_visit),
            se_birds_plot_visit=std(total_birds_plot))

compareins<-left_join(bird.plot,ins.plot,by="PLOT")
View(bird.div)
birds.melt<-melt(birds,id.vars=c("DIVERSITY","PLOT","VISIT","total_abun"),variable.name="ID")%>%
  left_join(sp.list[,c("ID","feeding.guild","Family","Order")],by="ID")
View(birds.melt)
birds.melt.guild<-birds.melt%>%
  group_by(feeding.guild,Family)%>%
  summarize(obs=sum(value))
View(birds.melt.guild)
#need to sort data by bird observations
#then create ymax and ymin using the cumSum
bird.guild<-birds.melt.guild%>%
  group_by(feeding.guild)%>%
  summarize(obs_total = sum(obs))
bird.guild$ordered<-c(2,6,4,1,8,3,7,5)
View(bird.guild)
guilding<-left_join(birds.melt.guild,bird.guild[,c('feeding.guild','ordered')],by='feeding.guild')
View(guilding)

guildsort<-guilding[order(guilding$ordered),]
View(guildsort)
guildsort$ymax<-cumsum(guildsort$obs)
guildsort$ymin<-guildsort$ymax-guildsort$obs
guildsort2<-guildsort[order(guildsort$ordered,guildsort$obs),]
guildsort2$famord<-1:25
guildsort2$famord<-as.ordered(guildsort2$famord)
guildsort2$ordered<-as.ordered(guildsort2$ordered)
View(guildsort2)
str(guildsort2)
##final plot of bird observations by feeding guild/family
obs.sum.plot<-ggplot(guildsort2)+
  geom_rect(aes(group=reorder(Family,famord),fill=feeding.guild, ymax=ymax,ymin=ymin,xmax=4,xmin=3),alpha=.7)+
  geom_rect(aes(group=reorder(feeding.guild,ordered),fill=feeding.guild,ymax=ymax,ymin=ymin,xmax=3,xmin=0))+
  xlim(c(0,4))+
  labs(x="")+
  theme(aspect.ratio=1)+
  coord_polar(theta="y")+
  theme_minimal()+
  scale_fill_brewer("Feeding Guild",labels=c("Carnivore","Insectivore (aerial)","Frugivore","Granivore","Insectivore (gleaning)","Nectivore","Omnivore"),palette="Set2",direction=-1)
obs.sum.plot
##this is really good except the lines through the middle pie suck
famcol<-c()
View(bird.guild)
obs.sum.plot<-ggplot()+
  geom_rect(guildsort2,aes(group=Family ymax=ymax,ymin=ymin,xmax=4,xmin=3),alpha=.7)+
  geom_rect(bird.guild,aes(group=feeding.guild,fill=feeding.guild,ymax=ymax,ymin=ymin,xmax=3,xmin=0))+
  xlim(c(0,4))+
  labs(x="")+
  theme(aspect.ratio=1)+
  coord_polar(theta="y")+
  theme_minimal()+
  scale_fill_brewer("Feeding Guild",labels=c("Carnivore","Insectivore (aerial)","Frugivore","Granivore","Insectivore (gleaning)","Nectivore","Omnivore"),palette="Set2",direction=-1)
obs.sum.plot
###to fix this change to percentages of lets do a stacked bar by diversity
View(birds.melt)
div.guild<-birds.melt%>%###this is the total number of birds~diversity+feeding guild
  group_by(DIVERSITY,feeding.guild)%>%
  summarize(total_birds = sum(value))
guilddiv<-ggplot(div.guild,aes(y=total_birds,x=reorder(feeding.guild,total_birds),fill=DIVERSITY))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()
guilddiv
guildtotal<-birds.melt%>%
  group_by(feeding.guild)%>%
  summarize(birds=sum(value))
View(guildtotal)
guildpie<-ggplot(guildtotal)+
  geom_bar(aes(y=birds,fill=feeding.guild))+coord_polar(theta="y")
guildpie


########################moving on



install.packages("productplots")
devtools::install_github("hadley/productplots")
library(productplots)
prodplot(guildsort2, ~feeding.guild+Family, c("hspine","vspine"))+coord_polar(theta="y")

################
##looking at species occurance and abundance
library(vegan)
bird.sp<-birds[,sps]
View(bird.sp)
t.sp<-t(bird.sp)
View(t.sp)
bird.mat<-vegdist(t.sp,method="jaccard",binary=T)
bird.mat
??install.packages("igraph")
library(igraph)
p<-graph.adjacency(bird.mat,mode="undirected",weighted=TRUE)
plot.igraph(p,vertex.label=sps)
V(g)$sp<-sps
plot.igraph(g)
V(p)$weight
E(g)
dev.off()
str(bird.mat)
devtools::install_github("briatte/ggnet")
library(ggnet)
library(network)
library(sna)
library(ggplot2)
bird.mat<-summarize_each(bird.mat,funs(1-x))
bird.mat<-as.matrix(bird.mat)
net<-network(bird.mat,directed=FALSE)
net
ggnet2(net)

install.packages("cooccur")
library(cooccur)
data(beetles)
bird.coo<-View(beetles)
bird.coo<-cooccur(bird.sp,type="spp_site",spp_names=TRUE)
summary(bird.coo)
obs.v.exp(bird.coo)
pair.profule(bird.coo)
plot.cooccur(bird.coo)
nmat<-create.N.matrix(bird.sp)
nmat
############################################
##labdsv
birds<-read.csv("enemy_bird_visit.csv")
drops<-c("Grand.Total","X.blank.")##clean up df
birds=birds[,!(names(birds) %in% drops)]
birds[is.na(birds)]<-0
cols<-names(birds)
drops<-c("DIVERSITY","PLOT","VISIT")
sps<-cols[-c(1:3)]##this is a list of the names of the birds only
bird.sp<-birds[,sps]

library("labdsv")
spc_pres<-apply(bird.sp>0,2,sum)

seq(1,57)[sort(spc_pres)==2]
spc_pres[spc_pres==7]
sort(spc_pres)

plot(sort(spc_pres),log='y',main="Cumulative Distribution of Species Occurrences",xlab='Cumulative Count of Species',ylab='Number of Plots')#cumulative plot of species(x) by plot(y)
##cumulative distribution of species occuranes
##what is the mean cover of each species when it ocurs? (not including 0 abs)
hist(log(spc_pres),10)
tmp<-apply(bird.sp,2,sum)

spc_mean<-tmp/spc_pres #avg cover for each species
plot(sort(spc_mean),main="Cumulative Species Abundance Distribution",
     xlab="Cumulative Number of Species", ylab="Mean Abundance")

##is the mean abun of sp correlated with # of plots occur in?
plot(spc_pres,spc_mean)
#yes

#is total abundance correlated with number of species?
plt_pres<-apply(bird.sp>0,1,sum)
plot(sort(plt_pres))
#yes

##total abun on each plot
plt_sum<-apply(bird.sp,1,sum)
plot(sqrt(plt_sum))

plot(plt_pres,plt_sum)##number of species/plot, total abundance
mean(plt_pres)#mean # species per plot
plt_pres[plt_sum>3]
spc_dat<-list(spc_pres,spc_mean,plt_pres,plt_sum)
names(spc_dat) <- c("spc_pres","spc_mean","plt_pres","plt_sum")

#species abundance calculations
abuocc(bird.sp)##this does everythign above

###########by plot ECDF
boxplot()




