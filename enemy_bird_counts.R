###Mexico diversity effects experiment
###data analysis for Bird Diversity observations in diversity plots
rm(list=ls())

library(dplyr)
library(vegan)
library(ggplot2)
library(reshape2)

std <- function(x) sd(x)/sqrt(length(x))

setwd("/Users/colleennell/Documents/R/enemies/")
birds<-read.csv("/Users/colleennell/Documents/R/enemies/enemy_bird_visit.csv")
sp.list<-read.csv("/Users/colleennell/Documents/R/enemies/enemy_bird_list.csv")

drops<-c("Grand.Total","X.blank.")##clean up df
birds=birds[,!(names(birds) %in% drops)]
birds[is.na(birds)]<-0
birds$PLOT<-as.character(birds$PLOT)

cols<-names(birds)
drops<-c("DIVERSITY","PLOT","VISIT")
sps<-cols[-c(1:3)]##this is a list of the names of the birds only

#total # birds per plot, total sp richness per plot?
bird.totals<-birds%>%
  mutate(birds_plot_visit=rowSums(.[sps]))%>%
  group_by(DIVERSITY,PLOT)%>%
  summarize(total_birds_plot=sum(birds_plot_visit),
            n_visit=length(VISIT),
            mean_birds_visit=mean(birds_plot_visit),
            se_birds_visit=std(birds_plot_visit),
            birds_30=mean_birds_visit*3,
            se_30=std(birds_plot_visit)*3)
View(bird.totals)


abun_by_plot<-ggplot(bird.totals,aes(x=reorder(PLOT,birds_30),y=birds_30,fill=DIVERSITY))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=birds_30-se_30,ymax=birds_30+se_30))+
  labs(x="Plot",y="Birds/30/plot")
abun_by_plot

birds$VISIT<-as.character(birds$VISIT)
birds$total_abun<-rowSums(birds[,sps])

plot_diff_abun<-aov(total_abun~PLOT+VISIT,data=birds)
summary(plot_diff_abun)##bird abundance varies by plot

bird.div<-bird.totals%>%
  group_by(DIVERSITY)%>%
  summarize(total_birds_div=sum(total_birds_plot),
            n_plots=length(PLOT),
            mean_birds_plot_30=mean(birds_30),
            se_birds_plot_30=std(birds_30),
            mean_birds_plot_visit=sum(total_birds_plot)/sum(n_visit),
            se_birds_plot_visit=std(total_birds_plot))

div_diff_abun<-aov(total_abun~DIVERSITY+PLOT,data=birds)
summary(div_diff_abun)

abun_by_div<-aov(mean_birds_visit~DIVERSITY,data=bird.totals)##plot is replicate
summary(abun_by_div)

#abundance differs between plot diversities
div_abun_plot<-ggplot(bird.div,aes(x=DIVERSITY,y=mean_birds_plot_30,color=DIVERSITY))+
  geom_point(size=10,shape=1)+
  geom_errorbar(aes(ymin=mean_birds_plot_30-se_birds_plot_30,ymax=mean_birds_plot_30+se_birds_plot_30),color="black",width=.1)+
  labs(x="Tree Diversity",y="Birds/30/plot")+
  theme_minimal()+
  theme(legend.position="none")
div_abun_plot+geom_point(data=bird.totals,aes(x=DIVERSITY,y=birds_30),size=2)

diff_div_abun<-bird.div[1,4]/bird.div[2,4]
diff_div_abun ##POLY plots have this many times more individuals

######################################
##convert to absence presence
View(birds)

bird.mat<-birds[,-1:-3]
bird.mat$sp<-ifelse(bird.mat$)

bird.sp.totals<-birds%>%
  group_by(DIVERSITY,PLOT)%>%
  summarize_each(funs(sum,na.rm=T))
View(bird.sp.totals)##this is the total # of birds by each species obs in each plot
##now want the total sp richness per plot





sum(bird.totals$birds_plot_visit)
##summarize by plot diversity, mean bird abundance by plot, and n_obs
n_visit<-birds%>%
  group_by(PLOT)%>%
  summarize(n_visit=length(VISIT))%>%
  select(PLOT,n_visit)
str(birds)
poly<-birds%>%
  filter(DIVERSITY=='P')%>%
  group_by(DIVERSITY,PLOT)%>%
  select(-VISIT)%>%
  summarize_each(funs(sum))
poly.birds<-poly[,sps]
##same for monoculture plots
mono<-birds%>%
  filter(DIVERSITY=='M')%>%
  group_by(DIVERSITY,PLOT)%>%
  select(-VISIT)%>%
  summarize_each(funs(sum))
mono.birds<-mono[,sps]

birds.by.plot<-rbind(poly,mono)
bb.plot<-birds.by.plot[,sps]
birds.by.plot$plot.diversity<-diversity(bb.plot)##calculate diversity by plot
##evenness
birds.by.plot$plot.evenness<-birds.by.plot$plot.diversity/log(specnumber(bb.plot))
View(birds.by.plot)

##Do evenness and shannon didder by diverisoty?
div_shan<-aov(plot.diversity~DIVERSITY,data=birds.by.plot)
summary(div_shan)#yes

shan_plot<-ggplot(birds.by.plot,aes(x=DIVERSITY,y=plot.diversity,color=DIVERSITY))+
  geom_point(size=10,shape=1)+labs(x="Tree Diversity",y="Shannon Diversity")+
  theme_minimal()+
  theme(legend.position="none")
shan_plot
+geom_point(data=bird.totals,aes(x=DIVERSITY,y=birds_30),size=2)

div_even<-aov(plot.evenness~DIVERSITY,data=birds.by.plot)
summary(div_even)#no
################################################
birdy<-rbind(mono,poly)
birdy2<-select(birdy,-total_abun)
plot.melt<-melt(birds,value.name="abundance",id.var=c("DIVERSITY","PLOT","VISIT"))
View(plot.melt)  ##the average number of species seen each visit
plot.melt$sp_count<-ifelse(plot.melt$abundance > 0,1,0)

plot.sprich<-plot.melt%>%
  group_by(DIVERSITY,PLOT,VISIT)%>%
  summarize(total_sp_rich = sum(sp_count))##sp richness by plot visit

View(plot.sprich)

plot.rich<-plot.sprich%>%
  group_by(DIVERSITY,PLOT)%>%
  summarize(mean_rich_plot=mean(total_sp_rich),se_rich_plot=std(total_sp_rich))

View(plot.rich)

div_rich<-aov(total_sp_rich~DIVERSITY+PLOT,data=plot.sprich)
summary(div_rich)##species richness varies with plot and plot diversity


div_sprich<-aov(mean_rich_plot~DIVERSITY,data=plot.rich)
summary(div_sprich)###richness differs between diveristy, but this does not account for abundance

View(plot.melt)
plot.info<-plot.melt%>%
  rename(ID = variable)%>%
  left_join(sp.list[,c("ID","feeding.guild")],by="ID")%>%
  group_by(DIVERSITY,PLOT,VISIT,feeding.guild)%>%
  summarize(gui_by_plot=sum(abundance))
View(plot.info)

cast.guild<-dcast(plot.info,DIVERSITY+PLOT+VISIT~feeding.guild,value.var="gui_by_plot")
View(cast.guild)
str(cast.guild)
cast.guild$total_abun<-rowSums(cast.guild[,4:11])
guild<-cast.guild%>%
  group_by(DIVERSITY,PLOT)%>%
  select(-VISIT)%>%
  summarize_each(funs(mean))

div.guild<-cast.guild%>%
  group_by(DIVERSITY)%>%
  select(-VISIT,-PLOT)%>%
  summarize_each(funs(sum))
View(div.guild)
prop.f<-function(x){
  x/
}
##proportion data
prop.guild<-melt(div.guild,value.name="abundance",id.var=c("DIVERSITY","total_abun"))%>%
  mutate(prop = abundance/total_abun)
View(prop.guild)
pp<-prop.guild%>%
  group_by(variable)%>%
  summarize(guild_abun=sum(abundance),total_abun=sum(total_abun))%>%
  mutate(prop=guild_abun/total_abun)
View(pp)


dc<-dcast(prop.guild, variable~DIVERSITY,value.var="prop")
dc$all<
View(dc)
guild.plot<-ggplot(prop.guild,aes(x=DIVERSITY,y=prop,fill=variable))+geom_bar(stat="identity",position="stack")
guild.plot##this is bad because looking at it it, insectivores are smaller portion in P than M
##but in reality due to abundance differences, they are many more insectivpres
##hwat is the point of this?
##for summary to ALL bird


dc.plot<-ggplot(pp,aes(x=reorder(variable,prop),y=prop,fill=variable))+geom_bar(stat="identity")
dc.plot
View(guild)  
abun.by.plot<-birds%>%
  mutate(abun = rowSums(.[sps]))%>%
  group_by(DIVERSITY,PLOT)%>%
  summarize(mean_abun=mean(abun),se_abun=std(abun))

abun.by.div<-abun.by.plot%>%
  group_by(DIVERSITY)%>%
  summarize(abun_div=mean(mean_abun),se_div=std(mean_abun))
View(abun.by.div)

View(sp.list)

################################################
##What does the bird community look like?
#composition of bird feeding guilds?

##avg mono plot is % each guild


##total birds, families, species, obs periods, 



bird.melt<-melt(birds[,-1],value.name="abundance",id.var=c("PLOT","VISIT"))

birds.guilds<-dcast(bird.melt,variable+VISIT~PLOT,value.var="abundance")%>%
  rename(ID = variable)%>%
  left_join(sp.list[,c('feeding.guild','ID')],birds.transp,by="ID")%>%
  select(-ID)%>%
  group_by(feeding.guild,VISIT)%>%
  summarize_each(funs(sum))

birds.guilds$total_birds_fg<-rowSums(birds.guilds[,-1:-2],na.rm=T)
sum(birds.guilds$total_birds_fg)


guild.summ<-melt(birds.guilds,value.name="abundance",id.var=c("feeding.guild","VISIT"))%>%
  rename(PLOT = variable)%>%
  dcast(feeding.guild+VISIT~PLOT,value.var="abundance")%>%
  group_by(feeding.guild)%>%
  summarize_each(funs(mean(.,na.rm=TRUE)))%>%
  select(-VISIT)%>%
  mutate(mean_birds_plot=rowMeans(.[,-1:-2]),total_birds_plot=rowSums(.[,-1:-2]))%>%
  select(feeding.guild,mean_birds_plot,total_birds_plot)%>%
  mutate(n_30 = mean_birds_plot*3)%>%
  mutate(n_30 = mean_birds_plot*3, per_total = mean_birds_plot/sum(mean_birds_plot))
View(guild.summ)
#######################
##phylogenetic diversity
##input species list at http://phylot.biobyte.de/treeGenerator.cgi

library(ape)
library(phytools)

str(sp.list)##species names in the plots
sp.list$names.phylo<-as.character(sp.list$names.phylo)
gsub(" ","_",sp.list$names.phylo)

birdtree<-read.nexus("BirdSupertree.tre")##super tree of birds http://currents.plos.org/treeoflife/article/reweaving-the-tapestry-a-supertree-of-birds/
##plot(birdtree) do not do this, takes forever
allbirds<-as.list(birdtree$tip.label)
mex_sp<-as.list(sp.list$names.phylo) ##species names of interest
mex_sp_tree<-allbirds[!allbirds %in% mex_sp] ##drop mexican species for drop.tip list
mex_sp_tree

prune.tree<-drop.tip(birdtree,birdtree$tip.label[-match(levels(mex_sp_tree),birdtree$tip.label)])
prune.tree

mex_tree<-drop.tip(birdtree,setdiff(birdtree$tip.label,mex_sp_tree)) #prune tree to species of interest
mex_tree
write.tree(birdtree)
species<-

plot(mex_tree)
str(mex_tree)

pdf("my_tree_shit.pdf")
plot(as.phylo(mex_tree))
dev.off()


##phylogenetic diversity
library(picante)
View(sp.list)
sp.list$