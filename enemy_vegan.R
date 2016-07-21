###enemies analysis pt 2
###measuring diversity

library(vegan)
library(dplyr)

birds<-read.csv("enemy_bird_visit.csv")
sp.list<-read.csv("enemy_bird_list.csv")

std <- function(x) sd(x)/sqrt(length(x))

drops<-c("Grand.Total","X.blank.")##clean up df
birds=birds[,!(names(birds) %in% drops)]
birds[is.na(birds)]<-0
cols<-names(birds)
drops<-c("DIVERSITY","PLOT","VISIT")
sps<-cols[-c(1:3)]##this is a list of the names of the birds only
birds$VISIT<-as.character(birds$VISIT)
birds$total_abun<-rowSums(birds[,sps])
bird.sp<-birds[,sps]#matrix plot x sp

##comparing overall richness, diversity, abundance between monocultures and polycultures

##rarfaction
source("/Users/colleennell/Documents/R/functions/rarefaction_f.r")
#comparing species richness between treatments after standardizing for sampling effort
##gotelli & colwell (2001); buddle et al (2005)
bird.rare<-rarefaction(bird.sp,subsample=5,plot=TRUE,error=FALSE,legend=TRUE)
View(bird.sp)


##Diversity indices
birds$div<-diversity(bird.sp)##plot/visit level shannon calculation
birds$VISIT<-as.numeric(birds$VISIT)
birds.sum<-birds%>% #plot-level data
  group_by(DIVERSITY,PLOT)%>%
  summarise_each(funs(sum))##total species abundance per plot
##use matrix from this to calculate diversity on the pooled observations/plot
#standardize obs by n_visit
birds.sum.mat<-birds.sum[,sps]
birds.sum$div.sum<-diversity(birds.sum.mat)#diveristy of sum species abundances per plot
#vs birds.sum$div which is the sum of diversity per plot (useless)
##but div.sum does not account for differences in # of visits
birds.mean<-birds%>%
  group_by(DIVERSITY,PLOT)%>%
  summarise_each(funs(mean)) ##means per visit
birds.mean.mat<-birds.mean[,sps]
birds.mean$div.mean<-diversity(birds.mean.mat) ##this is diversity calculated on the mean abunndances for each plot/visit
#vs birds.mean$div which is the mean of the diversity measure at the plot/visit, should be same as birds.plot.mean$mean_plot_div
birds.plot.mean<-birds%>%
  group_by(DIVERSITY,PLOT)%>%
  summarize(n_visit = length(VISIT),plot_abun = sum(total_abun), 
            mean_plot_abun = mean(total_abun), se_plot_abun = std(total_abun),
            mean_plot_div = mean(div), se_plot_div = std(div)) ##mean diversity plot/visit

birds.plot<-left_join(birds.plot.mean,birds.sum[,c("total_abun","PLOT","div.sum")],by="PLOT")
View(birds.plot)
birds.plot.2<-left_join(birds.plot,birds.mean[,c("PLOT","div.mean")],by="PLOT")
View(birds.plot.2)
##div.sum and div.mean are alsmot exactly the same- mean is adjusted for n_visits so winner
trp<-lm(mean_plot_div~div.mean,data=birds.plot.2)
summary(aov(trp))
plot(trp)
lm1<-ggplot(birds.plot.2,aes(x=div.mean,y=mean_plot_div))+geom_point(size=2)+geom_smooth(method="lm",se=F)
lm1
##highly correlated with mean_plot_div which is based on the diversity measure for each visit
##use either?
View(birds.plot.2)


##plot/visit level analyses on diversity metrics
##does bird diversity vary with tree diversity?
##run model against visit-level data
divbydiv<-aov(div~DIVERSITY*PLOT+VISIT,data=birds)
summary(divbydiv)
qq.plot(birds$div)
shapiro.test(birds$div)
ks.test(birds$div,pnorm)
birdres<-merge(birds,residuals(divbydiv))
View(birdres)
hist.div.res<-ggplot(birdres,aes(x=y,fill=DIVERSITY,color=DIVERSITY),alpha=.6)+geom_density(alpha=.5)+
  theme_minimal()+labs(y="Density",x="Residuals(Bird Div)")+
  scale_fill_manual("Plot Diversity",labels=c("Monoculture","Polyculture"),values=c("darkslateblue","firebrick2"))+scale_color_manual("Plot Diversity",labels=c("Monoculture","Polyculture"),values=c("darkslateblue","firebrick2"))+
  theme(legend.position="none")
hist.div.res
###species diversity varies by tree diversity, not plot
abunbydiv<-aov(total_abun~DIVERSITY*PLOT+PLOT/VISIT,data=birds)
summary(abunbydiv)
##abundance varies by diversity

##species richness
birds$richness<-specnumber(bird.sp)##no adjustment for indivudals
richbydiv<-aov(richness~DIVERSITY*PLOT+VISIT,data=birds)
summary(richbydiv)
##diversity mediates species richness


###rarefaction
#sp rich increases with sample size, and may warp comparisons
#to resolve this, rarefy sp richness to same number of individs
birds.sum$rich.rar<-rarefy(birds.sum.mat,min(rowSums(birds.sum.mat)))##rarefies to sample size of 2- not that elpful
birds.sum$richness<-specnumber(birds.sum.mat)

#compare sp rich with different sample sizes

##species abundance models
rad<-radfit(bird.sp)#fit model for each site
rad
##lognormal

##species accumulation and beta
##modelling collections of sites, their species richness, estimate unseen species

#sp acumulation curves...but what is the estimated diversity? extrapolations?
bird.poly<-birds%>%
  filter(DIVERSITY == 'P')
bird.poly.mat<-bird.poly[,sps]
sac.poly<-specaccum(bird.poly.mat)
plot(sac.poly,ci.type="polygon",ci.col="blue")
bird.mono<-birds%>%
  filter(DIVERSITY == 'M')
bird.mono.mat<-bird.mono[,sps]
sac.mono<-specaccum(bird.mono.mat)
plot(sac.mono,ci.type="polygon",ci.col="yellow")

##beta
beta.poly<-vegdist(bird.poly.mat,binary=FALSE)##empty rows, needs to be binary
mean(beta.poly)
betadiver(help=TRUE)
z<-betadiver(bird.poly.mat,"z")#this is the slope of species accum
quantile(z)
##want to compare beta diversity among plots of same diversity- what is the average species turnover between poly plots vs mono plots?


##species pool
specpool #colleciton of sites
estimateR #single site

poly.spec<-specpool(bird.poly.mat)##prints out several indices
poly.spec#extrapolated richness values for all polysculture

mono.spec<-specpool(bird.mono.mat)
mono.spec
##sampling effort?

sppool<-estimateR(bird.sp[1,])
sppool

#pool size for each site
#use plot-level sums
#lognormal abundance?
birds.sum.mat
veiledspec(prestondistr(birds.sum.mat[2,]))
str(birds.sum.mat)

###birds over time\

install.packages("rich")
library(rich)
test<-rich(matrix=bird.sp,verbose=F)
test$cr #cumulative richness
test$mr ##mean richness over n samples
test$mrsd #sdev of mean richness
$singletons
$doubletons
$uniques

rarc.all<-rarc(matrix=bird.sp,nrandom=99)
View(rarc.all)#a df of bootstrap estimates of richness and individs for pltoting rarefaction curve
rare.all<-ggplot(rarc.all,aes(x=samples,y=richness))+geom_point(size=2)+theme_minimal()
rare.all #richness by effort
rare.all.ind<-ggplot(rarc.all,aes(x=individuals,y=richness))+geom_point(size=2)+theme_minimal()
rare.all.ind #richness by individuals

##by div
rarc.poly<-rarc(bird.poly.mat,nrandom=99)
rarc.mono<-rarc(bird.mono.mat,nrandom=99)

div.rarc<-ggplot()+#by samples
  geom_point(data=rarc.poly,aes(x=samples,y=richness),size=3,shape=1,color="firebrick2")+
  geom_point(data=rarc.mono,aes(x=samples,y=richness),size=3,shape=2,color="darkslateblue")+
  theme_minimal()
div.rarc

div.rarc.ind<-ggplot()+#by individuals
  geom_point(data=rarc.poly,aes(x=individuals,y=richness),size=3,shape=1,color="firebrick2")+
  geom_point(data=rarc.mono,aes(x=individuals,y=richness),size=3,shape=2,color="darkslateblue")+
  theme_minimal()
div.rarc.ind

##comparing 2 communities
#extrapolate richness using largest number of sampling units
polyfy<-raref(matrix=bird.poly.mat,dens=sum(bird.mono.mat),nrandom=300)
monofy<-raref(matrix=bird.mono.mat,dens=sum(bird.mono.mat),nrandom=300)
polyfy$Sinterp[2] #interpolating
monofy$Sinterp[2]
##bootstrapping
polyfy2<-raref2(matrix=bird.poly.mat,dens=sum(bird.mono.mat),tolerance=0.01,nrandom=300)
monofy2<-raref2(matrix=bird.mono.mat,dens=sum(bird.mono.mat),tolerance=0.01,nrandom=300)
polyfy2$mean.boot
polyfy2$sd.boot
monofy2$mean.boot
monofy2$sd.boot##these numbers are all very similar
polyfy2
##comparing cumulative spcies richness
#only one value..no test
c2cv(com1=bird.poly.mat,com2=bird.mono.mat,nrandom=999,verbose=F)#are these communities different?
#base on the average
#sig

#c2rcv - control for effect of differences in density/effort

##average species richness over 2 sets of samples
##using randomization test
mo<-rich(bird.mono.mat,nrandom=50,verbose=T)
po<-rich(bird.poly.mat,nrandom=50,verbose=T)
c2m(pop1=mo$sumrow,pop2=po$sumrow,nrandom=999,verbose=F)
#very different

#shared species
allplot<-list(bird.mono.mat,bird.poly.mat)
shared(allplot)#absolute and relative number of species shared
#observed on diag, shared above, total richness below

####dong similar things with BiodiversityR
install.packages("BiodiversityR")
library(BiodiversityR)
