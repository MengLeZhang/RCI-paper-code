##  Descriptive statistics ####
##  We need to create some basic descriptions about the cities and ttwa that we are using in our analysis
##  Start: 3/5/2017
##  Load in the data/library of functions ----
source.file<-'RCI functions.R' #path to source
source(source.file)
load('../Data/Analysis data/England and Wales benefits 0111 final.Rdata')
jsa.df<-read.csv('../Data/DWP data/jsa 2000 to 2016.csv',stringsAsFactors = F)
jsa.df$lsoa01cd<-substr(jsa.df$super.output.areas...lower.layer,1,9)
table(jsa.df$lsoa01cd%in%ew.2001$lsoa01cd) #the missing variable is just the coloum total
ew.2001<-merge(ew.2001,jsa.df,by='lsoa01cd') #replace the ew.2001 with a merged version

##  Read in city centres file.
city.centres<-read.csv('../Data/City centres/UK city centres.csv')

##  Getting working population/workplace pop
ew.la<-ew.2001@data[ew.2001@data$la%in%city.centres$la,]
ew.la$count<-1
stats<-c('w.pop2001','w.pop2011',"work.pop2001","work.pop2011",'jsa2001','jsa2011','is2001','is2011','ib2001','ib2011','count')
la.tab<-aggregate(ew.la[,stats],by=list(ew.la$la),FUN=sum)
la.tab
write.csv(la.tab,'../Results/la stats.csv')

##  jsa stats for la
which.may<-grep('May',names(ew.2001))
la.tab.jsa<-aggregate(ew.la[,which.may],by=list(ew.la$la),FUN=sum)
write.csv(la.tab.jsa,'../Results/la stats jsa.csv')

##  getting ttwa stats
which.ttwa<-c(unique(ew.la$ttwa),'London')
ew.ttwa<-ew.2001@data[ew.2001@data$ttwa%in%which.ttwa,]
ew.ttwa$count<-1
ttwa.tab<-aggregate(ew.ttwa[,stats],by=list(ew.ttwa$ttwa),FUN=sum)
write.csv(ttwa.tab,'../Results/ttwa stats.csv')

##  jsa stats for ttwa
which.may<-grep('May',names(ew.2001))
ttwa.tab.jsa<-aggregate(ew.ttwa[,which.may],by=list(ew.ttwa$ttwa),FUN=sum)
write.csv(ttwa.tab.jsa,'../Results/ttwa stats jsa.csv')


##  how is each ttwa split?
##  Distance to nearest centre
##  Load inthe dataset with multiple centre and make it into a spatial points df
ttwa.centres<-read.csv('../Data/City centres/ttwa 2011 la centres.csv',stringsAsFactors = F)
ttwa.centres<-na.omit(ttwa.centres[1:4]) #we do not need the last note col
ttwa.centres$EastingC<-as.numeric(ttwa.centres$EastingC)
ttwa.centres.sp<-SpatialPointsDataFrame(coords= coordinates(ttwa.centres[,c('EastingC','NorthingC')]),data=data.frame(ttwa.centres),proj4string=CRS(proj4string(ew.2001)))
j
##  Define the routine
ew.ttwa$closest<-NA
for (j in 1:length(which.ttwa)){
  temp.ttwa<-subset(ew.ttwa,ttwa==which.ttwa[j])
  temp.centres<-ttwa.centres.sp[ttwa.centres.sp$LA%in%temp.ttwa$la,] #centres of las in the ttwa
  if(nrow(temp.centres)<1)next

  ## Computing the distance to population weighted centroid (xand y cords in cent.x and cent,y
  centroids<-temp.ttwa[,c("cent.x","cent.y")]
  ##  list of vectors denoting distance of a centre
  
  distances<-list(NULL)
  for(i in 1:nrow(temp.centres@data)){
    distances[[i]]<-euclid.dist(point=c(t(temp.centres@data[i,c('EastingC','NorthingC')])),x=centroids)
  }
  distances<-do.call(cbind,distances)
  dist.nearest<-apply(distances,1,min)
  centre<-as.character(temp.centres@data$LA[apply(distances,1,which.min)]) #tells us which centre
  ew.ttwa$closest[ew.ttwa$ttwa==which.ttwa[j]]<-centre #ordering should be preserved from the subsetting
}

##  now to se stats
temp.ttwa.tab<-list(NULL)
for (j in 1:length(which.ttwa)){
  temp.ttwa<-subset(ew.ttwa,ttwa==which.ttwa[j])
  
  temp.tab<-aggregate(temp.ttwa[,stats],by=list(temp.ttwa$closest),FUN=sum)
  if(nrow(temp.tab)<1)next
  temp.ttwa.tab[[j]]<-cbind(ttwa=which.ttwa[j],temp.tab)
}

ttwa.tab2<-do.call(rbind,temp.ttwa.tab)
write.csv(ttwa.tab2,'../Results/ttwa stats 2.csv')

##  JSA descriptives