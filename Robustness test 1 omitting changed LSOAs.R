#######################################################
##  Robustness test: This is the LA analysis but      #
##  omitting those areas where the 2001 has changed   #
##  This is to test the sensitive of the analysis     #
##  Start: 7/2/2017                                   #
#######################################################

##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
##  Data load;
load('LSOA data/Analysis file/England and Wales benefits 0111.Rdata')
city.centres<-read.csv('City centres/UK city centres.csv')
conversion.0111<-read.csv('LSOA data/LSOA 2001 to 2011/LSOA01_LSOA11_LAD11_EW_LU.csv',stringsAsFactors = F)

##  This is the syntax change
unchanged<-unique(conversion.0111$LSOA01CD[conversion.0111$CHGIND=='U']) 
ew.2001<-ew.2001[ew.2001$lsoa01cd%in%unchanged,]
nrow(ew.2001@data)

##  The subsetting to only certain cities
valid.cities<-as.character(city.centres$la[city.centres$la%in%ew.2001$la])
cities.list<-list(NULL)
for (i in 1:length(valid.cities)){
  cities.list[[i]]<-ew.2001[ew.2001$la%in%valid.cities[i],]
}
names(cities.list)<-valid.cities
##  So now we have a cities.list file of only the relevant cities

##  Second: Now it is a good time to go ahead and making our RCIs; first we will get the distance of each zone's centroids from several midpoints.
for (i in 1:length(cities.list)){
  temp.mids<-city.centres[city.centres$la==valid.cities[i],]
  centroids<-getSpPPolygonsLabptSlots(cities.list[[i]])
  cities.list[[i]]$dist.a<-euclid.dist(point=c(t(temp.mids[,3:4])),x=centroids)
  cities.list[[i]]$dist.b<-euclid.dist(point=c(t(temp.mids[,5:6])),x=centroids)
  cities.list[[i]]$dist.c<-euclid.dist(point=c(t(temp.mids[,7:8])),x=centroids)
  cities.list[[i]]$dist.d<-euclid.dist(point=c(t(temp.mids[,9:10])),x=centroids)
}
warnings() #use cord methods--well whatever
rm(ew.2001) #remove to save space

##  Four: Now we can actually just do the rci for the various measures we want. We will show the point estimates which will show that the choice of midpoints is not important

RCI.tables<-list(NULL)
for (i in 1:length(cities.list)){
  
  temp.df<-cities.list[[i]]@data #we only need the data file from here on in 
  names(temp.df)
  ##  We will need to work out the rci for the various cols
  temp.out<-list(NULL)
  for(j in 1:3){
    
    id2001<-which(names(temp.df)%in%c('jsa2001','is2001','ib2001'))[j]
    id2011<-which(names(temp.df)%in%c('jsa2011','is2011','ib2011'))[j] #this is the cols for the various year data fo each measure
    id.dist<-which(names(temp.df)%in%c('dist.a','dist.b','dist.c','dist.d'))
    
    rci2001<-apply(temp.df[,id.dist],2,rci,y=temp.df[,id2001],x=temp.df$w.pop2001-temp.df[,id2001])
    rci2011<-apply(temp.df[,id.dist],2,rci,y=temp.df[,id2011],x=temp.df$w.pop2011-temp.df[,id2011])
    temp.out[[j]]<-cbind(rci2001,rci2011)
  }
  
  temp.res<-data.frame(do.call(cbind,temp.out))
  temp.res$jsadiff<-temp.res[,2]-temp.res[,1]
  temp.res$isdiff<-temp.res[,4]-temp.res[,3]
  temp.res$ibdiff<-temp.res[,6]-temp.res[,5]
  temp.res<-round(temp.res,4)
  temp.res$city<-names(cities.list)[[i]]
  RCI.tables[[i]]<-temp.res
}

RCI.tables<-do.call(rbind,RCI.tables)
write.csv(RCI.tables,file='RCI LA point estimates robust 1.csv')
