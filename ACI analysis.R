#####################################################
##  Absolute centralisation                     #####
##  This is to actually look at the absolute    #####
##  centralisation to see what is happening     #####
##  Start: 9/2/2017                             #####
#####################################################

##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
##  Data load;
load('LSOA data/Analysis file/England and Wales benefits 0111.Rdata')
city.centres<-read.csv('City centres/UK city centres.csv')

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

ACI.tables<-list(NULL)
for (i in 1:length(cities.list)){

  temp.df<-cities.list[[i]]@data #we only need the data file from here on in 
  names(temp.df)
  ##  We will need to work out the rci for the various cols
  temp.out<-list(NULL)
  for(j in 1:3){
    
    id2001<-which(names(temp.df)%in%c('jsa2001','is2001','ib2001'))[j]
    id2011<-which(names(temp.df)%in%c('jsa2011','is2011','ib2011'))[j] #this is the cols for the various year data fo each measure

 
    aci2001<-rci(sort.var=temp.df$dist.d,y=temp.df[,id2001],x=temp.df$dist.d)
    aci2011<-rci(sort.var=temp.df$dist.d,y=temp.df[,id2011],x=temp.df$dist.d)
    temp.out[[j]]<-cbind(aci2001,aci2011)
  }
  
  temp.res<-data.frame(do.call(cbind,temp.out))
  temp.res$jsadiff<-temp.res[,2]-temp.res[,1]
  temp.res$isdiff<-temp.res[,4]-temp.res[,3]
  temp.res$ibdiff<-temp.res[,6]-temp.res[,5]
  temp.res<-round(temp.res,4)
  temp.res$city<-names(cities.list)[[i]]
  ACI.tables[[i]]<-temp.res
}
ACI.tables
ACI.tables<-do.call(rbind,ACI.tables)
write.csv(ACI.tables,file='Results/LA Boundaries/ACI LA point estimates claimants.csv')

##  The same stat for non-claimants
ACI.tables<-list(NULL)
for (i in 1:length(cities.list)){
  
  temp.df<-cities.list[[i]]@data #we only need the data file from here on in 
  names(temp.df)
  ##  We will need to work out the rci for the various cols
  temp.out<-list(NULL)
  for(j in 1:3){
    
    id2001<-which(names(temp.df)%in%c('jsa2001','is2001','ib2001'))[j]
    id2011<-which(names(temp.df)%in%c('jsa2011','is2011','ib2011'))[j] #this is the cols for the various year data fo each measure
    
    
    aci2001<-rci(sort.var=temp.df$dist.d,y=temp.df$w.pop2001-temp.df[,id2001],x=temp.df$dist.d)
    aci2011<-rci(sort.var=temp.df$dist.d,y=temp.df$w.pop2011-temp.df[,id2011],x=temp.df$dist.d)
    temp.out[[j]]<-cbind(aci2001,aci2011)
  }
  
  temp.res<-data.frame(do.call(cbind,temp.out))
  temp.res$jsadiff<-temp.res[,2]-temp.res[,1]
  temp.res$isdiff<-temp.res[,4]-temp.res[,3]
  temp.res$ibdiff<-temp.res[,6]-temp.res[,5]
  temp.res<-round(temp.res,4)
  temp.res$city<-names(cities.list)[[i]]
  ACI.tables[[i]]<-temp.res
}

ACI.tables<-do.call(rbind,ACI.tables)
write.csv(ACI.tables,file='Results/LA Boundaries/ACI LA point estimates non-claimants.csv')

##  Excellent. Now to just extract the data. 