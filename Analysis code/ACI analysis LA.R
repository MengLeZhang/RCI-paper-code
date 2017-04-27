##################################################################################
##  ACI analysis                                                                ##
##  This is a script that looks at the ACI over time                            ##
##  This will be based strongly around how we did RCI                           ##
##  Start: 21/4/2017                                                            ##
##################################################################################


##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
##  Data load;
load('../Data/Analysis data/England and Wales benefits 0111 final.Rdata')
city.centres<-read.csv('../Data/City centres/UK city centres.csv')

##  The subsetting to only certain cities
valid.cities<-as.character(city.centres$la[city.centres$la%in%ew.2001$la])
cities.list<-list(NULL)
for (i in 1:length(valid.cities)){
  cities.list[[i]]<-ew.2001[ew.2001$la%in%valid.cities[i],]
}
names(cities.list)<-valid.cities
##  So now we have a cities.list file of only the relevant cities
rm(ew.2001) #remove to save space


##  Second: We need to create two different variables. One is distance from city centre (we will choose mid point D). Another is by some sort of accessibility index

##  Distance by centre. Variable dist.d
for (i in 1:length(cities.list)){
  temp.mids<-city.centres[city.centres$la==valid.cities[i],]
  centroids<-getSpPPolygonsLabptSlots(cities.list[[i]])
  cities.list[[i]]$dist.d<-euclid.dist(point=c(t(temp.mids[,9:10])),x=centroids)
}

##  Accessibility based on Hansen (1959). Guess between -1 and -2 for the exponent
##  This is a for loop that calulcate the index (with an extra 100m added to distance for terminal time)
cities.emp<-list(NULL)
for (k in 1:length(cities.list)){
  temp.df<-cities.list[[k]]
  centroids<-getSpPPolygonsLabptSlots(temp.df)
  temp.df<-cities.list[[k]]@data
  
  temp.emp<-list(NULL)
  for (i in 1:nrow(temp.df)){
    temp.dist<-euclid.dist(point=centroids[i,],x=centroids)
    temp.out<-list(NULL)
    
    ## 2001 accessibility index. We need the negative of accessibility
    hansen1.2001<--sum(temp.df$work.pop2001*(temp.dist+100)^(-1))
    hansen2.2001<--sum(temp.df$work.pop2001*(temp.dist+100)^(-2))
    
    ## 2011 accessibility index
    hansen1.2011<--sum(temp.df$work.pop2011*(temp.dist+100)^(-1))
    hansen2.2011<--sum(temp.df$work.pop2011*(temp.dist+100)^(-2))
    
    temp.emp[[i]]<-cbind(hansen1.2001,hansen1.2011,hansen2.2001,hansen2.2011)
  }
  cities.emp[[k]]<-do.call(rbind,temp.emp)
  colnames(cities.emp[[k]])<-c('hansen1.2001','hansen1.2011','hansen2.2001','hansen2.2011')
}

for (i in 1:length(cities.list)){
  cities.list[[i]]<-cbind(cities.list[[i]],cities.emp[[i]])
}

head(cities.list[[i]]@data)

##  ACI routine----
##  Third: Now for each city we have to establish a routine for working out the ACI results
##  There is a variable st_areasha that gives us the area

##  First we will get the point estimates; this is for checking as much as anything else
ACI.tables<-list(NULL)
for (i in 1:length(cities.list)){
  
  temp.df<-cities.list[[i]]@data #we only need the data file from here on in 
  names(temp.df)
  ##  We will need to work out the rci for the various cols
  for(j in 1:3){
    
    id2001<-which(names(temp.df)%in%c('jsa2001','is2001','ib2001'))[j]
    id2011<-which(names(temp.df)%in%c('jsa2011','is2011','ib2011'))[j] #this is the cols for the various year data fo each measure
    id.dist01<-which(names(temp.df)%in%c('dist.d'))
    id.dist11<-which(names(temp.df)%in%c('dist.d'))
    
    y.2001<-cbind(temp.df[,id2001],temp.df$w.pop2001-temp.df[,id2001]) #cols of the claimant and non-claimants
    y.2011<-cbind(temp.df[,id2011],round(temp.df$w.pop2011)-temp.df[,id2011])
    
    aci2001<-apply(y.2001,2,rci,sort.var=temp.df[,id.dist01],x=temp.df$st_areasha)
    aci2011<-apply(y.2011,2,rci,sort.var=temp.df[,id.dist11],x=temp.df$st_areasha)
    temp.out[[j]]<-cbind(aci2001,aci2011)
  }
  
  temp.res<-data.frame(do.call(cbind,temp.out))
  temp.res$jsadiff<-temp.res[,2]-temp.res[,1]
  temp.res$isdiff<-temp.res[,4]-temp.res[,3]
  temp.res$ibdiff<-temp.res[,6]-temp.res[,5]
  temp.res<-round(temp.res,4)
  temp.res$city<-names(cities.list)[[i]]
  temp.res$type<-c('claimants','non-claimants')
  ACI.tables[[i]]<-temp.res
}
ACI.tables
ACI.tables<-do.call(rbind,ACI.tables)
write.csv(ACI.tables,file='../Results/ACI LA point estimates.csv')

## End for now
rm(list = ls())