######################################################
##  London ACI                                      ##
##  Start: 21/4/2017                                ##
######################################################

##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
##  Data load;
load('../Data/Analysis data/England and Wales benefits 0111 final.Rdata')

##  load in ttwa
TTWA.2011<- readOGR(dsn='../Data/TTWA 2011', layer='Travel_to_Work_Areas_December_2011_Full_Extent_Boundaries_in_United_Kingdom') 
TTWA.2011<-gBuffer(TTWA.2011, byid=TRUE, width=-1)

##  Read in city centres file.
city.centres<-read.csv('../Data/City centres/UK city centres.csv')
city.centres<-city.centres[1,] #take London only

## Get the city centre points
mono.centres.sp<-SpatialPointsDataFrame(coords= coordinates(city.centres[,c('EastingD','NorthingD')]),data=data.frame(city.centres),proj4string=CRS(proj4string(TTWA.2011)))
##  Now we subset to just the ttwa that we are interested in
sub.ttwa<-TTWA.2011[mono.centres.sp,]
sub.ttwa<-sub.ttwa[ew.2001,] #Those in England and Wales
sub.ttwa<-gBuffer(sub.ttwa, byid=TRUE, width=-0.1)

##  Step two: 
##  The subsetting to only certain ttwa
valid.ttwa<-as.character(sub.ttwa$ttwa11nm)
ttwa.list<-list(NULL)
for (i in 1:length(valid.ttwa)){
  ttwa.list[[i]]<-ew.2001[sub.ttwa[i,],]
}
names(ttwa.list)<-valid.ttwa
rm(ew.2001) # remvoe shp file to save space
names(ttwa.list)

##  Second: Unlike LA we need 3 different distance ordering variables. One is distance from city centre (we will choose mid point D). Another is closes distance to a centre. Yet another is by some sort of accessibility index

##  Distance by centre. Variable dist.d. Each ttwa has only one match to a city
for (i in 1:length(ttwa.list)){
  temp.mids<-mono.centres.sp[ttwa.list[[i]],]
  centroids<-getSpPPolygonsLabptSlots(ttwa.list[[i]])
  ttwa.list[[i]]$dist.d<-euclid.dist(point=c(t(temp.mids@data[,9:10])),x=centroids)
}

##  Distance to nearest centre
##  Not necesarily for London

##  Accessibility based on Hansen (1959). Guess between -1 and -2 for the exponent
##  This is a for loop that calulcate the index (with an extra 100m added to distance for terminal time)
ttwa.emp<-list(NULL)
for (k in 1:length(ttwa.list)){
  temp.df<-ttwa.list[[k]]
  centroids<-getSpPPolygonsLabptSlots(temp.df)
  temp.df<-ttwa.list[[k]]@data
  
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
  ttwa.emp[[k]]<-do.call(rbind,temp.emp)
  colnames(ttwa.emp[[k]])<-c('hansen1.2001','hansen1.2011','hansen2.2001','hansen2.2011')
}

for (i in 1:length(ttwa.list)){
  ttwa.list[[i]]<-cbind(ttwa.list[[i]],ttwa.emp[[i]])
}


##  ACI routine----
##  Third: Now for each city we have to establish a routine for working out the ACI results
##  There is a variable st_areasha that gives us the area

##  First we will get the point estimates; this is for checking as much as anything else
ACI.tables<-list(NULL)
for (i in 1:length(ttwa.list)){
  
  temp.df<-ttwa.list[[i]]@data #we only need the data file from here on in 
  names(temp.df)
  ##  We will need to work out the rci for the various cols
  temp.out<-list(NULL)
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
  temp.res$city<-names(ttwa.list)[[i]]
  temp.res$type<-c('claimants','non-claimants')
  ACI.tables[[i]]<-temp.res
}
ACI.tables
ACI.tables<-do.call(rbind,ACI.tables)
write.csv(ACI.tables,file='../Results/ACI London point estimates.csv')

## End for now