##############################################
##  ACI TTWA analysis                       #
##  Start: 21/4/2017                        #
##############################################

##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
##  Data load;
load('../Data/Analysis data/England and Wales benefits 0111 final.Rdata')
used.crs<-crs(ew.2001)

##  Read in city centres file.
city.centres<-read.csv('../Data/City centres/UK city centres.csv')
##  For now we will omit London (which we will handle with a diff script)
city.centres<-city.centres[-1,]
mono.centres.coords<-coordinates(city.centres[,c('EastingD','NorthingD')])
mono.centres.sp<-SpatialPointsDataFrame(mono.centres.coords, city.centres, proj4string = used.crs)

which.ttwa<-ew.2001@data$ttwa[ew.2001@data$la%in%city.centres$la]
##  getting ttwa
valid.ttwa<-unique(which.ttwa)

##  Step two: 
##  The subsetting to only certain ttwa
ttwa.list<-list(NULL)
for (i in 1:length(valid.ttwa)){
  ttwa.list[[i]]<-subset(ew.2001,ttwa==valid.ttwa[i])
}
names(ttwa.list)<-valid.ttwa
rm(ew.2001) # remvoe shp file to save space

##  Second: Unlike LA we need 3 different distance ordering variables. One is distance from city centre (we will choose mid point D). Another is closes distance to a centre. Yet another is by some sort of accessibility index
##  Distance by centre. Variable dist.d. Each ttwa has only one match to a city


for (i in 1:length(ttwa.list)){
  temp.mids<-mono.centres.sp[ttwa.list[[i]],]
  centroids<-ttwa.list[[i]]@data[,c("cent.x","cent.y")]
  ttwa.list[[i]]$dist.d<-euclid.dist(point=c(t(temp.mids@data[,9:10])),x=centroids)
}

##  Distance to nearest centre
##  Load inthe dataset with multiple centre and make it into a spatial points df
ttwa.centres<-read.csv('../Data/City centres/ttwa 2011 la centres.csv',stringsAsFactors = F)
ttwa.centres<-na.omit(ttwa.centres[1:4]) #we do not need the last note col
ttwa.centres$EastingC<-as.numeric(ttwa.centres$EastingC)
ttwa.centres.sp<-SpatialPointsDataFrame(coords= coordinates(ttwa.centres[,c('EastingC','NorthingC')]),data=data.frame(ttwa.centres),proj4string=used.crs)

##  Define the routine
for (j in 1:length(ttwa.list)){
  
  temp.ttwa<-ttwa.list[[j]]
  temp.centres<-ttwa.centres.sp[ttwa.centres.sp$LA%in%ttwa.list[[j]]$la,] #centre of large LAs even partially in the zone
  
  ## Computing the distance to centres stat
  centroids<-temp.ttwa@data[,c("cent.x","cent.y")]
  ##  list of vectors denoting distance of a centre
  distances<-list(NULL)
  for(i in 1:nrow(temp.centres@data)){
    distances[[i]]<-euclid.dist(point=c(t(temp.centres@data[i,c('EastingC','NorthingC')])),x=centroids)
  }
  distances<-do.call(cbind,distances)
  ttwa.list[[j]]$dist.nearest<-apply(distances,1,min)
  ttwa.list[[j]]$centre<-as.character(temp.centres@data$LA[apply(distances,1,which.min)]) #tells us which centre
}

##  Accessibility based on Hansen (1959). Guess between -1 and -2 for the exponent
##  This is a for loop that calulcate the index (with an extra 100m added to distance for terminal time)
ttwa.emp<-list(NULL)
for (k in 1:length(ttwa.list)){
  
  temp.df<-ttwa.list[[k]]@data
  centroids<-temp.df[,c("cent.x","cent.y")]
  
  temp.emp<-list(NULL)
  for (i in 1:nrow(temp.df)){
    
    temp.dist<-euclid.dist(point=as.numeric(centroids[i,]),x=centroids)
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
RCI.tables<-list(NULL)
gRCI.tables<-list(NULL)
for (i in 1:length(ttwa.list)){
  
  temp.df<-ttwa.list[[i]]@data #we only need the data file from here on in 
  ##  We will need to work out the rci for the various cols
  temp.rci<-list(NULL)
  temp.grci<-list(NULL)
  
  for(j in 1:3){
    id2001<-which(names(temp.df)%in%c('jsa2001','is2001','ib2001'))[j]
    id2011<-which(names(temp.df)%in%c('jsa2011','is2011','ib2011'))[j] #this is the cols for the various year data fo each measure
    id.dist01<-which(names(temp.df)%in%c('dist.d','dist.nearest','hansen1.2001','hansen2.2001'))
    id.dist11<-which(names(temp.df)%in%c('dist.d','dist.nearest','hansen1.2011','hansen2.2011'))
    
    g.rci2001<-apply(temp.df[,id.dist01],2,g.rci,y=temp.df[,id2001],x=temp.df$st_areasha)
    g.rci2011<-apply(temp.df[,id.dist11],2,g.rci,y=temp.df[,id2011],x=temp.df$st_areasha)
    temp.tab<-cbind(g.rci2001,g.rci2011)
    out.tab<-aggregate(temp.tab,by=list(temp.df$centre),sum)
    total.ttwa<-c(colSums(out.tab[,-1]))
    total.ttwa<-data.frame(matrix(total.ttwa,ncol=2))
    total.ttwa$diff<-total.ttwa$X2-total.ttwa$X1
    
    temp.rci[[j]]<-total.ttwa
    temp.grci[[j]]<-out.tab
  }
  
  #rci table
  
  temp.res<-data.frame(do.call(cbind,temp.rci))
  temp.res<-round(temp.res,4)
  temp.res$city<-names(ttwa.list)[[i]]
  temp.res$stat<-c('dist.d','dist.nearest','hansen1','hansen2')
  RCI.tables[[i]]<-temp.res
  
  ##Tables  
  temp.res2<-data.frame(do.call(rbind,temp.grci))
  temp.diff2<-temp.res2[,6:9]-temp.res2[,2:5]
  temp.res2<-data.frame(temp.res2,temp.diff2)
  temp.res2[,-1]<-round(temp.res2[,-1],4)
  temp.res2$city<-names(ttwa.list)[[i]]
  temp.res2$stat<-c('jsa','is','ib')
  gRCI.tables[[i]]<-temp.res2
}
RCI.tables
RCI.tables<-do.call(rbind,RCI.tables)
colnames(RCI.tables)<-c(rep('jsa',3),rep('is',3),rep('ib',3))
write.csv(RCI.tables,file='../Results/ACI TTWA point estimates.csv')

gRCI.tables
gRCI.tables<-do.call(rbind,gRCI.tables)
write.csv(gRCI.tables,file='../Results/gACI TTWA point estimates.csv')


## End for now
rm(list = ls())
