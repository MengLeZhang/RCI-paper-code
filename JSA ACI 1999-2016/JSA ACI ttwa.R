##################################################################################
##  ACI analysis for ttwa                                                         ##
##  This is a script that looks at the ACI over time                            ##
##  but using the dwp jsa over a period of years                                ##
##  Start: 8/5/2017                                                            ##
##################################################################################
##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
##  Data load;
load('../Data/Analysis data/England and Wales benefits 0111 final.Rdata')
used.crs<-crs(ew.2001)

##  We need to merge the dataset with jsa data
jsa.df<-read.csv('../Data/DWP data/jsa 2000 to 2016.csv',stringsAsFactors = F)
jsa.df$lsoa01cd<-substr(jsa.df$super.output.areas...lower.layer,1,9)
table(jsa.df$lsoa01cd%in%ew.2001$lsoa01cd) #the missing variable is just the coloum total
ew.2001<-merge(ew.2001,jsa.df,by='lsoa01cd') #replace the ew.2001 with a merged version


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
##  We repeat it for every col for jsa
ACI.tables<-list(NULL)
for (i in 1:length(ttwa.list)){
  temp.df<-ttwa.list[[i]]@data #we only need the data file from here on in 
  names(temp.df)
  ##  We will need to work out the rci for the various cols
  ##all the census stuff is for may so we find the cols that say may
  which.may<-grep('May',names(temp.df))

  aci.may.d<-apply(temp.df[,which.may],2,rci,sort.var=temp.df$dist.d,x=temp.df$st_areasha)
  aci.may.near<-apply(temp.df[,which.may],2,rci,sort.var=temp.df$dist.nearest,x=temp.df$st_areasha)
  
  ACI.tables[[i]]<-data.frame(rbind(aci.may.d,aci.may.near),city=names(ttwa.list)[i])
  
}
ACI.tables
ACI.tables<-do.call(rbind,ACI.tables)
write.csv(ACI.tables,file='../Results/ACI ttwa point estimates 00-16.csv')


## End for now
rm(list = ls())