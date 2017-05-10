##################################################################################
##  ACI analysis for London                                                         ##
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

##  Subset the LSOA file to just london
london<-subset(ew.2001,ttwa=='London')
rm(ew.2001) # remove shp file to save space

## Get the city centre points
##  Read in city centres file.
city.centres<-read.csv('../Data/City centres/UK city centres.csv')
##  For now we will omit London (which we will handle with a diff script)
mono.centres.coords<-coordinates(city.centres[,c('EastingD','NorthingD')])
mono.centres.sp<-SpatialPointsDataFrame(mono.centres.coords, city.centres, proj4string = used.crs)

##  Step two: 
##  The subsetting to only certain ttwa
ttwa.list<-list(london)
names(ttwa.list)<-'London'


##  Second: Unlike LA we need 3 different distance ordering variables. One is distance from city centre (we will choose mid point D). Another is closes distance to a centre. Yet another is by some sort of accessibility index

##  Distance by centre. Variable dist.d. Each ttwa has only one match to a city
for (i in 1:length(ttwa.list)){
  temp.mids<-mono.centres.sp[ttwa.list[[i]],]
  centroids<-ttwa.list[[i]]@data[,c("cent.x","cent.y")]
  ttwa.list[[i]]$dist.d<-euclid.dist(point=c(t(temp.mids@data[,9:10])),x=centroids)
}

##  Distance to nearest centre
##  Not necesarily for London

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
  
  ACI.tables[[i]]<-aci.may.d
  
}
ACI.tables
ACI.tables<-do.call(rbind,ACI.tables)
write.csv(ACI.tables,file='../Results/ACI london point estimates 00-16.csv')


## End for now
rm(list = ls())
