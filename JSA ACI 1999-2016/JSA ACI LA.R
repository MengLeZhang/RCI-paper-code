##################################################################################
##  ACI analysis for LA                                                         ##
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

##  We need to merge the dataset with jsa data
jsa.df<-read.csv('../Data/DWP data/jsa 2000 to 2016.csv',stringsAsFactors = F)
jsa.df$lsoa01cd<-substr(jsa.df$super.output.areas...lower.layer,1,9)
table(jsa.df$lsoa01cd%in%ew.2001$lsoa01cd) #the missing variable is just the coloum total
ew.2001<-merge(ew.2001,jsa.df,by='lsoa01cd') #replace the ew.2001 with a merged version

##  City centre
city.centres<-read.csv('../Data/City centres/UK city centres.csv')

##  The subsetting to only certain cities used in the analysis
valid.cities<-as.character(city.centres$la[city.centres$la%in%ew.2001$la])
cities.list<-list(NULL)
for (i in 1:length(valid.cities)){
  cities.list[[i]]<-ew.2001[ew.2001$la%in%valid.cities[i],]
}
names(cities.list)<-valid.cities
##  So now we have a cities.list file of only the relevant cities
rm(ew.2001) #remove large .shp to save space

##  Second: We need to create two different variables. One is distance from city centre (we will choose mid point D). Another is by some sort of accessibility index

##  Distance by centre. Variable dist.d
for (i in 1:length(cities.list)){
  temp.mids<-city.centres[city.centres$la==valid.cities[i],]
  centroids<-cities.list[[i]]@data[,c("cent.x","cent.y")]
  cities.list[[i]]$dist.d<-euclid.dist(point=c(t(temp.mids[,9:10])),x=centroids)
}

##  Accessibility based on Hansen (1959). Guess between -1 and -2 for the exponent
##  This is a for loop that calulcate the index (with an extra 100m added to distance for terminal time)
cities.emp<-list(NULL)
for (k in 1:length(cities.list)){
  temp.df<-cities.list[[k]]
  centroids<-cities.list[[k]]@data[,c("cent.x","cent.y")]
  temp.df<-cities.list[[k]]@data
  
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
  cities.emp[[k]]<-do.call(rbind,temp.emp)
  colnames(cities.emp[[k]])<-c('hansen1.2001','hansen1.2011','hansen2.2001','hansen2.2011')
}

for (i in 1:length(cities.list)){
  cities.list[[i]]<-cbind(cities.list[[i]],cities.emp[[i]])
}


##  ACI routine----
##  Third: Now for each city we have to establish a routine for working out the ACI results
##  There is a variable st_areasha that gives us the area

##  First we will get the point estimates; this is for checking as much as anything else
##  We repeat it for every col for jsa
ACI.tables<-list(NULL)
for (i in 1:length(cities.list)){
  
  temp.df<-cities.list[[i]]@data #we only need the data file from here on in 
  names(temp.df)
  ##  We will need to work out the rci for the various cols
  ##all the census stuff is for may so we find the cols that say may
    which.may<-grep('May',names(temp.df))
    
    aci.may<-apply(temp.df[,which.may],2,rci,sort.var=temp.df$dist.d,x=temp.df$st_areasha)
    
  ACI.tables[[i]]<-aci.may
}
ACI.tables
ACI.tables<-do.call(rbind,ACI.tables)
ACI.tables<-data.frame(ACI.tables,city=names(cities.list))
write.csv(ACI.tables,file='../Results/ACI LA point estimates 00-16.csv')

##> Table of centre populations within 2km;
ew.cities<-do.call(rbind,cities.list)
names(ew.cities)
stats<-c('May.08','May.09','dist.d')

la2k<-subset(ew.cities@data,subset=dist.d<2000)
la2k.tab<-aggregate(la2k[,stats],by=list(la2k$la),FUN=sum)
write.csv(la2k.tab,'../Results/JSA count within 2k LA.csv')

## End for now
rm(list = ls())
