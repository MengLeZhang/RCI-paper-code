##  Robustness test: ####
##  This is the LA analysis but      #
##  omitting those areas where the 2001 has changed   #
##  This is to test the sensitive of the analysis     #
##  Start: 7/2/2017                                   #
##  Update: 27/4/2017 We will try to efficiently do   #
##  this recycling most of the rci code. 

##  Pre: Load in all the RCI functions we need ----
source.file<-'RCI functions.R' #path to source
source(source.file)


##  First: we load in the map and variables datasets ####
##  Data load;
load('../Data/Analysis data/England and Wales benefits 0111 final.Rdata')
city.centres<-read.csv('../Data/City centres/UK city centres.csv')
conversion.0111<-read.csv('../Data/LSOA 2001 to 2011/LSOA01_LSOA11_LAD11_EW_LU.csv',stringsAsFactors = F)

##  !This is the syntax change!
unchanged<-unique(conversion.0111$LSOA01CD[conversion.0111$CHGIND=='U'])  #pick out LSOA codes that are unchanged
ew.2001<-ew.2001[ew.2001$lsoa01cd%in%unchanged,] #subset to only those unchanged codes
nrow(ew.2001@data)
###

##  The subsetting to only certain cities
valid.cities<-as.character(city.centres$la[city.centres$la%in%ew.2001$la])
cities.list<-list(NULL)
for (i in 1:length(valid.cities)){
  cities.list[[i]]<-ew.2001[ew.2001$la%in%valid.cities[i],]
}
names(cities.list)<-valid.cities
##  So now we have a cities.list file of only the relevant cities
rm(ew.2001) #remove to save space

##  Second: We need to create two different variables. One is distance from city centre (we will choose mid point D). Another is by some sort of accessibility index ####

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

##  RCI routine #####
##  Now for each city we have to establish a routine for working out the RCI results
##  This is split into several secions

##  First we will get the point estimates; this is for checking as much as anything else----
RCI.tables<-list(NULL)
for (i in 1:length(cities.list)){
  
  temp.df<-cities.list[[i]]@data #we only need the data file from here on in 
  names(temp.df)
  ##  We will need to work out the rci for the various cols
  temp.out<-list(NULL)
  for(j in 1:3){
    
    id2001<-which(names(temp.df)%in%c('jsa2001','is2001','ib2001'))[j]
    id2011<-which(names(temp.df)%in%c('jsa2011','is2011','ib2011'))[j] #this is the cols for the various year data fo each measure
    id.dist01<-which(names(temp.df)%in%c('dist.d','hansen1.2001','hansen2.2001'))
    id.dist11<-which(names(temp.df)%in%c('dist.d','hansen1.2011','hansen2.2011'))
    
    rci2001<-apply(temp.df[,id.dist01],2,rci,y=temp.df[,id2001],x=temp.df$w.pop2001-temp.df[,id2001])
    rci2011<-apply(temp.df[,id.dist11],2,rci,y=temp.df[,id2011],x=round(temp.df$w.pop2011)-temp.df[,id2011])
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
RCI.tables
RCI.tables<-do.call(rbind,RCI.tables)
write.csv(RCI.tables,file='../Results/Robustness test 1/RCI LA point estimates RT1.csv')


##  DI results: point estimates only ----
##  This has been made by essentially modifying the bayesian code
di.results<-list(NULL)
for (i in 1:length(cities.list)){
  saved.name<-names(cities.list)[[i]]
  temp.df<-cities.list[[i]]
  var.name<-c('jsa','ib','is')

  saved.results<-list(NULL) #A list that will contain 3 objects; the saved results for jsa, ib, is
  
  for (j in 1:3){

    
    id2001<-which(names(temp.df)%in%c('jsa2001','is2001','ib2001'))[j]
    id2011<-which(names(temp.df)%in%c('jsa2011','is2011','ib2011'))[j] 
    
    pred01<-temp.df@data[,id2001]
    pred11<-temp.df@data[,id2011]
    
    ##Calculating the DI
    di01<-sum(abs(pred01/sum(pred01)-(temp.df$w.pop2001-pred01)/sum(temp.df$w.pop2001-pred01)))/2
    di11<-sum(abs(pred11/sum(pred11)-(temp.df$w.pop2011-pred11)/sum(temp.df$w.pop2011-pred11)))/2
    saved.results[[j]]<-cbind(di01,di11,diff=di11-di01)
    colnames(saved.results[[j]])<-paste(var.name[j],colnames(saved.results[[j]]))
  }
  
  di.results[[i]]<-cbind(do.call(cbind,saved.results),city=saved.name)
}
di.raw.tab<-do.call(rbind,di.results)

write.csv(di.raw.tab,file='../Results/Robustness test 1//DI LA raw CI.csv')


## End script: ####
rm(list = ls())
