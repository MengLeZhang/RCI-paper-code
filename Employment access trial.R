#########################################################
##  RCI by employment access trial: Sheffield         ###
##  We are going to try something different here      ###
##  Employment access is measured by local aci        ###
##  We will trial the routine using sheffield as the  ###
##  example                                           ###
##  Start: 7/3/2017                                   ###
#########################################################

##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
##  Data load;
load('LSOA data/Analysis file/England and Wales benefits 0111.Rdata')
oa.lsoa.tab<-read.csv('LSOA data/OA 2001/OA to LSOA 2001 lookup table.csv',stringsAsFactors = F)
oa.workpop.2001<-read.csv('LSOA data/OA 2001/work pop sheffield 2001.csv',stringsAsFactors = F)
oa.workpop.2001<-oa.workpop.2001[substr(oa.workpop.2001$Area,1,6)=='oa2001',] #single out the oa codes
oa.workpop.2001$Area<-substr(oa.workpop.2001$Area,8,nchar(oa.workpop.2001$Area))

sum(!oa.workpop.2001$Area%in%oa.lsoa.tab$oa)## all in
oa.lsoa.tab$workpop2001<-oa.workpop.2001$X2001.00[match(oa.lsoa.tab$oa,oa.workpop.2001$Area)]

lsoa.workpop.2001<-aggregate(oa.lsoa.tab$workpop2001,by=list(oa.lsoa.tab$lsoa),sum)
na.omit(lsoa.workpop.2001)

##  Appending the data
ew.2001$workpop.2001<-lsoa.workpop.2001$x[match(ew.2001$lsoa01cd,lsoa.workpop.2001$Group.1)]
summary(ew.2001$workpop.2001)
##  Stage 1: Creating the variable----

##  We will still need to subset to the cities we need
cities.list<-list(Sheffield=ew.2001[ew.2001$la%in%'Sheffield',])
summary(cities.list$Sheffield@data) #all good
qtm(cities.list$Sheffield,fill='workpop.2001') #quick plot to show that yes the city centre does contain most of the employment
##  So now we have a cities.list file of only the relevant cities
rm(ew.2001) #remove to save space

##  Second: We need a for loop routine: for each city k calculate the ACI for each i zone. This means getting hold of the distance from one centre to all the other centres
##  This will produce in theory a list cities.laci which contains the local Aci for each point in a city

cities.laci<-list(NULL)
k<-1
for (k in 1:length(cities.list)){
  temp.df<-cities.list[[k]]
  centroids<-getSpPPolygonsLabptSlots(temp.df)
  temp.df<-cities.list[[k]]@data
  
  
  temp.laci<-list(NULL)
  for (i in 1:nrow(temp.df)){
    
    temp.dist<-euclid.dist(point=centroids[i,],x=centroids)
    temp.out<-list(NULL)
    log(temp.dist)+0.1
    aci2001<-rci(sort.var=temp.dist,y=temp.df$workpop.2001,x=temp.dist)
    log.aci2001<-rci(sort.var=log(temp.dist+0.01),y=temp.df$workpop.2001,x=log(temp.dist+0.01))
    temp.laci[[i]]<-cbind(aci2001,log.aci2001)
  }
  cities.laci[[k]]<-do.call(rbind,temp.laci)
  colnames(cities.laci[[k]])<-c('aci2001','log.aci2001')
}

for (i in 1:length(cities.list)){
  cities.list[[i]]<-cbind(cities.list[[i]],cities.laci[[i]])
}
cities.list[[i]]@data
cor(x=cities.list[[i]]$aci2001,y=cities.list[[i]]$log.aci2001)
cor(x=cities.list[[i]]$aci2001,y=cities.list[[i]]$log.aci2001,method='spearman') #really high spearman's rank

qtm(cities.list[[1]],fill='aci2001') #ah right we have to use the inverse of the local aci here

##  Stage 2: Calculating the RCI/ Gini?----
##  This is straight up how we are calculating the RCI
gini.tables<-list(NULL)
for (i in 1:length(cities.list)){
  
  temp.df<-cities.list[[i]]@data #we only need the data file from here on in 
  names(temp.df)
  ##  We will need to work out the rci for the various cols
  temp.out<-list(NULL)
  for(j in 1:3){
    
    id2001<-which(names(temp.df)%in%c('jsa2001','is2001','ib2001'))[j]
    id.dist<-which(names(temp.df)%in%c('aci2001','log.aci2001'))
    
    gini2001<-apply(-temp.df[,id.dist],2,rci,y=temp.df[,id2001],x=temp.df$w.pop2001-temp.df[,id2001]) #use negative lcoal aci
    temp.out[[j]]<-gini2001
  }
  temp.out
  temp.res<-data.frame(do.call(cbind,temp.out))
  temp.res<-round(temp.res,4)
  temp.res$city<-names(cities.list)[[i]]
  gini.tables[[i]]<-temp.res
}

gini.tables<-do.call(rbind,gini.tables)
write.csv(gini.tables,file='gini LA point estimates.csv')

