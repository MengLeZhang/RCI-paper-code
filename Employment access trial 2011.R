#########################################################
##  RCI by employment access trial: Sheffield         ###
##  We are going to try something different here      ###
##  Employment access is measured by local aci        ###
##  We will trial the routine                         ###
##  Start: 7/3/2017                                   ###
##  We have the 2001 and 2011 workplace data but we   ###
##  need to trial several different ways of doing the ###
##  orderings of the rci                              ###
#########################################################

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
rm(ew.2001)
##  Stage 1: Creating the variable----
##  We need a for loop routine: for each city k calculate the ACI for each i zone. This means getting hold of the distance from one centre to all the other centres
##  This will produce in theory a list cities.laci which contains the local Aci for each point in a city
head(cities.list[[1]])
cities.laci<-list(NULL)
for (k in 1:length(cities.list)){
  temp.df<-cities.list[[k]]
  centroids<-getSpPPolygonsLabptSlots(temp.df)
  temp.df<-cities.list[[k]]@data
  
  temp.laci<-list(NULL)
  for (i in 1:nrow(temp.df)){
    temp.dist<-euclid.dist(point=centroids[i,],x=centroids)
    temp.out<-list(NULL)

    ## 2001 accessibility index
    aci2001<-rci(sort.var=temp.dist,y=temp.df$work.pop2001,x=temp.dist)
    hansen1.2001<-sum(temp.df$work.pop2001*(temp.dist+100)^(-1))
    hansen2.2001<-sum(temp.df$work.pop2001*(temp.dist+100)^(-2))

    ## 2011 accessibility index
    aci2011<-rci(sort.var=temp.dist,y=temp.df$work.pop2011,x=temp.dist)
    hansen1.2011<-sum(temp.df$work.pop2011*(temp.dist+100)^(-1))
    hansen2.2011<-sum(temp.df$work.pop2011*(temp.dist+100)^(-2))
    
    temp.laci[[i]]<-cbind(aci2001,aci2011,hansen1.2001,hansen1.2011,hansen2.2001,hansen2.2011)
  }
  cities.laci[[k]]<-do.call(rbind,temp.laci)
  colnames(cities.laci[[k]])<-c('aci2001','aci2011','hansen1.2001','hansen1.2011','hansen2.2001','hansen2.2011')
}

for (i in 1:length(cities.list)){
  cities.list[[i]]<-cbind(cities.list[[i]],cities.laci[[i]])
}

##  This is just checking the cor
for (i in 1:length(cities.list)){
print(names(cities.list)[i])
print(cor(cities.list[[i]]@data[,c('aci2001','hansen1.2001','hansen2.2001')]),method='spearman') #really high spearman's rank
}


qtm(cities.list[[1]],fill='hansen.x.2') #ah right we have to use the inverse of the local aci here

##  Stage 2: Calculating the RCI/ Gini?----
##  This is straight up how we are calculating the RCI
gini.tables<-list(NULL)
for (i in 1:length(cities.list)){
  i<-1
  temp.df<-cities.list[[i]]@data #we only need the data file from here on in 

  ##  We will need to work out the rci for the various cols
  temp.out<-list(NULL)
  for(j in 1:3){
    
    id2001<-which(names(temp.df)%in%c('jsa2001','is2001','ib2001'))[j]
    id2011<-which(names(temp.df)%in%c('jsa2011','is2011','ib2011'))[j]
    
    id.dist01<-which(names(temp.df)%in%c('aci2001','hansen1.2001','hansen2.2001'))
    id.dist11<-which(names(temp.df)%in%c('aci2011','hansen1.2011','hansen2.2011'))
    cbind()
    id.dist11
    gini2011
    gini2001<-apply(-temp.df[,id.dist01],2,rci,y=temp.df[,id2001],x=round(temp.df$w.pop2001-temp.df[,id2001])) #use
    gini2011<-apply(-temp.df[,id.dist11],2,rci,y=temp.df[,id2011],x=round(temp.df$w.pop2011-temp.df[,id2011])) #use negative lcoal aci
    temp.out[[j]]<-gini2011
  }

  temp.res<-data.frame(do.call(cbind,temp.out))
  temp.res<-round(temp.res,4)
  temp.res$city<-names(cities.list)[[i]]
  gini.tables[[i]]<-temp.res
}
gini.tables

gini.tables<-do.call(rbind,gini.tables)
write.csv(gini.tables,file='gini LA point estimates 2011.csv')

