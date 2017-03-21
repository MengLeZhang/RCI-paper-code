################################################
##  Multiple centres                        ####
##  This is the script for multiple centres ####
##  Start: 23/2/2017                        ####
################################################

##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  Step one: As usual we load in the england and wales data and the city centres list. Then we need to read in the TTWA file. We also read in the rural urban classification.
load('LSOA data/Analysis file/England and Wales benefits 0111.Rdata')

##  load in ttwa
TTWA.2011<- readOGR(dsn='LSOA data/TTWA 2011', layer='Travel_to_Work_Areas_December_2011_Full_Extent_Boundaries_in_United_Kingdom') 
TTWA.2011<-gBuffer(TTWA.2011, byid=TRUE, width=-1)

##  Read in city centres file.
city.centres<-read.csv('City centres/UK city centres.csv')

##  Step two: Get the city centre points
mono.centres.sp<-SpatialPointsDataFrame(coords= coordinates(city.centres[-1,c('EastingD','NorthingD')]),data=data.frame(city.centres[-1,]),proj4string=CRS(proj4string(TTWA.2011)))
##  Now we subset to just the ttwa that we are interested in
sub.ttwa<-TTWA.2011[mono.centres.sp,]
sub.ttwa<-sub.ttwa[ew.2001,]


table.la<-list(NULL)
for (i in 1:length(mono.centres.sp)){
  temp.ttwa<-sub.ttwa[i,]
  table.la[[i]]<-cbind(ttwa=as.character(sub.ttwa$ttwa11nm[i]),la=unique(ew.2001[temp.ttwa,]$la))
}
no.na<-lapply(table.la,length)>1 #basically we select those which have entries
table.la<-do.call(rbind,table.la[no.na])
write.csv(table.la,'City centres/ttwa 2011 la overlaps.csv')
table(table.la[,2])#some are used more than once
write.csv(table(table.la[,2]),'City centres/ttwa 2011 la counts.csv')


##  Step three: Right; we have labouriously collected the location of big urban centres. Now let's go and try to do the multiple rci for all the ttwas that we have. 
ttwa.centres<-read.csv('City centres/ttwa 2011 la centres.csv',stringsAsFactors = F)
ttwa.centres<-na.omit(ttwa.centres[1:4]) #we do not need the last note col
str(ttwa.centres)
ttwa.centres$EastingC<-as.numeric(ttwa.centres$EastingC)
ttwa.centres.sp<-SpatialPointsDataFrame(coords= coordinates(ttwa.centres[,c('EastingC','NorthingC')]),data=data.frame(ttwa.centres),proj4string=CRS(proj4string(TTWA.2011)))

##  Define the function 
all.outs<-list(NULL)
for (j in 1:length(sub.ttwa)){
temp.ttwa<-sub.ttwa[j,]
temp.la<-unique(ew.2001[temp.ttwa,]$la)
temp.centres<-ttwa.centres.sp[ttwa.centres.sp$LA%in%temp.la,]
temp.city<-ew.2001[temp.ttwa,]

## Computing the distance to centres stat
centroids<-getSpPPolygonsLabptSlots(temp.city)
distances<-list(NULL)
for(i in 1:nrow(ttwa.centres.sp@data)){
  distances[[i]]<-euclid.dist(point=c(t(ttwa.centres.sp@data[i,c('EastingC','NorthingC')])),x=centroids)
}
head(distances)
distances<-do.call(cbind,distances)
temp.city$dist<-apply(distances,1,min)
temp.city$centre<-apply(distances,1,which.min) #tell us which centre
temp.city$centre<-ttwa.centres.sp@data[apply(distances,1,which.min),1]

##  Now for the grci
table.list<-list(NULL)
for (k in 1:3){
  id2001<-which(names(temp.city)%in%c('jsa2001','is2001','ib2001'))[k]
  id2011<-which(names(temp.city)%in%c('jsa2011','is2011','ib2011'))[k] #this is the cols for the various year data fo each measure
  g.rci2001<-g.rci(x=temp.city$w.pop2001-temp.city@data[,id2001],y=temp.city@data[,id2001],sort.var=temp.city$dist)
  g.rci2011<-g.rci(x=temp.city$w.pop2011-temp.city@data[,id2011],y=temp.city@data[,id2011],sort.var=temp.city$dist)
  
  ##  Making the tables
  tab.a<-aggregate(g.rci2001,by=list(temp.city$centre),sum)
  tab.b<-aggregate(g.rci2011,by=list(temp.city$centre),sum)
  tab<-data.frame(centre=tab.a$Group.1,ben2001=tab.a$x,ben2011=tab.b$x)
  tab$centre<-as.character(tab$centre)
  total<-c('Total',colSums(tab[,2:3]))
  table.list[[k]]<-rbind(tab,total)
}
out<-do.call(cbind,table.list)
out<-data.frame(out,ttwa=temp.ttwa$ttwa11nm)
all.outs[[j]]<-out
}
all.outs.tab<-do.call(rbind,all.outs)

write.csv(all.outs.tab,'Results/ttwa grci.csv')
