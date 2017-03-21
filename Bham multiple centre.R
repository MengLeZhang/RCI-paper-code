################################################
##  Multiple centres                        ####
##  Let's do the RCI given multiple centres ####
##  for TTWAs                               ####
##  This is a temp one for creating the TTWA####
##  multiple centre RCI for anotehr city    ####
##  Let's do Birmingham                     ####
##  Start: 23/2/2017                        ####
################################################

##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  Step one: As usual we load in the england and wales data and the city centres list. Then we need to read in the TTWA file. We also read in the rural urban classification.
load('LSOA data/Analysis file/England and Wales benefits 0111.Rdata')

##  load in ttwa
TTWA.2011<- readOGR(dsn='LSOA data/TTWA 2011', layer='Travel_to_Work_Areas_December_2011_Full_Extent_Boundaries_in_United_Kingdom') 

##  We can use get the TTWA boundaires; shrink them a bit; find which LAs the cover and then get the city centre for those LAs. We will do Bham
bham.id<-which(TTWA.2011@data$ttwa11nm%in%'Birmingham')
bham.ttwa<-TTWA.2011[Bham.id,]
bham.ttwa<-gBuffer(bham.ttwa, byid=TRUE, width=-1) #shrinking
bham.lsoa<-ew.2001[bham.ttwa,]
write.csv(table(bham.lsoa$la),'City centres/Bham las.csv')
## We will then get the quick list of centres for these places; find places in these LAs where there is a mjor settlement over 70k. Once that is done we will read in the special files

##  Step two: Create spatial centres: We will using the trai nstations/main transport
##  Create a centres spatial object
bham.centres<-read.csv('City centres/Bham las centres.csv',stringsAsFactors = F)
names(bham.centres)
centres<-na.omit(bham.centres[,c(2,4,5)])
centres.sp<-SpatialPointsDataFrame(coords= coordinates(centres[,c('EastingA','NorthingA')]),data=data.frame(centres),proj4string=CRS(proj4string(TTWA.2011)))
centres.sp
##  Step three: We can take the TTWA for one place first; manchester
plot(bham.lsoa)
rm(ew.2001);rm(TTWA.2011)
qtm(bham.lsoa,fill='jsa2001')+qtm(centres.sp,text='Var1')
##  Definitely seems like centralisation in this picture
head(centres.sp)# let's omit sandwell aand walsall since their centres are well outside the map
#centres.sp<-centres.sp[-c(3,6),] #tried to omit sandwell and walsall but it makes no diff

##  Step four: Right now we can try to compute RCI
centroids<-getSpPPolygonsLabptSlots(bham.lsoa)
centres.sp
distances<-list(NULL)
for(i in 1:nrow(centres.sp@data)){
  distances[[i]]<-euclid.dist(point=c(t(centres.sp@data[i,2:3])),x=centroids)
}
head(distances)
distances<-do.call(cbind,distances)
bham.lsoa$dist<-apply(distances,1,min)
bham.lsoa$centre<-apply(distances,1,which.min) #tell us which centre
bham.lsoa$centre<-centres.sp@data[apply(distances,1,which.min),1]
qtm(bham.lsoa,fill='centre')+qtm(centres.sp,text='Var1')

##  Okay now for the RCI computation
temp.out<-list(NULL)
temp.df<-bham.lsoa@data
temp.df
for(j in 1:3){
  head(temp.df)
  id2001<-which(names(temp.df)%in%c('jsa2001','is2001','ib2001'))[j]
  id2011<-which(names(temp.df)%in%c('jsa2011','is2011','ib2011'))[j] #this is the cols for the various year data fo each measure

  rci2001<-rci(sort.var=temp.df$dist,y=temp.df[,id2001],x=temp.df$w.pop2001-temp.df[,id2001])
  rci2011<-rci(sort.var=temp.df$dist,y=temp.df[,id2011],x=temp.df$w.pop2011-temp.df[,id2011])
  temp.out[[j]]<-cbind(rci2001,rci2011)
}

##  now to see the results
temp.out #to be honestit doesn't change the results too much
##  So in conclusion the numbers related to JSA does decentralise
write.csv(c(temp.out),'Results/TTWA boundaries/bham multi rci.csv')

##  1/2/2017: Let's get out the disaggregated RCI
head(bham.lsoa@data)
temp.df<-bham.lsoa@data[order(bham.lsoa@data$dist),]

test<-function(df=temp.df,var){head(df$var)}
test(var=jsa2001)

Rci.disagg<-function(y,pop,df=temp.df){
  cumy<-cumsum(y)/sum(y)
  cumx<-cumsum(pop-y)/sum(pop-y)
  n.lsoa<-length(y)
  
  part.a<-(cumx-c(0,cumx[-n.lsoa])) #b[n]-b[n-1]
  part.b<-(cumy+c(0,cumy[-n.lsoa]))##a[n]+a[n-1]
  part.c<-(cumx+c(0,cumx[-n.lsoa])) #b[n]+b[n-1]
  
  rci.dis<-part.a*(part.b-part.c)
  
  return(aggregate(rci.dis,by=list(df$centre),sum))
}


bham.dis<-cbind(Rci.disagg(temp.df$jsa2001,temp.df$w.pop2001),Rci.disagg(temp.df$jsa2011,temp.df$w.pop2011),
      Rci.disagg(temp.df$is2001,temp.df$w.pop2001),Rci.disagg(temp.df$is2011,temp.df$w.pop2011),
      Rci.disagg(temp.df$ib2001,temp.df$w.pop2001),Rci.disagg(temp.df$ib2011,temp.df$w.pop2011))



write.csv(bham.dis,'Results/TTWA boundaries/bham multi dis rci.csv')


