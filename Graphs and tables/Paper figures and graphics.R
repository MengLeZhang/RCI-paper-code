################################################################
##  Graphics for paper                                        ##
##  This is an organising file that records the misc. figures ##
##  used in the paper                                         ##
##  Start: 8/2/2017                                           ##
################################################################

# Load library
source.file<-'RCI functions.R' #path to source
source(source.file)

##  1) A figure showing incompatible LSOAs in 2001 and 2011----
##  Firstt load in the maps
load('LSOA data/England and Wales 0111.Rdata')
shp.2011<-readOGR(dsn='LSOA data/LSOA 2011', layer='Lower_Layer_Super_Output_Areas_December_2011_Full_Extent__Boundaries_in_England_and_Wales')

##  Find example of a changed LSOA
conversion.0111<-read.csv('LSOA data/LSOA 2001 to 2011/LSOA01_LSOA11_LAD11_EW_LU.csv',stringsAsFactors = F)
con.sub<-conversion.0111[conversion.0111$CHGIND=='X',]
con.sub[con.sub$LSOA11CD%in%'E01032606',]
##E01011288 in leeds is one LSOA in 2001 that has an odd relationship to LSOA 2011
example.lsoa<-ew.2001[ew.2001$lsoa01cd=='E01011288',]
example.lsoa <- gBuffer(example.lsoa, byid=TRUE, width=-1) #make the border slightly smaller to show image better
##  Example

plot(shp.2011[example.lsoa,],border='blue')
plot(example.lsoa, border='red',,add=T)


##  2) Example of multiple RCI; using the Leeds example
ttwa<- readOGR(dsn='TTWA 2011', layer='Travel_to_Work_Areas_December_2011_Full_Extent_Boundaries_in_United_Kingdom') 
ttwa.centres<-read.csv('City centres/UK city centres ttwa.csv')

centres<-na.omit(ttwa.centres[,c(1,7,8)])
centres.sp<-SpatialPointsDataFrame(coords= coordinates(centres[,c('EastingC','NorthingC')]),data=data.frame(centres),proj4string=CRS(proj4string(ttwa)))

##  We can take the TTWA for one place: manchester and include Stockport 
manchester<-ttwa[ttwa$ttwa11nm=='Manchester',]
manchester<-gBuffer(manchester, byid=TRUE, width=-0.5) #need to slightly reduce border
man.centres<-centres.sp[centres.sp$Cities..towns...districts%in%c('Manchester','Stockport'),]
manchester<-ew.2001[manchester,]
rm(ew.2001)

##  Next we get the distance from each centroid
centroids<-getSpPPolygonsLabptSlots(manchester)

distances<-list(NULL)
for(i in 1:2){
  distances[[i]]<-euclid.dist(point=c(t(man.centres@data[i,2:3])),x=centroids)
}
distances<-do.call(cbind,distances)
manchester$dist<-apply(distances,1,min)
manchester$centre<-apply(distances,1,which.min) #tell us which centre
manchester$centre<-as.character(man.centres@data[apply(distances,1,which.min),1])


##  Now let's subset the picture 
qtm(manchester)+qtm(man.centres[1,],text='Cities..towns...districts')
plot.A<-tm_shape(manchester)+tm_fill(col='centre',palette = c('red','blue'),alpha=0.5)+tm_borders()+qtm(man.centres,text='Cities..towns...districts')

## Now for the gg plot; we will need to only do the first 500 or so zones just to show the method
manchester1<-manchester@data[order(distances[,1]),] #first plot for manchester only
manchester1$cum.x<-cumsum(manchester1$w.pop2001-manchester1$jsa2001)/sum(manchester1$w.pop2001-manchester1$jsa2001)
manchester1$cum.y<-cumsum(manchester1$jsa2001)/sum(manchester1$jsa2001)

##  Now for the rest
manchester2<-manchester@data[order(manchester$dist),]
manchester2<-manchester2[1:600,]
manchester2$cum.x<-cumsum(manchester2$w.pop2001-manchester2$jsa2001)/sum(manchester2$w.pop2001-manchester2$jsa2001)
manchester2$cum.y<-cumsum(manchester2$jsa2001)/sum(manchester2$jsa2001)


##  First plot is the generic one centre RCI
plot.B<-ggplot(manchester1, aes(x=cum.x, y=cum.y)) +
  geom_line()+
  geom_line(data=manchester1, aes(x=cum.x, y=cum.x))+
  geom_ribbon(data=manchester1, aes(ymax=cum.y,ymin=cum.x),fill="grey", alpha=0.8)+
  ylab('Cumulative proportion of claimants')+xlab('Cumulative proportion of non-claimants')
plot.B

##  The ggplot routine is just WAY too laggy; it will not allow us to do like the 1000+lines so instead we need to basically time down the RCI to the first 600 or so cases of manchester
start.man<-which(manchester2$centre=='Manchester')
start.stock<-which(manchester2$centre=='Stockport')

##  Basically to do this we are writing a lot of lines of geom_ribbon code
A<-'ggplot(manchester2, aes(x=cum.x, y=cum.y)) +
  geom_line()+
geom_line(data=manchester2, aes(x=cum.x, y=cum.x))'
for(i in 1:length(start.man)){
  A<-paste(A,'+geom_ribbon(data=manchester2[c(',start.man[i],',',start.man[i]+1,'),],
           aes(ymax=cum.y,ymin=cum.x),fill="red", alpha=0.5)')
}
for(i in 1:length(start.stock)){
  A<-paste(A,'+geom_ribbon(data=manchester2[c(',start.stock[i],',',start.stock[i]+1,'),],
           aes(ymax=cum.y,ymin=cum.x),fill="blue", alpha=0.5)')
}
man.rci.plot<-eval(parse(text=A))
plot.C<-man.rci.plot+
  ylab('Cumulative proportion of claimants')+xlab('Cumulative proportion of non-claimants')
plot.C


##  3) Rcplots of regions with very large drops in each of the three stats
city.centres<-read.csv('City centres/UK city centres.csv')

##  get a new statistic for the proportion of people living in a spot
ew.2001$jsa2001.prop<-ew.2001$jsa2001/sum(ew.2001$jsa2001)*1000
ew.2001$jsa2011.prop<-ew.2001$jsa2011/sum(ew.2001$jsa2011)*1000
ew.2001$is2001.prop<-ew.2001$is2001/sum(ew.2001$is2001)*1000
ew.2001$is2011.prop<-ew.2001$is2011/sum(ew.2001$is2011)*1000
ew.2001$ib2001.prop<-ew.2001$ib2001/sum(ew.2001$ib2001)*1000
ew.2001$ib2011.prop<-ew.2001$ib2011/sum(ew.2001$ib2011)*1000


ew.2001$n.jsa2001.prop<-(ew.2001$w.pop2001-ew.2001$jsa2001)/sum(ew.2001$w.pop2001-ew.2001$jsa2001)*1000
ew.2001$n.jsa2011.prop<-(ew.2001$w.pop2011-ew.2001$jsa2011)/sum(ew.2001$w.pop2011-ew.2001$jsa2011)*1000
ew.2001$n.is2001.prop<-(ew.2001$w.pop2001-ew.2001$is2001)/sum(ew.2001$w.pop2001-ew.2001$is2001)*1000
ew.2001$n.is2011.prop<-(ew.2001$w.pop2011-ew.2001$is2011)/sum(ew.2001$w.pop2011-ew.2001$is2011)*1000
ew.2001$n.ib2001.prop<-(ew.2001$w.pop2001-ew.2001$ib2001)/sum(ew.2001$w.pop2001-ew.2001$ib2001)*1000
ew.2001$n.ib2011.prop<-(ew.2001$w.pop2011-ew.2001$ib2011)/sum(ew.2001$w.pop2011-ew.2001$ib2011)*1000


names(ew.2001)

##  The subsetting to only certain cities
valid.cities<-as.character(city.centres$la[city.centres$la%in%ew.2001$la])
cities.list<-list(NULL)
for (i in 1:length(valid.cities)){
  cities.list[[i]]<-ew.2001[ew.2001$la%in%valid.cities[i],]
}
names(cities.list)<-valid.cities

##  Plot JSA for Brighton which has huge JSA change
names(cities.list)
Brigh<-grep('Brighton',names(cities.list))
Brigh<-cities.list[[Brigh]]

qtm(Brigh,fill='jsa2001.prop')
qtm(Brigh,fill='jsa2011.prop')

##  Plot JSA for Notthing which has huge IS and IB change
names(cities.list)
Nott<-grep('Nott',names(cities.list))
Nott<-cities.list[[Nott]]
?qtm
qtm(Nott,fill='ib2001.prop')+qtm(centres.sp,symbols.size = 0.5,symbols.col = 'red')
qtm(Nott,fill='ib2011.prop')+qtm(centres.sp,symbols.size = 0.5,symbols.col = 'red')
qtm(Nott,fill='n.ib2001.prop')+qtm(centres.sp,symbols.size = 0.5,symbols.col = 'red')
qtm(Nott,fill='n.ib2011.prop')+qtm(centres.sp,symbols.size = 0.5,symbols.col = 'red') #okay...

##  Huh this is definitely right
sum(Nott$w.pop2011)
sum(Nott$w.pop2001)

##  Plot JSA for Bristol which has huge IS and IB change
names(cities.list)
Bris<-grep('Bris',names(cities.list))
Bris<-cities.list[[Bris]]
avonmouth<-which.max(Bris$st_areasha) #get rid of avonmouth
Bris<-Bris[-avonmouth,]
qtm(Bris,fill='ib2001.prop')+qtm(centres.sp,symbols.size = 0.5,symbols.col = 'red')
qtm(Bris,fill='ib2011.prop')+qtm(centres.sp,symbols.size = 0.5,symbols.col = 'red')
qtm(Bris,fill='n.ib2001.prop')+qtm(centres.sp,symbols.size = 0.5,symbols.col = 'red')
qtm(Bris,fill='n.ib2011.prop')+qtm(centres.sp,symbols.size = 0.5,symbols.col = 'red') #okay... so we have more closer

