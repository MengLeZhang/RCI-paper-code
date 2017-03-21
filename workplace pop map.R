#######################################################
### This is a trial of mapping especially for mapping #
##  the workplace pop in London so that gwilym and I  #
##  can take a look                                   #
##  2/3/2017                                          #
#######################################################


##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  Step one: We need to load in the 2011 shp data
shp.2011<-readOGR(dsn='LSOA data/LSOA 2011', layer='Lower_Layer_Super_Output_Areas_December_2011_Full_Extent__Boundaries_in_England_and_Wales')
work.pop2011<-read.csv('LSOA data/LSOA 2011/workplace pop 2011.csv')


##  Now to include the workplace pop data
head(work.pop2011)
head(shp.2011@data)
table(shp.2011@data$lsoa11cd%in%work.pop2011$geography.code) #okay so all the shp file rows are in there

shp.2011@data$work.pop2011<-work.pop2011$Population..All.usual.residents.aged.16.to.74..measures..Value[mmatch(shp.2011@data$lsoa11cd,work.pop2011$geography.code)]

##  load in ttwa
TTWA.2011<- readOGR(dsn='LSOA data/TTWA 2011', layer='Travel_to_Work_Areas_December_2011_Full_Extent_Boundaries_in_United_Kingdom') 
TTWA.2011<-gBuffer(TTWA.2011, byid=TRUE, width=-0.5) #shrink by a wee bit

##  Find london and slough
london.id<-grep('London',TTWA.2011$ttwa11nm)
slough.id<-grep('Slough',TTWA.2011$ttwa11nm)

##  Finally we subset to the areas
plot(TTWA.2011[london.id,])
plot(TTWA.2011[slough.id,])
london.shp<-list(london=shp.2011[TTWA.2011[london.id,],],
                 slough=shp.2011[TTWA.2011[slough.id,],])
rm(shp.2011,TTWA.2011)

##  2) Now for those plots!

##  To use ggplot we must fortify the spatial data
lnd.f<-fortify(london.shp$london)
london.shp$london@data$id<-row.names(london.shp$london@data)
lnd.f<-left_join(lnd.f,london.shp$london@data) #for some reason we must rejoin up the data file
lnd.f$work.density2011<-lnd.f$work.pop2011/lnd.f$st_areasha
head(lnd.f)

ggplot(data=lnd.f,aes(long,lat,group=group,fill=work.pop2011))+geom_polygon()
ggplot(data=lnd.f,aes(long,lat,group=group,fill=work.density2011))+geom_polygon()+coord_equal()+
  labs(x='Eastings (m)',y='Northing (m)',fill='Workplace pop  per sq. metre')+
  ggtitle('Workplace pop. London TTWA 2011')

##  Now for slough
slh.f<-fortify(london.shp$slough)
london.shp$slough@data$id<-row.names(london.shp$slough@data)
slh.f<-left_join(slh.f,london.shp$slough@data) #for some reason we must rejoin up the data file
slh.f$work.density2011<-slh.f$work.pop2011/slh.f$st_areasha


ggplot(data=slh.f,aes(long,lat,group=group,fill=work.density2011))+geom_polygon()+coord_equal()+
  labs(x='Eastings (m)',y='Northing (m)',fill='Workplace pop  per sq. metre')+
  ggtitle('Workplace pop. Slough TTWA 2011')


ggplot(data=london.shp$london,aes(x=))
head(london.shp$london)
