#####################################################
##  LSOA conversion                               ###
##  Start: 6/2/2017                               ###
##  We identify the changed areas and then make   ###
##  them compatible                               ###
#####################################################

# Load library
source.file<-'RCI functions.R' #path to source
source(source.file)

##  Step one: We load in the population and boundary files
load('LSOA data/England and Wales 0111.Rdata')

##  Population 2011
pop.2011<-read.csv('LSOA data/LSOA pop census 2011.csv',stringsAsFactors = F)
shp.2011<-readOGR(dsn='LSOA data/LSOA 2011', layer='Lower_Layer_Super_Output_Areas_December_2011_Full_Extent__Boundaries_in_England_and_Wales')

names(pop.2011)
pop.2011$working.age<-pop.2011$Age..All.usual.residents..measures..Value-rowSums(pop.2011[,c(6:11,18:21)]) #we subtract from the total amount to avoid rounding errors; 18-65
shp.2011$w.pop2011<-pop.2011$working.age[match(shp.2011$lsoa11cd,pop.2011$geography.code)] #matching to get the pop

##  Step two: We need to explore whether the overlap function will ever work
plot(shp.2011[ew.2001[1,],]);plot(ew.2001[1,],add=T,col='red') #ah okay so it will end up geting tge surrounding areas as well due to its border; not a prob once we factor in the size of the area

##  So we do not need to impute for those regions where LSOA are unchanged. 
conversion.0111<-read.csv('LSOA data/LSOA 2001 to 2011/LSOA01_LSOA11_LAD11_EW_LU.csv',stringsAsFactors = F)
table(conversion.0111$CHGIND) ##so loads of unchanged; a fair few merges 313 and 163 wierd ones
c.lsoa<-conversion.0111$LSOA01CD[conversion.0111$CHGIND=='X'|conversion.0111$CHGIND=='M'] #so only mergers and wierd cases

##now to jsut keep the lsoa codes that have changed
ew.2001.changed<-ew.2001[ew.2001$lsoa01cd%in%c.lsoa,]
rm(ew.2001)

##  Step 2b: Fixing polygons
# any bad polys?
sum(gIsValid(shp.2011, byid=TRUE)==FALSE) #4 bad ones
sum(gIsValid(ew.2001.changed, byid=TRUE)==FALSE) #2 bad ones

##  We can 'fix' bad polygon by adding a zero buffer
shp.2011 <- gBuffer(shp.2011, byid=TRUE, width=0)
ew.2001.changed <- gBuffer(ew.2001.changed, byid=TRUE, width=0)


### 3B: Now for the actual imputation routine; where we assign their pop based on area
ew.n<-nrow(ew.2001.changed)
pop.est<-list(NULL)

  pi <- intersect(shp.2011, ew.2001.changed[1:ew.n,]) #so this ought to find the areas of shp.2011 covered by each area of EW2001 ; This takes a while but is doable
  warnings() #a fair few too few points and self intersections

    # Extract areas from polygon objects then attach as attribute
  areas <- sapply(pi@polygons, FUN=function(x) {slot(x, 'area')})
  # Combine attributes info and areas 
  pi$area<-areas
  
  ##  Right now for estimating the population share by area
  pi$prop<-round((pi$area/pi$st_areasha.1),2)
  pi$pop.est2011<-round((pi$area/pi$st_areasha.1)*pi$w.pop2011,2)

  pop.est.tab<-aggregate(pi$pop.est2011,by=list(pi$lsoa01cd),FUN=sum) #gives us fir the changers the population using 2001 boundaries
  pop.est.tab$CHGIND<-conversion.0111$CHGIND[match(pop.est.tab$Group.1,conversion.0111$LSOA01CD)]

write.csv(pop.est.tab,'LSOA data/Area ratio method for 2011 pop.csv')
##  Right we save it and continue  
rm(list = ls())
