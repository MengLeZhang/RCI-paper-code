#####################################################
##  LSOA conversion                               ###
##  Start: 8/3/2017                               ###
##  We identify the changed areas and then make   ###
##  them compatible                               ###
##  This is a brand new script because we can get ###
##  even better estimates using OA                ###
##  Note: 8/3/2017  For now we will calculate     ###
##  for workplace pop but in future we will do for  #
##  pop as well                                   ###
#####################################################

# Load library
source.file<-'RCI functions.R' #path to source
source(source.file)

##  Step one: We load in the population and boundary files
load('../Data/Analysis data/England and Wales 0111 temp.Rdata')

##  Population 2011 by output area; two files
pop.2011A<-read.csv('../Data/OA 2011/pop oa 2011 A.csv',stringsAsFactors = F)
pop.2011B<-read.csv('../Data/OA 2011/pop oa 2011 B.csv',stringsAsFactors = F)
pop.2011<-cbind(pop.2011A,pop.2011B[,-1])
pop.2011$w.pop2011<-rowSums(pop.2011[,-1]) #18-65s

##  Workplace pop by oa
workpop.2011<-read.csv('../Data/OA 2011/workplace oa 2011.csv',stringsAsFactors = F)
shp.2011<-readOGR(dsn='../Data/OA 2011', layer='Output_Area_December_2011_Generalised_Clipped_Boundaries_in_England_and_Wales')

shp.2011$w.pop2011<-pop.2011$w.pop2011[match(shp.2011$oa11cd,pop.2011$X2011.output.area)] #matching to get the pop
shp.2011$workpop2011<-workpop.2011$X2011[match(shp.2011$oa11cd,workpop.2011$X2011.output.area)]

##  Step two: We need to explore whether the overlap function will ever work
##  So we do not need to impute for those regions where LSOA are unchanged. Here is a table of changes
conversion.0111<-read.csv('../Data/LSOA 2001 to 2011/LSOA01_LSOA11_LAD11_EW_LU.csv',stringsAsFactors = F)
table(conversion.0111$CHGIND) ##so loads of unchanged; a fair few merges 313 and 163 wierd ones
c.lsoa<-conversion.0111$LSOA01CD[conversion.0111$CHGIND=='X'|conversion.0111$CHGIND=='M'] #so only mergers and wierd cases

##now to jsut keep the lsoa codes that have changed
ew.2001.changed<-ew.2001[ew.2001$lsoa01cd%in%c.lsoa,]
rm(ew.2001)

##  Step 2b: Fixing polygons
# any bad polys?
sum(gIsValid(shp.2011, byid=TRUE)==FALSE) #307 bad ones
sum(gIsValid(ew.2001.changed, byid=TRUE)==FALSE) #2 bad ones

##  We only need the OAs that are contained within the lsoas that changed; this will save space
shp.2011<-shp.2011[ew.2001.changed,]

##  We can 'fix' bad polygon by adding a zero buffer
shp.2011 <- gBuffer(shp.2011, byid=TRUE, width=0)
ew.2001.changed <- gBuffer(ew.2001.changed, byid=TRUE, width=0)

### 3B: Now for the actual imputation routine; where we assign their pop based on area
ew.n<-nrow(ew.2001.changed)
workpop.est<-list(NULL)

pi <- intersect(shp.2011, ew.2001.changed) #so this ought to find the areas of shp.2011 covered by each area of EW2001 ; This takes a while but is doable
warnings() #a fair few too few points and self intersections

# Extract areas from polygon objects then attach as attribute
areas <- sapply(pi@polygons, FUN=function(x) {slot(x, 'area')})
# Combine attributes info and areas 
pi$area<-areas
head(pi)
##  save the data
write.csv(pi@data,'../Data/Analysis data/Area ratio for 2001 LSOAS.csv')
#pi<-read.csv('LSOA data/Area ratio for 2001 LSOAS.csv',stringsAsFactors = F)

##  Right now for estimating the population share by area
pi$prop<-round((pi$area/pi$st_areasha.1),2)
pi$workpop.est2011<-round((pi$area/pi$st_areasha.1)*as.numeric(pi$workpop2011),2)
pi$pop.est2011<-round((pi$area/pi$st_areasha.1)*as.numeric(pi$w.pop2011),2)
head(pi)

pop.est.tab<-aggregate(cbind(pi$workpop.est2011,pi$pop.est2011),
                             by=list(pi$lsoa01cd),FUN=sum) #gives us fir the changers the population using 2001 boundaries
pop.est.tab$CHGIND<-conversion.0111$CHGIND[match(pop.est.tab$Group.1,conversion.0111$LSOA01CD)]
names(pop.est.tab)[2:3]<-c('workpop.2011','w.pop2011')

write.csv(pop.est.tab,'../Data/Analysis data/Area ratio method for 2011.csv')
##  Right we save it and continue  
rm(list = ls())
