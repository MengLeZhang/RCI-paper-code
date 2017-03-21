########################################################
##  Census output area to lsoa 2001                   ##
##  For 2001 we cannot find which census oa maps to   ##
##  which lsoa. We can create this table ourselves    ##
##  Start: 7/3/2017                                   ##
########################################################

##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
##  Data load OA an LSOA england wales shp files
ew.2001<- readOGR(dsn='../Data/LSOA 2001', layer='Lower_Layer_Super_Output_Areas_December_2001_Full_Extent_Boundaries_in_England_and_Wales')
oa.2001<- readOGR(dsn='../Data/OA 2001', layer='Output_Areas_December_2001_Full_Clipped_Boundaries_in_England_and_Wales') 
## The OA file is huge but for our purposes if a oa centroid is within a lsoa then that oa is within as well so we can save space
oa.2001.centre<-getSpPPolygonsLabptSlots(oa.2001)
oa.2001.name<-oa.2001$oa01cd
oa.2001<-SpatialPointsDataFrame(coords= oa.2001.centre,data=data.frame(oa.2001.name),proj4string=CRS(proj4string(ew.2001)))

##  Second; we need to basically use this very simple routine to find the oas within each lsoa; might take a while
oa.lsoa.list<-list(NULL)
length(ew.2001)
for (i in 1:length(ew.2001)){
sub<-oa.2001[ew.2001[i,],]
oa.lsoa.list[[i]]<-cbind(oa=as.character(sub$oa.2001.name),lsoa=as.character(ew.2001[i,]$lsoa01cd))
}
nrow(oa.lsoa.tab)==nrow(oa.2001) #all there
oa.lsoa.tab<-do.call(rbind,oa.lsoa.list)
write.csv(oa.lsoa.tab,'../Data/OA 2001/OA to LSOA 2001 lookup table.csv')
rm(list = ls()) #finish by removing all
