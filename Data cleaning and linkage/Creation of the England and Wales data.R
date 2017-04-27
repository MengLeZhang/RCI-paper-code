#######################################################
##  This is the code used to generate our version of  #
##  the England and Wales datasets with all the       #
##  measures that we need                             #
##  Start: 25/11/2016 so we have JSA, IS, IB for 2001 #
##  and 2010. So far we only have pop for 2001 due to #
##  changes in LSOA                                   #
#######################################################

##  Pre: Get functions; mostly used just to load in the general gis functions rather than our custom rci ones
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First step: we load in the data for the whole of England first ----
### An extra step to check all English LSOA:
ew.2001<- readOGR(dsn='../Data/LSOA 2001', layer='Lower_Layer_Super_Output_Areas_December_2001_Full_Extent_Boundaries_in_England_and_Wales') #England and Wales technically; huge number of LSOAs so seems right

##  Let's smarten up the la names; they have a 4 digit code and space at their ends (which I do not need)
ew.2001$la<-as.character(ew.2001@data$lsoa01nm); head(ew.2001$la)
ew.2001$la<-substr(ew.2001$la,1,nchar(as.character(ew.2001@data$lsoa01nm))-5)

##  Second step: We go ahead and insert in the population stats ----
##  2001; this is the only one for now
pop.2001<-read.csv('../Data/LSOA 2001/LSOA pop census 2001.csv')
names(pop.2001)## Right we need to get the number between 18 and 65
pop.2001$working.age<-pop.2001$Age..All.usual.residents..measures..Value-rowSums(pop.2001[,c(5:10,17:20)])
ew.2001$w.pop2001<-pop.2001$working.age[match(pop.2001$geography.code,ew.2001$lsoa01cd)] #append the working age pop to ew.2001

##  JSA----
jsa2001<-read.csv('../Data/DWP data/jsa200105.csv',stringsAsFactors = F)
jsa2011<-read.csv('../Data/DWP data/jsa201105.csv',stringsAsFactors = F)

##  Now we add in the appropriate amounts to the data
jsa.id2001<-match(ew.2001$lsoa01cd,jsa2001$Lower.Layer.SOA...Data.Zone.Code)
jsa.id2011<-match(ew.2001$lsoa01cd,jsa2011$Lower.Layer.SOA...Data.Zone.Code)
ew.2001$jsa2001<-jsa2001$Total[jsa.id2001]
ew.2001$jsa2011<-jsa2011$Total[jsa.id2011]

##  The IS datasets ----
is2001<-read.csv('../Data/DWP data/is200105.csv',stringsAsFactors = F)
is2011<-read.csv('../Data/DWP data/is201105.csv',stringsAsFactors = F)

##  Now to replace vectors
is.id2001<-match(ew.2001$lsoa01cd,is2001$Lower.Layer.SOA...Data.Zone.Code)
is.id2011<-match(ew.2001$lsoa01cd,is2011$Lower.Layer.SOA...Data.Zone.Code)
ew.2001$is2001<-is2001$Total[is.id2001]
ew.2001$is2011<-is2011$Total[is.id2011]

##  The IB datasets ----
ib2001<-read.csv('../Data/DWP data/ib200105.csv',stringsAsFactors = F)
ib2011<-read.csv('../Data/DWP data/ib201105.csv',stringsAsFactors = F)

##  Now to replace vectors
ib.id2001<-match(ew.2001$lsoa01cd,substr(ib2001$lsoa,1,9))
ib.id2011<-match(ew.2001$lsoa01cd,substr(ib2011$lsoa,1,9))
ew.2001$ib2001<-as.numeric(ib2001$total[ib.id2001])
ew.2001$ib2011<-as.numeric(ib2011$total[ib.id2011])


##  Third: We need to put in the workplace population of each LSOA in 2001
oa.lsoa.2001<-read.csv('../Data/OA 2001/OA to LSOA 2001 lookup table.csv',stringsAsFactors = F)
oa.workpop.2001<-read.csv('../Data/OA 2001/workplace oa 2001.csv',stringsAsFactors = F)
oa.workpop.2001$Area<-substr(oa.workpop.2001$Area,8,17)

##  Check all the areas are accounted for
sum(!oa.lsoa.2001$oa%in%oa.workpop.2001$Area) #0 areas are missing

oa.lsoa.2001$workpop.201<-as.numeric(oa.workpop.2001$X2001[match(oa.lsoa.2001$oa,oa.workpop.2001$Area)])
lsoa.workpop2001<-aggregate(oa.lsoa.2001$workpop.201,by=list(oa.lsoa.2001$lsoa),sum)
ew.2001$work.pop2001<-lsoa.workpop2001$x[match(ew.2001$lsoa01cd,lsoa.workpop2001$Group.1)]

##  Now we save tge ew.2001 data set for the future
save(file='../Data/Analysis data/England and Wales 0111 temp.Rdata',ew.2001)

##  End
rm(list = ls())
