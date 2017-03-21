#################################################
##  RCI with IB:                              ###
##  We can do the RCI with income support data###
##  this ought to be pretty easy to do as we  ###
##  just have to switch around our general rci###
##  function                                  ###
##  Start: 22/11/2016                         ###
#################################################

x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", 'CARBayes')
lapply(x, library, character.only = TRUE) # load the required packages; again this is a remarkably efficient way of doing the packages call

##  First: we load in the england and IS datasets-----
load('England and Wales JSA 0111.Rdata')
City.centres<-read.csv('C:/Users/USer/Documents/SMI analysis/LSOA data/City centres/UK city centres.csv')

##  The IS datasets
Ib2001<-read.csv('C:/Users/USer/Documents/SMI analysis/LSOA data/ib200105.csv',stringsAsFactors = F)
Ib2011<-read.csv('C:/Users/USer/Documents/SMI analysis/LSOA data/ib201105.csv',stringsAsFactors = F)

##  Now to replace the key vectors Total.x and Total.y with the variables from IS
head(Ib2011)
?substr
substr(Ib2001$lsoa,1,9)
table(substr(Ib2001$lsoa,1,9)%in%EW.JSA0111$lsoa01cd) #okay s one is not in here
Ib2001$lsoa[!substr(Ib2001$lsoa,1,9)%in%EW.JSA0111$lsoa01cd] #oh gine
table(substr(Ib2011$lsoa,1,9)%in%EW.JSA0111$lsoa01cd) 
Ib2011$lsoa[!substr(Ib2011$lsoa,1,9)%in%EW.JSA0111$lsoa01cd] #okay that's fine

##  Now to replace vectors
id2001<-match(EW.JSA0111$lsoa01cd,substr(Ib2001$lsoa,1,9))
id2011<-match(EW.JSA0111$lsoa01cd,substr(Ib2011$lsoa,1,9))
EW.JSA0111$Total.x<-as.numeric(Ib2001$total[id2001])
EW.JSA0111$Total.y<-as.numeric(Ib2011$total[id2011])
## Voila and we are done; so we can just continue with th rest

##  Step two: getting the list of cities
list<-lapply(City.centres$LA,grep,EW.JSA0111@data$LA...UA.Name.x)## index of centres
names(list)<-City.centres$LA
EngW.list<-list[which(lapply(list,length)>0)]
valid.cities<-names(list)[which(lapply(list,length)>0)]

##  Step three: After we created the RCI function (see below); we can do it for every city in our dataset
##  RCI.object(valid.cities[2],gis=EW.JSA0111,midpoints=City.centres,id.file=EngW.list) #Test
RCI.list<-lapply(valid.cities,RCI.object,gis=EW.JSA0111,midpoints=City.centres,id.file=EngW.list)

names(RCI.list)<-valid.cities

##  Save it all in one sheet
RCIs<-list(NULL)
for (i in 1:length(RCI.list)){
  RCIs[[i]]<-data.frame(RCI.list[[i]]$Result)
  RCIs[[i]]$city<-rep(names(RCI.list)[i],4)
  RCIs[[i]]$midpoint<-1:4
}

RCI.saved<-do.call(rbind,RCIs)
write.csv(RCI.saved,'Saved IB RCI England 22112016.csv')
RCI.saved #interesting


##`Step four: Now let's done this again but this time with the Leroux
library(shapefiles)
library(sp)
library(spdep)
library(CARBayes)
library(MCMCpack)
library(truncdist)

pop.2001<-read.csv('C:/Users/USer/Documents/SMI analysis/LSOA data/2001 pop est/LSOA 2001 neat.csv')
head(pop.2001)## Right we need to get the number over 16
pop.2001$over16<-pop.2001$All.Persons..All.Ages-pop.2001$All.Persons..Aged.0.15
head(EngW.list)
table(pop.2001$LSOA_CODE%in%EW.JSA0111$lsoa01cd) #okay so they are all in here.
EW.JSA0111$over16<-pop.2001$over16[match(pop.2001$LSOA_CODE,EW.JSA0111$lsoa01cd)]

##  Right now let's do the leroux
LeRoux.2001<-lapply(valid.cities,LeRoux.RCI.object2001,gis=EW.JSA0111,midpoints=City.centres,id.file=EngW.list)
warnings() #no big warnings

##  Step four: Now we jsut save the results
RCI.LR<-list(NULL)

for (i in 1:length(LeRoux.2001)){
  temp<-lapply(LeRoux.2001[[i]]$Results,quantile,probs=c(0.025,0.5,0.975))
  RCI.LR[[i]]<-data.frame(do.call(rbind,temp))
  RCI.LR[[i]]$city<-rep(names(RCI.list)[i],4)
  RCI.LR[[i]]$midpoint<-1:4
}

saved.LR2001<-do.call(rbind,RCI.LR)
write.csv(saved.LR2001, file='England LeRoux IB 2001.csv')
