#####################################################
##  LSOA conversion                               ###
##  For this we basically set up the 2001 areas   ###
##  and get their 2011 pops. For 2001 areas that  ###
##  were split up we just sum their 2011 components #
##  For other that were merged or had odd borders ###
##  we base it on area and population density     ###
##  Start: 6/2/2017                               ###
##  8/3/2017: We will also add the workplace pop  ###
##  and normal pop done using oa                  ###
#####################################################

source.file<-'RCI functions.R' #path to source
source(source.file)

##  Step one: We need to load up the changes file.
conversion.0111<-read.csv('../Data/LSOA 2001 to 2011/LSOA01_LSOA11_LAD11_EW_LU.csv',stringsAsFactors = F)

##  Load in data; as we will see 4 codes are missing; not sure why. But is liklely to have minimum effect so long as the missing codes were not unchanged LSOAs between 2001 and 2011.
pop.2011<-read.csv('../Data/LSOA 2011/LSOA pop census 2011.csv',stringsAsFactors = F)
workpop.2011<-read.csv('../Data/LSOA 2011/workplace pop census 2011.csv',stringsAsFactors = F)
table(pop.2011$geography.code%in%conversion.0111$LSOA11CD)
pop.2011$geography[!pop.2011$geography.code%in%conversion.0111$LSOA11CD] #These codes are not there...
saved.missing2011<-pop.2011$geography.code[!pop.2011$geography.code%in%conversion.0111$LSOA11CD]
table(conversion.0111$LSOA11CD%in%pop.2011$geography.code) #but all the 2011 codes in the conversion file are in the census; great...
##  Note this would affect the results for Leeds, Nottingham and London

##  Amend the LSOA code for workplace pop
workpop.2011$lsoacd<-substr(workpop.2011$X2011.super.output.area...lower.layer,1,9)

##  Step three: Well for now we can continue with our census conversion I suppose
load('../Data/Analysis data/England and Wales 0111 temp.Rdata')
saved.missing2011%in%ew.2001$lsoa01cd #so none of the missing are in the 2001 lsoacds by their exact name. So therefore they are codes were incompatible in some way (i.e. change =X)

##  First let's start by add the populations in 
pop.2011$working.age<-pop.2011$Age..All.usual.residents..measures..Value-rowSums(pop.2011[,c(6:11,18:21)]) 
temp.pop<-pop.2011$working.age[match(conversion.0111$LSOA11CD,pop.2011$geography.code)] #temp pop is ordered via the conversion datafile
temp.workpop<-workpop.2011$X2011[match(conversion.0111$LSOA11CD,workpop.2011$lsoacd)]

##  Second; We will create a temporary dataframe based on 2001 LSOAs
lsoa.2001<-data.frame(l2001=unique(conversion.0111$LSOA01CD),pop=NA,workpop=NA) #dataframe with 2001 code and NA for pop

##  First we will find the codes where there is no change and create the pop
lsoa.2001$pop<-temp.pop[match(lsoa.2001$l2001,conversion.0111$LSOA01CD)]
lsoa.2001$workpop<-temp.workpop[match(lsoa.2001$l2001,conversion.0111$LSOA01CD)]

unchanged<-unique(conversion.0111$LSOA01CD[conversion.0111$CHGIND=='U']) 
lsoa.2001$pop[!lsoa.2001$l2001%in%unchanged]<-NA
lsoa.2001$workpop[!lsoa.2001$l2001%in%unchanged]<-NA
summary(lsoa.2001$pop) ##so basically we first made them the same then put NA for those that are changed
##  some lsoa 2001 appear twice because they were split (and therefore require 2+ rows in the data file)


##  Second we make the population stats for those that have been split from 2001 into several 2011 LSOAs.
splitter<-unique(conversion.0111$LSOA01CD[conversion.0111$CHGIND=='S']) #codes for those that have split

for (i in 1:length(splitter)){
code<-splitter[i]
est.2001<-sum(temp.pop[(conversion.0111$LSOA01CD%in%code)]) #this selects the lines in the conversion where the 2011 code was part of the previous 2001 code
workest.2001<-sum(temp.workpop[(conversion.0111$LSOA01CD%in%code)])
lsoa.2001$pop[lsoa.2001$l2001%in%code]<-est.2001
lsoa.2001$workpop[lsoa.2001$l2001%in%code]<-workest.2001
}

### Third we will go ahead and replace the Mergers and X areas with more suitable values derived from our area based method.
pop.est2011<-read.csv('../Data/Analysis data/Area ratio method for 2011.csv',stringsAsFactors = F)

lsoa.2001$pop[match(pop.est2011$Group.1,lsoa.2001$l2001)]<-pop.est2011$w.pop2011
lsoa.2001$workpop[match(pop.est2011$Group.1,lsoa.2001$l2001)]<-pop.est2011$workpop.2011

##  Right and now we ought to be done!! Final step is to load in the data and save the .shp file
ew.2001$w.pop2011<-lsoa.2001$pop[match(ew.2001$lsoa01cd,lsoa.2001$l2001)]
ew.2001$work.pop2011<-lsoa.2001$workpop[match(ew.2001$lsoa01cd,lsoa.2001$l2001)]
save(file='../Data/Analysis data/England and Wales benefits 0111 final.Rdata',ew.2001) ##Now we can delete the temp file if we wish
rm(list = ls())


