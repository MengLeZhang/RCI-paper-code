########################################################
##  ggplot graph of changes in ACI                  ####
##  start: 22/4/2017                                ####
########################################################

# Load library
source.file<-'RCI functions.R' #path to source
source(source.file)

##  Load in LA results
la.aci<-read.csv('../Results/ACI LA point estimates.csv',stringsAsFactors = F)

ggplot(aes(y=jsadiff,x=city,data=la.aci))

ggplot(data=la.aci, aes(x=city,y=jsadiff,fill=type)) + geom_bar(stat='identity',position=position_dodge())

ttwa.aci<-read.csv('../Results/ACI TTWA point estimates.csv',stringsAsFactors = F)
ggplot(data=ttwa.aci, aes(x=city,y=jsadiff,fill=type)) + geom_bar(stat='identity',position=position_dodge())

london.aci<-read.csv('../Results/ACI London point estimates.csv',stringsAsFactors = F)
ggplot(data=london.aci, aes(x=city,y=jsadiff,fill=type)) + geom_bar(stat='identity',position=position_dodge())

##  Load in LA results for RCI
##  We include London in there
la.rci<-read.csv('../Results/RCI LA point estimates.csv',stringsAsFactors = F)
london.rci<-read.csv('../Results/RCI London point estimates.csv',stringsAsFactors = F)

la.rci<-rbind(la.rci,london.rci)
la.rci$type<-c('Distance','Lamdba=1','Lambda=2')

##  JSA
ggplot(data=subset(la.rci,type=='Distance'), aes(x=city,y=jsadiff)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (JSA)')+xlab('LA')+
  ggtitle('Difference in RCI for LAs based on distance (JSA)')

##  IS
ggplot(data=subset(la.rci,type=='Distance'), aes(x=city,y=isdiff)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (IS)')+xlab('LA')+
  ggtitle('Difference in RCI for LAs based on distance (IS)')

##Ib
ggplot(data=subset(la.rci,type=='Distance'), aes(x=city,y=ibdiff)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (IB)')+xlab('LA')+
  ggtitle('Difference in RCI for LAs based on distance (IB)')

##  Based on the hansen measures
ggplot(data=subset(la.rci,type!='Distance'), aes(x=city,y=jsadiff,fill=type)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (JSA)')+xlab('LA')+
  ggtitle('Difference in RCI for LAs based on employment access')

ggplot(data=subset(la.rci,type!='Distance'), aes(x=city,y=isdiff,fill=type)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (IS)')+xlab('LA')+
  ggtitle('Difference in RCI for LAs based on employment access')

ggplot(data=subset(la.rci,type!='Distance'), aes(x=city,y=ibdiff,fill=type)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (IB)')+xlab('LA')+
  ggtitle('Difference in RCI for LAs based on employment access')

##  TTWA stats 
ttwa.rci<-read.csv('../Results/RCI TTWA point estimates.csv',stringsAsFactors = F)
london.rci<-read.csv('../Results/RCI London point estimates.csv',stringsAsFactors = F)

ttwa.rci$type<-c('Distance','Distance nearest','Lamdba=1','Lambda=2')

##  JSA
ggplot(data=subset(ttwa.rci,type%in%c('Distance','Distance nearest')), aes(x=city,y=jsadiff,fill=type)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (JSA)')+xlab('TTWA')+
  ggtitle('Difference in RCI for TTWAs based on distance (JSA)')

##  IS
ggplot(data=subset(ttwa.rci,type%in%c('Distance','Distance nearest')), aes(x=city,y=isdiff,fill=type)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (IS)')+xlab('TTWA')+
  ggtitle('Difference in RCI for TTWAs based on distance (IS)')

##Ib
ggplot(data=subset(ttwa.rci,type%in%c('Distance','Distance nearest')), aes(x=city,y=ibdiff,fill=type)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (IB)')+xlab('TTWA')+
  ggtitle('Difference in RCI for TTWAs based on distance (IB)')


##  Now for the stats using employment
ggplot(data=subset(ttwa.rci,type%in%c('Lamdba=1','Lambda=2')), aes(x=city,y=jsadiff,fill=type)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (JSA)')+xlab('TTWA')+
  ggtitle('Difference in RCI for TTWAs based on employment access')

ggplot(data=subset(ttwa.rci,type%in%c('Lamdba=1','Lambda=2')), aes(x=city,y=isdiff,fill=type)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (IS)')+xlab('TTWA')+
  ggtitle('Difference in RCI for TTWAs based on employment access')

ggplot(data=subset(ttwa.rci,type%in%c('Lamdba=1','Lambda=2')), aes(x=city,y=ibdiff,fill=type)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (IB)')+xlab('TTWA')+
  ggtitle('Difference in RCI for TTWAs based on employment access')


### example distribution in a city
distance<-1:100
freq<-500-0.2*(distance^1.5)+rnorm(100,sd=10)
example.df<-data.frame(distance,freq)
plot(x=distance,freq)
ggplot(data=example.df, aes(x=distance,y=freq)) + 
  geom_bar(stat='identity') +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab('Population')+xlab('Distance from city centre')+
  ggtitle('Example plot of population by distance from city centre')
