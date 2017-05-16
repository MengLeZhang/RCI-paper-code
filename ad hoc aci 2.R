file:///C:/Users/USer/Documents/SMI analysis/UK RCI paper 1/Results/ACI LA point estimates.csv
########################################################
##  ggplot graph of changes in ACI                  ####
##  start: 22/4/2017                                ####
########################################################

# Load library
source.file<-'RCI functions.R' #path to source
source(source.file)

##  Load in LA results
la.aci<-read.csv('../Results/ACI LA point estimates.csv',stringsAsFactors = F)
head(la.aci)

##  Load in LA results for RCI
##  We include London in there
la.aci<-la.aci[grep('dist.nearest',la.aci$NA..1),]

##  JSA
ggplot(data=subset(la.aci), aes(x=NA.,y=jsa.2)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (JSA)')+xlab('TTWA')+
  ggtitle('Difference in RCI for TTWAs (JSA, 2001-2011)')

##  IS
ggplot(data=subset(la.rci,type=='Distance'), aes(x=city,y=isdiff)) + 
  geom_bar(stat='identity',position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('Difference in RCI (IS)')+xlab('LA')+
  ggtitle('Difference in RCI for LAs based on distance (IS)')

