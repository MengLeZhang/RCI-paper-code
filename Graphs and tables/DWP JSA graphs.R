##  Graphing JSA over 2000-2016 ####
##  Start: 9/5/2017 We need to set out to graph the info about aci over cities and ttwa to show that the
##  recession caused a big dip in aci. Also the number of claimants jumped up in all cities during that time
##> library----
graph.lib<-c('ggplot2')
lapply(graph.lib,library,character.only=T)
aci_la.tab
##  Graph 1: Graphing aci for la and ttwa----
aci_la.tab<-read.csv('../Results/ACI LA point estimates 00-16.csv',stringsAsFactors = F)
aci_london.tab<-read.csv('../Results/ACI london point estimates 00-16.csv',stringsAsFactors = F)
aci_london.tab$city<-'london'
aci_la.tab<-rbind(aci_la.tab,aci_london.tab) #merge together

##  reformat it to have var for la, date and aci
which.may<-grep('May',names(aci_la.tab))


temp.list<-c(NULL)
for (i in 1:length(which.may)){
  year<-names(aci_la.tab)[which.may[i]]
  aci<-c(aci_la.tab[,year])
  
  year_b4<-year
  if(i>1){year_b4<-names(aci_la.tab)[which.may[i-1]]}
  aci_diff<-c(aci_la.tab[,year])-c(aci_la.tab[,year_b4])
  temp.list[[i]]<-data.frame(aci,aci_diff,year,stringsAsFactors = F)
}

aci_la.gg<-do.call(rbind,temp.list)
aci_la.gg$city<-aci_la.tab$city
aci_la.gg$base_aci<-aci_la.tab$May.00-aci_la.gg$aci #standardised to their 00 aci

##  Sort of want to omit 2014 onwards
omit.14<-c('May.14','May.15','May.16')
aci_la.gg<-aci_la.gg[!aci_la.gg$year%in%omit.14,]

##  Raw change in ACI--not pretty but will ahve to do
ggplot(aes(x=year,y=aci,group=city,colour=city),data=aci_la.gg)+ geom_line(stat='identity')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('ACI based on JSA')+xlab('Year')+
  ggtitle('Changes in ACI 2000-2016 (LAs incl. London)')

##  Next is the turn of ttwa----
aci_ttwa.tab<-read.csv('../Results/ACI ttwa point estimates 00-16.csv',stringsAsFactors = F)
which.nearest<-grep('near',aci_ttwa.tab$X)
aci_ttwa.tab<-aci_ttwa.tab[which.nearest,]

##  reformat it to have var for ttwa, date and aci
which.may<-grep('May',names(aci_ttwa.tab))
temp.list<-c(NULL)
for (i in 1:length(which.may)){
  year<-names(aci_ttwa.tab)[which.may[i]]
  aci<-c(aci_ttwa.tab[,year])
  
  year_b4<-year
  if(i>1){year_b4<-names(aci_ttwa.tab)[which.may[i-1]]}
  aci_diff<-c(aci_ttwa.tab[,year])-c(aci_ttwa.tab[,year_b4])
  temp.list[[i]]<-data.frame(aci,aci_diff,year,stringsAsFactors = F)
}
aci_ttwa.gg<-do.call(rbind,temp.list)
aci_ttwa.gg$city<-aci_ttwa.tab$city
aci_ttwa.gg$base_aci<-aci_ttwa.tab$May.00-aci_ttwa.gg$aci #standardised to their 00 aci

##  Sort of want to omit 2014 onwards
omit.14<-c('May.14','May.15','May.16')
aci_ttwa.gg<-aci_ttwa.gg[!aci_ttwa.gg$year%in%omit.14,]


##  Raw change in ACI--not pretty but will ahve to do
ggplot(aes(x=year,y=aci,group=city,colour=city),data=aci_ttwa.gg)+ geom_line(stat='identity')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ylab('ACI based on JSA')+xlab('Year')+
  ggtitle('Changes in ACI 2000-2016 (ttwas; nearest centre)')
