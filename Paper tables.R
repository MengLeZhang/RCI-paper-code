#######################################################
##  Paper tables.                                   ###
##  This is just to sort out the ordering on the    ###
##  tables.                                         ###
##  Start: 9/2/2017                                 ###
#######################################################

##  Step one: load in the tables
jsa.tab<-read.csv(file='Results/LA boundaries/jsa RCI table.csv',stringsAsFactors = F)
is.tab<-read.csv(file='Results/LA boundaries/is RCI table.csv',stringsAsFactors = F)
ib.tab<-read.csv(file='Results/LA boundaries/ib RCI table.csv',stringsAsFactors = F)

##  We need to seperate them and organise by largest changers
diff.id<-seq(3,nrow(jsa.tab),3)
id.2001<-seq(1,nrow(jsa.tab),3)
id.2011<-seq(2,nrow(jsa.tab),3)

tabs.list<-list(jsa.tab,is.tab,ib.tab)

for (i in (1:length(tabs.list))){
  temp.tab<-tabs.list[[i]]
  tab.ord<-order(temp.tab[diff.id,3]) #biggest changers by stat
  ord.2001<-(temp.tab[id.2001,])[tab.ord,]
  ord.2011<-(temp.tab[id.2011,])[tab.ord,]
  ord.diff<-(temp.tab[diff.id,])[tab.ord,]
  tabs.list[[i]]<-cbind(ord.2001,ord.2011,ord.diff)
}

write.csv(tabs.list[[1]],'Results/LA boundaries/jsa tab ordered.csv')
write.csv(tabs.list[[2]],'Results/LA boundaries/is tab ordered.csv')
write.csv(tabs.list[[3]],'Results/LA boundaries/ib tab ordered.csv')

##  How about the ACI tables?
RCI.tables<-read.csv('RCI LA point estimates.csv',stringsAsFactors = F)
write.csv(file='Results/LA boundaries/point est tab ordered.csv',RCI.tables[seq(4,nrow(RCI.tables),4),])
