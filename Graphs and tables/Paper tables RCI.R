################################################
##  Paper tables RCI                        ####
##  start: 11/4/2017                        ####
################################################

source('Tabling function.R')
##  Just a quick and easy function to extract the right cols from a table

##  The RCI
##  la
la.rci<-read.csv('C:/Users/USer/Documents/SMI analysis/UK RCI paper 1/Results/RCI LA raw CI.csv')
names(la.rci)
format.la.rci
format.la.rci<-data.frame(jsa2001=tab.format1(la.rci[,2],la.rci[,3],la.rci[,4]),
                         jsa2011=tab.format1(la.rci[,5],la.rci[,6],la.rci[,7]),
                         jsadiff=tab.format1(la.rci[,8],la.rci[,9],la.rci[,10]),
                         ib2001=tab.format1(la.rci[,11],la.rci[,12],la.rci[,13]),
                         ib2011=tab.format1(la.rci[,14],la.rci[,15],la.rci[,16]),
                         ibdiff=tab.format1(la.rci[,17],la.rci[,18],la.rci[,19]),
                         is2001=tab.format1(la.rci[,20],la.rci[,21],la.rci[,22]),
                         is2011=tab.format1(la.rci[,23],la.rci[,24],la.rci[,25]),
                         isdiff=tab.format1(la.rci[,26],la.rci[,27],la.rci[,28]),
                         city=la.rci$saved.name,
                         stat=c('Distance','Lambda(1)','Lambda(2)'))


write.csv(format.la.rci,file='../Results/Formatted results/la rci formatted.csv')

## la mcar
la.rci.mcar<-read.csv('C:/Users/USer/Documents/SMI analysis/UK RCI paper 1/Results/RCI LA raw CI mcar.csv')
names(la.rci.mcar)

format.la.rci.mcar<-data.frame(jsa2001=tab.format1(la.rci.mcar[,2],la.rci.mcar[,3],la.rci.mcar[,4]),
                               jsa2011=tab.format1(la.rci.mcar[,5],la.rci.mcar[,6],la.rci.mcar[,7]),
                               jsadiff=tab.format1(la.rci.mcar[,8],la.rci.mcar[,9],la.rci.mcar[,10]),
                               ib2001=tab.format1(la.rci.mcar[,11],la.rci.mcar[,12],la.rci.mcar[,13]),
                               ib2011=tab.format1(la.rci.mcar[,14],la.rci.mcar[,15],la.rci.mcar[,16]),
                               ibdiff=tab.format1(la.rci.mcar[,17],la.rci.mcar[,18],la.rci.mcar[,19]),
                               is2001=tab.format1(la.rci.mcar[,20],la.rci.mcar[,21],la.rci.mcar[,22]),
                               is2011=tab.format1(la.rci.mcar[,23],la.rci.mcar[,24],la.rci.mcar[,25]),
                               isdiff=tab.format1(la.rci.mcar[,26],la.rci.mcar[,27],la.rci.mcar[,28]),
                               city=la.rci.mcar$saved.name,
                               stat=c('Distance','Lambda(1)','Lambda(2)'))
write.csv(format.la.rci.mcar,file='../Results/Formatted results/la rci mcar formatted.csv')

##  ttwa
ttwa.rci<-read.csv('C:/Users/USer/Documents/SMI analysis/UK RCI paper 1/Results/RCI TTWA raw CI.csv')
names(ttwa.rci)

format.ttwa.rci<-data.frame(jsa2001=tab.format1(ttwa.rci[,2],ttwa.rci[,3],ttwa.rci[,4]),
                            jsa2011=tab.format1(ttwa.rci[,5],ttwa.rci[,6],ttwa.rci[,7]),
                            jsadiff=tab.format1(ttwa.rci[,8],ttwa.rci[,9],ttwa.rci[,10]),
                            ib2001=tab.format1(ttwa.rci[,11],ttwa.rci[,12],ttwa.rci[,13]),
                            ib2011=tab.format1(ttwa.rci[,14],ttwa.rci[,15],ttwa.rci[,16]),
                            ibdiff=tab.format1(ttwa.rci[,17],ttwa.rci[,18],ttwa.rci[,19]),
                            is2001=tab.format1(ttwa.rci[,20],ttwa.rci[,21],ttwa.rci[,22]),
                            is2011=tab.format1(ttwa.rci[,23],ttwa.rci[,24],ttwa.rci[,25]),
                            isdiff=tab.format1(ttwa.rci[,26],ttwa.rci[,27],ttwa.rci[,28]),
                            city=ttwa.rci$saved.name,
                            stat=c('Distance','Nearest distane','Lambda(1)','Lambda(2)'))
write.csv(format.ttwa.rci,file='../Results/Formatted results/ttwa rci formatted.csv')

## ttwa mcar
ttwa.rci.mcar<-read.csv('C:/Users/USer/Documents/SMI analysis/UK RCI paper 1/Results/RCI TTWA raw CI mcar.csv')
names(ttwa.rci.mcar)

format.ttwa.rci.mcar<-data.frame(jsa2001=tab.format1(ttwa.rci.mcar[,2],ttwa.rci.mcar[,3],ttwa.rci.mcar[,4]),
                                 jsa2011=tab.format1(ttwa.rci.mcar[,5],ttwa.rci.mcar[,6],ttwa.rci.mcar[,7]),
                                 jsadiff=tab.format1(ttwa.rci.mcar[,8],ttwa.rci.mcar[,9],ttwa.rci.mcar[,10]),
                                 ib2001=tab.format1(ttwa.rci.mcar[,11],ttwa.rci.mcar[,12],ttwa.rci.mcar[,13]),
                                 ib2011=tab.format1(ttwa.rci.mcar[,14],ttwa.rci.mcar[,15],ttwa.rci.mcar[,16]),
                                 ibdiff=tab.format1(ttwa.rci.mcar[,17],ttwa.rci.mcar[,18],ttwa.rci.mcar[,19]),
                                 is2001=tab.format1(ttwa.rci.mcar[,20],ttwa.rci.mcar[,21],ttwa.rci.mcar[,22]),
                                 is2011=tab.format1(ttwa.rci.mcar[,23],ttwa.rci.mcar[,24],ttwa.rci.mcar[,25]),
                                 isdiff=tab.format1(ttwa.rci.mcar[,26],ttwa.rci.mcar[,27],ttwa.rci.mcar[,28]),
                                 city=ttwa.rci.mcar$saved.name,
                                 stat=c('Distance','Nearest distane','Lambda(1)','Lambda(2)'))
write.csv(format.ttwa.rci.mcar,file='../Results/Formatted results/ttwa rci mcar formatted.csv')


##  london
london.rci<-read.csv('C:/Users/USer/Documents/SMI analysis/UK RCI paper 1/Results/RCI London raw CI.csv')
names(london.rci)

format.london.rci<-data.frame(jsa2001=tab.format1(london.rci[,2],london.rci[,3],london.rci[,4]),
                              jsa2011=tab.format1(london.rci[,5],london.rci[,6],london.rci[,7]),
                              jsadiff=tab.format1(london.rci[,8],london.rci[,9],london.rci[,10]),
                              ib2001=tab.format1(london.rci[,11],london.rci[,12],london.rci[,13]),
                              ib2011=tab.format1(london.rci[,14],london.rci[,15],london.rci[,16]),
                              ibdiff=tab.format1(london.rci[,17],london.rci[,18],london.rci[,19]),
                              is2001=tab.format1(london.rci[,20],london.rci[,21],london.rci[,22]),
                              is2011=tab.format1(london.rci[,23],london.rci[,24],london.rci[,25]),
                              isdiff=tab.format1(london.rci[,26],london.rci[,27],london.rci[,28]),
                              city=london.rci$saved.name,
                              stat=c('Distance','Lambda(1)','Lambda(2)'))
write.csv(format.london.rci,file='../Results/Formatted results/london rci formatted.csv')

## london mcar
london.rci.mcar<-read.csv('C:/Users/USer/Documents/SMI analysis/UK RCI paper 1/Results/RCI London raw CI mcar.csv')
names(london.rci.mcar)

format.london.rci.mcar<-data.frame(jsa2001=tab.format1(london.rci.mcar[,2],london.rci.mcar[,3],london.rci.mcar[,4]),
                                   jsa2011=tab.format1(london.rci.mcar[,5],london.rci.mcar[,6],london.rci.mcar[,7]),
                                   jsadiff=tab.format1(london.rci.mcar[,8],london.rci.mcar[,9],london.rci.mcar[,10]),
                                   ib2001=tab.format1(london.rci.mcar[,11],london.rci.mcar[,12],london.rci.mcar[,13]),
                                   ib2011=tab.format1(london.rci.mcar[,14],london.rci.mcar[,15],london.rci.mcar[,16]),
                                   ibdiff=tab.format1(london.rci.mcar[,17],london.rci.mcar[,18],london.rci.mcar[,19]),
                                   is2001=tab.format1(london.rci.mcar[,20],london.rci.mcar[,21],london.rci.mcar[,22]),
                                   is2011=tab.format1(london.rci.mcar[,23],london.rci.mcar[,24],london.rci.mcar[,25]),
                                   isdiff=tab.format1(london.rci.mcar[,26],london.rci.mcar[,27],london.rci.mcar[,28]),
                                   city=london.rci.mcar$saved.name,
                                   stat=c('Distance','Lambda(1)','Lambda(2)'))
write.csv(format.london.rci.mcar,file='../Results/Formatted results/london rci mcar formatted.csv')



