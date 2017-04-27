###########################################
##  Results tables                      ###
##  start: 11/4/2017                    ###
###########################################

source('Tabling function.R')
##  Just a quick and easy function to extract the right cols from a tabl



##  The DI simiilarity index
##  la
la.di<-read.csv('C:/Users/USer/Documents/SMI analysis/UK RCI paper 1/Results/DI LA raw CI.csv')
format.la.di<-data.frame(jsa=tab.format1(la.di[,2],la.di[,3],la.di[,4]),
           ib=tab.format1(la.di[,5],la.di[,6],la.di[,7]),
           is=tab.format1(la.di[,8],la.di[,9],la.di[,10]),
           city=la.di$city,
           stat=la.di$X)
write.csv(format.la.di,file='../Results/Formatted results/la di formatted.csv')

la.di.mcar<-read.csv('C:/Users/USer/Documents/SMI analysis/UK RCI paper 1/Results/DI LA raw CI mcar.csv')
format.la.di.mcar<-data.frame(jsa=tab.format1(la.di.mcar[,2],la.di.mcar[,3],la.di.mcar[,4]),
                              ib=tab.format1(la.di.mcar[,5],la.di.mcar[,6],la.di.mcar[,7]),
                              is=tab.format1(la.di.mcar[,8],la.di.mcar[,9],la.di.mcar[,10]),
                              city=la.di.mcar$city,
                              stat=la.di.mcar$X)
write.csv(format.la.di.mcar,file='../Results/Formatted results/la di mcar formatted.csv')

##  ttwa
ttwa.di<-read.csv('C:/Users/USer/Documents/SMI analysis/UK RCI paper 1/Results/DI TTWA raw CI.csv')
format.ttwa.di<-data.frame(jsa=tab.format1(ttwa.di[,2],ttwa.di[,3],ttwa.di[,4]),
                           ib=tab.format1(ttwa.di[,5],ttwa.di[,6],ttwa.di[,7]),
                           is=tab.format1(ttwa.di[,8],ttwa.di[,9],ttwa.di[,10]),
                           city=ttwa.di$city,
                           stat=ttwa.di$X)
write.csv(format.ttwa.di,file='../Results/Formatted results/ttwa di formatted.csv')

ttwa.di.mcar<-read.csv('C:/Users/USer/Documents/SMI analysis/UK RCI paper 1/Results/DI TTWA raw CI mcar.csv')
format.ttwa.di.mcar<-data.frame(jsa=tab.format1(ttwa.di.mcar[,2],ttwa.di.mcar[,3],ttwa.di.mcar[,4]),
                                ib=tab.format1(ttwa.di.mcar[,5],ttwa.di.mcar[,6],ttwa.di.mcar[,7]),
                                is=tab.format1(ttwa.di.mcar[,8],ttwa.di.mcar[,9],ttwa.di.mcar[,10]),
                                city=ttwa.di.mcar$city,
                                stat=ttwa.di.mcar$X)
write.csv(format.ttwa.di.mcar,file='../Results/Formatted results/ttwa di mcar formatted.csv')
