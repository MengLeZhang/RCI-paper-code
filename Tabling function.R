###################################################
##  This is the tabling function for our results  #
##  Start: 11/4/2017                              #
###################################################

test<-read.csv('C:/Users/USer/Documents/SMI analysis/UK RCI paper 1/Results/DI LA raw CI mcar.csv')
?prettyNum

##  Table format 1: This format is for the bayesian credible intervals median (loewr, upper); 
##  Need to had a pretty formate for the median
tab.format1<-function(median,lower,upper,digits=3){
  median<-formatC(median,digits=digits, format='f',flag=c(' ','#'))

  lower<-formatC(lower,digits=digits, format='f',flag=c(' ','#'))
  upper<-formatC(upper,digits=digits, format='f',flag=c(' ','#'))
  paste(median,' (',lower,', ',upper,') ',sep='')  
}
