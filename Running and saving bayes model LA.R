#######################################################
##  Bayesian models for RCI based on LA boundaries    #
##  Start: 18/3/2017                                 #
#######################################################
##  source: https://en.wikipedia.org/wiki/List_of_cities_in_the_United_Kingdom

##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
##  Data load;
load('../Data/Analysis data/England and Wales benefits 0111 final.Rdata')
city.centres<-read.csv('../Data/City centres/UK city centres.csv')

##  The subsetting to only certain cities used in the analysis
valid.cities<-as.character(city.centres$la[city.centres$la%in%ew.2001$la])
cities.list<-list(NULL)
for (i in 1:length(valid.cities)){
  cities.list[[i]]<-ew.2001[ew.2001$la%in%valid.cities[i],]
}
names(cities.list)<-valid.cities
##  So now we have a cities.list file of only the relevant cities

rm(ew.2001) #remove large .shp to save space

##  Second: Now we need to do the model and get the results. We will run the models and save the results
burnin=20000; n.sample=40000; n.thin=10
for (i in 1:length(cities.list)){
    saved.name<-names(cities.list)[[i]]
    print(paste(saved.name,'model is initialising'))
    temp.df<-cities.list[[i]]

    W.nb.city <- poly2nb(temp.df)
    W.list.city <- nb2listw(W.nb.city, style = "B") #b is binary code
    W.city <- nb2mat(W.nb.city, style = "B")
    n.city <- nrow(W.city) 
    
    ##  We now have to get the models for each DWP statistic
    formula2001<-list(jsa2001~1,ib2001~1,is2001~1)
    formula2011<-list(jsa2011~1,ib2011~1,is2011~1)
    var.name<-c('jsa','ib','is')

    for (j in 1:length(formula2001)){
    models<-list(NULL)
    formula.y1<-formula2001[[j]]
    formula.y2<-formula2011[[j]]
    
    print(paste(saved.name,var.name[j],'2001 model is running'))    
    models[[1]] <-S.CARleroux(formula=formula.y1, data=temp.df, family="binomial", trials=temp.df$w.pop2001, W=W.city, burnin=burnin, n.sample=n.sample, thin=n.thin)
    
    print(paste(saved.name,var.name[j],'model 2 is running'))
    models[[2]] <-S.CARleroux(formula=formula.y2, data=temp.df, family="binomial", trials=round(temp.df$w.pop2011), W=W.city, burnin=burnin, n.sample=n.sample, thin=n.thin)

    print(paste(saved.name,'model results are saving to ../Data/Analysis data/Models estimates/LA'))
    save(models,file=paste('../Data/Analysis data/Model estimates/LA/',saved.name,var.name[j],'.Rdata',sep=''))
    rm(models)
    }
}
    
##  Third: We will run the results but this time for the pooled model between years
burnin=20000; n.sample=40000; n.thin=10
for (i in 1:length(cities.list)){

  saved.name<-names(cities.list)[[i]]
  print(paste(saved.name,'MVS.CARleroux model is initialising'))
  temp.df<-cities.list[[i]]
  
  W.nb.city <- poly2nb(temp.df)
  W.list.city <- nb2listw(W.nb.city, style = "B") #b is binary code
  W.city <- nb2mat(W.nb.city, style = "B")
  n.city <- nrow(W.city) 
  
  
  ##  Creating the dependent variables
  jsa.mat <- cbind(temp.df$jsa2001, temp.df$jsa2011)
  jsa <- as.numeric(t(jsa.mat))
  ib.mat <- cbind(temp.df$ib2001, temp.df$ib2011)
  ib <- as.numeric(t(ib.mat))
  is.mat <- cbind(temp.df$is2001, temp.df$is2011)
  is <- as.numeric(t(is.mat))
  
  N.mat <- cbind(temp.df$w.pop2001, round(temp.df$w.pop2011))
  N <- as.numeric(t(N.mat))

  ##  We now have to get the models for each DWP statistic
  formula.MCAR<-list(jsa~1,ib~1,is~1)
  var.name<-c('jsa','ib','is')
  
  for (j in 1:length(formula.MCAR)){

    model.MCAR <- MVS.CARleroux(formula=formula.MCAR[[j]], family='binomial',trials=N, W=W.city, burnin=burnin, n.sample=n.sample, thin=n.thin)
    
    print(paste(saved.name,'MCAR model results are saving to ../Data/Analysis data/Models estimates/LA'))
    save(model.MCAR,file=paste('../Data/Analysis data/Model estimates/LA/MCAR',saved.name,var.name[j],'.Rdata',sep=''))
    rm(model.MCAR)
  }
}

##  End: All the models should have been saved as R objects for us to use later. 