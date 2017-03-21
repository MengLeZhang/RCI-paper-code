#######################################################
##  This is the analysis for the biggest cities based #
##  on local authority                                #
##  This list was decided based on pop                #
##  Start: 6/2/2017                                   #
#######################################################
##  source: https://en.wikipedia.org/wiki/List_of_cities_in_the_United_Kingdom

##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
##  Data load;
load('LSOA data/Analysis file/England and Wales benefits 0111.Rdata')
city.centres<-read.csv('City centres/UK city centres.csv')

##  The subsetting to only certain cities
valid.cities<-as.character(city.centres$la[city.centres$la%in%ew.2001$la])
cities.list<-list(NULL)
for (i in 1:length(valid.cities)){
  cities.list[[i]]<-ew.2001[ew.2001$la%in%valid.cities[i],]
}
names(cities.list)<-valid.cities
##  So now we have a cities.list file of only the relevant cities

##  Second: Now it is a good time to go ahead and making our RCIs; first we will get the distance of each zone's centroids from several midpoints.
for (i in 1:length(cities.list)){
temp.mids<-city.centres[city.centres$la==valid.cities[i],]
centroids<-getSpPPolygonsLabptSlots(cities.list[[i]])
cities.list[[i]]$dist.a<-euclid.dist(point=c(t(temp.mids[,3:4])),x=centroids)
cities.list[[i]]$dist.b<-euclid.dist(point=c(t(temp.mids[,5:6])),x=centroids)
cities.list[[i]]$dist.c<-euclid.dist(point=c(t(temp.mids[,7:8])),x=centroids)
cities.list[[i]]$dist.d<-euclid.dist(point=c(t(temp.mids[,9:10])),x=centroids)
}
warnings() #use cord methods--well whatever
rm(ew.2001) #remove to save space

##  Four: Now we can actually just do the rci for the various measures we want. We will show the point estimates which will show that the choice of midpoints is not important

RCI.tables<-list(NULL)
for (i in 1:length(cities.list)){

  temp.df<-cities.list[[i]]@data #we only need the data file from here on in 
  names(temp.df)
##  We will need to work out the rci for the various cols
  temp.out<-list(NULL)
  for(j in 1:3){

    id2001<-which(names(temp.df)%in%c('jsa2001','is2001','ib2001'))[j]
    id2011<-which(names(temp.df)%in%c('jsa2011','is2011','ib2011'))[j] #this is the cols for the various year data fo each measure
    id.dist<-which(names(temp.df)%in%c('dist.a','dist.b','dist.c','dist.d'))
    
    rci2001<-apply(temp.df[,id.dist],2,rci,y=temp.df[,id2001],x=temp.df$w.pop2001-temp.df[,id2001])
    rci2011<-apply(temp.df[,id.dist],2,rci,y=temp.df[,id2011],x=round(temp.df$w.pop2011)-temp.df[,id2011])
    temp.out[[j]]<-cbind(rci2001,rci2011)
    }

temp.res<-data.frame(do.call(cbind,temp.out))
temp.res$jsadiff<-temp.res[,2]-temp.res[,1]
temp.res$isdiff<-temp.res[,4]-temp.res[,3]
temp.res$ibdiff<-temp.res[,6]-temp.res[,5]
temp.res<-round(temp.res,4)
temp.res$city<-names(cities.list)[[i]]
RCI.tables[[i]]<-temp.res
}

RCI.tables<-do.call(rbind,RCI.tables)
write.csv(RCI.tables,file='RCI LA point estimates.csv')


##  Five: We will simplify the results table by only obtaining the bayesian ci for midpoint 4. 
##  We will do one results table at a time
burnin<-20000
n.sample<-40000
n.thin<-10

##  jsa results
jsa.analysis<-rci.la.bayes(formula(jsa2001~1),formula(jsa2011~1),burnin=20000, n.sample=40000, n.thin=10)
jsa.mod<-jsa.analysis$models
save(file='Results/LA boundaries/jsa bayesian results.Rdata',jsa.mod)
jsa.tab<-do.call(rbind,jsa.analysis$table)
write.csv(file='Results/LA boundaries/jsa RCI table.csv',jsa.tab)
jsa.tab
rm(jsa.analysis,jsa.mod)

## is results
is.analysis<-rci.la.bayes(formula(is2001~1),formula(is2011~1),burnin=20000, n.sample=40000, n.thin=10)
is.mod<-is.analysis$models
save(file='Results/LA boundaries/is bayesian results.Rdata',is.mod)
is.tab<-do.call(rbind,is.analysis$table)
write.csv(file='Results/LA boundaries/is RCI table.csv',is.tab)

rm(is.analysis,is.mod)

##  ib results
ib.analysis<-rci.la.bayes(formula(ib2001~1),formula(ib2011~1),burnin=20000, n.sample=40000, n.thin=10)
ib.mod<-ib.analysis$models
save(file='Results/LA boundaries/ib bayesian results.Rdata',ib.mod)
ib.tab<-do.call(rbind,ib.analysis$table)
write.csv(file='Results/LA boundaries/ib RCI table.csv',ib.tab)

rm(ib.analysis,ib.mod)

##  Custom function: Here is the function that I am using for the results. It outputs the analysis for each city--including the model--as well as just the samples for RCI. 

rci.la.bayes<-function(formula.y1,formula.y2,burnin=burnin, n.sample=n.sample, n.thin=n.sample){

  out.mods<-list(NULL)
  out.tab<-list(NULL)
  
for (i in 1:length(cities.list)){

  temp.df<-cities.list[[i]]
  city.name<-names(cities.list)[[i]]
  print(city.name);print(i)
  
  W.nb.city <- poly2nb(temp.df)
  W.list.city <- nb2listw(W.nb.city, style = "B") #b is binary code
  W.city <- nb2mat(W.nb.city, style = "B")
  n.city <- nrow(W.city) 

  models<-list(NULL)
  models[[1]] <-S.CARleroux(formula=formula.y1, data=temp.df, family="binomial", trials=temp.df$w.pop2001, W=W.city, burnin=burnin, n.sample=n.sample, thin=n.thin)
  
  models[[2]] <-S.CARleroux(formula=formula.y2, data=temp.df, family="binomial", trials=round(temp.df$w.pop2011), W=W.city, burnin=burnin, n.sample=n.sample, thin=n.thin)
    
  pred01<-predict.simple(models[[1]],temp.df$w.pop2001)
  pred11<-predict.simple(models[[2]],round(temp.df$w.pop2011))
  
  out.mods[[i]]<-models #save the model
  rm(models)
  
  ##  Part B: Saving the RCI
  
  rci.2001<-list(NULL)
  for (j in 1:nrow(pred01)){
    rci.2001[[j]]<-rci(sort.var=temp.df$dist.d,x=temp.df$w.pop2001-pred01[j,],y=pred01[j,])
  }
  rci.2001<-unlist(rci.2001)
  
  rci.2011<-list(NULL)
  for (j in 1:nrow(pred11)){
    rci.2011[[j]]<-rci(sort.var=temp.df$dist.d,x=temp.df$w.pop2011-pred11[j,],y=pred11[j,])
  }
  rci.2011<-unlist(rci.2011)
  
  res.labels<-paste(city.name,c('2001','2011','Diff'))
  result<-lapply(list(rci.2001,rci.2011,rci.2011-rci.2001),quantile,probs=c(0.025,0.5,0.975))
  
  formatted<-lapply(result,function(x){formatC( round( x, 3 ), format='f', digits=3 )})
  formatted<-lapply(formatted,function(x){paste(x[2],' (',x[1],',',x[3],') ',sep='')}) #for formatted brackets
  
  result<-data.frame(do.call(rbind,result),label=paste(res.labels),formatted=unlist(formatted))

  out.tab[[i]]<-result
}
  return(list(models=out.mods,table=out.tab))
}
