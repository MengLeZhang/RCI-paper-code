#######################################################
##  Bayesian models for RCI based on 2011 TTWA        # 
##  boundaries                                        #
##  Start: 18/3/2017                                 #
##  Updated: 21/3/2017                                #
##  This script now omits london but brings the rest  #
##  of the models up to correct burnin/samples        #
#######################################################

##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
##  Data load;
load('../Data/Analysis data/England and Wales benefits 0111 final.Rdata')
city.centres<-read.csv('../Data/City centres/UK city centres.csv')
city.centres<-city.centres[-1,] #omit London

##  load in ttwa
TTWA.2011<- readOGR(dsn='../Data/TTWA 2011', layer='Travel_to_Work_Areas_December_2011_Full_Extent_Boundaries_in_United_Kingdom') 
TTWA.2011<-gBuffer(TTWA.2011, byid=TRUE, width=-1)

## Get the city centre points
mono.centres.sp<-SpatialPointsDataFrame(coords= coordinates(city.centres[,c('EastingD','NorthingD')]),data=data.frame(city.centres),proj4string=CRS(proj4string(TTWA.2011)))
##  Now we subset to just the ttwa that we are interested in

##  Before we do that need to also make sure london is included as well.
sub.ttwa<-TTWA.2011[mono.centres.sp,]
sub.ttwa<-sub.ttwa[ew.2001,] #Those in England and Wales
sub.ttwa<-gBuffer(sub.ttwa, byid=TRUE, width=-0.1)

##  Step two: 
##  The subsetting to only certain ttwa
valid.ttwa<-as.character(sub.ttwa$ttwa11nm)
ttwa.list<-list(NULL)
for (i in 1:length(valid.ttwa)){
  ttwa.list[[i]]<-ew.2001[sub.ttwa[i,],]
}
names(ttwa.list)<-valid.ttwa
rm(ew.2001) # remvoe shp file to save space
names(ttwa.list)

##  Second: Now we need to do the model and get the results. We will run the models and save the results
burnin=20000; n.sample=40000; n.thin=10
for (i in 1:length(ttwa.list)){
  saved.name<-names(ttwa.list)[[i]]
  print(paste(saved.name,'model is initialising'))
  temp.df<-ttwa.list[[i]]
  
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
    
    print(paste(saved.name,'model results are saving to ../Data/Analysis data/Models estimates/TTWA'))
    save(models,file=paste('../Data/Analysis data/Model estimates/TTWA/',saved.name,var.name[j],'.Rdata',sep=''))
    rm(models)
  }
}


##  Third: We will run the results but this time for the pooled model between years
burnin=20000; n.sample=40000; n.thin=10
for (i in 1:length(ttwa.list)){
  
  saved.name<-names(ttwa.list)[[i]]
  print(paste(saved.name,'MVS.CARleroux model is initialising'))
  temp.df<-ttwa.list[[i]]
  
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
    
    print(paste(saved.name,'MCAR model results are saving to ../Data/Analysis data/Models estimates/TTWA'))
    save(model.MCAR,file=paste('../Data/Analysis data/Model estimates/TTWA/MCAR',saved.name,var.name[j],'.Rdata',sep=''))
    rm(model.MCAR)
  }
}

##  End: All the models should have been saved as R objects for us to use later. 