#######################################################
##  Bayesian models for RCI based on London TTWA        # 
##  boundaries                                        #
##  Start:  21/3/2017                                 #
##  We are treating London separately because of how  #
##  large it is; it will simply take a fair while to  #
##  get the MCMC results                              #
#######################################################

##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
##  Data load; Just taking London
load('../Data/Analysis data/England and Wales benefits 0111 final.Rdata')

##  load in ttwa
TTWA.2011<- readOGR(dsn='../Data/TTWA 2011', layer='Travel_to_Work_Areas_December_2011_Full_Extent_Boundaries_in_United_Kingdom') 
TTWA.2011<-gBuffer(TTWA.2011, byid=TRUE, width=-1)
london<-TTWA.2011[grep('London',TTWA.2011$ttwa11nm),]

##  Subset the LSOA file to just london
london<-ew.2001[london,]

##  Step two: 
##  The subsetting to only certain ttwa
rm(ew.2001) # remove shp file to save space

##  Second: Now we need to do the model and get the results. We will run the models and save the results
burnin=20000; n.sample=40000; n.thin=10

saved.name<-'London'
W.nb.city <- poly2nb(london)
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
  models[[1]] <-S.CARleroux(formula=formula.y1, data=london, family="binomial", trials=london$w.pop2001, W=W.city, burnin=burnin, n.sample=n.sample, thin=n.thin)
  
  print(paste(saved.name,var.name[j],'model 2 is running'))
  models[[2]] <-S.CARleroux(formula=formula.y2, data=london, family="binomial", trials=round(london$w.pop2011), W=W.city, burnin=burnin, n.sample=n.sample, thin=n.thin)
  
  print(paste(saved.name,'model results are saving to ../Data/Analysis data/Models estimates/London'))
  save(models,file=paste('../Data/Analysis data/Model estimates/London/',saved.name,var.name[j],'.Rdata',sep=''))
  rm(models)
}


##  Third: We will run the results but this time for the pooled model between years
burnin=20000; n.sample=40000; n.thin=10
for (i in 1:length(ttwa.list)){
  saved.name<-names(ttwa.list)[[i]]
  print(paste(saved.name,'MCAR model is initialising'))
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
    
    model.MCAR <- binomial.MCARleroux(formula=formula.MCAR[[j]], trials=N, W=W.city, burnin=burnin, n.sample=n.sample, thin=thin)
    
    print(paste(saved.name,'MCAR model results are saving to ../Data/Analysis data/Models estimates/TTWA'))
    save(models,file=paste('../Data/Analysis data/Model estimates/TTWA/MCAR',saved.name,var.name[j],'.Rdata',sep=''))
    rm(models)
  }
}


##  End: All the models should have been saved as R objects for us to use later. 