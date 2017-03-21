#################################################################
##  General RCI function;                                     ###
##  We need a way to make our rci stuff even more general so  ###
##  that for our analyses we can sort of use a simple set of  ###
##  functions for the analysis                                ###
##  Start: 25/11/2016                                         ###
##  I think perhaps we should look into creating a few separat###
##  functions: one for making deciles; one for RCI; one for   ###
##  the leroux predictions. This ought to actually cover most ###
##  of what is needed (I reckon)                              ###
##  Let's use proper R conventions and use lower case to start###
##  our functions                                             ###
##  Update: 6/12/2016; we made a mistake in calculating RCI   ###
##  we thought to use the distance measure as the x axis but  ###
##  actually the x axis is the cumsum of the number of people ###
##  not in poverty. We have since corrected this              ###
##  Update: 7/12/2016 We added a general option to the RCI    ###
##  comand that tells us if we want to split or not. Usually  ###
##  we do not                                                 ###
##  Update: 1/2/2017. I am going to change the functions to   ###
##  decompose the rci based on the centres.                   ###
#################################################################

##  Step one: If calling from a source() command it would be nice to having it load up all the packages we need
x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", 'CARBayes','shapefiles','sp','spdep','MCMCpack','truncdist','raster')
lapply(x, library, character.only = TRUE) # load the required packages; again this is a remarkably 
warnings()

##  Step two: We just actually need a a simple function to take coordinates about the middle of city and make new distance from centroids and percentiles. Actually we probably ought to do this seperately really in order to better see the results. 
##  Right so we basically need to take a midpoint and a matrix of points x to that point
##  25/11/2016 Tested and works
euclid.dist<-function(point,x){
  temp.mat<-t(matrix(rep(point,nrow(x)),nrow=2))
  distance<-sqrt(rowSums((x-temp.mat)^2))
  return(distance)
}

##  we call it x tiles cause we do not know what type of splits we will need; however it will go from lowers to highests as wish
make.xtiles<-function(x,splits){
  as.numeric(cut(x,breaks=quantile(x, probs=seq(0,1, by=1/splits), na.rm=TRUE), include.lowest=TRUE))
}

##  Step three: The RCI function that we want to sort of use has already been made
##  It takes two things; a vector of x and y. We will take it in traditional RCI that actually x=distance and y= stat. x is the one we will generate as percentiles. We have generalised it to make it take more than just deciles. Instead we will jsut include the xtiles function in there to generalise it further--less steps computing things from distance to xtiles in the code. default splits is 10
##  25/11/2016: Tested with bristol data ands works fine
##  Update: 6/12/2016 We have been using the wrong formula of RCI; we need x and y to be different variables which are all ordered depending on their distance; we will correct the script by putting a new variable called sort.var which is the variable we sort them by. Then another variable called splits (which is the same as x.splits but change to stop cofustion). 

rci<-function(x,y,sort.var,split=F,n.splits=10){
  #x=Cities.list[[1]]$over16-Cities.list[[1]]$jsa2001;y=Cities.list[[1]]$jsa2001;splits=10;sort.var=Cities.list[[1]]$dist.d; n.splits=10; split=F
  
  if(split==T){sort.var<-make.xtiles(x=sort.var,splits=n.splits)}  #sort in ascending order by percentile--assuming that we are using splits
  ascending<-sort(sort.var)

  N<-length(ascending)

  ##  Now to get the sums per percentile group for our x and ys
  sum.x<-rep(NA,N);sum.y<-rep(NA,N)
  for (i in 1:N){
    sum.x[i]<-sum(x[sort.var==ascending[i]])
    sum.y[i]<-sum(y[sort.var==ascending[i]])#i think this is superfluous but let's go with it
  }
  ##  cumulative sums
  csum.x<-cumsum(sum.x)/sum(sum.x)
  csum.y<-cumsum(sum.y)/sum(sum.y)

  ## The below has been checked to be right;
  out<-t(csum.y[-N])%*%csum.x[-1]-t(csum.y[-1])%*%csum.x[-N]
  return(out)
}

##  Step 3b: it ought to be quite easy change the rci to a decomposed R: We just change the out part of the RCI function

g.rci<-function(x,y,sort.var,split=F,n.splits=10){
  #x=Cities.list[[1]]$over16-Cities.list[[1]]$jsa2001;y=Cities.list[[1]]$jsa2001;splits=10;sort.var=Cities.list[[1]]$dist.d; n.splits=10; split=F
  
  if(split==T){sort.var<-make.xtiles(x=sort.var,splits=n.splits)}  #sort in ascending order by percentile--assuming that we are using splits
  ascending<-sort(sort.var)
  
  N<-length(ascending)
  
  ##  Now to get the sums per percentile group for our x and ys
  sum.x<-rep(NA,N);sum.y<-rep(NA,N)
  for (i in 1:N){
    sum.x[i]<-sum(x[sort.var==ascending[i]])
    sum.y[i]<-sum(y[sort.var==ascending[i]])#i think this is superfluous but let's go with it
  }
  ##  cumulative sums
  csum.x<-cumsum(sum.x)/sum(sum.x)
  csum.y<-cumsum(sum.y)/sum(sum.y)
  
  ## The below has been checked to be right; This is a vector giving the individual contribution that each areal unit makes to RCI
  out<-(csum.x-c(0,csum.x[-N]))*(csum.y+c(0,csum.y[-N]))-(csum.x+c(0,csum.x[-N]))*(csum.x-c(0,csum.x[-N])) #we are multple vectors instead of getting their products. So we can use the indexing
  return(out)
}




##  Step four: we probably just need a simple routine getting the predicted number of people doing x in an area using a simple intercept leroux model; this only works for that model only
predict.simple<-function(LRmodel,trials){
  N.zones<-ncol(LRmodel$samples$phi)
  fitted.vals<-t(rep(1,N.zones)%*%t(LRmodel$samples$beta)) + LRmodel$samples$phi #This was fixed
  prob<-exp(fitted.vals)/(1+exp(fitted.vals))
  outcomes<-prob%*%diag(trials) #This ought to be it for the an estimate of JSA in every place
  return(outcomes)
}

