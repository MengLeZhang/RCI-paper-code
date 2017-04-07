#######################################################
##  This is the analysis for the biggest cities based #
##  on local authority                                #
##  This analysis is for London Only!                 #
##  Due to the large size of the bayes samples        #
##  Start: 18/3/2017                                  #
#######################################################

##  Pre: Load in all the RCI functions we need
source.file<-'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
##  Data load;
load('../Data/Analysis data/England and Wales benefits 0111 final.Rdata')

##  load in ttwa
TTWA.2011<- readOGR(dsn='../Data/TTWA 2011', layer='Travel_to_Work_Areas_December_2011_Full_Extent_Boundaries_in_United_Kingdom') 
TTWA.2011<-gBuffer(TTWA.2011, byid=TRUE, width=-1)

##  Read in city centres file.
city.centres<-read.csv('../Data/City centres/UK city centres.csv')
city.centres<-city.centres[1,] #take London only

## Get the city centre points
mono.centres.sp<-SpatialPointsDataFrame(coords= coordinates(city.centres[,c('EastingD','NorthingD')]),data=data.frame(city.centres),proj4string=CRS(proj4string(TTWA.2011)))
##  Now we subset to just the ttwa that we are interested in
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

##  Second: Unlike LA we need 3 different distance ordering variables. One is distance from city centre (we will choose mid point D). Another is closes distance to a centre. Yet another is by some sort of accessibility index

##  Distance by centre. Variable dist.d. Each ttwa has only one match to a city
for (i in 1:length(ttwa.list)){
  temp.mids<-mono.centres.sp[ttwa.list[[i]],]
  centroids<-getSpPPolygonsLabptSlots(ttwa.list[[i]])
  ttwa.list[[i]]$dist.d<-euclid.dist(point=c(t(temp.mids@data[,9:10])),x=centroids)
}

##  Distance to nearest centre
##  Not necesarily for London

##  Accessibility based on Hansen (1959). Guess between -1 and -2 for the exponent
##  This is a for loop that calulcate the index (with an extra 100m added to distance for terminal time)
ttwa.emp<-list(NULL)
for (k in 1:length(ttwa.list)){
  temp.df<-ttwa.list[[k]]
  centroids<-getSpPPolygonsLabptSlots(temp.df)
  temp.df<-ttwa.list[[k]]@data
  
  temp.emp<-list(NULL)
  for (i in 1:nrow(temp.df)){
    temp.dist<-euclid.dist(point=centroids[i,],x=centroids)
    temp.out<-list(NULL)
    
    ## 2001 accessibility index. We need the negative of accessibility
    hansen1.2001<--sum(temp.df$work.pop2001*(temp.dist+100)^(-1))
    hansen2.2001<--sum(temp.df$work.pop2001*(temp.dist+100)^(-2))
    
    ## 2011 accessibility index
    hansen1.2011<--sum(temp.df$work.pop2011*(temp.dist+100)^(-1))
    hansen2.2011<--sum(temp.df$work.pop2011*(temp.dist+100)^(-2))
    
    temp.emp[[i]]<-cbind(hansen1.2001,hansen1.2011,hansen2.2001,hansen2.2011)
  }
  ttwa.emp[[k]]<-do.call(rbind,temp.emp)
  colnames(ttwa.emp[[k]])<-c('hansen1.2001','hansen1.2011','hansen2.2001','hansen2.2011')
}

for (i in 1:length(ttwa.list)){
  ttwa.list[[i]]<-cbind(ttwa.list[[i]],ttwa.emp[[i]])
}


##  RCI routine----
##  Third: Now for each city we have to establish a routine for working out the RCI results
##  First we will get the point estimates; this is for checking as much as anything else
RCI.tables<-list(NULL)
for (i in 1:length(ttwa.list)){
  
  temp.df<-ttwa.list[[i]]@data #we only need the data file from here on in 
  names(temp.df)
  ##  We will need to work out the rci for the various cols
  temp.out<-list(NULL)
  for(j in 1:3){
    
    id2001<-which(names(temp.df)%in%c('jsa2001','is2001','ib2001'))[j]
    id2011<-which(names(temp.df)%in%c('jsa2011','is2011','ib2011'))[j] #this is the cols for the various year data fo each measure
    id.dist01<-which(names(temp.df)%in%c('dist.d','hansen1.2001','hansen2.2001'))
    id.dist11<-which(names(temp.df)%in%c('dist.d','hansen1.2011','hansen2.2011'))
    
    rci2001<-apply(temp.df[,id.dist01],2,rci,y=temp.df[,id2001],x=temp.df$w.pop2001-temp.df[,id2001])
    rci2011<-apply(temp.df[,id.dist11],2,rci,y=temp.df[,id2011],x=round(temp.df$w.pop2011)-temp.df[,id2011])
    temp.out[[j]]<-cbind(rci2001,rci2011)
  }
  
  temp.res<-data.frame(do.call(cbind,temp.out))
  temp.res$jsadiff<-temp.res[,2]-temp.res[,1]
  temp.res$isdiff<-temp.res[,4]-temp.res[,3]
  temp.res$ibdiff<-temp.res[,6]-temp.res[,5]
  temp.res<-round(temp.res,4)
  temp.res$city<-names(ttwa.list)[[i]]
  RCI.tables[[i]]<-temp.res
}
RCI.tables
RCI.tables<-do.call(rbind,RCI.tables)
write.csv(RCI.tables,file='../Results/RCI London point estimates.csv')

##  Second step for the output is to simply get the above but accounting for the uncertainty
##  The results are already saved; we will complete this for the estimates that used two separate models
rci.raw.ci<-list(NULL)

for (i in 1:length(ttwa.list)){
  saved.name<-names(ttwa.list)[[i]]
  temp.df<-ttwa.list[[i]]
  
  var.name<-c('jsa','ib','is')
  
  saved.results<-list(NULL) #A list that will contain 3 objects; the saved results for jsa, ib, is
  
  for (j in 1:length(var.name)){
    load(file=paste('../Data/Analysis data/Model estimates/London/',saved.name,var.name[j],'.Rdata',sep='')) #all the list objects are called 'models'
    
    pred01<-predict.simple(models[[1]],temp.df$w.pop2001)
    pred11<-predict.simple(models[[2]],round(temp.df$w.pop2011))
    
    rm(models)
    
    
    ##Distance RCI
    rci.dist.d.2001<-list(NULL)
    rci.dist.d.2011<-list(NULL)
    for (k in 1:nrow(pred01)){
      rci.dist.d.2001[[k]]<-rci(sort.var=temp.df$dist.d,x=temp.df$w.pop2001-pred01[k,],y=pred01[k,])
      rci.dist.d.2011[[k]]<-rci(sort.var=temp.df$dist.d,x=temp.df$w.pop2011-pred11[k,],y=pred11[k,])
    }
    rci.dist.d.2001<-unlist(rci.dist.d.2001)
    rci.dist.d.2011<-unlist(rci.dist.d.2011)
    rci.dist.d.diff<-rci.dist.d.2011-rci.dist.d.2001
    result.dist.d<-lapply(list(rci.dist.d.2001,rci.dist.d.2011,rci.dist.d.diff),quantile,probs=c(0.5,0.025,0.975))

    ##Hansen 1
    rci.hansen1.2001<-list(NULL)
    rci.hansen1.2011<-list(NULL)
    for (k in 1:nrow(pred01)){
      rci.hansen1.2001[[k]]<-rci(sort.var=temp.df$hansen1.2001,x=temp.df$w.pop2001-pred01[k,],y=pred01[k,])
      rci.hansen1.2011[[k]]<-rci(sort.var=temp.df$hansen1.2011,x=temp.df$w.pop2011-pred11[k,],y=pred11[k,])
    }
    rci.hansen1.2001<-unlist(rci.hansen1.2001)
    rci.hansen1.2011<-unlist(rci.hansen1.2011)
    rci.hansen1.diff<-rci.hansen1.2011-rci.hansen1.2001
    result.hansen1<-lapply(list(rci.hansen1.2001,rci.hansen1.2011,rci.hansen1.diff),quantile,probs=c(0.5,0.025,0.975))
    
    ##Hansen 2
    rci.hansen2.2001<-list(NULL)
    rci.hansen2.2011<-list(NULL)
    for (k in 1:nrow(pred01)){
      rci.hansen2.2001[[k]]<-rci(sort.var=temp.df$hansen2.2001,x=temp.df$w.pop2001-pred01[k,],y=pred01[k,])
      rci.hansen2.2011[[k]]<-rci(sort.var=temp.df$hansen2.2011,x=temp.df$w.pop2011-pred11[k,],y=pred11[k,])
    }
    rci.hansen2.2001<-unlist(rci.hansen2.2001)
    rci.hansen2.2011<-unlist(rci.hansen2.2011)
    rci.hansen2.diff<-rci.hansen2.2011-rci.hansen2.2001
    result.hansen2<-lapply(list(rci.hansen2.2001,rci.hansen2.2011,rci.hansen2.diff),quantile,probs=c(0.5,0.025,0.975))
    
    saved.results[[j]]<-rbind(unlist(result.dist.d),
                              unlist(result.hansen1),unlist(result.hansen2))
    row.names(saved.results[[j]])<-c('dist.d','hansen.1','hansen.2')
    colnames(saved.results[[j]])<-paste(var.name[j],colnames(saved.results[[j]]))
  }
  rci.raw.ci[[i]]<-do.call(cbind,saved.results)
  rci.raw.ci[[i]]<-cbind(rci.raw.ci[[i]],saved.name)
}
rci.raw.tab<-do.call(rbind,rci.raw.ci)
write.csv(rci.raw.tab,file='../Results/RCI London raw CI.csv')

##  Results for the MCAR model

rci.mcar.ci<-list(NULL)

for (i in 1:length(ttwa.list)){
  saved.name<-names(ttwa.list)[[i]]
  temp.df<-ttwa.list[[i]]
  
  var.name<-c('jsa','ib','is')
  
  saved.results<-list(NULL) #A list that will contain 3 objects; the saved results for jsa, ib, is
  
  for (j in 1:length(var.name)){
    print(paste(saved.name,j))
    load(file=paste('../Data/Analysis data/Model estimates/London/MCAR',saved.name,var.name[j],'.Rdata',sep='')) #all the list objects are called 'models'
    
    N.mat <- cbind(temp.df$w.pop2001, round(temp.df$w.pop2011))
    N <- as.numeric(t(N.mat)) #N is an alternating vector [i.e. (pop01,pop11,pop01,pop11 etc for eacg zone)]. Ditto for our fitted values
    
    pred<-predict.simple(model.MCAR,N)
    odds<-which(1:ncol(pred)%%2==1)
    pred01<-pred[,odds] #the odd number rows are the results for each zone in 2001
    pred11<-pred[,odds+1]
    
    rm(model.MCAR,pred)
    
    ##Distance RCI
    rci.dist.d.2001<-list(NULL)
    rci.dist.d.2011<-list(NULL)
    for (k in 1:nrow(pred01)){
      rci.dist.d.2001[[k]]<-rci(sort.var=temp.df$dist.d,x=temp.df$w.pop2001-pred01[k,],y=pred01[k,])
      rci.dist.d.2011[[k]]<-rci(sort.var=temp.df$dist.d,x=temp.df$w.pop2011-pred11[k,],y=pred11[k,])
    }
    rci.dist.d.2001<-unlist(rci.dist.d.2001)
    rci.dist.d.2011<-unlist(rci.dist.d.2011)
    rci.dist.d.diff<-rci.dist.d.2011-rci.dist.d.2001
    result.dist.d<-lapply(list(rci.dist.d.2001,rci.dist.d.2011,rci.dist.d.diff),quantile,probs=c(0.5,0.025,0.975))
    
    ##Hansen 1
    rci.hansen1.2001<-list(NULL)
    rci.hansen1.2011<-list(NULL)
    for (k in 1:nrow(pred01)){
      rci.hansen1.2001[[k]]<-rci(sort.var=temp.df$hansen1.2001,x=temp.df$w.pop2001-pred01[k,],y=pred01[k,])
      rci.hansen1.2011[[k]]<-rci(sort.var=temp.df$hansen1.2011,x=temp.df$w.pop2011-pred11[k,],y=pred11[k,])
    }
    rci.hansen1.2001<-unlist(rci.hansen1.2001)
    rci.hansen1.2011<-unlist(rci.hansen1.2011)
    rci.hansen1.diff<-rci.hansen1.2011-rci.hansen1.2001
    result.hansen1<-lapply(list(rci.hansen1.2001,rci.hansen1.2011,rci.hansen1.diff),quantile,probs=c(0.5,0.025,0.975))
    
    ##Hansen 2
    rci.hansen2.2001<-list(NULL)
    rci.hansen2.2011<-list(NULL)
    for (k in 1:nrow(pred01)){
      rci.hansen2.2001[[k]]<-rci(sort.var=temp.df$hansen2.2001,x=temp.df$w.pop2001-pred01[k,],y=pred01[k,])
      rci.hansen2.2011[[k]]<-rci(sort.var=temp.df$hansen2.2011,x=temp.df$w.pop2011-pred11[k,],y=pred11[k,])
    }
    rci.hansen2.2001<-unlist(rci.hansen2.2001)
    rci.hansen2.2011<-unlist(rci.hansen2.2011)
    rci.hansen2.diff<-rci.hansen2.2011-rci.hansen2.2001
    result.hansen2<-lapply(list(rci.hansen2.2001,rci.hansen2.2011,rci.hansen2.diff),quantile,probs=c(0.5,0.025,0.975))
    unlist(result.hansen2)
    
    saved.results[[j]]<-rbind(unlist(result.dist.d),unlist(result.hansen1),unlist(result.hansen2))
    row.names(saved.results[[j]])<-c('dist.d','hansen.1','hansen.2')
    colnames(saved.results[[j]])<-paste(var.name[j],colnames(saved.results[[j]]))
  }
  rci.mcar.ci[[i]]<-do.call(cbind,saved.results)
  rci.mcar.ci[[i]]<-cbind(rci.mcar.ci[[i]],saved.name)
}

rci.mcar.ci<-do.call(rbind,rci.mcar.ci)
write.csv(rci.mcar.ci,file='../Results/RCI London raw CI mcar.csv')






##  Dissimilarity index

di.results<-list(NULL)
for (i in 1:length(ttwa.list)){
  saved.name<-names(ttwa.list)[[i]]
  temp.df<-ttwa.list[[i]]
  var.name<-c('jsa','ib','is')
  
  saved.results<-list(NULL) #A list that will contain 3 objects; the saved results for jsa, ib, is
  
  for (j in 1:length(var.name)){
    load(file=paste('../Data/Analysis data/Model estimates/London/',saved.name,var.name[j],'.Rdata',sep='')) #all the list objects are called 'models'
    
    pred01<-predict.simple(models[[1]],temp.df$w.pop2001)
    pred11<-predict.simple(models[[2]],round(temp.df$w.pop2011))
    rm(models)
    
    di01<-c(NULL);di11<-c(NULL)
    for (k in 1:nrow(pred01)){
      di01[[k]]<-sum(abs(pred01[k,]/sum(pred01[k,])-(temp.df$w.pop2001-pred01[k,])/sum(temp.df$w.pop2001-pred01[k,])))/2
      di11[[k]]<-sum(abs(pred11[k,]/sum(pred11[k,])-(temp.df$w.pop2011-pred11[k,])/sum(temp.df$w.pop2011-pred11[k,])))/2
    }
    saved.results[[j]]<-lapply(list(di01,di11,di11-di01),quantile,probs=c(0.5,0.025,0.975))
    saved.results[[j]]<-do.call(rbind,saved.results[[j]])
    row.names(saved.results[[j]])<-c('DI2001','DI2011','Diff')
    colnames(saved.results[[j]])<-paste(var.name[j],colnames(saved.results[[j]]))
  }
  
  di.results[[i]]<-cbind(do.call(cbind,saved.results),city=saved.name)
}
di.raw.tab<-do.call(rbind,di.results)
write.csv(di.raw.tab,file='../Results/DI London raw CI.csv')


##  DI using the MCAR model

di.results.mcar<-list(NULL)
for (i in 1:length(ttwa.list)){
  saved.name<-names(ttwa.list)[[i]]
  temp.df<-ttwa.list[[i]]
  var.name<-c('jsa','ib','is')
  
  saved.results<-list(NULL) #A list that will contain 3 objects; the saved results for jsa, ib, is
  
  for (j in 1:length(var.name)){
    
    print(paste(saved.name,j))
    load(file=paste('../Data/Analysis data/Model estimates/London/MCAR',saved.name,var.name[j],'.Rdata',sep='')) #all the list objects are called 'models'
    
    N.mat <- cbind(temp.df$w.pop2001, round(temp.df$w.pop2011))
    N <- as.numeric(t(N.mat)) #N is an alternating vector [i.e. (pop01,pop11,pop01,pop11 etc for eacg zone)]. Ditto for our fitted values
    
    pred<-predict.simple(model.MCAR,N)
    odds<-which(1:ncol(pred)%%2==1)
    pred01<-pred[,odds] #the odd number rows are the results for each zone in 2001
    pred11<-pred[,odds+1]
    
    rm(model.MCAR,pred)
    
    di01<-c(NULL);di11<-c(NULL)
    for (k in 1:nrow(pred01)){
      di01[[k]]<-sum(abs(pred01[k,]/sum(pred01[k,])-(temp.df$w.pop2001-pred01[k,])/sum(temp.df$w.pop2001-pred01[k,])))/2
      di11[[k]]<-sum(abs(pred11[k,]/sum(pred11[k,])-(temp.df$w.pop2011-pred11[k,])/sum(temp.df$w.pop2011-pred11[k,])))/2
    }
    saved.results[[j]]<-lapply(list(di01,di11,di11-di01),quantile,probs=c(0.5,0.025,0.975))
    saved.results[[j]]<-do.call(rbind,saved.results[[j]])
    row.names(saved.results[[j]])<-c('DI2001','DI2011','Diff')
    colnames(saved.results[[j]])<-paste(var.name[j],colnames(saved.results[[j]]))
  }
  
  di.results.mcar[[i]]<-cbind(do.call(cbind,saved.results),city=saved.name)
}
di.raw.mcar.tab<-do.call(rbind,di.results.mcar)
write.csv(di.raw.mcar.tab,file='../Results/DI London raw CI mcar.csv')

##  End
