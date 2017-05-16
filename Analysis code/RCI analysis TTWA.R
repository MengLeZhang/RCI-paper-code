##  This is the analysis for the biggest cities based on travel to work area####                      
##  Start: 18/3/2017                                  #
##  We are excluding london from this for now         #
##  Update: 11/5/2017 
##  rewritten for purposes of style and clarity

##  Pre: Load in all the RCI functions we need----
source.file <- 'RCI functions.R' #path to source
source(source.file)

##  1: Load in the map and variables datasets-----
##  Data load;
load('../Data/Analysis data/England and Wales benefits 0111 final.Rdata')
used.crs <- crs(ew.2001)

##  Read in city centres file.
city.centres <- read.csv('../Data/City centres/UK city centres.csv')
##  For now we will omit London (which we will handle with a diff script)
city.centres <- city.centres[-1,]
mono.centres.coords <- coordinates(city.centres[, c('EastingD', 'NorthingD')])
mono.centres.sp <- SpatialPointsDataFrame(mono.centres.coords, city.centres, 
                                        proj4string = used.crs)

which.ttwa <- ew.2001@data$ttwa[ew.2001@data$la %in% city.centres$la]
##  getting ttwa
valid.ttwa <- unique(which.ttwa)

##  2: The subsetting to only certain ttwa and putting data in lists ----
ttwa.list <- list(NULL)
for (i in 1:length(valid.ttwa)){
  ttwa.list[[i]] <- subset(ew.2001, ttwa==valid.ttwa[i])
}

names(ttwa.list) <- valid.ttwa
rm(ew.2001) # remvoe shp file to save space

##  3: Creating distance variables using functions----
##  Unlike LA we need 3 different distance ordering variables. One is distance 
##  from city centre (we will choose mid point D). Another is closes distance to
##  a centre. Yet another is by some sort of accessibility index
##  Distance by centre. Variable dist.d. Each ttwa has only one match to a city

##  Distance to centre
for (i in 1:length(ttwa.list)){
  temp.mids<-mono.centres.sp[ttwa.list[[i]],]
  centroids<-ttwa.list[[i]]@data[,c("cent.x","cent.y")]
  ttwa.list[[i]]$dist.d<-euclid.dist(point=c(t(temp.mids@data[,9:10])),x=centroids)
}

##  Distance to nearest centre
##  Load inthe dataset with multiple centre and make it into a spatial points df
ttwa.centres <- read.csv('../Data/City centres/ttwa 2011 la centres.csv', 
                       stringsAsFactors = F)
ttwa.centres <- na.omit(ttwa.centres[1:4]) #we do not need the last note col
ttwa.centres$EastingC <- as.numeric(ttwa.centres$EastingC)
ttwa.centres.sp<-SpatialPointsDataFrame(
  coords = coordinates(ttwa.centres[,c('EastingC', 'NorthingC')]), 
  data = data.frame(ttwa.centres), 
  proj4string = used.crs)

##  Define the routine
for (j in 1:length(ttwa.list)) {

  temp.ttwa <- ttwa.list[[j]]
  temp.centres <- ttwa.centres.sp[ttwa.centres.sp$LA %in% ttwa.list[[j]]$la, ] 
  #centre of large LAs even partially in the zone
  
  ## Computing the distance to centres stat
  centroids <- temp.ttwa@data[, c("cent.x", "cent.y")]
  ##  list of vectors denoting distance of a centre
  distances <- list(NULL)
  for(i in 1:nrow(temp.centres@data)){
    distances[[i]] <- euclid.dist(
      point=c(t(temp.centres@data[i, c('EastingC', 'NorthingC')])),
      x=centroids)
  }
  
  distances <- do.call(cbind,distances)
  ttwa.list[[j]]$dist.nearest <- apply(distances, 1, min)
  ttwa.list[[j]]$centre <- as.character(temp.centres@data$LA[apply(distances, 1, which.min)]) #tells us which centre
}

##  Accessibility based on Hansen (1959). Guess between -1 and -2 for the exponent
##  This is a for loop that calulcate the index (with an extra 100m added to distance for terminal time)
ttwa.emp <- list(NULL)
for (k in 1:length(ttwa.list)){

  temp.df <- ttwa.list[[k]]@data
  centroids <- temp.df[, c("cent.x", "cent.y")]

  temp.emp <- list(NULL)
  for (i in 1:nrow(temp.df)){

    temp.dist <- euclid.dist(point=as.numeric(centroids[i, ]), x=centroids)
    temp.out <- list(NULL)
    
    ## 2001 accessibility index. We need the negative of accessibility
    hansen1.2001 <- -sum(temp.df$work.pop2001*(temp.dist+100)^(-1))
    hansen2.2001 <- -sum(temp.df$work.pop2001*(temp.dist+100)^(-2))
    
    ## 2011 accessibility index
    hansen1.2011 <- -sum(temp.df$work.pop2011*(temp.dist+100)^(-1))
    hansen2.2011 <- -sum(temp.df$work.pop2011*(temp.dist+100)^(-2))
    
    temp.emp[[i]] <- cbind(hansen1.2001, hansen1.2011,
                           hansen2.2001, hansen2.2011)
  }
  ttwa.emp[[k]] <- do.call(rbind, temp.emp)
  colnames(ttwa.emp[[k]]) <- c('hansen1.2001', 'hansen1.2011',
                               'hansen2.2001', 'hansen2.2011')
}

for (i in 1:length(ttwa.list)){
  ttwa.list[[i]] <- cbind(ttwa.list[[i]], ttwa.emp[[i]])
}


##  4: RCI routines----

##  4.1: First we will get the point estimates; this is for checking ====
RCI.tables <- list(NULL)
gRCI.tables <- list(NULL)
for (i in 1:length(ttwa.list)){

  temp.df <- ttwa.list[[i]]@data #we only need the data file from here on in 
  ##  We will need to work out the rci for the various cols
  temp.rci <- list(NULL)
  temp.grci <- list(NULL)

  for(j in 1:3){
    id2001 <- which(names(temp.df)%in%c('jsa2001','is2001','ib2001'))[j]
    id2011 <- which(names(temp.df)%in%c('jsa2011','is2011','ib2011'))[j] #this is the cols for the various year data fo each measure
    id.dist01 <- which(names(temp.df)%in%c('dist.d','dist.nearest','hansen1.2001','hansen2.2001'))
    id.dist11 <- which(names(temp.df)%in%c('dist.d','dist.nearest','hansen1.2011','hansen2.2011'))

    g.rci2001 <- apply(temp.df[,id.dist01],2,g.rci,y=temp.df[,id2001],x=temp.df$w.pop2001-temp.df[,id2001])
    g.rci2011 <- apply(temp.df[,id.dist11],2,g.rci,y=temp.df[,id2011],x=round(temp.df$w.pop2011)-temp.df[,id2011])
    temp.tab <- cbind(g.rci2001,g.rci2011)
    out.tab <- aggregate(temp.tab,by=list(temp.df$centre),sum)
    total.ttwa <- c(colSums(out.tab[,-1]))
    total.ttwa <- data.frame(matrix(total.ttwa,ncol=2))
    total.ttwa$diff <- total.ttwa$X2-total.ttwa$X1
    
    temp.rci[[j]] <- total.ttwa
    temp.grci[[j]] <- out.tab
  }

#rci table
  
  temp.res<-data.frame(do.call(cbind,temp.rci))
  temp.res<-round(temp.res,4)
  temp.res$city<-names(ttwa.list)[[i]]
  temp.res$stat<-c('dist.d','dist.nearest','hansen1','hansen2')
  RCI.tables[[i]]<-temp.res

##Tables  
  temp.res2<-data.frame(do.call(rbind,temp.grci))
  temp.diff2<-temp.res2[,6:9]-temp.res2[,2:5]
  temp.res2<-data.frame(temp.res2,temp.diff2)
  temp.res2[,-1]<-round(temp.res2[,-1],4)
  temp.res2$city<-names(ttwa.list)[[i]]
  temp.res2$stat<-c('jsa','is','ib')
  gRCI.tables[[i]]<-temp.res2
}
##  Assemble total RCI tables
RCI.tables <- do.call(rbind, RCI.tables)
colnames(RCI.tables)<-c(rep('jsa', 3), rep('is', 3), rep('ib', 3))
write.csv(RCI.tables, file='../Results/RCI TTWA point estimates.csv')
##  Assemble disaggregated RCI tables
gRCI.tables<-do.call(rbind, gRCI.tables)
write.csv(gRCI.tables,file='../Results/gRCI TTWA point estimates.csv')


##  4.2: Now create the results taking into account uncertainty. CAR version====  
##  The results are already saved; 
rci.raw.ci<-list(NULL)

for (i in 1:length(ttwa.list)){
  saved.name<-names(ttwa.list)[[i]]
  temp.df<-ttwa.list[[i]]
  
  var.name<-c('jsa','ib','is')
  
  saved.results<-list(NULL) #A list that will contain 3 objects; the saved results for jsa, ib, is
  
  for (j in 1:length(var.name)){
    load(file=paste('../Data/Analysis data/Model estimates/TTWA/',
                    saved.name,var.name[j],
                    '.Rdata',sep='')) #all the list objects are called 'models'
    
    pred01 <- predict.simple(models[[1]],temp.df$w.pop2001)
    pred11 <- predict.simple(models[[2]],round(temp.df$w.pop2011))

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

    ##Distance to any centre
    rci.dist.nearest.2001<-list(NULL)
    rci.dist.nearest.2011<-list(NULL)
    for (k in 1:nrow(pred01)){
      rci.dist.nearest.2001[[k]]<-rci(sort.var=temp.df$dist.nearest,x=temp.df$w.pop2001-pred01[k,],y=pred01[k,])
      rci.dist.nearest.2011[[k]]<-rci(sort.var=temp.df$dist.nearest,x=temp.df$w.pop2011-pred11[k,],y=pred11[k,])
    }
    rci.dist.nearest.2001<-unlist(rci.dist.nearest.2001)
    rci.dist.nearest.2011<-unlist(rci.dist.nearest.2011)
    rci.dist.nearest.diff<-rci.dist.nearest.2011-rci.dist.nearest.2001
    result.dist.nearest<-lapply(list(rci.dist.nearest.2001,rci.dist.nearest.2011,rci.dist.nearest.diff),quantile,probs=c(0.5,0.025,0.975))
    
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

    saved.results[[j]]<-rbind(unlist(result.dist.d),unlist(result.dist.nearest),
                              unlist(result.hansen1),unlist(result.hansen2))
    row.names(saved.results[[j]])<-c('dist.d','dist.nearest','hansen.1','hansen.2')
    colnames(saved.results[[j]])<-paste(var.name[j],colnames(saved.results[[j]]))
  }
  rci.raw.ci[[i]]<-do.call(cbind,saved.results)
  rci.raw.ci[[i]]<-cbind(rci.raw.ci[[i]],saved.name)
}
rci.raw.tab<-do.call(rbind,rci.raw.ci)
write.csv(rci.raw.tab,file='../Results/RCI TTWA raw CI.csv')

##  4.2.1: Addendum--doing the CAR for the disaggregated RCI results====

rci.raw.ci <- list(NULL)
for (i in 1:length(ttwa.list)){
  
  ##Load in the model data
  saved.name <-names(ttwa.list)[[i]]
  temp.df <- ttwa.list[[i]]
  
  var.name <- c('jsa', 'ib', 'is')
  
  saved.results <- list(NULL) #A list that will contain 3 objects; 
  #the saved results for jsa, ib, is
  
  ##  Now to seperately get the results for each type of claimant
  for (j in 1:length(var.name)){
    load(file=paste('../Data/Analysis data/Model estimates/TTWA/',
                    saved.name,var.name[j],
                    '.Rdata',sep='')) #all the list objects are called 'models'
    
    pred01 <- predict.simple(models[[1]],temp.df$w.pop2001)
    pred11 <- predict.simple(models[[2]],round(temp.df$w.pop2011))
    
    rm(models)
    
    ##Distance to any centre: tabled by nearest centre
    rci.results.tab <- list(NULL)

    for (k in 1:nrow(pred01)){
      ##  g.rci will output a vector showing the rci contribution of each lsoa
      rci.lsoa2001<-g.rci(sort.var=temp.df$dist.nearest,
                                        x=temp.df$w.pop2001-pred01[k,],
                                        y=pred01[k,])

      rci.lsoa2011<-g.rci(sort.var=temp.df$dist.nearest,
                                        x=temp.df$w.pop2011-pred11[k,],
                                        y=pred11[k,])
      ## Now do a table showing rci per centre
      bind.rci<-cbind(rci2001=rci.lsoa2001, rci2011=rci.lsoa2011)
      rci.results.tab[[k]] <- aggregate(bind.rci, by=list(temp.df$centre), sum)
    
    }
    rci.results.tab <- do.call(rbind, rci.results.tab)
    rci.results.tab$diff <- rci.results.tab$rci2011 - rci.results.tab$rci2001
    saved.results[[j]] <- aggregate(rci.results.tab[, -1], 
                                    by = list(rci.results.tab$Group.1),
                                    quantile, probs=c(0.5,0.025,0.975))
    
    colnames(saved.results[[j]])<-paste(var.name[j],colnames(saved.results[[j]]))
    
  }
  rci.raw.ci[[i]]<-do.call(cbind,saved.results)
  rci.raw.ci[[i]]<-cbind(rci.raw.ci[[i]],saved.name)
}

rci.raw.tab<-do.call(rbind,rci.raw.ci)
write.csv(rci.raw.tab,file='../Results/RCI TTWA raw CI disagg.csv')

##  4.3:The MCAR version of the same====

rci.mcar.ci<-list(NULL)

for (i in 1:length(ttwa.list)){
  saved.name<-names(ttwa.list)[[i]]
  temp.df<-ttwa.list[[i]]
  
  var.name<-c('jsa','ib','is')
  
  saved.results<-list(NULL) #A list that will contain 3 objects; the saved results for jsa, ib, is
  
  for (j in 1:length(var.name)){
    print(paste(saved.name,j))
    load(file=paste('../Data/Analysis data/Model estimates/TTWA/MCAR',saved.name,var.name[j],'.Rdata',sep='')) #all the list objects are called 'models'
    
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
write.csv(rci.mcar.ci,file='../Results/RCI TTWA raw CI mcar.csv')

##  4.3.1: MCAR version of the disaggregated results =====
rci.mcar.ci<-list(NULL)

for (i in 1:length(ttwa.list)){
  saved.name<-names(ttwa.list)[[i]]
  temp.df<-ttwa.list[[i]]
  
  var.name<-c('jsa','ib','is')
  
  saved.results<-list(NULL) #A list that will contain 3 objects; the saved results for jsa, ib, is
  
  for (j in 1:length(var.name)){
    print(paste(saved.name,j))
    load(file=paste('../Data/Analysis data/Model estimates/TTWA/MCAR',saved.name,var.name[j],'.Rdata',sep='')) #all the list objects are called 'models'
    
    N.mat <- cbind(temp.df$w.pop2001, round(temp.df$w.pop2011))
    N <- as.numeric(t(N.mat)) #N is an alternating vector [i.e. (pop01,pop11,pop01,pop11 etc for eacg zone)]. Ditto for our fitted values
    
    pred<-predict.simple(model.MCAR,N)
    odds<-which(1:ncol(pred)%%2==1)
    pred01<-pred[,odds] #the odd number rows are the results for each zone in 2001
    pred11<-pred[,odds+1]
    
    rm(model.MCAR,pred)
    
    ##Distance to any centre: tabled by nearest centre
    rci.results.tab <- list(NULL)
    
    for (k in 1:nrow(pred01)){
      ##  g.rci will output a vector showing the rci contribution of each lsoa
      rci.lsoa2001<-g.rci(sort.var=temp.df$dist.nearest,
                          x=temp.df$w.pop2001-pred01[k,],
                          y=pred01[k,])
      
      rci.lsoa2011<-g.rci(sort.var=temp.df$dist.nearest,
                          x=temp.df$w.pop2011-pred11[k,],
                          y=pred11[k,])
      ## Now do a table showing rci per centre
      bind.rci<-cbind(rci2001=rci.lsoa2001, rci2011=rci.lsoa2011)
      rci.results.tab[[k]] <- aggregate(bind.rci, by=list(temp.df$centre), sum)
      
    }
    rci.results.tab <- do.call(rbind, rci.results.tab)
    rci.results.tab$diff <- rci.results.tab$rci2011 - rci.results.tab$rci2001
    saved.results[[j]] <- aggregate(rci.results.tab[, -1], 
                                    by = list(rci.results.tab$Group.1),
                                    quantile, probs=c(0.5,0.025,0.975))
    
    colnames(saved.results[[j]])<-paste(var.name[j],colnames(saved.results[[j]]))
    
  }
  
  rci.mcar.ci[[i]]<-do.call(cbind,saved.results)
  rci.mcar.ci[[i]]<-cbind(rci.mcar.ci[[i]],saved.name)
}

rci.mcar.ci<-do.call(rbind,rci.mcar.ci)
write.csv(rci.mcar.ci,file='../Results/RCI TTWA raw CI mcar disagg.csv')

##  Dissimilarity index----

di.results<-list(NULL)
for (i in 1:length(ttwa.list)){
  saved.name<-names(ttwa.list)[[i]]
  temp.df<-ttwa.list[[i]]
  var.name<-c('jsa','ib','is')
  
  saved.results<-list(NULL) #A list that will contain 3 objects; the saved results for jsa, ib, is
  
  for (j in 1:length(var.name)){
    load(file=paste('../Data/Analysis data/Model estimates/TTWA/',saved.name,var.name[j],'.Rdata',sep='')) #all the list objects are called 'models'
    
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
write.csv(di.raw.tab,file='../Results/DI TTWA raw CI.csv')

## MCAR version

di.results.mcar<-list(NULL)
for (i in 1:length(ttwa.list)){
  saved.name<-names(ttwa.list)[[i]]
  temp.df<-ttwa.list[[i]]
  var.name<-c('jsa','ib','is')
  
  saved.results<-list(NULL) #A list that will contain 3 objects; the saved results for jsa, ib, is
  
  for (j in 1:length(var.name)){
    
    print(paste(saved.name,j))
    load(file=paste('../Data/Analysis data/Model estimates/TTWA/MCAR',saved.name,var.name[j],'.Rdata',sep='')) #all the list objects are called 'models'
    
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
write.csv(di.raw.mcar.tab,file='../Results/DI TTWA raw CI mcar.csv')

##  End
rm(list = ls())
