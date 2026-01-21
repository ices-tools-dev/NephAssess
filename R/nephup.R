###############################################################
# Read Nephrops fisheries into a FLR stock object
# Created by: Carlos Mesquita
# Date: 30/11/2015
# Created: 31/04/2016
# Modified: 21/01/2026
# Packges used: 
# R (4.3.2)
##This new "nephup" function replaces previous function created by
#Neil. This function reads the stock object from the previous year
#and adds new data from txt files (same format as before)
# Modified: 21/04/2017 #To include BMS data if available
# Modified: 21/01/2026 #amendment on the function to work with the most recent FLRCore package version
###############################################################

nephup <-
function(wdir, stock.object, lfile, bmsfile=NULL, filenames)
{
  ###################
  ##  Load data    ##
  ###################
  
  #Load previous years stock object
  env <- new.env()
  load(paste0(wdir,stock.object), envir=env, verbose = T)
  obj.name<- ls(env=env)[grep("nephup.", ls(env=env))]
  stock.obj<- env[[obj.name]]
  
  
  ################
  ##  LANDINGS  ##
  ################
  
  gear.names<- dimnames(stock.obj@landings)$landings
  season<- dimnames(stock.obj@landings)$season
  quarterly.landings.data<-read.table(paste(wdir, lfile, sep=""), skip=3, header=F)
  colnames(quarterly.landings.data)<-c("Year", "Quarter", gear.names)
  quarterly.landings.data$Quarter<- gsub("[[:alpha:]]", "", quarterly.landings.data$Quarter)
  landings.start.year <- as.numeric(strsplit(readLines(paste(wdir, lfile, sep=""))[2], split="\t")[[1]][1])
  landings.end.year   <- as.numeric(strsplit(readLines(paste(wdir, lfile, sep=""))[2], split="\t")[[1]][2])
  
  #Fix to allow "expand" function to work on stock object. This is because in the new FLCore library a "validate" function was introduced which fails due to the "landings" slot not being considerate valid. CM 21/01/2020
  temp.obj.landings<- stock.obj@landings
  stock.obj@landings<- computeLandings(stock.obj)
  #temp.obj.landings<- expand(temp.obj.landings, year=as.numeric(stock.obj@range[names(stock.obj@range) == "minyear"]):landings.end.year)
  temp.obj.landings<- window(temp.obj.landings, start=as.numeric(stock.obj@range[names(stock.obj@range) == "minyear"]), end=landings.end.year) #21/06/2026 expand replaced by window
  

  #Expand existing object to the new landings year 
  #stock.obj<- suppressWarnings(expand(stock.obj, year=as.numeric(stock.obj@range[names(stock.obj@range) == "minyear"]):landings.end.year))
  stock.obj<- window(stock.obj, end=landings.end.year) #21/06/2026 expand replaced by window
  stock.obj@landings<- temp.obj.landings
  
  #Expand the 2 attributes to the stock object related with BMS
  #attr(stock.obj,"bms")<- suppressWarnings(expand(attr(stock.obj,"bms"), year=as.numeric(stock.obj@range[names(stock.obj@range) == "minyear"]):landings.end.year)); attr(stock.obj,"bms")[,as.character(landings.end.year),,,,]<- 0
  #attr(stock.obj,"bms.n")<- suppressWarnings(expand(attr(stock.obj,"bms.n"), year=as.numeric(stock.obj@range[names(stock.obj@range) == "minyear"]):landings.end.year)); 
  attr(stock.obj,"bms")<- window(attr(stock.obj,"bms"), start=as.numeric(stock.obj@range[names(stock.obj@range) == "minyear"]), end=landings.end.year); attr(stock.obj,"bms")[,as.character(landings.end.year),,,,]<- 0 #21/06/2026 expand replaced by window
  attr(stock.obj,"bms.n")<- window(attr(stock.obj,"bms.n"), start=as.numeric(stock.obj@range[names(stock.obj@range) == "minyear"]), end=landings.end.year) #21/06/2026 expand replaced by window
  
  
  for(y in as.character(seq(landings.start.year,landings.end.year)))
  {
    for(g in gear.names)
    {
      for(s in season)
      {
        stock.obj@landings[g,y,,s,,]<- quarterly.landings.data[quarterly.landings.data$Year %in% y & quarterly.landings.data$Quarter %in% s,g]
        
      }
    }
  }
  
  
  ####################
  ##  BMS reported  ##
  ####################
  
  if(!is.null(bmsfile))
  {
    quarterly.bms.data<-read.table(paste(wdir, bmsfile, sep=""), skip=3, header=F)
    colnames(quarterly.bms.data)<-c("Year", "Quarter", gear.names)
    quarterly.bms.data$Quarter<- gsub("[[:alpha:]]", "", quarterly.bms.data$Quarter)
    bms.start.year <- min(quarterly.bms.data$Year)
    bms.end.year   <- max(quarterly.bms.data$Year)
    
    for(y in as.character(seq(bms.start.year,bms.end.year)))
    {
      for(g in gear.names)
      {
        for(s in season)
        {
          attr(stock.obj,"bms")[g,y,,s,,]<- quarterly.bms.data[quarterly.bms.data$Year %in% y & quarterly.bms.data$Quarter %in% s,g]
          
        }
      }
    }
  }
  
  
  
  ######################################
  ##  MARKET SAMPLING - Landings LFs  ##
  ######################################
  
  ## reads in the market sampling index file
  landings.files <- read.table(paste0(wdir,filenames), sep="\t",head=T,stringsAsFactors=F)
  landings.files <- landings.files[landings.files$category=="landings",]
  if(any(!is.na(landings.files[,"file"])))
  {
    season<- dimnames(stock.obj@landings.n)$season
    len.class<- dimnames(stock.obj@landings.n)$lengths
    sex.vector<- dimnames(stock.obj@landings.n)$unit
    
    for(y in as.character(landings.files[!is.na(landings.files$file),"year"]))
    {
      temp <- read.table(paste0(wdir,landings.files[landings.files$year==y,"file"]), skip=4, sep="\t", comment.char="", header=T)
      temp<-cbind(temp[,2], temp[,3], temp[,4], temp[,6], temp[,7], temp[,9], temp[,10], temp[,12], temp[,13])
      temp2<- matrix(nrow=length(seq(9,71,by=1)), ncol=ncol(temp))
      temp2[,1]<- seq(9,71,by=1)
      temp2[temp2[,1] %in% temp[,1],2:9]<- temp[,2:9]
      temp2[is.na(temp2)]<- 0
      temp2<- as.data.frame(temp2)
      temp2$vec<- sort(rep(seq(9,71,by=2),2))[-64]
      temp2<- aggregate(list(temp2[,2:9]), list(temp2[,c("vec")]), sum)
      grid<- expand.grid(sex.vector,season)
      colnames(temp2)<- c("L", paste(grid$Var1, grid$Var2, sep="_"))
      
      for(l in len.class)
      {
        for(s in season)
        {
          for(x in sex.vector)
          {
            stock.obj@landings.n[l,y,x,s,,]<- round(temp2[temp2$L==l,paste(x,s,sep="_")]/1000,3)
          }
        }
      }
    }
  }
  
  
  ###################
  ##  DISCARDS LFs ##
  ###################
  
  discard.files <- read.table(paste0(wdir,filenames), sep="\t",head=T,stringsAsFactors=F)
  discard.files <- discard.files[discard.files$category=="discards",]
  if(any(!is.na(discard.files[,"file"])))
  {
    season<- dimnames(stock.obj@discards.n)$season
    len.class<- dimnames(stock.obj@discards.n)$lengths
    sex.vector<- dimnames(stock.obj@landings.n)$unit
    
    for(y in as.character(discard.files[!is.na(discard.files$file),"year"]))
    {
      temp <- read.table(paste0(wdir,discard.files[discard.files$year==y,"file"]), skip=4, sep="\t", comment.char="", header=T)
      temp<-cbind(temp[,2], temp[,3], temp[,4], temp[,6], temp[,7], temp[,9], temp[,10], temp[,12], temp[,13])
      temp2<- matrix(nrow=length(seq(9,71,by=1)), ncol=ncol(temp))
      temp2[,1]<- seq(9,71,by=1)
      temp2[temp2[,1] %in% temp[,1],2:9]<- temp[,2:9]
      temp2[is.na(temp2)]<- 0
      temp2<- as.data.frame(temp2)
      temp2$vec<- sort(rep(seq(9,71,by=2),2))[-64]
      temp2<- aggregate(list(temp2[,2:9]), list(temp2[,c("vec")]), sum)
      grid<- expand.grid(sex.vector,season)
      colnames(temp2)<- c("L", paste(grid$Var1, grid$Var2, sep="_"))
      
      for(l in len.class)
      {
        for(s in season)
        {
          for(x in sex.vector)
          {
            stock.obj@discards.n[l,y,x,s,,]<- round(temp2[temp2$L==l,paste(x,s,sep="_")]/1000,3)
          }
        }
      }
    }
  }
  
  
  ################
  ##  BMS LFs   ##
  ################
  
  bms.files <- read.table(paste0(wdir,filenames), sep="\t",head=T,stringsAsFactors=F)
  bms.files <- bms.files[bms.files$category=="bms",]
  if(any(!is.na(bms.files[,"file"])))
  {
    season<- dimnames(stock.obj@bms.n)$season
    len.class<- dimnames(stock.obj@bms.n)$lengths
    sex.vector<- dimnames(stock.obj@landings.n)$unit
    
    for(y in as.character(bms.files$year))
    {
      temp <- read.table(paste0(wdir,bms.files[bms.files$year==y,"file"]), skip=4, sep="\t", comment.char="", header=T)
      temp<-cbind(temp[,2], temp[,3], temp[,4], temp[,6], temp[,7], temp[,9], temp[,10], temp[,12], temp[,13])
      temp2<- matrix(nrow=length(seq(9,71,by=1)), ncol=ncol(temp))
      temp2[,1]<- seq(9,71,by=1)
      temp2[temp2[,1] %in% temp[,1],2:9]<- temp[,2:9]
      temp2[is.na(temp2)]<- 0
      temp2<- as.data.frame(temp2)
      temp2$vec<- sort(rep(seq(9,71,by=2),2))[-64]
      temp2<- aggregate(list(temp2[,2:9]), list(temp2[,c("vec")]), sum)
      grid<- expand.grid(sex.vector,season)
      colnames(temp2)<- c("L", paste(grid$Var1, grid$Var2, sep="_"))
      
      for(l in len.class)
      {
        for(s in season)
        {
          for(x in sex.vector)
          {
            attr(stock.obj,"bms.n")[l,y,x,s,,]<- round(temp2[temp2$L==l,paste(x,s,sep="_")]/1000,3)
          }
        }
      }
    }
  }
  
  
  #############################
  ##  Complete stock object  ##
  #############################
   
  #For the new year(s), sum landings and discards to obtain catch.n & get mean wts at length from previous year
  for(y in as.character(seq(landings.start.year,landings.end.year)))
  {
    bms.n<- stock.obj@bms.n[,y,,,,]; bms.n[is.na(bms.n)]<- 0
    stock.obj@catch.n[,y,,,,]<- stock.obj@landings.n[,y,,,,] + stock.obj@discards.n[,y,,,,] + bms.n
    stock.obj@stock.wt[,y,,,,]<- stock.obj@stock.wt[,as.character(as.numeric(y)-1),,,,]
    stock.obj@catch.wt[,y,,,,]<- stock.obj@catch.wt[,as.character(as.numeric(y)-1),,,,]
    stock.obj@landings.wt[,y,,,,]<- stock.obj@landings.wt[,as.character(as.numeric(y)-1),,,,]
    stock.obj@discards.wt[,y,,,,]<- stock.obj@discards.wt[,as.character(as.numeric(y)-1),,,,]
    stock.obj@stock[,y,,,,]<- stock.obj@stock[,as.character(as.numeric(y)-1),,,,]
  }
  
  #Catch and discards biomass (numbers*length)
  stock.obj@catch <- computeCatch(stock.obj)
  stock.obj@discards <- computeDiscards(stock.obj)
  
  #Return new object
  return(stock.obj)  
}
