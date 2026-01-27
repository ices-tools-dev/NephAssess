nephup.dh <-
function(wdir, lfile, msfile,lclass, lwparams)
{

  ###################
  ##  USER INPUTS  ##
  ###################

  ## FILE LOCATIONS

  #"Enter working directory, e.g. 'C:/Work/NEPH-UP/CL/'"
  working.directory<-wdir
  #working.directory<-"C:/Work/NEPH-UP/CL/"

  #"Enter quarterly effort/landings data file, e.g. 'Frscl.txt'"
  landings.file<-lfile
  #landings.file<-"Frscl04b.txt"

  #"Enter market sampling index file name, e.g. 'Msclind.txt'"
  ms.index<-msfile
  #ms.index<-"Msclind.txt"

  #"Enter length class width (mm) for use on market sample data"

  ## LENGTH-WEIGHT PARAMETERS

  ## (this can be changed to give a "terminal" style interface to the function)

  #fill.in.yr1 <- 1990

  #fill.in.yr2 <- 1993


  ################
  ##  LANDINGS  ##
  ################

  paste(strsplit(readLines(paste(working.directory, landings.file, sep=""))[1], "\t")[[1]], sep=" ")->area.name
  stock<-paste(area.name[1])
  area.name<-area.name[area.name!=""]


  for(i in (2:length(area.name))){
  stock<-paste(stock, area.name[i], sep="_")
  }

  ## creates a text string of the area name


  quarterly.landings.data<-read.table(paste(working.directory, landings.file, sep=""), skip=3, header=F)
  colnames(quarterly.landings.data)<-c("Year", "Quarter", "OTB_CRU", "OTT_CRU", "OTHER", "FPO")

  landings.start.year <- as.numeric(strsplit(readLines(paste(working.directory, landings.file, sep=""))[2], split="\t")[[1]][1])
  landings.end.year   <- as.numeric(strsplit(readLines(paste(working.directory, landings.file, sep=""))[2], split="\t")[[1]][2])

  neph.landings <- FLQuant(dimnames=list(landings=c("OTB_CRU", "OTT_CRU", "OTHER", "FPO"), year = landings.start.year:landings.end.year  ,
  season = 1:4, area =stock))

  # Reads specified file in, skipping first 3 lines, and names columns

  start.rows <- round(seq(1,dim(quarterly.landings.data)[1], by=4), digits=1)

  for(i in (1:length(start.rows))){

      neph.landings[,,,1,]["OTB_CRU",i] <- quarterly.landings.data[start.rows[i],"OTB_CRU"]
      neph.landings[,,,2,]["OTB_CRU",i] <- quarterly.landings.data[start.rows[i]+1,"OTB_CRU"]
      neph.landings[,,,3,]["OTB_CRU",i] <- quarterly.landings.data[start.rows[i]+2,"OTB_CRU"]
      neph.landings[,,,4,]["OTB_CRU",i] <- quarterly.landings.data[start.rows[i]+3,"OTB_CRU"]

	  neph.landings[,,,1,]["OTT_CRU",i] <- quarterly.landings.data[start.rows[i],"OTT_CRU"]
      neph.landings[,,,2,]["OTT_CRU",i] <- quarterly.landings.data[start.rows[i]+1,"OTT_CRU"]
      neph.landings[,,,3,]["OTT_CRU",i] <- quarterly.landings.data[start.rows[i]+2,"OTT_CRU"]
      neph.landings[,,,4,]["OTT_CRU",i] <- quarterly.landings.data[start.rows[i]+3,"OTT_CRU"]

	  neph.landings[,,,1,]["OTHER",i] <- quarterly.landings.data[start.rows[i],"OTHER"]
      neph.landings[,,,2,]["OTHER",i] <- quarterly.landings.data[start.rows[i]+1,"OTHER"]
      neph.landings[,,,3,]["OTHER",i] <- quarterly.landings.data[start.rows[i]+2,"OTHER"]
      neph.landings[,,,4,]["OTHER",i] <- quarterly.landings.data[start.rows[i]+3,"OTHER"]

      neph.landings[,,,1,]["FPO",i] <- quarterly.landings.data[start.rows[i],"FPO"]
      neph.landings[,,,2,]["FPO",i] <- quarterly.landings.data[start.rows[i]+1,"FPO"]
      neph.landings[,,,3,]["FPO",i] <- quarterly.landings.data[start.rows[i]+2,"FPO"]
      neph.landings[,,,4,]["FPO",i] <- quarterly.landings.data[start.rows[i]+3,"FPO"]

  }

  neph.landings@units<-"Tonnes"

  annual.landings<- neph.landings[,,,1,]+neph.landings[,,,2,]+neph.landings[,,,3,]+neph.landings[,,,4,]


  ################
  ##  DISCARDS  ##
  ################



  #######################
  ##  MARKET SAMPLING  ##
  #######################

  sampling.files<-readLines(paste(working.directory, ms.index, sep=""))

  ## reads in the market sampling index file


  if(is.na(as.numeric(strsplit(sampling.files[2], split=",")[[1]][1]))==TRUE){
  market.sampling.start.year <- as.numeric(strsplit(sampling.files[2], split=" ")[[1]][1])
  market.sampling.end.year   <- as.numeric(strsplit(sampling.files[2], split=" ")[[1]][2])
    }
  if(is.na(as.numeric(strsplit(sampling.files[2], split=",")[[1]][1]))==FALSE){
  market.sampling.start.year <- as.numeric(strsplit(sampling.files[2], split=",")[[1]][1])
  market.sampling.end.year   <- as.numeric(strsplit(sampling.files[2], split=",")[[1]][2])
    }

  number.of.files <-  market.sampling.end.year-market.sampling.start.year+1

  ## extracts the dates of first and last files and works out the number of files to expect

  ms.list<-vector("list", length=number.of.files)
  ms.weights<-vector("list", length=number.of.files)
  total.male.weight<-vector(length=number.of.files)
  total.female.weight<-vector(length=number.of.files)

  ## sets up lists and vectors for weights

  male.a   <- as.numeric(strsplit(sampling.files[3], split=",")[[1]][1])
  male.b   <- as.numeric(strsplit(sampling.files[3], split=",")[[1]][2])
  female.a <- as.numeric(strsplit(sampling.files[4], split=",")[[1]][1])
  female.b <- as.numeric(strsplit(sampling.files[4], split=",")[[1]][2])


  neph.discard.wt <- FLQuant(dimnames=list(lengths=seq(9,71, by=2), year = landings.start.year:landings.end.year,
                    unit = c("Male", "Female"), season = 1:4, area =stock))

  lcats <- round(seq(9,71, by=2), digits=0)
  lens  <- c(9:71)

  for (q in (1:4)){
      for (y in (1:(landings.end.year-landings.start.year+1))){
          for (l in (1:32)){
                  neph.discard.wt[l,y,1,q,1] <- round((male.a*((lcats[l]+1)^male.b))/1000, digits=3)
                  neph.discard.wt[l,y,2,q,1] <- round((female.a*((lcats[l]+1)^female.b))/1000, digits=3)
                  }
          }
      }


  ## inputs the length/weight relationships


  neph.landings.n.temp  <- FLQuant(dimnames=list(lengths=round(seq(9,71, by=1), digits=0),
                      year = landings.start.year:landings.end.year,
                      unit = c("Male", "Female"), season = 1:4, area =stock))

  neph.landings.n  <- FLQuant(dimnames=list(lengths=round(seq(9,71, by=2), digits=0),
                      year = landings.start.year:landings.end.year,
                      unit = c("Male", "Female"), season = 1:4, area =stock))


  neph.landings.n@units<-"Thousands"


  ## in old format files, range of sizes goes from 15-71	(up to 2008)
  old.format.files<- seq(1, 2008-market.sampling.start.year+1, by=1)

  ## in new format files, range of sizes goes from 9-71	(2009-2010)
  new.format.files<- seq(2009-market.sampling.start.year+1, 2010-market.sampling.start.year+1, by=1)

  ## in intercatch format files, range of sizes varies		(from 2011)
  intercatch.format.files<- seq(2011-market.sampling.start.year+1, market.sampling.end.year-market.sampling.start.year+1, by=1)



  ##loop to read market sampling files up to 2008
  for(i in (old.format.files)){

  temp      <- read.csv(paste(working.directory, sampling.files[4+i], sep=""), skip=6, comment.char="", header=T, nrows=57)
  min.length<- 15
  max.length<- 71

  sampling.year <- (as.numeric(strsplit(strsplit(readLines(paste(working.directory, sampling.files[4+i], sep=""))[[2]], split=" ")[[1]][4], split="")[[1]][1])*1000)+
  (as.numeric(strsplit(strsplit(readLines(paste(working.directory, sampling.files[4+i], sep=""))[[2]], split=" ")[[1]][4], split="")[[1]][2])*100)+
  (as.numeric(strsplit(strsplit(readLines(paste(working.directory, sampling.files[4+i], sep=""))[[2]], split=" ")[[1]][4], split="")[[1]][3])*10)+
  (as.numeric(strsplit(strsplit(readLines(paste(working.directory, sampling.files[4+i], sep=""))[[2]], split=" ")[[1]][4], split="")[[1]][4]))

  ## reads in each file - this relies on the format used by Ian, without the blank column in "A"

  sampled.landings<-cbind(temp[,3], temp[,4]+temp[,5], temp[,7], temp[,8]+temp[,9], temp[,11], temp[,12]+temp[,13], temp[,15], temp[,16]+temp[,17])

  sampled.landings<-rbind(c(0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0),
  c(0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0), sampled.landings)

  ## sets landings to zero for 9-14 mm prawns - this is possibly not appropriate now that FMD gives landings at
  ## these lengths, check with Nick.

  ## assigns row and column headings to the sampling matrix

  neph.landings.n.temp[,(sampling.year-landings.start.year+1),1,1,] <- sampled.landings[,1]/10
  neph.landings.n.temp[,(sampling.year-landings.start.year+1),2,1,] <- sampled.landings[,2]/10

  neph.landings.n.temp[,(sampling.year-landings.start.year+1),1,2,] <- sampled.landings[,3]/10
  neph.landings.n.temp[,(sampling.year-landings.start.year+1),2,2,] <- sampled.landings[,4]/10

  neph.landings.n.temp[,(sampling.year-landings.start.year+1),1,3,] <- sampled.landings[,5]/10
  neph.landings.n.temp[,(sampling.year-landings.start.year+1),2,3,] <- sampled.landings[,6]/10

  neph.landings.n.temp[,(sampling.year-landings.start.year+1),1,4,] <- sampled.landings[,7]/10
  neph.landings.n.temp[,(sampling.year-landings.start.year+1),2,4,] <- sampled.landings[,8]/10

  }


  ##loop to read market sampling files between 2009-2010
  for(i in (new.format.files)){

  temp      <- read.csv(paste(working.directory, sampling.files[4+i], sep=""), skip=6, comment.char="", header=T, nrows=63)
  min.length<- 9
  max.length<- 71

  sampling.year <- (as.numeric(strsplit(strsplit(readLines(paste(working.directory, sampling.files[4+i], sep=""))[[2]], split=" ")[[1]][4], split="")[[1]][1])*1000)+
  (as.numeric(strsplit(strsplit(readLines(paste(working.directory, sampling.files[4+i], sep=""))[[2]], split=" ")[[1]][4], split="")[[1]][2])*100)+
  (as.numeric(strsplit(strsplit(readLines(paste(working.directory, sampling.files[4+i], sep=""))[[2]], split=" ")[[1]][4], split="")[[1]][3])*10)+
  (as.numeric(strsplit(strsplit(readLines(paste(working.directory, sampling.files[4+i], sep=""))[[2]], split=" ")[[1]][4], split="")[[1]][4]))

  ## reads in each file - this relies on the format used by Ian, without the blank column in "A"

  sampled.landings<-cbind(temp[,3], temp[,4]+temp[,5], temp[,7], temp[,8]+temp[,9], temp[,11], temp[,12]+temp[,13], temp[,15], temp[,16]+temp[,17])

  ## assigns row and column headings to the sampling matrix

  neph.landings.n.temp[,(sampling.year-landings.start.year+1),1,1,] <- sampled.landings[,1]/10
  neph.landings.n.temp[,(sampling.year-landings.start.year+1),2,1,] <- sampled.landings[,2]/10

  neph.landings.n.temp[,(sampling.year-landings.start.year+1),1,2,] <- sampled.landings[,3]/10
  neph.landings.n.temp[,(sampling.year-landings.start.year+1),2,2,] <- sampled.landings[,4]/10

  neph.landings.n.temp[,(sampling.year-landings.start.year+1),1,3,] <- sampled.landings[,5]/10
  neph.landings.n.temp[,(sampling.year-landings.start.year+1),2,3,] <- sampled.landings[,6]/10

  neph.landings.n.temp[,(sampling.year-landings.start.year+1),1,4,] <- sampled.landings[,7]/10
  neph.landings.n.temp[,(sampling.year-landings.start.year+1),2,4,] <- sampled.landings[,8]/10

  }


  ##loop to read market sampling files from 2011 - intercatch format (CM 18/4/2012)
  for(i in (intercatch.format.files)){

  temp      <- read.table(paste(working.directory, sampling.files[4+i], sep=""), skip=1, sep="\t", comment.char="", header=T)

  sampling.year <-  seq(market.sampling.start.year, market.sampling.end.year, by=1)[i]

  sampled.landings<-cbind(temp[,2], temp[,3], temp[,4], temp[,6], temp[,7], temp[,9], temp[,10], temp[,12], temp[,13])

  ## assigns row and column headings to the sampling matrix

  for(j in 1:dim(sampled.landings)[1])
   {
  	  neph.landings.n.temp[as.character(sampled.landings[j,1]),(sampling.year-landings.start.year+1),1,1,] <- round(sampled.landings[j,2]/1000, 3)
	  neph.landings.n.temp[as.character(sampled.landings[j,1]),(sampling.year-landings.start.year+1),2,1,] <- round(sampled.landings[j,3]/1000, 3)

	  neph.landings.n.temp[as.character(sampled.landings[j,1]),(sampling.year-landings.start.year+1),1,2,] <- round(sampled.landings[j,4]/1000, 3)
	  neph.landings.n.temp[as.character(sampled.landings[j,1]),(sampling.year-landings.start.year+1),2,2,] <- round(sampled.landings[j,5]/1000, 3)

	  neph.landings.n.temp[as.character(sampled.landings[j,1]),(sampling.year-landings.start.year+1),1,3,] <- round(sampled.landings[j,6]/1000, 3)
	  neph.landings.n.temp[as.character(sampled.landings[j,1]),(sampling.year-landings.start.year+1),2,3,] <- round(sampled.landings[j,7]/1000, 3)

	  neph.landings.n.temp[as.character(sampled.landings[j,1]),(sampling.year-landings.start.year+1),1,4,] <- round(sampled.landings[j,8]/1000, 3)
	  neph.landings.n.temp[as.character(sampled.landings[j,1]),(sampling.year-landings.start.year+1),2,4,] <- round(sampled.landings[j,9]/1000, 3)
	}

  }

  neph.landings.n.temp[is.na(neph.landings.n.temp)]<- 0


  odd.rows <-round(seq(1,62,by=2), digits=0)
  even.rows<-round(seq(2,62,by=2),digits=0)

  for(i in (1:31)){

  neph.landings.n[i,,,,] <- neph.landings.n.temp[odd.rows[i]] + neph.landings.n.temp[even.rows[i]]

  }

  neph.landings.n[32,,,,] <- neph.landings.n.temp[63,,,,]


  neph.landings.n[is.na(neph.landings.n)]<- 0


  #12/04/2020 fix to make this function work with latest R and FLCore versions
  # return.stock <- FLStock( name=stock, landings.n=neph.landings.n,
  #                 stock.wt = neph.discard.wt,
  #                 landings.wt=neph.discard.wt)
  # return.stock@landings <- neph.landings
  #end fix

  #27/01/2026 fix to work with latest FLCore version (AA)
  dn <- dimnames(neph.landings.n)
  empty_FLQ <- function(x) {
    FLQuant(NA, dimnames = dimnames(x))
  }

  return.stock <- FLStock(
    name        = stock,
    stock.n     = empty_FLQ(neph.landings.n),
    stock.wt    = neph.discard.wt,
    landings.n  = neph.landings.n,
    landings.wt = neph.discard.wt,
    discards.n  = empty_FLQ(neph.landings.n),
    discards.wt = empty_FLQ(neph.discard.wt),
    catch.n     = empty_FLQ(neph.landings.n),
    catch.wt    = empty_FLQ(neph.discard.wt),
    m           = empty_FLQ(neph.landings.n),
    mat         = empty_FLQ(neph.landings.n),
    harvest     = empty_FLQ(neph.landings.n),
    harvest.spwn= empty_FLQ(neph.landings.n),
    m.spwn      = empty_FLQ(neph.landings.n)
  )
  return.stock@landings <- neph.landings
  #end fix

  return.stock@stock.wt@units    <- "Kg"
  return.stock@stock.wt@units    <- "Kg"



  return(return.stock)
}

