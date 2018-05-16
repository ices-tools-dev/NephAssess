plots.advice<- 
function(wk.dir, f.u, MSY.hr, stock.object, international.landings, tv_results, Exploitation_summary)
{
  f.u <- check.fu (f.u)
  setwd(wk.dir)

  hist.land <- read.csv(international.landings, header=T) 
  first.disc.yr <-switch(f.u,1990,"fladen"=2000,"jura"=9999)
  first.catch.yr <- min(read.csv(tv_results, header=T)$year)
  last.catch.yr <-stock.object@range["maxyear"]
  hist.land <-subset(hist.land,Year>=first.catch.yr)
  stock.object <-trim(stock.object,year=first.catch.yr:last.catch.yr); attr(stock.object,"bms.n")<- trim(attr(stock.object,"bms.n"),year=first.catch.yr:last.catch.yr)
  tot.wt <-seasonSums(quantSums(unitSums(stock.object@landings.n*stock.object@landings.wt))) + seasonSums(quantSums(unitSums(stock.object@bms.n*stock.object@discards.wt)))
  discards.wt<- seasonSums(quantSums(unitSums(stock.object@discards.n*stock.object@discards.wt)))
  discards.wt[,hist.land$Year<first.disc.yr] <-NA
  raising.factor <-hist.land$Total/tot.wt
  raising.factor[,hist.land$Year>=2011] <-1 
  hist.land$discards.tonnes <-round(c(discards.wt*raising.factor),0)

  win.metafile(paste(f.u, "_advice_landings.wmf", sep=""), height=2.4, width=2.4/0.55, pointsize=12)
  par(mar=c(1.5, 1.5, 1.5, 0.5))
  tmp=barplot(rbind(hist.land$Total,hist.land$discards.tonnes)/1000,plot=F, space=1.5)
  with(hist.land, tmp<- barplot(rbind(Total,discards.tonnes)/1000,names=Year, col=c("#9AC2B7","#F15D2A"), axes=F, axisnames = F, 
        ylim=c(0,max(hist.land$Total + hist.land$discards.tonnes,na.rm=T)*1.1/1000), space=1.5))
  legend("topright", legend=c("Discards","Landings"), fill=c("#F15D2A","#9AC2B7"),  inset=0.01, cex=0.6, bty="n", ncol=2)
  axis(1, at=tmp[seq(1,length(tmp),by=5)], labels=hist.land$Year[seq(1,length(tmp),by=5)],cex.axis=0.6,tck=-0.03, mgp = c(3, 0.06, 0))  
  axis(2, mgp = c(3, 0.5, 0),las=1,cex.axis=0.6,tck=-0.03)
  title(ylab="1000 tonnes", cex.lab=0.6,line=0.9)
  title("Catches", cex.main=0.9,line=0.5)
  box()
  dev.off()

##########################################################
#UW tv survey
##########################################################
  
  hist.tv <- read.csv(tv_results, header=T)
  tv.series <-data.frame(year=hist.tv$year)
  first.ci.year <-min(tv.series$year)
  tv.series$abundance <-hist.tv$abundance
  tv.series$confidence.interval <-hist.tv$confidence.interval
 	tv.series$upper <- with(tv.series, abundance+confidence.interval)
	tv.series$lower <- with(tv.series, abundance-confidence.interval)	
	trigger <-min(tv.series$abundance[tv.series$year<2010],na.rm=TRUE)
  if (f.u == "jura"){trigger <- -100}
  
  win.metafile(paste(f.u, "_advice_tv.wmf", sep=""), height=2.4, width=2.4/0.55, pointsize=12)
  par(mar=c(1.5, 1.5, 1.5, 0.5))  
  with(tv.series, plot(year,abundance, type="l", lty=1, lwd=3,xlab="", ylab="", axes=F,ylim=c(0,max(upper,na.rm=T)*1.05)))
  with(tv.series, lines(year,upper, type="l", lty=3, lwd=2))
  with(tv.series, lines(year,lower, type="l", lty=3, lwd=2))
  axis(1, at=tv.series$year[seq(1,length(tv.series$year),by=5)], cex.axis=0.6,tck=-0.03, mgp = c(3, 0.08, 0))  
  axis(2, mgp = c(3, 0.4, 0),las=1,cex.axis=0.6,tck=-0.03)
  title(ylab="Abundance (millions)", cex.lab=0.6,line=1)
  title("Stock size index - abundance", cex.main=0.9,line=0.5)
  box()
	abline(h=trigger, lty=3, col="#EB6B14" ,lwd=3)
	dev.off()
	
	
##########################################################
#Harvest rates
##########################################################     
  win.metafile(paste(f.u, "_advice_HR.wmf", sep=""), height=2.4, width=2.4/0.55, pointsize=12)
  par(mar=c(1.5, 1.5, 1.5, 0.5))  
  HRs <- read.csv(Exploitation_summary, header=T)
  
  with(HRs, plot(harvest.ratio~year, type="l",cex=1,bty="l", 
        ylim=c(0, max(harvest.ratio,MSY.hr+1,na.rm=T)), lwd=1.5,axes=F,xlab="",ylab=""))
  axis(1, at=HRs$year[seq(1,length(HRs$year),by=5)], cex.axis=0.6,tck=-0.03, mgp = c(3, 0.06, 0))  
  axis(2, mgp = c(3, 0.4, 0),las=1,cex.axis=0.6,tck=-0.03)
  title(ylab="Harvest Rate (%)", cex.lab=0.6,line=0.9)
  title("Harvest Rate", cex.main=0.9,line=0.6)
  box()
  abline(h=MSY.hr,lty=3,col="#F7A487",lwd=3)
  dev.off()
}
 
 