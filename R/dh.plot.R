dh.plot <-

function(wk.dir, survey.year, coast.file, fu.dens, f.u = c("devils hole"), bubble.scale = 1)
{
	old.dir <- setwd(wk.dir)
	on.exit(setwd(old.dir), add = TRUE)

#	uk.coast <- read.table("scottish_coast.txt", header=T)

	get.fname <-
	function (base.name)
	{
		fname <- function(i) paste(base.name, i, ".jpg", sep="")
		out <- fname(i <- 1)
		while (file.exists(out))
		out <- fname(i <- i + 1)
		return ( out )
	}
	
	jpeg(get.fname(paste(wk.dir, f.u, "_survey.plot_",survey.year,".", sep = "")), width = 640, height = 640, 
		pointsize = 12, bg = "white", res = NA, quality=100)
	
	if (f.u== "devils hole")  
	{
    plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-0.5,2.5), ylim=c(56, 57.5))
		#, main="Fladen")
		#mtext(side=3, text="FU 07 Management Unit G", cex=0.7, line=0.5)
		polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")
  
		lines(x=c(0,2), y=c(57.5,57.5), lwd=2, lty=2)
    lines(x=c(0,2), y=c(56.0,56.0), lwd=2, lty=2)   
		lines(x=c(2,2), y=c(56,57.5), lwd=2, lty=2)
    lines(x=c(0,0), y=c(56,57.5), lwd=2, lty=2)
		
    polyg <-devils.hole.vms.poly
    plot(polyg, add=T, poly.args = list(col=2, border=NA))
    
    #plot holes
    pts<- get.pts(polyg)
    logic.pts.hole<- lapply(1:length(pts), function(x) 
    {
      pts[[x]]$hole == T
    })
    
    pts.hole<- pts[logic.pts.hole==T]
    pts.hole<- new("gpc.poly", pts=pts.hole)
    plot(pts.hole, add=T, poly.args = list(col="skyblue", border=NA))

 #   lapply(fladen.surfer.poly[c(17:20,24,25)],polygon,col="olivedrab",border=NA)
 #   lapply(fladen.surfer.poly[c(1,2,3,4,23)],polygon,col="green",border=NA)
#		lapply(fladen.surfer.poly[c(5,7,9,14)],polygon,col="lightgreen",border=NA)
#		lapply(fladen.surfer.poly[10],polygon,col="darkgreen",border=NA)
 #   lapply(fladen.surfer.poly[12],polygon,col="lightgreen",border=NA)
 #   lapply(fladen.surfer.poly[c(13,22)],polygon,col="green",border=NA)
#		lapply(fladen.surfer.poly[c(6,8,11,15,16,21)],polygon,col="skyblue",border=NA)


		
#		points(fu.dens$lons, fu.dens$lats, cex=bubble.scale*fu.dens$average.density, lwd=3)
    points(fu.dens$lons[fu.dens$average.density>0], fu.dens$lats[fu.dens$average.density>0], 
        cex=bubble.scale*fu.dens$average.density[fu.dens$average.density>0], lwd=3)
    points(fu.dens$lons[fu.dens$average.density==0], fu.dens$lats[fu.dens$average.density==0], 
        pch="x",cex=2, lwd=2,col="red")
		polygon(uk.coast, col="grey75")
		title(survey.year, cex.main=2.5)
		box()
		
	} 
	
	dev.off()

}
	
	
	
	
	
	