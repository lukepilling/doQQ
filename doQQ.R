##################################################################################################################
#### Function to create a QQ plot (expected vs. observed P-values) with shaded confidence intervals & lambda value
#### Luke Pilling

#### Inputs/options
#### ==============
#### p              ::  {REQUIRED} vector of P-values
#### main           ::  {optional} title of plot
#### subtitle       ::  {optional} subtitle of plot

#### outFile        ::  {optional} file name of PNG graphic to produce
#### outFile.width  ::  {optional} {default=700} width (pixels) of PNG graphic to produce
#### outFile.height ::  {optional} {default=500} height (pixels) of PNG graphic to produce


## Do QQ plot
doQQ <- function(p=NULL, 
                 outFile=NULL, 
                 outFile.width=700, 
                 outFile.height=500, 
                 main="", 
                 subtitle="")  {

        ## check inputs appropriate -- return error if not
        if (is.null(p))  stop("\n\n## Need to specify a valid numeric vector of P-values [p=...]\n\n")
        if (!is.numeric(p))  stop("\n\n## Vector of P-values needs to be numeric\n\n")
        
        p <- na.omit(p)
        if (min(p) <= 0)  stop("\n\n## The lowest P-value is below 0; something is wrong\n\n")
        if (max(p) > 1)   stop("\n\n## The largest P-value is greater than 1; something is wrong\n\n")
        
        if (is.null(outFile))   print("## Not printing to file - if you want to then specify a valid output file name [outFile=...]")

        ## compute lambda
        ll <- lambda(p)
        
	obs <- na.omit( -log(p,10) ) # observed data
	N <- length(obs) ## number of p-values
	
	## create the null distribution
	## (-log10 of the uniform)
	null <- -log(1:N/N,10)
	MAX  <- max(c(obs,null))
	max2 <- max(null)
	
	## create the confidence intervals
	c95 <- rep(0,N)
	c05 <- rep(0,N)
	
	## the jth order statistic from a uniform(0,1) sample has a beta(j,n-j+1)
	## distribution (Casella & Berger, 2002, 2nd edition, pg 230, Duxbury)
	
	for(i in 1:N){
	  c95[i] <- qbeta(0.95,i,N-i+1)
	  c05[i] <- qbeta(0.05,i,N-i+1)
	}
	
	## negative log10
	nln_c95 <- -log(c95,10)
	nln_c05 <- -log(c05,10)
	
	## start plot
	if (!is.null(outFile))  png(filename=outFile, width=outFile.width, height=outFile.height, units="px")
	
	## setup the qqplot/axis
	plot(NULL, ylim=c(0,MAX), xlim=c(0,max2), xlab="", ylab="", xaxt='n', yaxt='n', ann=FALSE)
	par(new=T)
	
	## plot the confidence intervals, shaded
	polygon(c( rev(null), null ), c( rev(nln_c95), nln_c05 ), col = "grey75", border = NA)
	
	## plot QQ over shading
	par(new=T)
	abline(0, 1, col="grey40", lty = 2 , lwd = 0.4)
	par(new=T)
	qqplot(null, 
	       obs, 
	       ylim=c(0,MAX),
	       xlim=c(0,max2), 
	       main=main, 
	       xlab="Expected p-values ( -log10 )",
	       ylab="Observed p-values ( -log10 )", 
	       cex.main=1.2, 
	       cex.lab=1.1,
	       cex.axis=1.1,
	       pch=20)
	
	## add subtitle
	mtext(subtitle, line=0.3)
	
	## add lambda
	legend("topleft", legend=paste("Lambda = ", round(ll, digits=4)));
	
	## close plot
	if (!is.null(outFile))  dev.off()
	
}

## Compute lambda
lambda <- function(p) median(qchisq(p, df=1, lower.tail=FALSE), na.rm=TRUE) / qchisq(0.5, df=1)
