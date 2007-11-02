skzs <- function(data, y, x1, x2, delta1, delta2, h1, h2, k=1, show.edges=FALSE, plot=TRUE)
{	
	s1 <- diff(sort(data[,x1]))
	s2 <- diff(sort(data[,x2]))
	if (h1 >= min(s1[s1 > 0]))
		stop("Invalid 'h1': Value should be much less than the minimum difference of consecutive x1 values")  
	if (h2 >= min(s2[s2 > 0]))
		stop("Invalid 'h2': Value should be much less than the minimum difference of consecutive x2 values")  
	if (delta1 >= (max(data[,x1]) - min(data[,x1])))
		stop("Invalid 'delta1': Value should be much less than the difference of the max and min x1 values") 
	if (delta2 >= (max(data[,x2]) - min(data[,x2])))
		stop("Invalid 'delta2': Value should be much less than the difference of the max and min x2 values") 
	origx1 <- data[,x1]
	origx2 <- data[,x2]
	origy <- data[,y]
	x1range <- range(data[,x1])
	x2range <- range(data[,x2])
	d1 <- delta1/2
	d2 <- delta2/2
	for (i in 1:k) {
		data <- as.vector(data)                         
		maxx1 <- max(data[,x1])	                    
		minx1 <- min(data[,x1])			        
		maxx2 <- max(data[,x2])
		minx2 <- min(data[,x2])
		yvals <- data[,y]
		xk1 <- seq(minx1 - d1, maxx1 + d1, h1)
		xk2 <- seq(minx2 - d2, maxx2 + d2, h2)
		xk <- expand.grid(xk1 = xk1, xk2 = xk2)
		zk <- array(NA, dim = c(nrow(xk),1))
		for (j in 1:nrow(xk)) {
			w1 <- abs(data[,x1] - xk$xk1[j])
			w1[w1 > d1] <- NA
			w2 <- abs(data[,x2] - xk$xk2[j])
			w2[w2 > d2] <- NA
			Ik <- which(!(is.na(w1) | is.na(w2)))
			YIk <- yvals[Ik]
			zk[j] <- mean(YIk)      
		}
		xk$zk <- zk
		data <- na.omit(xk)
		x1 <- 1
		x2 <- 2
		y <- 3
	}           
		if (show.edges == FALSE){
			x1d <- data[data[,1] >= min(x1range) & data[,1] <= max(x1range), ]   
			x2d <- x1d[(x1d[,2] >= min(x2range)) & (x1d[,2] <= max(x2range)), ]   
			data <- na.omit(x2d)
		}
		if (plot == TRUE){
			plot(wireframe(zk ~ xk1 * xk2, data,drape = TRUE, colorkey = TRUE, scales = list(arrows = FALSE)))    
		}		
	return(data)
}

