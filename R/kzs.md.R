kzs.md <- function(y, x, delta, d, k = 1, edges = FALSE)
{	
	s <- matrix(0, nrow = nrow(x)-1, ncol = ncol(x))
	if(nrow(x) != length(y))
		stop("The lengths of 'x' and 'y' must be equal")
	for (i in 1:ncol(x)) {
		s[,i] <- diff(sort(x[,i]))
		if (d[i] > min(s[,i][s[,i] > 0]))     
			stop("Invalid 'd': For each X, 'd' must be less than or equal to the minimum of the difference of consecutive X values") 
		if (d[i] <= 0)
			stop("Invalid 'd': For each X, 'd' must be a positive real number")
		if (delta[i] >= (max(x[,i]) - min(x[,i])))
			 stop("Invalid 'delta': For each X, 'delta' must be less than the difference of the max and min X") 
		if (delta[i] <= 0)
			stop("Invalid 'delta': For each X, 'delta' must be a positive real number")
	}
	h <- delta/2 
	xd <- as.data.frame(x)
	vars <- ncol(x)    
	for (j in 1:k) {
		xi <- x                           
		yi <- y
		maxx <- apply(xi, 2, max)	                    
		minx <- apply(xi, 2, min)			        
		xks <- vector("list", ncol(xi))
		for(i in 1:ncol(xi)) {
			xks[[i]] <- seq(minx[i] - h[i], maxx[i] + h[i], d[i])
		}
		xk <- do.call("expand.grid", xks)	
		yk <- numeric(length = nrow(xk))
		for (i in 1:nrow(xk)) {
			cond <- lapply(1:vars, function(m) { abs(outer(x[,m], xk[,m][i], "-")) } )     
			wk <- t(mapply(function(x, y){ x[x > y] <- NA; x }, cond, h, SIMPLIFY = TRUE))
			Ik.mat <- 0*wk + col(wk)
			intersection <- function(x, y, ...){
	     			if (missing(...)) intersect(x, y)
				else intersect(x, intersection(y, ...))
			}
			Ik.list <- split(Ik.mat, 1:nrow(Ik.mat))
			names(Ik.list) <- NULL
			Ikn <- do.call(intersection, Ik.list) 
			Ik <- Ikn[!is.na(Ikn)]
			YIk <- yi[Ik]
			yk[i] <- mean(YIk)
		}	
		df <- data.frame(cbind(xk, yk))
		data <- na.omit(df)
		x <- as.matrix(data[1:vars])
		y <- as.matrix(data$yk) 
	}        
	if (edges == FALSE) {
		btw.ind <- vector("list", ncol(xd))
		for (v in 1:ncol(xd)) {
			btw.ind[[v]] <- which((data[,v] < min(xd[,v])) | (data[,v] > max(xd[,v])))
		}
		idx <- unique(sort(unlist(btw.ind)))
		data <- data[-idx,]
	}		
	return(data)
}