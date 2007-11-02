argskzs <- function(data, x1, x2) {
	delta1 <- max(data[,x1]) - min(data[,x1])
	delta2 <- max(data[,x2]) - min(data[,x2])
	sx1 <- sort(data[,x1])
	sx2 <- sort(data[,x2])
	dx1 <- diff(sx1)
	dx2 <- diff(sx2)
	minx1 <- min(dx1[dx1 > 0])
	minx2 <- min(dx2[dx2 > 0])
	arg11 <- sprintf("delta1 must be a real number much less than %s", delta1)
	arg12 <- sprintf("delta2 must be a real number much less than %s", delta2)	
	arg21 <- sprintf("h1 must be a positive real number less than %s", minx1)
	arg22 <- sprintf("h2 must be a positive real number less than %s", minx2)
	lst <- list(delta1 = arg11, delta2 = arg12, h1 = arg21, h2 = arg22)
	return(lst)
}

