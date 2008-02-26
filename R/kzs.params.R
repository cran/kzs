kzs.params <- function(x, dimension) {
	delta <- numeric(dimension)
	desc <- character(dimension)
	d <- numeric(dimension)
	ddesc <- character(dimension)
	for (i in 1:dimension) {
		delta[i] <- max(x[,i]) - min(x[,i])
		sx <- sort(x[,i])
		dx <- diff(sx)
		d[i] <- min(dx[dx > 0])
		desc[i] <- paste("For x",i,", delta must be a positive real number much less than ", delta[i], sep = "")
		ddesc[i] <- paste("For x",i,", d must be a positive real number less than ", d[i], sep = "")
	}	
	lst <- list(delta = desc, d = ddesc)
	return(lst)
}