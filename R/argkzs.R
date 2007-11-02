argkzs <- function(data, x) {
	delta <- max(data[,x]) - min(data[,x])
	sx <- sort(data[,x])
	dx <- diff(sx)
	minx <- min(dx[dx > 0])
	arg1 <- sprintf("delta must be a real number much less than %s", delta)
	arg2 <- sprintf("h must be a positive real number less than %s", minx)
	lst <- list(delta = arg1, h = arg2)
	return(lst)
}

