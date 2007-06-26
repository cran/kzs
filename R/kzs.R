`kzs` <-
function(x, delta, h, k=1, show.edges=FALSE)
{
if (h >= min(diff(sort(x[,1]))))
stop("Invalid 'h': Value should be much less than the minimum difference of consecutive X values")  
if (delta >= (max(x[,1])-min(x[,1])))
stop("Invalid 'delta': Value should be much less than the difference of the max and min X values") 
orig_data<-as.vector(x)
xrange <- range(x)
d <- delta/2
v <- -delta/(2*h)
for (i in 1:k) {
data <- as.vector(x)                        
maxx <- max(data[,1])                    
minx <- min(data[,1])        
yvals <- data[,2]                           
ord <- sort(data[,1])                                                                                      
Nk <- (maxx-minx)/h
incrm <- seq(v,Nk-v,by=1)
xk <- as.vector(ord[1]+(incrm*h))           
zk <- numeric(length(xk))         
for (j in 1:length(xk)) {
w <- abs(ord-xk[j])              
Ik <- yvals[w <= d]  
zk[j] <- mean(Ik)   
}
Zk <- data.frame(zk)                      
x <- data.frame(cbind(xk,Zk))              
xf <- na.omit(x)                
if (show.edges == FALSE){
y2 <- x[-(1:(-v*k)),]
y3 <- y2[-(nrow(y2):(nrow(y2)-(-v*k)+1)),]
xj <- as.data.frame(y3)
xf <- na.omit(xj)
}
plot(xf$zk~xf$xk,xlab=expression(paste(x[k])),ylab=expression(paste(Y(x[k]))),xlim=xrange,ylim=c(min(xf$zk),max(xf$zk)),type='l',pch=19,col='black')        
}
return(xf)
}

