x=rt(1000,12)            #random t with df 12
hist(x,col="lightblue",freq=F) #histgram
y=seq(-4,4,.1)
points(y,dt(y,df=12),type="l")
op=par(fig=c(.02,.42,.53,.99),new=TRUE)            #to add a new small graph
qqnorm(x,xlab="",ylab="",main="",axes=FALSE)       #qqnorm in the small graph
qqline(x,col="red",lwd=2)   #line in the small graph
box(lwd=3)   #line width of the frame box for the smll graph
par(op)

##############################
set.seed(1)
par(mar=c(5,5,5,5))     #to set the margin
x1=sort(rnorm(100,mean=20,sd=5))   #asumption this Interest rate
x2=x1^3     #this is GDP
plot(x2,axes=FALSE,type="l",col="blue",xlab="",ylab="")   #plot GDP
axis(1)      #add the axis on bottom
axis(2,col="blue")   #add the axis on left
par(new=TRUE)       #add a new plot on  the current graph
plot(x1,axes=FALSE,type="l",col="red",xlab="",ylab="")  #plot interest rate
axis(4,col="red")   #add the axis on right
mtext(c("Time","GDP(mil.$)","Interest rate(%)"),side=c(1,2,4),line=3) #give the three axes lablea
title("GDP with interest rate")  #to give main title


##########################################
split.screen(c(2,1))
split.screen(c(1,2),2)
screen(1)
plot((1:10)^2,type="l",main="screen(1)")
screen(3)
close.screen(all=TRUE)


######################################
set.seed(125)
x=1:200
y=cumsum(rnorm(200))   #simulate an AR(1) model
int1=y-1.96       #calculate the 95% level confident interval
int2=y+1.96
plot(y,xlab="t",ylab=expression(y[t]))    #plot the data
polygon(c(x,rev(x)),c(int1,rev(int2)),col="gray",border=NA)  #show the band
lines(y,type="l",col="blue",lwd=2)   #plot the line again
?polygon
x <- c(1:9, 8:1) 
plot(x)
y <- c(1, 2*(5:3), 2, -1, 17, 9, 8, 2:9)
plot(y)
op <- par(mfcol = c(1, 1))
for(xpd in c(FALSE, TRUE, NA)) {
  plot(1:10, main = paste("xpd =", xpd))
  box("figure", col = "pink", lwd = 3)
  polygon(x, y, xpd = xpd, col = "orange", lty = 2, lwd = 2, border = "red")
}
n <- 100
xx <- c(0:n, n:0)
yy <- c(c(0, cumsum(stats::rnorm(n))), rev(c(0, cumsum(stats::rnorm(n)))))
plot   (xx, yy, type = "n", xlab = "Time", ylab = "Distance")
polygon(xx, yy, col = "gray", border = "red")
title("Distance Between Brownian Motions")
