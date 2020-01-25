# Bruce Goldfeder
# CSI PhD Candidacy Project
# Dr. Edward Wegman
# Nov 26, 2019

# Load ISO library
library(Iso)

# Variables
size <- 1500

# Create Monte Carlo (MC)
# First create a random normal distribution
x <- rnorm(size)

x# Order the data from smallest to largest providing order statistic?
x <- sort(x, decreasing=FALSE)

# Then provide a jitter
#noise <- .05*(max(y)-min(y))

#y<-y+rnorm(size)*noise

#x <- x-min(x)

# Provide X values for raw data plot
x <- seq(1:size)/size

## Plot the raw data order statistic and R density plot
#d <- density(y)
#plot(x,y, main="Monte Carlo Order Statistic Plot")
#plot(d,main="Monte Carlo Density Using R Kernel")

# Calculate the raw density estimate

n<-size

g=c(0)

# I may have the value for n incorrect?
for (j in 2:size) {
  g[j]=1/(n*(x[j]-x[j-1]))}
plot(x,g)
lines(x[order(x)], g[order(x)], xlim=range(x), ylim=range(g), pch=16)

# ISO Library method ufit
#z <- ufit(g,x=x,type="b")
#plot(x,g)
#lines(z,col="red")
#plot(z$h,do.points=FALSE,col.hor="blue",col.vert="blue",add=TRUE)

# ISO Library method PAVA
z<-g
zstar <- pava(z)
plot(z)
lines(zstar,type='s')
# Using the stepfunction:
zstar <- pava(g,stepfun=TRUE)
plot(g)
plot(zstar,add=TRUE,verticals=FALSE,pch=20,col.points="red")