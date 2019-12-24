# Bruce Goldfeder
# PhD Candidacy Exam
# Visualization Practicum - Dr. Ed Wegman

# Load the Iso package
#install.packages("Iso", repos='http://cran.us.r-project.org')

library(Iso)

# Generate pseudo-random numbers in normal distribution
set.seed(144)

size<-1500
x<-rnorm(size)
y<-sort(x,decreasing=FALSE)

# Plot the histogram of X and the plot of ordered values Y
hist(x)
plot(y,main="Plot for Ordered Values Y")

# Run the Raw Density function on x
# Output the g vector from the loop
n<-size
g=c()

# Calculate the raw density using the adjusted formula (divide by y[j] to eliminate spikes)
# The loop requires two elements in x and results in a vector of length size-1 or 1499 elements
for (j in 2:size) {
    #g[j-1]=1/(n*(curr-prev)) # original formula
    g[j-1]<-1/n*((y[j]-y[j-1])/abs(y[j]))  # normalized to reduce spikes
}

# Plot the generated Raw Density
plot(x[1:size-1],g,main="Raw Density", 
     xlab="X", ylab="Density")


# Find the min and max for x the original normal distribution
leftmd<-min(x)
rightmd<-max(x)

# Solve using ufit with g the raw density and y the ordered normal distribution
# to find the "interval" of overlap coming in from the left and the right (increasing and decreasing) pava

right<-ufit(g,x=y[1:size-1],lmode=rightmd,rc=TRUE)
pavright<-pava(right$y)

left<-ufit(g,x=y[1:size-1],lmode=leftmd,lc=TRUE)
pavleft<-pava(left$y,decreasing=TRUE)

# Plot the Overlap in single graph
plot(y[1:size-1],pavright,type="o",col="green",main="Overlap Analysis using PAVA from right and left")
lines(y[1:size-1],pavleft,bg="blue",type="o",col="blue")
legend(-3, .00011, legend=c("Right PAVA", "Left PAVA"),
       col=c("green", "blue"), lty=1:2, cex=0.8)


# Finds the most common value in the large set of values coming in from the left (eliminates roundoff errors)
leftin<-sort(table(pavleft[1:800]),decreasing=TRUE)[1]
lval<-names(leftin)[1]
# Finds the last index of the entry line coming in from the left
lidx<-max(which(pavleft %in% c(lval)))

# Finds the most common value in the large set of values coming in from the right (eliminates roundoff errors)
rightin<-sort(table(pavright[size-1:700]),decreasing=TRUE)[1]
rval<-names(rightin)[1]
# Finds the first index of the entry line coming in from the right
ridx<-match(c(rval),pavright)

# Using the ridx and lidx index values loop through the overlap to find Log-Likelihoods
# When done find the Max Likelihood
lelist<-c()
yhatmx<-matrix(NA, nrow=lidx-ridx+1, ncol=size-1)
cnt<-0
for (i in ridx:lidx) {
  cnt<-cnt+1
  fhatx1m1<-ufit(g,x=y[1:size-1],lmode=y[i])
  yhat<-fhatx1m1$y
  yhatmx[cnt,]<-yhat
  yhat<-yhat*sign(yhat)  # make all values positive for log-likelihood
  logyhat<-log(yhat)
  like<-sum(logyhat)
  lelist[i]<-like
}

MLE<-min(lelist,na.rm=TRUE)  # This is the most negative log value which is the largest value exp^-min
MLEidx<-which(lelist == MLE)
MLEval<-y[MLEidx]

# Plot the fhats finding the min and max for the y range setting
yhatmax<-max(as.numeric(unlist(yhatmx)))
ymax<-yhatmax*1.01  # make it 1% higher than max value

for (k in 1:(lidx-ridx+1)) {
  plot(y[1:size-1],yhatmx[k,],xlim=c(leftmd,rightmd),ylim=c(0.0,ymax),main=paste("PDF for Mode at index",ridx+k-1), 
       xlab="X", ylab="Density")
}

# For log-likelihood the lowest negative value will generate the least negative number
plot(y[lidx:ridx],lelist[lidx:ridx],main="FHat Log-Likelihoods", sub="Smallest value will generate the least negative number",
     xlab="X", ylab="Likelihood of FHat")


MLEval
MLEidx
