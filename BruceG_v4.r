# Bruce Goldfeder
# PhD Candidacy Exam
# Visualization - Dr. Ed Wegman

# Load the Iso package
#install.packages("Iso", repos='http://cran.us.r-project.org')
#install.packages('dplyr')

library(Iso)
#library(dplyr)

# Generate pseudo-random numbers in normal distribution
#set.seed(123)
set.seed(144)

size<-1500
x<-rnorm(size)
y<-sort(x,decreasing=FALSE)
hist(y)
plot(y)

# Run the raw density function on x
# Output the g vector
n<-size
g=c()
prev<-y[1]  # the j-1 variable
curr<-0.1   # the j variable this should be overwritten in loop
# The loop requires two elements in x and results in a vector of length size-1 or 1499 elements
for (j in 2:size) {
    curr<-y[j]
    #g[j-1]=1/(n*(curr-prev))
    g[j-1]<-1/n*((y[j]-y[j-1])/abs(y[j]))
    prev<-y[j]
}

plot(x[1:size-1],g)
#gsort<-sort(g,decreasing=FALSE)
#plot(x[1:size-1],gsort)

# Run the pool adjacent violators algorithm

h<-pava(g,decreasing = FALSE)
plot(y[1:size-1],h)

h2<-pava(g,decreasing = TRUE)  # stepfun gives the edge numbers
plot(y[1:size-1],h2)

#h2<-pava(g,decreasing=TRUE)
#plot(y[1:size-1],h2,pch=21,bg="blue")
#lines(y[1:size-1],h2,pch=21,bg="blue"

# Guessing on this one
#hleft<-ufit(h,x=y[size-1],lmode=leftmd,lc=TRUE)
#plot(hleft)

# Find the min and max for x the original normal distribution
leftmd<-min(x)
rightmd<-max(x)

# Solve using ufit with g the raw density and y the ordered normal distribution
# to find the "interval" where the overlap we need to determine MLE

right<-ufit(g,x=y[1:size-1],lmode=rightmd,rc=TRUE)
pavright<-pava(right$y)
plot(y[1:size-1],pavright)
lines(right)
left<-ufit(g,x=y[1:size-1],lmode=leftmd,lc=TRUE)
pavleft<-pava(left$y,decreasing=TRUE)
lines(y[1:size-1],pavleft,bg="blue")
lines(left)

# Find the last element of the left entering value (top left of graph)
# Find the first element of the right leaving value (top right of graph)
# lval<-left$y[1] # This implementation was a bit naive and did not take into account minor roundoff errors

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
  plot(y[1:size-1],yhatmx[k,],xlim=c(leftmd,rightmd),ylim=c(0.0,ymax))
}

# For log-likelihood the lowest negative value will generate the least negative number
plot(lelist[lidx:ridx])


MLEval
MLEidx
