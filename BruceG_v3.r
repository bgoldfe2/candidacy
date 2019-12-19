# Bruce Goldfeder
# PhD Candidacy Exam
# Visualization - Dr. Ed Wegman

# Load the Iso package
#install.packages("Iso", repos='http://cran.us.r-project.org')
#install.packages("dplyr", repos='http://cran.us.r-project.org')

library(Iso)
#library(dplyr)

# Generate pseudo-random numbers in normal distribution
#set.seed(123)
set.seed(124)
size<-1500
x<-rnorm(size)
y<-sort(x,decreasing=FALSE)
hist(y)
plot(y)

# Run the raw density function on x
# Output the g vector
n<-size
g=c()
prev<-x[1]  # the j-1 variable
curr<-0.1   # the j variable this should be overwritten in loop
# The loop requires two elements in x and results in a vector of length size-1 or 1499 elements
for (j in 2:size) {
    curr<-x[j]
    g[j-1]=1/(n*(curr-prev))
    prev<-x[j]
}

plot(g)
gsort<-sort(g,decreasing=FALSE)
plot(gsort)

# Run the pool adjacent violators algorithm
h<-pava(g)
plot(y[1:1499],h)  # I seem to not use this to solve for MLE???

# This section depicts the manual discovery of the mode using the left-most point of the right most bar of points
#h[1500]
#h[1000]
#h[800]
#h[900]

# Find the min and max for x the original normal distribution
leftmd<-min(x)
rightmd<-max(x)

# Solve using ufit with g the raw density and y the ordered normal distribution
# to find the "interval" where the overlap we need to determine MLE
left<-ufit(g,x=y[1:1499],lmode=leftmd,lc=TRUE)
plot(left)
right<-ufit(g,x=y[1:1499],lmode=rightmd,rc=TRUE)
plot(right)
#right
#left

# Via visual inspection find the overlapping elements from 
# The left and the right and use M1 as the mode
# This is automated in code below
# Convert those to y values as the two modes to find MLE

# Find the last element of the left entering value (top left of graph)
# Find the first element of the right leaving value (top right of graph)
lval<-left$y[1]
lidx<-length(which(left$y == lval))
rval<-right$y[1499]
ridx<-1499-length(which(right$y == rval))

lelist<-c()
for (i in lidx:ridx) {
  fhatx1m1<-ufit(g,x=y[1:1499],lmode=y[i])
  yhat<-fhatx1m1$y
  yhat<-yhat*sign(yhat)  # make all values positive for log-likelihood
  logyhat<-log(yhat)
  like<-sum(logyhat)
  lelist[i-(lidx-1)]<-like
}

MLE<-min(lelist)  # This is the most negative log value which is the largest value exp^-min
MLEidx<-which(lelist == MLE)
MLEval<-y[MLEidx]

