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
plot(y[1:1499],h)

# This section depicts the manual discovery of the mode using the left-most point of the right most bar of points
#h[1500]
#h[1000]
#h[800]
#h[900]

# Find the min and max for x the original normal distribution
leftmd<-min(x)
rightmd<-max(x)

left<-ufit(g,x=y[1:1499],lmode=leftmd,lc=TRUE)
plot(left)
right<-ufit(g,x=y[1:1499],lmode=rightmd,rc=TRUE)
plot(right)
#right
#left

# Via visual inspection find the overlapping elements from 
# The left and the right and use M1 as the mode
# For the seed 123 the two overlapped values are both 762
# Convert those to y values as the two modes to check
# The value of the ordered version of x at 762 is 0.05473653
fhatx1m1<-ufit(g,x=y[1:1499],lmode=0.05473653)
fhatx1m1
plot(fhatx1m1)

# Remove spike and come in from left and right
fhatx1m1$y[762]
z<-rep(TRUE,1499)
z[762]<-FALSE
fhatnospk<-fhatx1m1$y[z]

# The MLE calculation will need to be made for each possible value of Mode
#multiply each element
# mle for x1m1
mle1<-prod(fhatx1m1(1:1500))
mle1<-prod(fhatx1m1[1:1500])
mle1<-prod(fhatx1m1$y)
mle1
# get 40 or so mle's and pick the least negative one
stuff<-history
stuff
history
save.image("C:\\Users\\bruce\\dev\\candidacy\\dec14edweg.RData")
plot(h)
