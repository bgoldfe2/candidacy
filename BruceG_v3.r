# Bruce Goldfeder
# PhD Candidacy Exam
# Visualization - Dr. Ed Wegman

# Load the Iso package
#install.packages("Iso", repos='http://cran.us.r-project.org')
library(Iso)

# Generate pseudo-random numbers in normal distribution
set.seed(123)
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
plot(h)

# This section depicts the manual discovery of the mode using the left-most point of the right most bar of points
#h[1500]
#h[1000]
#h[800]
#h[900]

left<-ufit(g,x=y,lmode=-3.715927,lc=TRUE)
plot(left)
right<-ufit(g,x=y,lmode=4.066409,rc=TRUE)
plot(right)
plot(left)
right
left
x[750]
fhatx1m1<-ufit(g,x=y,lmode=0.0)
fhatx1m1
plot(fhatx1m1)
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
