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
#h[850]
#h[875]
#h[850:875]
#index<-856
#x[856]
incX<-x[1500:856]
h[500]
h[850]
h[800]
h[825]
h[800:825]
plot[incX]
plot[incX]
incX
y2<-y[1500:856]
plot(y2)
y3<-y[1:844]
plot(y3)
plot(y2)
plot(y3)
y3<-y[1:856]
plot(y3)
y_unord<-x[1:856]
plot(x)
plot(y_unord)
y_ord(y_unord, decreasing=TRUE)
y_ord<-(y_unord, decreasing=TRUE)
y_ord<-sort(y_unord, decreasing=TRUE)
plot(y_ord)
pv2<-pava(y_ord)
plot(pv2)
pv2<-pava(y_unord)
plot(pv2)
plot(x,y)
plot(y,x)
plot(y_ord,x[1:856])
plot(x,y)
plot(x,g)
inc<-ufit(g,x=x,lc=TRUE)
plot(inc)
inc<-ufit(g,x=y,lc=TRUE)
plot(inc)
max(inc)
max(unlist(inc))
modeInc<-max(unlist(inc))
modeInc
dec<-ufit(g,x=y,rc=TRUE)
plot(dec)
plot(dec)
dec<-ufit(g,x=y,lmode=1500,rc=TRUE)
plot(dec)
inc<-ufit(g,x=y,lmode=1,lc=TRUE)
plot(inc)
plot(dec)
right<-ufit(g,x=y,lmode=1500,rc=TRUE)
plot(right)
left<-ufit(g,x=y,lmode=1,lc=TRUE)
plot(left)
left<-ufit(g,x=-y,lmode=1,lc=TRUE)
plot(left)
max(y)
min(x)
left<-ufit(g,x=-y,lmode=-3.715927,lc=TRUE)
plot(left)
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
