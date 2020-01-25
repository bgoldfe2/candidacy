# Testing Brobdingnag Cran library if needed
install.packages("Rmpfr", repos='http://cran.us.r-project.org')
library(Rmpfr)

## Using "mpfr" numbers instead of regular numbers...
n1.25 <- mpfr(5, precBits = 256)/4
n1.25

## and then "everything" just works with the desired chosen precision:hig
n1.25 ^ c(1:7, 20, 30) ## fully precise; compare with
print(1.25 ^ 30, digits=19)
exp(n1.25)

## --- Large integers from package 'gmp':
Z <- as.bigz(7)^(1:200)
head(Z, 40)
## mfpr(Z) by default chooses the correct *maximal* default precision:
mZ. <- mpfr(Z)
## more efficiently chooses precision individually
m.Z <- mpfr(Z, precBits = frexpZ(Z)$exp)
## the precBits chosen are large enough to keep full precision:
stopifnot(identical(cZ <- as.character(Z),
                    as(mZ.,"character")),
          identical(cZ, as(m.Z,"character")))

## 1/3 + 1 = 4/3 :
as.bigq(1,3) + 1
r <- as.bigq(12, 47)
stopifnot(r ^ 3 == r*r*r)

num = as.bigq(10)^250

( x3 <- c( mpfr(pi, 140)) )
formatMpfr(x,scientific = TRUE)

maybe<-c( mpfr(num))

i32 <- mpfr(1:32, precBits = 64)
format(num,   base=  2, drop0trailing=TRUE)
format(num,   base= 16, drop0trailing=TRUE)
format(num, base=  10, scientific=TRUE,drop0trailing=TRUE)# using scientific notation for [17..32]

as.numeric(num)
