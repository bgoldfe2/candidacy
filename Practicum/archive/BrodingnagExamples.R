# Testing Brobdingnag Cran library if needed
install.packages("Brobdingnag", repos='http://cran.us.r-project.org')
library(Brobdingnag)

x <- as.brob(1:10)
y <- 1e10
x+y
as.numeric((x+y)-1e10)
x^(1/y)

a <- as.brob(1:10)
a <- cbrob(a, as.brob(10)^1e26)
a
as.numeric(a)
as.complex(10i + a)

a <- as.brob(10)^(0.5 + 97:103)
a < 1e100

plot(as.brob(1:10)^1e26)

googol <- as.brob(10)^100
googol
googol + googol/2
1/(googol + 1e99)
(1:10)^googol

googolplex <- 10^googol
googolplex
googolplex * googol # practically the same as googolplex (!)