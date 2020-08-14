### simulate segementd data
x <- c(1:10, 11:20)
y <- numeric(20)
## Create first segment
y[1:10] <- 20:11 + rnorm(10, 0, 1.5)
## Create second segment
y[11:20] <- seq(11, 15, len=10) + rnorm(10, 0, 0.8)
## Plot it
plot(x,y, ylim=c(5, 25), pch=16)

#### fit a segmended model
library(segmented)
lin.mod <- lm(y ~ x)
### psi is the initial guess
segmented.mod <- segmented(lin.mod, seg.Z= ~x, psi=13)
segmented.mod
plot(segmented.mod)
### results shows that 10.56 is the change point
c.tmp <- 10.45

### in our case since we have annual data it might be better to use x at integer, so here I will use change point at 11
c.tmp <- 11
x.u <- ifelse(x > c.tmp, 1, 0)

### so the model is y = intercept + beta_1*x + beta_2*(x-changepoint)*x.u
test <- segmented.mod$coef[1] + segmented.mod$coef[2] * x + segmented.mod$coef[3] * (x-c.tmp) * x.u

plot(x, y, ylim=c(5, 25), pch=16)
lines(x, test, col=2)



