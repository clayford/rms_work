library(rms)

# from ?ols help page
set.seed(1)
x1 <- runif(200)
x2 <- sample(0:3, 200, TRUE)
distance <- (x1 + x2/3 + rnorm(200))^2
d <- datadist(x1,x2)
options(datadist="d")   # No d -> no summary, plot without giving all details


f <- ols(sqrt(distance) ~ rcs(x1,4) + scored(x2), x=TRUE)
f1 <- ols(sqrt(distance) ~ rcs(x1,quantile(x1, c(min(x), 0.05, 0.35, 0.65, 0.95, max(x)))) + x2, x=TRUE)
f1
library(splines)
f2 <- lm(sqrt(distance) ~ ns(x1, df = 3) + x2)
f2 <- lm(sqrt(distance) ~ ns(x1, knots = quantile(x1, c(0.05, 0.35, 0.65, 0.95))) + x2)
summary(f2)
# could use d <- datadist(f); options(datadist="d") at this point,
# but predictor summaries would not be stored in the fit object for
# use with Predict, summary.rms.  In that case, the original
# dataset or d would need to be accessed later, or all variable values
# would have to be specified to summary, plot
anova(f)
which.influence(f)
summary(f)
summary.lm(f)    # will only work if penalty and penalty.matrix not used


# Fit a complex model and approximate it with a simple one
n <- 300
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
x4 <- runif(n)
y <- x1 + x2 + rnorm(n, mean = 0, sd = 1.5)
f    <- ols(y ~ rcs(x1,4) + x2 + x3 + x4)
pred <- fitted(f)   # or predict(f) or f$linear.predictors
f2   <- ols(pred ~ rcs(x1,4) + x2 + x3 + x4, sigma=1)
# sigma=1 prevents numerical problems resulting from R2=1
fastbw(f2, aics=100000)
# This will find the best 1-variable model, best 2-variable model, etc.
# in predicting the predicted values from the original model
options(datadist=NULL)


# generate data
set.seed(1)
x <- sort(runif(100, 0, 4))
y <- 0.3 + cos(x*pi) + rnorm(100, sd = 0.5)

# model with rms::rcs
library(rms)
m1 <- ols(y ~ rcs(x, 5))
coef(m1)

# model with splines::ns
library(splines)
kn <- quantile(x, probs = c(.05,.275,.5,.725,.95))
x_ns <- ns(x, knots = kn[2:4], Boundary.knots = c(kn[1],kn[5]))
m2 <- lm(y ~ x_ns)
coef(m2)

# compare fitted lines
plot(x,y)
lines(x, fitted(m1), col = 1)
lines(x, fitted(m2), col = 2)
