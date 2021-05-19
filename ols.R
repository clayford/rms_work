library(rms)

# from ?ols help page
set.seed(1)
x1 <- runif(200)
x2 <- sample(0:3, 200, TRUE)
distance <- (x1 + x2/3 + rnorm(200))^2
d <- datadist(x1,x2)
options(datadist="d")   # No d -> no summary, plot without giving all details

# scored: expand categorical predictor having k numeric levels into linear term
# and k − 2 dummy variables
f <- ols(sqrt(distance) ~ rcs(x1,4) + scored(x2), x=TRUE)
f
# summary.lm(f)

# comparing to lm()
f1 <- lm(sqrt(distance) ~ rcs(x1,4) + scored(x2))
summary(f1)

anova(f)
anova(f, indnl = FALSE) # exclude individual tests for non-linearity
# anova.rms uses Type 2 SS
# x1, x2 and ERROR lines match those in anova.lm()
# The REGRESSION line matches F-statistic line in summary.lm()
car::Anova(f1)

# replicate x1 nonlinear line in anova.rms()
f1a <- lm(sqrt(distance) ~ x1 + scored(x2))
anova(f1a, f1)

# replicate x2 nonlinear line in anova.rms()
f1b <- lm(sqrt(distance) ~ rcs(x1,4) + x2)
anova(f1b, f1)

# replicate TOTAL nonlinear line in anova.rms()
f1c <- lm(sqrt(distance) ~ x1 + x2)
anova(f1c, f1)


# could use d <- datadist(f); options(datadist="d") at this point,
# but predictor summaries would not be stored in the fit object for
# use with Predict, summary.rms.  In that case, the original
# dataset or d would need to be accessed later, or all variable values
# would have to be specified to summary, plot

which.influence(f, cutoff = 0.3)
summary(influence.measures(f1)) # base R

# trying to understand summary.rms
summary(f)

# how to get effect
diff(predict(f, newdata = data.frame(x1 = c(0.2937, 0.74248),
                                     x2 = c(0, 0))))

predict(f)

# replicating using lm() and emmeans()
library(emmeans)

# lm() again but with ns()
f2 <- lm(sqrt(distance) ~ splines::ns(x1,3) + ordered(x2))
summary(f2)
f2.rg <- ref_grid(f2,at = list(x1 = c(0.74248355, 0.29369846),
                                   x2 = 0))
f2.rg
contrast(f2.rg, method = "pairwise")
confint(contrast(f2.rg, method = "pairwise"))

# compare to summary.rms
summary(f)



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


