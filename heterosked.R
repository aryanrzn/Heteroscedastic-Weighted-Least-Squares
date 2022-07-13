x = rnorm(100,0,3)
y = 3-2*x + rnorm(100,0,sapply(x,function(x){1+0.5*x^2}))
# Plot the data
plot(x,y)
# Plot the true regression line
abline(a=3,b=-2,col="grey")
# Fit by ordinary least squares
fit.ols = lm(y~x)
# Plot that line
abline(fit.ols,lty="dashed")
###############################################################################################################################################
par(mfrow=c(1,2))
plot(x,residuals(fit.ols))
plot(x,(residuals(fit.ols))^2)
par(mfrow=c(1,1))
###############################################################################################################################################
# Generate more random samples from the same model and the same x values,but different y values
# Inputs: number of samples to generate
# Presumes: x exists and is defined outside this function
# Outputs: errors in linear regression estimates
ols.heterosked.example = function(n) {
y = 3-2*x + rnorm(n,0,sapply(x,function(x){1+0.5*x^2}))
fit.ols = lm(y~x)
# Return the errors
return(fit.ols$coefficients - c(3,-2))
}
# Calculate average-case errors in linear regression estimates (SD of slope and intercept)
# Inputs: number of samples per replication, number of replications (defaults to 10,000)
# Calls: ols.heterosked.example
# Outputs: standard deviation of intercept and slope
ols.heterosked.error.stats = function(n,m=10000) {
ols.errors.raw = t(replicate(m,ols.heterosked.example(n)))
# transpose gives us a matrix with named columns
intercept.sd = sd(ols.errors.raw[,"(Intercept)"])
slope.sd = sd(ols.errors.raw[,"x"])
return(list(intercept.sd=intercept.sd,slope.sd=slope.sd))
}
###############################################################################################################################################
# Plot the data
plot(x,y)
# Plot the true regression line
abline(a=3,b=-2,col="grey")
# Fit by ordinary least squares
fit.ols = lm(y~x)
# Plot that line
abline(fit.ols,lty="dashed")
fit.wls = lm(y~x, weights=1/(1+0.5*x^2))
abline(fit.wls,lty="dotted")
###############################################################################################################################################
### As previous two functions, but with weighted regression
# Generate random sample from model (with fixed x), fit by weighted least
# squares
# Inputs: number of samples
# Presumes: x fixed outside function
# Outputs: errors in parameter estimates
wls.heterosked.example = function(n) {
y = 3-2*x + rnorm(n,0,sapply(x,function(x){1+0.5*x^2}))
fit.wls = lm(y~x,weights=1/(1+0.5*x^2))
# Return the errors
return(fit.wls$coefficients - c(3,-2))
}
# Calculate standard errors in parameter estiamtes over many replications
# Inputs: number of samples per replication, number of replications (defaults
# to 10,000)
# Calls: wls.heterosked.example
# Outputs: standard deviation of estimated intercept and slope
wls.heterosked.error.stats = function(n,m=10000) {
wls.errors.raw = t(replicate(m,wls.heterosked.example(n)))
# transpose gives us a matrix with named columns
intercept.sd = sd(wls.errors.raw[,"(Intercept)"])
slope.sd = sd(wls.errors.raw[,"x"])
return(list(intercept.sd=intercept.sd,slope.sd=slope.sd))
}
ols.heterosked.error.stats(10)
wls.heterosked.error.stats(10)