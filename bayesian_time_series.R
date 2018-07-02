library(bsts)     # load the bsts package
data(iclaims)     # bring the initial.claims data into scope
initial.claims

ss <- AddLocalLinearTrend(list(), initial.claims$iclaimsNSA)
ss <- AddSeasonal(ss, initial.claims$iclaimsNSA, nseasons = 52)
model1 <- bsts(initial.claims$iclaimsNSA,
               state.specification = ss,
               niter = 1000)
plot(model1$sigma.trend.slope)

plot(model1)
plot(model1, "components")  # plot(model1, "comp") works too!
plot(model1, "help")


pred1 <- predict(model1, horizon = 12)
plot(pred1, plot.original = 156)

# Fit a bsts model with expected model size 1, the default.
model2 <- bsts(iclaimsNSA ~ .,
               state.specification = ss,
               niter = 1000,
               data = initial.claims)


# Fit a bsts model with expected model size 5, to include more coefficients.
model3 <- bsts(iclaimsNSA ~ .,
               state.specification = ss,
               niter = 1000,
               data = initial.claims,
               expected.model.size = 5)  # Passed to SpikeSlabPrior.

plot(model2, "coef")
plot(model3, "coef")

bsts.prediction.errors(model1)

CompareBstsModels(list("Model 1" = model1,
                       "Model 2" = model2,
                       "Model 3" = model3),
                  colors = c("black", "red", "blue"))

ss1 <- AddLocalLinearTrend(list(), sp500)
model1 <- bsts(sp500, state.specification = ss1, niter = 1000)
pred1 <- predict(model1, horizon = 360)


ss2 <- AddSemilocalLinearTrend(list(), sp500)
model2 <- bsts(sp500, state.specification = ss2, niter = 1000)
pred2 <- predict(model2, horizon = 360, newdata = initial.claims)


plot(pred2, plot.original = 360, ylim = range(pred1))
plot(pred1, plot.original = 360, ylim = range(pred1))

## Because 'y' is 0/1 and the state is on the logit scale the default prior
## assumed by AddLocalLevel won't work here, so we need to explicitly set the
## priors for the variance of the state innovation errors and the initial value
## of the state at time 0.  The 'SdPrior' and 'NormalPrior' functions used to
## define these priors are part of the Boom package.  See R help for
## documentation.  Note the truncated support for the standard deviation of the
## random walk increments in the local level model.
ss <- AddLocalLevel(list(),
                    sigma.prior = SdPrior(sigma.guess = .1,
                                          sample.size = 1,
                                          upper.limit = 1),
                    initial.state.prior = NormalPrior(0, 5))


## Tell bsts that the observation equation should be a logistic regression by
## passing the 'family = "logit"' argument.
ts.model <- bsts(nber ~ ., ss, data = gdp, niter = 20000,
                 family = "logit", expected.model.size = 10)

ss <- AddSeasonal(ss, y, nseasons = 24)
ss <- AddSeasonal(ss, y, nseasons = 7, season.duration = 24)


library(tidyverse, quietly = TRUE)
library(bsts, quietly = TRUE)    
data(iclaims)
.data <- initial.claims
claims <- .data$iclaimsNSA
plot(claims, ylab = "")
(model_components <- list())

#Regressors
# ?AddAr
# ?AddAutoAr
# ?AddLocalLevel
# ?AddLocalLinearTrend
# ?AddStudentLocalLinearTrend
# ?AddGeneralizedLocalLinearTrend

summary(model_components <- AddLocalLinearTrend(model_components, 
                                                y = claims))

#Seasonal Comps

# ?AddTrig # Trigonometric seasonal
# ?AddSeasonal
# ?AddNamedHolidays
# ?AddFixedDateHoliday
# ?AddNthWeekdayInMonthHoliday
# ?AddLastWeekdayInMonthHoliday

summary(model_components <- AddSeasonal(model_components, y = claims, 
                                        nseasons  = 52))
fit <- bsts(claims, model_components, niter = 2000)


burnin <- 500 # Throw away first 500 
tibble(
  date = as.Date(time(claims)),
  trend = colMeans(fit$state.contributions[-(1:burnin),"trend",]),
  seasonality = colMeans(fit$state.contributions[-(1:burnin),"seasonal.52.1",])) %>%
  gather("component", "value", trend, seasonality) %>%
  ggplot(aes(x = date, y= value)) + 
  geom_line() + theme_bw() + 
  theme(legend.title = element_blank()) + ylab("") + xlab("") +
  facet_grid(component ~ ., scales="free") + guides(colour=FALSE) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

pred <- predict(fit, horizon = 100, burn = burnin, quantiles = c(.05, .95))
plot(pred)

errors <- bsts.prediction.errors(fit, burn = 1000)
PlotDynamicDistribution(errors)

fit2 <- bsts(iclaimsNSA ~ ., state.specification = model_components, 
             data = initial.claims, niter = 1000)

colMeans(fit2$coefficients)
