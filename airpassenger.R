data(AirPassengers)
library(bsts)
y <- log10(AirPassengers)
ss <- AddLocalLinearTrend(
  list(), ## No previous state specification.
  y) ## Peek at the data to specify default priors.
ss <- AddSeasonal(
  ss, ## Adding state to ss.
  y, ## Peeking at the data.
  nseasons = 12) ## 12 "seasons"
model <- bsts(y, state.specification = ss, niter = 1000)
plot(model)
plot(model, "help")
plot(model, "comp") ## "components"
plot(model, "resid") ## "residuals"

plot(model, "comp", same.scale = FALSE) ## "components"
?bsts
