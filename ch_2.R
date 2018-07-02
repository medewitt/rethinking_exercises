#Explore Chapter 2

library(rethinking)

globe.qa <- map(
  alist(
    w ~ dbinom(9,p), #Binomial Liklihood
    p ~ dunif(0,1) #uniform prior
  ),
  data = list(w=6)
)

#Display summay of quadratic approximation

precis(globe.qa)

#Analytical Calculation of Above

w <- 6
n <- 9
curve(dbeta(x, w+1, n-w+1), from = 0, to = 1)

#Quadratic Approximaton
curve(dnorm(x, 0.67, .016), lty = 2, add= TRUE)

