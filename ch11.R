#Chapter 11 Ordered Categorical

library(rethinking)

d <- Trolley

simplehist(d$response, xlim = c(1,7), xlab = "response")

#discrete proportion of each response value
pr_k <- table(d$response)/nrow(d)

#cumsum to cummulative prorportions

cum_pr_k <- cumsum(pr_k)

plot(1:7, cum_pr_k, type = "b", xlab = "response", ylab = "cumulative proportion", ylim = c(0,1))

logit <- function(x)log(x/(1-x))

(lco <- logit(cum_pr_k))

#Ordered Model
# R~Ordered(p)
# logit(pk)
# alpha ~Normal(0,10)

m11.1 <- map(
  alist(
    response ~ dordlogit(phi, c(a1,a2,a3,a4,a5,a6)),
    phi <-0,
    c(a1,a2,a3,a4,a5,a6) ~dnorm(0,10)
  ),
  data = d,
  start = list(a1 = -2, a2 = -1, a3 = 0, a4 = 1, a5 =2, a6 = 2.5)
)

precis(m11.1)
#To get cummulative probabilties back
logistic(coef(m11.1))

m11.1stan <- map2stan(
  alist(
    response ~dordlogit(phi, cutpoints),
    phi <- 0,
    cutpoints ~dnorm(0,10)
  ),
  data = list(response = d$response),
  start = list(cutpoints = c(-2,1,0,1,2,2.5)),
  chains = 2, cores = 2
)
precis(m11.1stan, depth = 2)

str(d)
