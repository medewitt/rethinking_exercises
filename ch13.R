#Multi LEvel Chapter 13

library(rethinking)

data("UCBadmit")
d <- UCBadmit

d$male <- ifelse(d$applicant.gender == "male", 1, 0)
d$dept_id <- coerce_index(d$dept)


m13.2 <- map2stan(
  alist(
    admit~dbinom(applications, p),
    logit(p) <- a_dept[dept_id] + bm*male,
    a_dept[dept_id] ~dnorm(a, sigma_dept),
    a ~dnorm(0,10),
    bm ~ dnorm(0,1),
    sigma_dept ~ dcauchy(0,2)
  ),
  data = d, warmup = 500, iter = 4500, chains = 3
)
precis(m13.2, depth = 2)

plot(precis(m13.2, depth = 2))


m13.3 <- map2stan(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) ~ a_dept[dept_id] + bm_dept[dept_id]*male,
    c(a_dept, bm_dept)[dept_id] ~dmvnorm2(c(a,bm), sigma_dept, Rho),
    a ~dnorm(0,10),
    bm~dnorm(0,1),
    sigma_dept~dcauchy(0,2),
    Rho ~dlkjcorr(2)
  ),
  data = d, warmup = 500, iter = 4500, chains = 3
)
plot(precis(m13.3, depth = 2))
