#ch10

library(rethinking)
data("UCBadmit")

d <- UCBadmit

d$male <- ifelse(d$applicant.gender == "male", 1, 0)

m10.6 <- map(
  alist(
    admit ~dbinom(applications, p),
    logit(p) <- a + bm*male,
    a ~ dnorm(0,10),
    bm ~ dnorm(0,10)
  ),
  data = d
)

m10.7 <- map(
  alist(
    admit ~dbinom(applications, p),
    logit(p) <- a,
    a ~ dnorm(0,10),
    bm ~ dnorm(0,10)
  ),
  data = d
)

compare(m10.6, m10.7)

precis(m10.6)

post <- extract.samples(m10.6)
p.admit.males <- logistic(post$a + post$bm)
p.admit.females <- logistic(post$a)
diff.admit <- p.admit.males-p.admit.females
quantile(diff.admit, c(0.025, .5, .975))


#Add some lvels

d$dept_id <- coerce_index(d$dept)

m10.8 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a[dept_id],
    a[dept_id] ~ dnorm(0,10)
  ),
  data = d
)

m10.9 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a[dept_id] + bm*male,
    a[dept_id] ~ dnorm(0,10),
    bm ~ dnorm(0,10)
  ),
  data = d
)

#10.8 and 10.9 are clear winners
compare(m10.6, m10.7, m10.8, m10.9)

precis(m10.9, depth = 2)

m10.9stan <- map2stan(m10.9, chains = 2, iter = 2500, warmup = 500)

precis(m10.9stan, depth = 2)
pairs(m10.9stan)

post<- extract.samples(m10.9stan)
head(post)

glimm
