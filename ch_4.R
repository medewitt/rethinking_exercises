#Chapter 4 Examples

library(rethinking)

data("Howell1")

d <- Howell1

str(d)

#Only work with data that the age >= 18

d2 <- d[d$age >= 18,]

#this removed some observations


# model -------------------------------------------------------------------
# h ~ N(mu, sigma) Height model
# mu ~ N(178, 20)
# sigma ~ uniform(0,50)

#Plot priors to get a good understanding of distribution
curve(dnorm(x, 178, 20), from = 100, to = 250)

curve(dunif(x, 0, 50), from = -10, 60)

sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0 ,50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

#MAking the map model

flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~dnorm(178, 20),
  sigma ~ dunif(0,50)
)

m4.1 <- map(flist, d2)
precis(m4.1)
vcov(m4.1)
cov2cor(vcov(m4.1))


m4.3 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b* weight,
    a ~dnorm(156,100),
    b ~dnorm(0,10),
    sigma ~ dunif(0,50)
  ),
  data = d2
)
precis(m4.3, corr = TRUE)

#Notice a and b are correlated. This means same information is converyed. Best to center

d2$weight.c <- d2$weight - mean(d2$weight)

m4.4 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b* weight.c,
    a ~dnorm(156,100),
    b ~dnorm(0,10),
    sigma ~ dunif(0,50)
  ),
  data = d2
)
precis(m4.4, corr = TRUE)


#Now see the scatter

N<- 10
dN <- d2[1:N,]
mN <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b* weight,
    a ~dnorm(156,100),
    b ~dnorm(0,10),
    sigma ~ dunif(0,50)
  ),
  data = dN
)

#extract 20 from posterior

post <- extract.samples(mN, n = 20)
plot(dN$weight, dN$height, xlim = range(d2$weight), ylim = range(d2$height),
     col=rangi2, xlab="weight", ylab="height")
mtext(concat("N = ", N))

for(i in 1:20){
  abline(a = post$a[i], b = post$b[i], col = col.alpha("black", .3))
}

#Predict

mu_at_50 <- post$a + post$b * 50

dens(mu_at_50, col= rangi2, lwd = 2, xlab = "mu|weight = 50")

HPDI(mu_at_50, prob = .89)
mu <- link(m4.3)
plot(precis(m4.3))
