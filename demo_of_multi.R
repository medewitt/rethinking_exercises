
set.seed(336)

questions <- factor(c("Strongly agree", "Agree", "Neither", "Disagree", "Strongly Disagree"),
                    c("Strongly Disagree", "Disagree", "Neither", "Agree", "Strongly agree"))

# Establish the vectors of probabilties
p1 <- c(.25, .25, .05, .30, .15)
p2 <-c(.25, .20, .02, .33, .20)
sum(p1)
sum(p1+p2)

group_1 <- rbinom(5, 100, p1)
group_2 <- rbinom(5, 100, p2)

df <- data.frame(questions, group_1, group_2)

#Prep for MC
group_1_n <- sum(group_1)
group_2_n <- sum(group_2)

#Priors
a <- .1
b <- .1

#Iterations
sampz <- 100000

#Draw from posterior
group_1_posterior <-list()
for(i in 1:length(group_1)){
  group_1_posterior[[i]] <- rbeta(sampz,group_1[i]+a,group_1_n-group_1[i]+b)
  
}

group_2_posterior <-list()
for(i in 1:length(group_2)){
  group_2_posterior[[i]] <- rbeta(sampz,group_2[i]+a,group_2_n-group_2[i]+b)
  
}

par(mfrow = c(1,2))
hist(group_1_posterior[[1]], breaks =60, main = "Group 1-Strongly Disagree", xlim = c(0,.5))
abline(v = mean(group_1_posterior[[1]]), col = "red")
hist(group_2_posterior[[1]], breaks =60, main = "Group 2-Strongly Disagree", xlim = c(0,.5))
abline(v = mean(group_2_posterior[[1]]), col = "red")

#Helper Function
library(purrr)

summarise_posterior <- function(x,y){
  mu_1 <- map_dbl(x, mean)
  mu_2 <- map_dbl(y, mean)
  
  sd_1 <- map_dbl(x, sd)
  sd_2 <- map_dbl(y, sd)
  
  delta_<-list()
  for(i in 1:length(x)){
    delta_[[i]] <- x[[i]]-y[[i]]
    delta_mu <- map_dbl(delta_, mean)
    cohens <-delta_mu/ (map_dbl( delta_, sd))
  }
  
  out <- cbind(mu_1, mu_2, sd_1, sd_2,delta_mu, cohens)
  
  out
  }

#Summary Statz
statz <- summarise_posterior(group_1_posterior, group_2_posterior)

cbind(statz, p1, p2 )
