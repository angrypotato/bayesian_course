library(tidyverse)
library(fields)

# 3.1.b
post_theta_df <- NULL 
for (theta in seq(0,1,0.1)) {
  prob <- choose(100, 57) * theta^57 * ((1 - theta)^(100 - 57))
  post_theta_df <- rbind(post_theta_df, data.frame(theta = theta, probability = prob))
}
post_theta_df %>%  ggplot() +
  geom_point(aes(x = theta, y = probability)) +
  geom_line(aes(x = theta, y = probability)) +
  labs(title = "probabilities under different theta", x = "theta", y = "probability")


# 3.1.c
post_theta_df <- NULL 
prior_theta <- 1/11
marginal_density <- 0
probability <- NULL
for (theta in seq(0,1,0.1)) {
  prob <- choose(100, 57) * theta^57 * ((1 - theta)^(100 - 57))
  probability <- rbind(probability, data.frame(theta = theta, probability = prob))
  marginal_prob <- prob*prior_theta
  marginal_density <- marginal_density + marginal_prob
  post_theta_df <- rbind(post_theta_df, data.frame(theta = theta, probability = prob))
}

post_theta_df <- post_theta_df %>%
  mutate(post_prob = (probability * 1/11)/marginal_density)

post_theta_df %>%  ggplot() +
  geom_point(aes(x = theta, y = post_prob)) +
  geom_line(aes(x = theta, y = post_prob)) +
  labs(title = "posterior distribution", x = "theta", y = "posterior probability")


# 3.1.d
post_theta_df <- NULL 
prior_theta <- 1
for (theta in seq(0,1,0.001)) {
  prob <- prior_theta * choose(100, 57) * theta^57 * ((1 - theta)^(100 - 57))
  post_theta_df <- rbind(post_theta_df, data.frame(theta = theta, probability = prob))
}

post_theta_df %>%  ggplot() +
  geom_line(aes(x = theta, y = probability)) +
  labs( x = "theta", y = "proprotinal to posterior density")


# 3.1.e
theta <- seq(0,1,0.001)
density <- dbeta(x=theta, 1+57, 1+100-57)
post_theta_df <- data.frame(theta = theta, density = density)
post_theta_df %>%  ggplot() +
  geom_line(aes(x = theta, y = density)) +
  labs(title = "posterior density", x = "theta", y = "probability density")


# 3.2
y <- 57
total <- 100
w <- 0.5
theta0 <- seq(0.1, 0.9, 0.1)
n0 <- c(1, 2, 8, 16, 32)
prior.a = post.a = prior.b = post.b = post.prob.theta = matrix(0,length(theta0),length(n0))

for(i in 1:length(theta0)){
  for(j in 1:length(n0)){
    prior.a[i, j] <- theta0[i] * n0[j]
    prior.b[i, j] <- (1 - theta0[i]) * n0[j]
    
    post.a <- prior.a[i, j] + y
    post.b <- prior.b[i, j] + total - y
    post.prob.theta[i, j] <- 1 - pbeta(w, post.a, post.b)
  }
}

image.plot(theta0, n0, post.prob.theta,
           xlab = "theta0", ylab = "n0",
           main = "p(theta > 0.5 | y)", col = heat.colors(100)[1:90])

contour(theta0, n0, post.prob.theta, add=TRUE)


# 3.4.a
theta <- seq(0, 1, 0.001)
prior <- ggplot() +
  geom_line(aes(x = theta, y = dbeta(theta, 2, 8))) +
  labs(title = "prior distribution", x = "theta",y = "Probability Density")
sample_model <- ggplot() +
  geom_line(aes(x = theta,y = choose(43, 15) * theta^15 * (1 - theta)^(43 - 15))) +
  labs(title = "Probability Distribution of y given theta", x = "theta", y = "Probability")
post <- ggplot() +
  geom_line(aes(x = theta, y = dbeta(theta, 2 + 15, 8 + 43 - 15))) +
  labs(title = "Posterior distribution of theta", x = "theta", y = "Probability Density")

lower <- qbeta(0.025, 17, 36, ncp = 0, lower.tail = TRUE, log.p = FALSE)
upper <- qbeta(0.975, 17, 36, ncp = 0, lower.tail = TRUE, log.p = FALSE)


# 3.4.b
theta <- seq(0, 1, 0.001)
prior <- ggplot() +
  geom_line(aes(x = theta, y = dbeta(theta, 8,2))) +
  labs(title = "prior distribution", x = "theta",y = "Probability Density")
sample_model <- ggplot() +
  geom_line(aes(x = theta,y = choose(43, 15) * theta^15 * (1 - theta)^(43 - 15))) +
  labs(title = "Probability Distribution of y given theta", x = "theta", y = "Probability")
post <- ggplot() +
  geom_line(aes(x = theta, y = dbeta(theta, 8 + 15, 2 + 43 - 15))) +
  labs(title = "Posterior distribution of theta", x = "theta", y = "Probability Density")

lower <- qbeta(0.025, 23, 30, ncp = 0, lower.tail = TRUE, log.p = FALSE)
upper <- qbeta(0.975, 23, 30, ncp = 0, lower.tail = TRUE, log.p = FALSE)


# 3.4.c
theta_prior_df <- NULL
for (theta in seq(0,1,0.001)) {
  beta1 <- dbeta(theta, 2, 8) 
  beta2 <- dbeta(theta, 8, 2) 
  beta3 <- (3 * beta1 + beta2) / 4
  theta_prior_df <- rbind(theta_prior_df,data.frame(theta = theta,beta1 = beta1,beta2 = beta2, beta3 = beta3))
}

color_legend = c("Beta(2, 8)" = "red",
                 "Beta(8, 2)" = "green",
                 "mixed beta" = "blue")

ggplot(data = theta_prior_df, aes(x = theta)) +
  geom_line(aes(y = beta1, color = "Beta(2, 8)")) +
  geom_line(aes(y = beta2, color = "Beta(8, 2)")) +
  geom_line(aes(y = beta3, color = "mixed beta")) +
  scale_color_manual(values = color_legend) +
  labs(title = "Density distribution of different prior",
       x = "theta",
       y = "Probability Density")


# 3.4.d.iii.
combine_theta_df <- NULL
for (theta in seq(0,1,0.001)) {
  beta1 <- dbeta(theta, 2, 8) 
  beta2 <- dbeta(theta, 8, 2) 
  prior_theta <- (3 * beta1 + beta2) / 4
  sample_model <- choose(43, 15) * theta^15 * (1 - theta)^(43 - 15)
  prob <- prior_theta*sample_model
  
  combine_theta_df <- rbind(combine_theta_df,data.frame(theta = theta,prob = prob))
}
combine_theta_df %>%
  ggplot() +
  geom_line(aes(x = theta, y = prob)) +
  labs(x = "theta",y = "Probability")

combine_theta_df$theta[which(combine_theta_df$prob == max(combine_theta_df$prob))]
