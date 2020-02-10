
# A common question is "How big should my sample be?". There's no simple answer,
#   but simulations can help you to decide.

# Earlier in the workshop, we simulated data for squirrel weights with sample size n = 10.
# What would happen if we used a smaller sample, or a bigger sample?
#   How good would our estimates be?

# The biological model
# ====================
# We will assume that squirrel weights are normally distributed, with mean
mu <- 181  # mean
sigma <- 11 #standard deviation

n <- 5  # sample size, we'll try different values later
( x <- rnorm(n, mu, sigma) )  # Produces a different sample each time we run it.
mean(x)
sd(x)

iter <- 1000  # The number of iterations = number of samples to generate
estimate5 <- numeric(iter)  # Create a store to hold all the estimates.

for(i in 1:iter)  {
  x <- rnorm(5, mu, sigma)
  estimate5[i] <- mean(x)
}

#############################
n <- 25  # sample size, we'll try different values later
( x <- rnorm(n, mu, sigma) )  # Produces a different sample each time we run it.
mean(x)
sd(x)

iter <- 1000  # The number of iterations = number of samples to generate
estimate25 <- numeric(iter)  # Create a store to hold all the estimates.

for(i in 1:iter)  {
  x <- rnorm(25, mu, sigma)
  estimate25[i] <- mean(x)
}

library(beeswarm)
# The beeswarm function needs a data frame as its input
df<-data.frame(n5=estimate5, n25=estimate25)

par(mfrow=c(1,2))
hist(estimate5, main="n = 10")
abline(v = mu, col='red')
hist(estimate25, main="n = 10")
abline(v = mu, col='red')


beeswarm(df, method='hex', cex=0.5, col='blue', las=1,
         xlab="Sample size", ylab="Estimated mean weight of squirrels")
abline(h = mu, col='red', lwd=2)


mean(abs(estimate5 - mu) > 5)
mean(abs(estimate25 - mu) > 5)
abline(h = c(mu - 5, mu + 5), col='red', lty=2)





iter <- 1000  # The number of iterations = number of samples to generate
estimate10 <- numeric(iter)  # Create a store to hold all the estimates.

for(i in 1:iter)  {
  x <- rnorm(10, mu, sigma)
  estimate10[i] <- mean(x)
}

head(estimate10)  # Look at the first 6 values
hist(estimate10)  # Display all 1000

# Check the mean of the estimates
mean(estimate10)
  # if our method is unbiased, this should be close to the true value.
  
# Try different sample sizes
# ==========================

## B ## Generate 1000 samples with n=4.
# Put the result in an object called 'estimate4' and check for bias.
# HINT: Copy-paste the code from section A and then change it.
iter <- 1000  # The number of iterations = number of samples to generate
estimate4 <- numeric(iter)  # Create a store to hold all the estimates.

for(i in 1:iter)  {
  x <- rnorm(4, mu, sigma)
  estimate4[i] <- mean(x)
}

## C ## Generate 1000 samples with n=30.
# Put the result in an object called 'estimate30' and check for bias.

iter <- 1000  # The number of iterations = number of samples to generate
estimate30 <- numeric(iter)  # Create a store to hold all the estimates.

for(i in 1:iter)  {
  x <- rnorm(30, mu, sigma)
  estimate30[i] <- mean(x)
}


# Compare the estimates
# =====================

# Method 1: look at histograms
# ----------------------------
par(mfrow = c(3, 1))  # Put 3 histograms into the same plot
hist(estimate10)
hist(estimate4)
hist(estimate30)
##### Add the histograms for n=4 and n=30.

# Which histogram is narrowest?
# ...not easy to see when the x axes are different.

# For comparison, give them all the same scale on the x axis (and proper titles)
#  and add the true value
hist(estimate10, xlim=c(700, 1300), main="n = 10")
abline(v = mu, col='red')
hist(estimate4, xlim=c(700, 1300), main="n = 10")
abline(v = mu, col='red')
hist(estimate30, xlim=c(700, 1300), main="n = 10")
abline(v = mu, col='red')
##### Add the histograms and red lines for n=4 and n=30.

par(mfrow = c(1, 1))  # Change back to a single plot when we have finished.

# Method 2: Calculate errors
# --------------------------
# Since we know the true value, we can calculate the errors, and summarise these as
#   Root Mean Square Error (RMSE)
sqrt(mean((estimate10 - mu)^2))
##### Do the same for n=4 and n=30
sqrt(mean((estimate4 - mu)^2))
sqrt(mean((estimate30 - mu)^2))

# Which has the lowest RMSE?

# Method 3: Beeswarm plots
# ------------------------
library(beeswarm)
# The beeswarm function needs a data frame as its input
df<-data.frame(n4=estimate4, n10=estimate10, n30=estimate30)




##### df <- data.frame(n4 = estimate4, n10 = estimate10, n30 = estimate30)
head(df)


#-----------------------

beeswarm(df)


par(mfrow=c(1,1))
# Ok... now let's make that prettier
beeswarm(df, method='hex', cex=0.5, col='blue', las=1,
  xlab="Sample size", ylab="Estimated mean weight of squirrels")
abline(h = mu, col='red', lwd=2)

# Which sample size is best? Why?
#n30 has a more narrow distribution, 
#which indicates better accuracy


# Decisions, decisions!
# =====================
# Bigger samples give more accurate estimates, but are also more expensive.
# How accurate do you need to be? How much money can you spend?


# Suppose we wanted to be within 100g of the true value, ie, between 900 and 1100g
# Add these to the beeswarm plot:
abline(h = c(mu - 10, mu + 10), col='red', lty=2)

# What's the probability that we will get an estimate with an error greater than 100g?
mean(abs(estimate10 - mu) > 100)
# Do the same for n=4 and n=30
mean(abs(estimate4 - mu) > 100)
mean(abs(estimate30 - mu) > 100)

# How robust is your decision?
# ============================
# Try running the code above with different values of mu and sigma:
#  a. How would your decision be affected if mu = 1200g instead of 1000?

#########################################################

#  b. How would your decision be affected if sigma = 75g instead of 150?


###########################################################
#  c. How would your decision be affected if sigma = 300g instead of 150?


