#2.1.
x <- runif(100, 1, 100)
mean_x <- round(mean(x), 2)
x
?runif()

#Value changes as the 100 values are randomly distributed and therefore the mean changes
#after every iteration of the code. 

x
class(x)
mean(x)

#to get the same value every time, you can set a seed (which 'fixes' the randomization)


set.seed(12345)
x <- runif(100, 1, 100)
mean_x <- round(mean(x), 2)
x


#2.2

x <- (1:1000)
x_alt <- seq(1, 1000)

y <- seq(999, 0, by = 1)

z = x_alt + y
z

#2.3
n <- 1e4
scale <- 1.5e4
income <- round( rbeta(n=n, shape1=3, shape2=8) * scale, 2) 

#According to economists, income typically follows a log-normal distribution.
#Changed the rbeta to reflect income distribution in the population more accurately, in accordance to:
#Is income a lognormal distribution?
#The traditional parametrization of the income distribution is log normal with a thick, Pareto upper tail. The classic explanation for log normality of income is Gibrat's (1931) law, which essentially models income as an accumulation of random multiplicative shocks.

library(ggplot2) # only load (run) once

# Plot the resulting curve
ggplot(data.frame(x = income), aes(x=x)) +
  geom_histogram(color = "#0065BD", fill = "#0065BD", alpha=0.5, bins = 100) +
  scale_x_continuous(breaks = seq(0, scale, 1e3)) + 
  labs(x = "Gross income", 
       y = "Counts") + 
  theme_minimal()

income_sum <- sum(income) #total income across the population
income_share <- income / income_sum #each individuals share of the summed income across the population

#More comprehensive code:

?sort
income_s <- sort(income) #sorts income

group <- c("Lower 1%", "Lower 50%", "Top 10%", "Top 1%") #splits the income based on different groups
p <- c(.1, .5, .9, .99)

boundary <- round(income_s[p*n], 0) #sets boundary for groups at a specific individual

low10_m <- mean( income_s[c(1:(.1*n))] ) #Calculates the mean income for the lowest 10% of the population
low50_m <- mean( income_s[c(1:(.5*n))] ) #Calculates the mean income for the lower 50% of the population
top10_m <- mean( income_s[c((.9*n):n)] ) #Calculates the mean income for the top 10% of the population
top1_m <- mean( income_s[c((.99*n):n)] ) #Calculates the mean income for the top 1% of the population

means <-  round( c(low10_m, low50_m, top10_m, top1_m) , 0) #puts the (rounded) means income of the groups specified above in a vector

income_summary <- data.frame(group, boundary, means)
income_summary

##       group boundary means
## 1  Lower 1%      618   398
## 2 Lower 50%     1865  1073
## 3   Top 10%     4014  4979
## 4    Top 1%     6125  6737

#Exercise 3
df <- data.frame(c(5,4,3,2,1), c(10,9,8,7,6), c(1,3,5,7,9), c(9, 7, 5, 3, 2), c(3, 77, 7, 4, 6))
means <- c(mean(df[, 1]), mean(df[, 2]), mean(df[, 3]), mean(df[, 4]), mean(df[, 5]))
means_2<- apply(df, 2, mean)

library(tidyverse)
names(diamonds)
View(diamonds)

library(dplyr)

#which, filter, select

?which
ideal_diamonds <- which(diamonds$cut == 'Ideal') #returns indices of ideal diamonds in vector form

?filter
ideal_diamonds_2 <- filter(diamonds, cut == 'Ideal') #returns dataframe containing all diamonds (and variables) of an ideal cut

?select
dimensions <- ideal_diamonds_2 %>% select(x, y, z)

       
