#Cian Twohig, 03769205
#Introduction to Bayesian Data Analysis: Assignment 1

rm(list = ls())

library(tidyverse)

#2 Dice

#1

dice1 <- c(1, 2, 3, 4, 5, 6) #Declaring first dice, obviously has six possible outcomes in the sample space
dice2 <- c(1, 2, 3, 4, 5, 6) #Declaring second dice

dice1 #Checking output of both dice
dice2

is.vector(dice1) #Checking if dice is a vector, just to be sure
class(dice1) #Checking the contents of the dice (numeric)

#2

?expand_grid() #Checking what the expand grid function does

#Using the expand_grid() function to create a tibble 
#consisting of all possible combinations of dice1 and dice 2 as outcomes.
outcomes <- expand_grid(dice1, dice2) 
outcomes
tail(outcomes)

#3

#Each possible combination of dice has the same probability of occuring. 
#Therefore, to specific the probability of each outcome, a column can be added simply dividing
#the total proability (= 1) by the total number of possible outcomes (36).

outcomes$prob <- (1 / count(outcomes))
outcomes

#Renamed column names for legibility etc.
colnames(outcomes) <- c('dice1', 'dice2', 'probability')
outcomes

#4

#Create sum of dice1 outcome and dice2 outcome by simply summing the first and second column.
outcomes$sum <- outcomes$dice1 + outcomes$dice2
outcomes

#adding column name for completeness
colnames(outcomes)[4] <- 'sum'

#5

?subset() #subset(x, subset, select, drop = FALSE, ...), select subsets of dataframe using subset, select for columns
?filter() #filter(.data, ..., .by = NULL, .preserve = FALSE), filter dataframe based on logical experession

#Using conditional probability to calculate probability where dice1 = 3, dice1 + dice2 >= 7.
#probability(Event2 | Event1) = P(E1 & E2) / P(E1)

#Subset of outcomes where dice1 = 3 and the sum of outcomes >= 7.
P_E1_E2_subset <- subset(outcomes, outcomes$dice1 == 3 & outcomes$sum >= 7)

#Subset of outcomes where dice1 = 3.
P_E1_subset <- subset(outcomes, outcomes$dice1 == 3)

#Execute conditional probability formula:
cond_prob <-sum(P_E1_E2_subset$probability) / sum(P_E1_subset$probability)

#The probability of the sum of two die being >= 7, given that the first dice equals 3, is 50%. 

#6

#Create a subset of the possible outcomes with a sum between 4 and 9:
sum_4to9 <- subset(outcomes, outcomes$sum >= 4 & outcomes$sum <= 9)

#sum the probability of these outcomes occuring:
sum(sum_4to9$probability)

#The probability of the sum of dice being between 4 and 9 is 75%. 

#7 What is the probability of the most probable sum?

#Table to give an indication of the how frequently certain values occur.
table(outcomes$sum)

#which.max(table()) to see which value occurs most often (7 in this case).
which.max(table(outcomes$sum))

#An alternative would be to write a function for the mode of a column, 
#unfortunately a built in mode() function does not exist in R. 

#for simplicity, I simply sum the probability of the sum of outcomes being equal to 7 below (I know it could be "fancier" to generalise to all dataframes).
sum(subset(outcomes, outcomes$sum == 7)$probability)

group_by(outcomes, outcomes$sum)

count(outcomes, sum)
max(count(outcomes, sum))
count(outcomes, sum)








