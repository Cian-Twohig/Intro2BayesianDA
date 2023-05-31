#----------------------------------Assignment 2--------------------------------------------------------

#Bayesian Updating

#1-----------------------------------------------------------------------------------------------------

#The probability of Company A's reviews being positive is 70%. 
prob_A <- 0.70

#The probability of Company B's reviews being positive is 50%.  
prob_B <- 0.50

#The probability of Company C's reviews being positive is 80%. 
prob_C <- 0.80

#These form the theta's of our bayesian updating, the input parameters as you will.
theta <- c(prob_A, prob_B, prob_C)

#Twice as probable that the reviews belong to company B than to A or C, resulting in a prior probability distribution of A = 0.25, B = 0.5, C = 0.25.
prior <- c(A = 0.25, B = 0.50, C = 0.25)

#6 are positive reviews, 4 are negative reviews. 

#A vector containing 6 positive reviews, denoted with "P".
positive_reviews <- rep("P", 6)

#A vector containing 4 negative reviews, denoted with "N".
negative_reviews <- rep("N", 4)

#All new observations (reviews) used to update the prior in one vector: 
total_reviews <- c(positive_reviews, negative_reviews)

#Computing post similarly to the R-files throughout the cours, using Bayes' Law. 

compute_post <- function(obs, poss){ 
  n <- length(total_reviews)  #the nunber of elements in total_reviews vector.
  P <- sum(obs=="P")          #the sum of positive reviews in the total_reviews vector.
  
  #Using the binomial function to see how likely the distribution of positive reviews is given the probability of positive reviews per company.
  #Using dbinom the likelihood of observing so many "P" positive reviews given the number of observations "n" and the theta probability of success.
  likelihood <- dbinom(P, n, theta)
  
  #According to Bayes' Law:
  post <- likelihood * prior
  post <- post / sum(post)
  
  return(post)
}

post <- compute_post(total_reviews, prior)
post

#The posterior distribution does indeed equal 0.29 for company A!   
round(post, 3)

#2-----------------------------------------------------------------------------------------------------------
#We now wish to update the posterior once more, with the newly available review data:
more_reviews <- c(rep("P", 9), rep("N", 1))

#Vector containing the 9 new positive reviews, and one negative review.
more_reviews

#Using the same recipe to calculate the posterior distribution, only now using the previously calculated 
#posterior distribution as the input distribution to "update". 

compute_updated_post <- function(obs, prev_post) {
  n <- length(obs)  #Length of new review vector (10 in this case).
  P <- sum(obs == "P")  #The total number of positive reviews in the new review vector (9 in this case).
  likelihood <- dbinom(P, n, theta) #Calculating the likelihood the of the data given the theta parameters.
  post <- likelihood * prev_post
  post <- post / sum(post)
  
  return(post)
}

#Based on the 10 new reviews, the previous posterior is updated to an updated posterior distribution.
updated_post <- compute_updated_post(more_reviews,  post)
updated_post

#The posterior probability that company C received the reviews has indeed risen by 33%:
updated_post - post

#3-----------------------------------------------------------------------------------------------------------

#There are two separate factories, A and B. The probability of a delivery coming from A or B is equal, namely both 50%.

P_A <- 0.50
P_B <- 0.50

#So, thinking logically and applying Bayes' Law, there are 4 scenario's in total:
#i) Delivery arrives from Factory A and is NOT defect (ND).
#ii) Delivery arrives from Factory B and is NOT defect (ND).
#iii) Delivery arrives from Factory A and is defect (D).
#iv) Delivery arrives from Factory B and is defect (D).

#So the total probability of all possible outcomes adds up to one, see below:
#P(ND|A) + P(D|A) + P(ND|B) + P(D|B) = 1

#So, we want to calculate the probability of iii) & iv), that the delivery arrives and contains defective products:
#P(D) = P(D | A) + P(D | B) 

#The factories are as good as identical, differing only in their defect rates (DR).

#Probability of the delivery being defect, given company A: 
Prob_Def_A <-  0.10

#Probability of the delivery being defect, given company B: 
Prob_Def_B <-  0.20

#So, logically, the probability of the next shipment being defect is the probability of the shipment being defect, times the probability of the shipment coming from said company.
#P(D) = P(D|A) * P(A) + P(D|B) * P(B)
#The defective rate is said to be certain based on historical data. 

Prob_Defective <- Prob_D_A * P_A + Prob_D_B * P_B 
Prob_Defective

#The probability of the a delivery being defective is 15%.
#This was calculated using Bayes' Law and basic probability theory. An important assumption is that the defective rates are constant,
#and that deliveries are equally likely to come from both factories. 

#However, to calculate the probability that the next shipment will contain defective products GIVEN that
#the initial delivery did contain defective products, we must calculate the probability of event using
#conditional probability where Event #1 is receiving a defective delivery (def_1), Event #2 is receiving a defective delivery AGAIN! (def_2). 
#What is the probability of Event 2 occurring, given that event 1 has occurred?

#The formula of Bayes' Conditional Probability: P(A | B) = P(B | A) * P(A) / P (B)
#P(A | B) = P(A union B) / P(B)
#P(Def_2 | Def_1) = P(Def_2 union Def_1) / P(Def_1)
#P(Def_2 union Def_1) = (P(Def_2 | Def_1, A) + P(Def_2| Def_1, B)) 
#P(Def_2 | Def_1, A) = P(Def_2 | A) *  P(Def_1 | A) * P(A)
#P(Def_2| Def_1, B) = P(Def_2 | B) *  P(Def_1 | B) * P(B)

#Filling it all in: #P(Def_2 | Def_1) = (P(Def_2 | A) *  P(Def_1 | A) * P(A) + P(Def_2 | B) *  P(Def_1 | B) * P(B)) / P(Def_1)
#Remember P(Def_2 | A) = P(Def_1 | A) = 0.1 = Prob_D_A
#P(Def_2 | B) = P(Def_1 | B) = 0.2 = Prob_D_B 

#As stated above, the probability of the initial delivery being defect:
P_Def1 = Prob_D_A * P_A + Prob_D_B * P_B #(= 0.15, Probability of Defectiveness as calculated before, independent of any previous events)

P_Def2_Def1 = (Prob_D_A * Prob_D_A * P_A + Prob_D_B * Prob_D_B * P_B) / P_D1
round(P_Def2_Def1, 3) #(= 0.167)

#So given that the initial delivery is defective, using Bayes' Law we have calculated that the probability of the next delivery being defective to be 16.7%. 
#Our beliefs of the probability of receiving a defective product is updated based on the evidence of the initial defective delivery, thus providing a different end result. 

#5-----------------------------------------------------------------------------------------------------------
#The initial probability of a delivery coming from Company A or B was equal, so 50-50. 
P_A <- 0.50
P_B <- 0.50

#The probability of the algorithm correctly recognizing a delivery from Factory A = 0.80 (test is positive for A).
#The probability of the algorithm correctly recognizing a delivery from Factory B = 0.65 (test is positive for B).

#The test for Factory A is positive! 

#P(Positive | A) = 0.80
#P(Positive | B) = 0.65

P_Positive_A = 0.80
P_Positive_B = 0.65

#P(A | Positive) --> Probability of the delivery coming from company A, if the test is positive. 
#P(A | Positive) = P(Positive | A) * P(A) / P(Positive)
#P(Positive) = P(Positive | A) * P(A) + P(Positive | B) * P(B)

#P(A | Positive) = P(Positive | A) * P(A) / (P(Positive | A) * P(A) + P(Positive | B) * P(B))
P_A_Positive = (P_Positive_A * P_A) / ((P_Positive_A * P_A) + (P_Positive_B * P_B))
P_A_Positive

#The posterior probability that the shipment is from Factory A given the output of the algorithm is 55.2%. 




#Redo your calculation, using both information, the fact that there are defected products and the fact that the algorithm is positive for Factory A.

#P(A | Positive, Defect) = P(Positive, Defect | A) * P(A) / P(Positive, Defect)

P_A <- 0.50
P_B <- 0.50

P_Positive_A <- 0.80
P_Positive_B <- 0.65

P_Defect_A <- 0.10
P_Defect_B <- 0.20

Prob_Defective #Probability of receiving a defective product = 0.15.
P_Def2_Def1 #probability of receiving a defective product again, after initial delivery was defective = 0.167.

# P(Positive, Defect | A) = P(Positive | A) * P(Defect | A) (assuming the machine learning algorithm and defect rate are completely independent).
# P(Positive, Defect | B) = P(Positive | B) * P(Defect | B)

#The probability of the product being both positive and defect (coming from a certain company):

P_Pos_Def_A <- P_Positive_A * P_Defect_A
P_Pos_Def_A

P_Pos_Def_B <- P_Positive_B * P_Defect_B
P_Pos_Def_B 

#The probability of the product being both positive and defect P(Positive, Defect) = P(Positive, Defect | A) * P(A) + P(Positive, Defect | B) * P(B):
P_Pos_Def <- P_Pos_Def_A * P_A + P_Pos_Def_B * P_B
P_Pos_Def

#Substitute all calculated values into: P(A | Positive, Defect) = P(Positive, Defect | A) * P(A) / P(Positive, Defect) to find the posterior probability
#that the shipment is from company A:

P_A_Pos_Def <- P_Pos_Def_A* P_A / P_Pos_Def
round(P_A_Pos_Def, 3)

#The posterior probability of the product coming from company A given that it is both defective and has tested positive of the machine learning algorithm,
#is 38.1%. 


#-----------------------------------------------------------------------------------------------------------------------------------------------------

#Bayesian Workflow

#6---------------------------------------------------------------------------------------------------------------------------------------------------
#If I am not mistaken, there is more land surface covered by water than by land. If I were to give an educated guess,
#I would say that a proportion of 30 - 40% of the world's surface is covered with land. 
#The Beta distribution is commonly used a prior, and in this case it would make sense as the beta distribution ranges from [0, 1] and
#this suits the question of gauging the land proportion (also measured in a range of 0 to 1).

#Based on RStudio file session_4_workflow and modified accordingly:

library(ggplot2) #Load in ggplot2 library for plotting.

range <- seq(0, 1, length.out = 1000) #Creating a vector of values on which the beta distribution can be evaluated.
range

?dbeta

#Beta distribution in which alpha = 3, beta = 5. 
distribution <- dbeta(range, shape1 = 3, shape2 = 5) 
#Using these parameters, the median of the distribution is ~35% land proportion, which suits our previous hypothesis quite well. This is a decent prior distribution.

#Need dataframe to plot:                                                     
beta <- data.frame(range, distribution)

#Plotting, changing labels and line thickness. 
ggplot(beta, aes(x = range, y = distribution)) + geom_line(size = 1.5) + labs(x = "Land Proportion",  y = "Probability Density") + theme_minimal()

#Is Github working now? 

