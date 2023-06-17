#Assignment 3
#Cian Twohig, 03769205

rm(list = ls())
library(tidyverse)
library(rethinking)

#1-------------------------------------------------------------------------------------------- 

#Loading the data and saving it in an object "df"

df <- read_csv("aging.csv")

#2--------------------------------------------------------------------------------------------

#Delete all rows containing missing values by using na.omit() function
df <- na.omit(df)

#3--------------------------------------------------------------------------------------------

#Checking descriptives and basic plots
summary(df)


#Using histogram to visualise variable distributions
hist(df$Age)
plot(df$Age)
barplot(df$Age)

#Interestingly, there seems to be a clear split in the age distribution (young vs old)

hist(df$RiskSeeking)
#Risk Seeking is reasonably normally distributed. 

hist(df$DecisionQuality)
#Decision Quality seems high gaussian. 

hist(df$Speed)
hist(df$NegAffect)
#NegAffect is left skewed (not a lot of people in a negative emotional state, thankfully! )

hist(df$Numeracy)
#Right skewed: generally the numeracy variable was high

#Splitting age into two groups:

#Seeing as the data has a clear group split (between Age 30 and Age 63 there lies the age gap of participants),
#We shall split the age groups into people under 40 years old and over 40 years old
df$AG <- ifelse(df$Age < 40, 1, 2)

#Checking by plotting:

barplot(df$AG)

#4--------------------------------------------------------------------------------------------

#Time to build some Gaussian models! We are looking to apply quadratic approximation to approximate the posterior distributions.

#Judging by the earlier plots of 'Decision Quality' and 'Risk Seeking', 

plot(density(df$DecisionQuality))
plot(density(df$RiskSeeking))

#After checking the plots once again, although not perfect, the variables seem to follow a Gaussian distribution.
#For the mean we shall use a weakly informative normal prior distribution, allowing us to incorporate some prior knowledge on the mean distribution
#based on the previous plots, assuming that the means of posterior likely corresponds somewhat to the prior.
#A possible prior distribution for decision Quality is an approximate mean of 0.6 and an approximate standard deviation of 0.15, which seems plausible given the plots. 
#The mean of Risk Seeking appears to lie at around 0.5 with a larger standard deviation then Decision Quality. 
#The sigma of the Decision Quality variable will be modeled using a uniform distribution between 0 and 0.2, 
#whereas the seemingly larger stdev of RiskSeeking modelled with uniform distributions between 0 and 0.3 (standard deviations obviously have to be > 0).

model_decqual <- quap(
  alist(DecisionQuality~ dnorm(mu, sigma),
        mu ~ dnorm(0.6 , 0.15),
        sigma ~ dunif(0 , 0.2)),
  data = df)
precis(model_decqual)

model_riskseek <- quap(
  alist(RiskSeeking~ dnorm(mu, sigma),
        mu ~ dnorm(0.5 , 0.2),
        sigma ~ dunif(0 , 0.3)),
  data = df)
precis(model_riskseek)

#To assess the uncertainty of the parameters and evaluate the model fit, we shall now use 
#the extract.samples() and the HPDI() function from the rethinking package to obtain the 95% highest posterior density intervals for the mean/mu parameter. 

#Extracting the (1000) samples:
DecQual_smp <- extract.samples(model_decqual, n = 1e3)
RiskSeek_smp <- extract.samples(model_riskseek, n = 1e3)

#Plotting the sample distribution to see if the model is a reasonable fit:

DecQual_smp %>%  ggplot(aes(x = mu)) +
  geom_density(color = "#552583", linewidth = 1, alpha = .1) +
  labs(x = expression(mu), 
       y = "Density") +
  theme_minimal()

RiskSeek_smp %>%  ggplot(aes(x = mu)) +
  geom_density(color = "#552583", linewidth = 1, alpha = .1) +
  labs(x = expression(mu), 
       y = "Density") +
  theme_minimal()

#Estimating the posterior interval for the mu parameter to check model precision using HPDI()
HPDI(DecQual_smp$mu, prob = 0.95)

#Fairly compact interval, ranging between ~ 0.63 to ~ 0.67

HPDI(RiskSeek_smp$mu, prob = 0.95)

#Once again a fairly compact interval, ranging between ~ 0.45 to ~ 0.49

#The results indicate that generally, around 65% of the choices made in the survey chose the option with the higher expected payoff, as indicated by the mu of decision quality.
#An estimated sigma of 0.09 for decision quality indicates relatively low variability, meaning that generally, with a large degree of confidence, participants chose the option with the higher expected payoff.

#In contrast, an estimated mu coefficient of 0.47 for the variable 'Risk Seeking' indicates that less than half of the observations were risk seeking, i.e., choosing the more unlikely option.
#You could interpret this as humans generally being riskaverse, or atleast not riskseeking.
#A sigma here of 0.09 also indicates low variability, indicating that the found risk seekingness is reasonably consistent across the participants.

#5--------------------------------------------------------------------------------------------

#We wish to compare the decision quality of young people vs old people. To do so, we'll run a quadratic approximation model
#in which we assume the decision quality per group to be the same. Therefore, we'll use the same prior distribution and mu & sigma for both groups.

model_youngpeople <- quap(
  alist(DecisionQuality~ dnorm(mu, sigma),
        mu ~ dnorm(0.6 , 0.15),
        sigma ~ dunif(0 , 0.2)),
  data = df[df$AG == 1,]) #Subset the dataset for young people only


model_oldpeople <- quap(
  alist(DecisionQuality~ dnorm(mu, sigma),
        mu ~ dnorm(0.6 , 0.15),
        sigma ~ dunif(0 , 0.2)),
  data =df[df$AG == 2,]) #Subset the dataset for old people only

precis(model_youngpeople)
#Results for younger people seem very similar to what we saw previously.

precis(model_oldpeople)
#Results for older people seem very similar to what we saw previously.

#Extracting 10000 samples from both posterior distributions. 
young_smp <- extract.samples(model_youngpeople, n = 10000)
old_smp <- extract.samples(model_oldpeople, n = 10000)

#Calculate the difference between the samples
diff<- young_smp$mu - old_smp$mu
mean(diff) 

#Difference between the samples of the two groups seems to be low.

#Visualizing the distribution of the difference
hist(diff)

#After plotting the difference, we can indeed conclude that the difference in decision quality between the two groups is minimal,
#although seeing a slight increase in decision quality under young people compared to older people.

#6--------------------------------------------------------------------------------------------

#Two main advantages of using standardization for this task: 

#1: Easier Interpretation of variables: by putting all the variables on the same scale, results can be interpreted in terms of standard deviations.
# The coefficients are simply the change in the dependent variable per one standard deviation change in the independent variable.

#2 Variables with different units of measurements can be compared easily although previously having had completely different scales.
# This removes the influence of different measuring scales of independent variables, which may effect the numeric outcome of the model and predictors which larger scales causing bias. 

#Standardizing the variables in the dataframe:

?scale()
df$Age <- standardize(df$Age)
df$RiskSeeking <- standardize(df$RiskSeeking)
df$DecisionQuality <- standardize(df$DecisionQuality)
df$Speed <- standardize(df$Speed)
df$Numeracy <- standardize(df$Numeracy)
df$NegAffect <- standardize(df$NegAffect)

#Estimating a simple linear model for the effect of numeracy on decision quality:

lm_decqual_numeracy <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma), #Assume decision quality to be normally distributed (saw this earlier too)
    mu <- a + b * Numeracy,
    a <- dnorm(0, 1), #Intercept has no particular prior distribution, so a normal dnorm(0, 1) will suffice. 
    b <- dnorm(0.5, 1), #We expect a positive relationship between people's decision quality and their numeracy, and therefore indicate that the slope is likely positive and normally distributed (weakly informative prior distribution). 
    sigma <- dunif(0, 2) #Taking a wide standard deviation uniform distribution, indicating no strong prior beliefs.
  ),
  data = df
)

precis(lm_decqual_numeracy)
#The estimated coefficient b for the effect of Numeracy on Decision Quality = 0.36 (st.dev. = 0.93).

#Estimating a simple linear model for the effect of speed on decision quality:

lm_decqual_speed <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma), #Assume decision quality to be normally distributed (saw this earlier too)
    mu <- a + b * Speed,
    a <- dnorm(0, 1), #Intercept has no particular prior distribution, so a normal dnorm(0, 1) will suffice. 
    b <- dnorm(0, 1), #In this case, there is no particular association assumed between decision quality and speed. Speed may be a symptom of a better ability to decide, but may also be impulsivity getting the better of people (and therefore their decision quality lowers).
    sigma <- dunif(0, 2) #Taking a wide standard deviation uniform distribution, indicating no strong prior beliefs.
  ),
  data = df
)

precis(lm_decqual_speed)

#The estimated coefficient b for the effect of speed on Decision Quality = 0.22 (st.dev. = 0.97).

lm_decqual_negaffect <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma), #Assume decision quality to be normally distributed (saw this earlier too)
    mu <- a + b * NegAffect,
    a <- dnorm(0, 1), #Intercept has no particular prior distribution, so a normal dnorm(0, 1) will suffice. 
    b <- dnorm(-0.5, 1), #A weakly informative prior is taken that people with a negative emotional reaction to the questionnaire are expected generally expected to perform worse decision-wise.
    sigma <- dunif(0, 2)), #Taking a wide standard deviation uniform distribution, indicating no strong prior beliefs.
  data = df
)

precis(lm_decqual_negaffect)

#The estimated coefficient b for the effect of speed on Decision Quality = -0.15 (st.dev. = 0.99).



#Based on the found coefficients, one can conclude that...

#Numeracy with a coefficient of 0.36 HAS TEH STRONGEST TOTAL EFFECT on decision quality. People's ability to operate with numeric information has the largest total effect on the decided on the highest expectation option.

#Speed with a coefficient of 0.22 has a positive effect on decision quality. People with a higher measure of fluid cognitive ability are better able to choose the higher expectation option throughout the questionnaire.

#Negative Affect with a coefficient of -0.15 has a negative effect on decision quality. People with a tendency of being in a negative emotional state are less likely to choose the higher expectation option.


plot(df$RiskSeeking, df$Numeracy) #Quick check to see if there is a clear trend

lm_riskseek_numeracy <- quap(
  alist(
    RiskSeeking ~ dnorm(mu, sigma), #Assume RiskSeeking to be normally distributed (saw this earlier too)
    mu <- a + b * Numeracy,
    a <- dnorm(0, 1), #Intercept has no particular prior distribution, so a normal dnorm(0, 1) will suffice. 
    b <- dnorm(0, 1), #We make no clear prior assumption on the relationship between numeracy and riskseekingness.
    sigma <- dunif(0, 2) #Taking a wide standard deviation uniform distribution, indicating no strong prior beliefs.
  ),
  data = df
)

precis(lm_riskseek_numeracy)

#The estimated coefficient b for the effect of Numeracy on Risk Seeking = - 0.13 (st.dev. = 0.99).

lm_riskseek_speed <- quap(
  alist(
    RiskSeeking ~ dnorm(mu, sigma), #Assume RiskSeeking to be normally distributed (saw this earlier too)
    mu <- a + b * Speed,
    a <- dnorm(0, 1), #Intercept has no particular prior distribution, so a normal dnorm(0, 1) will suffice. 
    b <- dnorm(0, 1), #We make no clear prior assumption on the relationship between speed and riskseekingness.
    sigma <- dunif(0, 2) #Taking a wide standard deviation uniform distribution, indicating no strong prior beliefs.
  ),
  data = df
)

precis(lm_riskseek_speed)

#The estimated coefficient b for the effect of Speed on Risk Seeking = - 0.12 (st.dev. = 0.99).


lm_riskseek_negaffect <- quap(
  alist(
    RiskSeeking ~ dnorm(mu, sigma), #Assume RiskSeeking to be normally distributed (saw this earlier too)
    mu <- a + b * NegAffect,
    a <- dnorm(0, 1), #Intercept has no particular prior distribution, so a normal dnorm(0, 1) will suffice. 
    b <- dnorm(0, 1), #We make no clear prior assumption on the relationship between a negative emotional affect and riskseekingness.
    sigma <- dunif(0, 2) #Taking a wide standard deviation uniform distribution, indicating no strong prior beliefs. 
  ),
  data = df
)

precis(lm_riskseek_negaffect)

#The estimated coefficient b for the effect of Negative Effect on Risk Seeking = - 0.28 (st.dev. = 0.96).


#With the effect of numeracy on risk seeking having a beta coefficient of -0.13, the effect is negative. One could conclude that people with more numerical proficiency generally are more risk averse.

#The effect of speed on risk seeking is estimated to have a coefficient of 0.12. Generally people with a higher measure of fluid cognitive ability tend to be more risk seeking.

#THE STRONGEST TOTAL EFFECT of a variable on risk seeking is the variable negative effect with a coefficient of -0.28. One could conclude that people with a negative emotional state are far more likely to avoid risk.

#8---------------------------------------------------------------------------------------------
#My assumptions on how the variables could be related:

#Based on previous findings are prior intuition, one would expect a higher level of fluid cognitive ability ('Speed') and a higher ability to operate numeric information ('Numeracy')
#to generally have a higher proportion of choices with a the higher expected payoff correctly chosen ('Decision Quality').

#Decision Quality does not affect are person's natural 'speed' or 'numeracy', and therefore is the dependent variable in this case.

#However, there may be a relationship between participant's level of fluid cognitivity and numeracy. I would argue, that people with a higher level of fluid cognitive ability are also capable better performance in operating numeric information ('numeracy').

cor(df$Numeracy, df$Speed) 

#With a correlation of 0.36, there is clearly an association between numeracy and speed. 

#Therefore I assume there to be a indirect causal effect of speed on decision quality, with numeracy acting as a mediator between speed and decision quality. 
#Furthermore, there may also be direct causal effect of speed on decision quality, besides the indirect effect via numeracy.

#We shall have to further inspect the relationship between speed and numeracy, as well as their effects on the decision quality using a linear model.

lm_decqual_numeracy_speed <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma), #Assume decision quality to be normally distributed (saw this earlier too)
    mu <- a + b_num * Numeracy + b_speed * Speed,
    a <- dnorm(0, 1), #Intercept has no particular prior distribution, so a normal dnorm(0, 1) will suffice. 
    b_num <- dnorm(0.5, 1), #We expect a positive relationship between people's numerical ability and their decision quality, as witnessed earlier. A weakly informative prior.
    b_speed <- dnorm(0.5, 1), #We also expect a positive relationship between people's speed and their decision quality, as witnessed earlier. A weakly informative prior.
    sigma <- dunif(0, 2) #Taking a wide standard deviation uniform distribution, indicating no strong prior beliefs.
  ),
  data = df
)

precis(lm_decqual_numeracy_speed)
summary(lm_decqual_numeracy_speed)

#We find a beta coefficient of 0.32 for numeracy and 0.11 for speed, when using a multiple linear model to gauge the effect on decision quality.

#Previously, we found the following for the single effects of speed / numeracy on decision quality:
precis(lm_decqual_numeracy)
precis(lm_decqual_speed)

#Both coefficients have decreased, numeracy from 0.36 to 0.32, and speed substantially from 0.22 to 0.12. 

#This would indicate that by accounting for both variables in the model, the association between speed and numeracy has been taken into account. In doing so,
#the a portion of the variance of the dependent variable decision quality has been explained, leading to lower coefficients. 

#When the coefficient of numeracy declines when controlling for speed, this indicates that numeracy's direct association on decision quality may be partly mediated by speed.
#Secondly, considering that the effect of speed on decision quality decreases when controlling for numeracy, this is an indication that numeracy is indeed a mediator for speed. 

#All in all, the initial assumption (that numeracy is a mediator for the effect of speed on decision quality) is only partially true. Although numeracy is a mediator,
#there is a more complicated interplay between speed and numeracy which cannot be simply put in terms of 'mediator' or 'confounder'.

#Lastly, there may be an omitted variable at play here, that we have not taken into account. General Intelligence, for example, may be a variable of which both speed and numeracy possibly stem.


