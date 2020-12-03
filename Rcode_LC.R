
############################################################
#                                                          #
#                      Assignment 1                        #
#                                                          #
############################################################

# Load packages	
library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(tidyverse) # for tidy code	
library(lm.beta) # for lm.beta	
library(gridExtra) # for grid.arrange	

# Load custom functions for creating the final table for the regression coefficients.	
coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

# Load data 
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

## Data exploration and recoding
data_sample_1 %>% 	
  summary()	

describe(data_sample_1)

data_sample_1 <- data_sample_1 %>% 
  mutate(sex = recode(sex,
                      "male" = "0",
                      "female" = "1"))

data_sample_1 <- data_sample_1 %>% 
  mutate(sex = factor(sex))

# scatterplot	
data_sample_1 %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain) +	
  geom_point()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain) +	
  geom_point()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain) +	
  geom_point()


## Build models 
# Model 1
mod_1 = lm(pain ~ age + sex, data = data_sample_1)	
summary(mod_1)

# Model 2
mod_2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1)
summary(mod_2)

## Identifying extreme cases with high leverage	
# Cook’s distance
mod_2 %>% 	
  plot(which = 4) 
mod_2 %>% 	
  plot(which = 5)	

# Dealing with outliers	
data_sample_1 %>% 	
  slice(c(93, 114, 150))	

# For case 93, age of 444 is unreasonable, and for case 150, the scores of STAI is 3.9, while scores of this scale should be of 20 to 80. Excluded these three cases of outliers.
data_sample_1.1 <- data_sample_1[-c(93, 114, 150),]
data_sample_1.1 %>% 	
  summary()	

# re-run models
mod_1.1 = lm(pain ~ age + sex, data = data_sample_1.1)	
summary(mod_1.1)
mod_3 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1.1)	
summary(mod_3)


#--------------------------------------------------
## Assumptions of linear regression	

#1. Normality (of the residuals)
mod_3 %>% 	
  plot(which = 2)	# QQplot
# All the points do not fall approximately along this reference line, so we can assume non-normality.

# Histogram	
residuals_mod_3 = enframe(residuals(mod_3))	
residuals_mod_3 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()
# And the residuals also look normal in this model


#2. Linearity (of the relationship)
mod_3 %>% 	
  residualPlots()	
# The tests are all non significant, so the linearity assumption seems to hold true in this model.	


#3. Homoscedasticty
mod_3 %>% 	
  plot(which = 3)	
mod_3 %>% 	
  ncvTest() # NCV test	
mod_3%>% 	
  bptest() # Breush-Pagan test	
# Both NCV test and Breush-Pagan test are not significant, indicating the homoscedasticty assumption holds true in this model.


#4.No excess multicollinearity
mod_3 %>% 
  vif()
# The vif of cortisol_serum and cortisol_saliva is above 3.	
data_sample_1 %>% 	
  select(pain, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva ) %>% 	
  pairs.panels(col = "red", lm = T)	

# The correlation matrix indicates that the correlation of cortisol_serum and cortisol_saliva is very high. Considering that there is hardly any difference between these two variables and measuring cortisol from saliva is easier and more common to operate than serum. Remove the predictor of serum. 
mod_4 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data_sample_1.1)
summary(mod_4)

#--------------------------------------------------
## Re-run the above checks for final data and model.
#1. Normality (of the residuals)
mod_4 %>% 	
  plot(which = 2)

# Histogram	
residuals_mod_4 = enframe(residuals(mod_3))	
residuals_mod_4 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()

#2. Linearity (of the relationship)
mod_4 %>% 	
  residualPlots()	
# The tests are all non significant, so the linearity assumption seems to hold true in this model.	

#3. Homoscedasticty
mod_4 %>% 	
  plot(which = 3)	
mod_4 %>% 	
  ncvTest() # NCV test	
mod_4%>% 	
  bptest() # Breush-Pagan test	
# Both NCV test and Breush-Pagan test are not significant, indicating the homoscedasticty assumption holds true in this model.

#4.No excess multicollinearity
mod_4 %>% 
  vif()

#--------------------------------------------------

## Compare the two models
# AIC	
AIC(mod_1.1)
AIC(mod_4)	
# ANOVA
anova(mod_1.1, mod_4)	

## Creating the final table for the regression coefficients
s = summary(mod_4)	
confint(mod_4)	
lm.beta(mod_4)	

s_p_values = as.character(round(s$coefficients[,4], 3))	
s_p_values[s_p_values != "0" & s_p_values != "1"] = substr(s_p_values[s_p_values != "0" & s_p_values != "1"], 2, nchar(s_p_values[s_p_values != "0" & s_p_values != "1"]))	
s_p_values[s_p_values == "0"] = "<.001"	

s_table = cbind(as.data.frame(round(cbind(coef(mod_4), confint(mod_4), c(0, lm.beta(mod_4)$standardized.coefficients[c(2,3)])), 2)), s_p_values)	
names(s_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
s_table["(Intercept)","Std.Beta"] = "0"	

s_table = coef_table(mod_4)	
s_table	

## The regression equation of model 2.
pain = 0.01 ∗ age - 0.31 * sex  - 0.03 * STAI + 0.14 * pain_cat - 0.24 * mindfulness + 0.60 * cortisol_saliva
# Note: for sex, male = 0, female = 1.



############################################################
#                                                          #
#                      Assignment 2                        #
#                                                          #
############################################################

# Load data 
home_sample_1 <- read.csv("~/Desktop/Home assignment/Assignment 1/home_sample_1.csv")

# Exclude cases as assignment 1
home_sample_1 <- home_sample_1[-c(93, 114, 150),]

# Build an initial model
initial_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = home_sample_1)	
summary(initial_model)

# Run a backward regression
initial_model_back = step(initial_model, direction = "backward")	
summary(initial_model_back)

#--------------------------------------------------
# ## Re-run the data and model diagnostics
#1. Normality (of the residuals)
initial_model_back  %>% 	
  plot(which = 2)

# Histogram	
residuals_initial_model_back = enframe(residuals(initial_model_back))	
residuals_initial_model_back %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()

#2. Linearity (of the relationship)
initial_model_back %>% 	
  residualPlots()	
# The tests are all non significant, so the linearity assumption seems to hold true in this model.	

#3. Homoscedasticty
initial_model_back %>% 	
  plot(which = 3)	
initial_model_back %>% 	
  ncvTest() # NCV test	
initial_model_back %>% 	
  bptest() # Breush-Pagan test	
# Both NCV test and Breush-Pagan test are not significant, indicating the homoscedasticty assumption holds true in this model.

#4.No excess multicollinearity
initial_model_back %>% 
  vif()

#--------------------------------------------------

## Built "backward_model" and "theory_based_model"
backward_model = lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = home_sample_1)	
summary(backward_model)
theory_based_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = home_sample_1)
summary(theory_based_model)

# Creating the final table for the regression coefficients
s2 = summary(backward_model)	
confint(backward_model)	
lm.beta(backward_model)	

s2_p_values = as.character(round(s2$coefficients[,4], 3))	
s2_p_values[s2_p_values != "0" & s2_p_values != "1"] = substr(s2_p_values[s2_p_values != "0" & s2_p_values != "1"], 2, nchar(s2_p_values[s2_p_values != "0" & s2_p_values != "1"]))	
s2_p_values[s2_p_values == "0"] = "<.001"	

s2_table = cbind(as.data.frame(round(cbind(coef(backward_model), confint(backward_model), c(0, lm.beta(backward_model)$standardized.coefficients[c(2,3)])), 2)), s2_p_values)	
names(s2_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
s2_table["(Intercept)","Std.Beta"] = "0"	

s2_table = coef_table(backward_model)	
s2_table

# Compare the backward model with initial model and theory-based model based on AIC
# Note that anova for model comparison is only appropriate if the two models are "nested"
AIC(initial_model)
AIC(backward_model)
AIC(theory_based_model)

## Make predictions on pain of home_sample_2 using the regression models or equations of the backward model and the theory-based model.
home_sample_2 <- read.csv("~/Desktop/Home assignment/Assignment 1/home_sample_2.csv")

# Run models
backward_model_2 = lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = home_sample_2)
summary(backward_model_2)

theory_based_model_2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = home_sample_2)
summary(theory_based_model_2)

## Testing performance on the test set	
# Calculate predicted values 
pred_backward <- predict(backward_model_2, home_sample_2)	
pred_theory_based <- predict(theory_based_model_2, home_sample_2)	

# Calculate the sum of squared residuals 	
RSS_backward = sum((home_sample_2[,"pain"] - pred_backward)^2)	
RSS_theory_based = sum((home_sample_2[,"pain"] - pred_theory_based)^2)	
RSS_backward
RSS_theory_based

# The regression equation of the backward model
Pain = 1.62 - 0.04 ∗ Age + 0.34 * Sex  + 0.11 * Pain_cat – 0.28 * Mindfulness + 0.53 * Cortisol. 
# Note: for sex, male = 0, female = 1


############################################################
#                                                          #
#                      Assignment 3                        #
#                                                          #
############################################################

# Load more packages
library(cAIC4) # for cAIC		
library(r2glmm) # for r2beta		
library(lme4) # for lmer	
library(lmerTest) # to get singificance test in lmer	
library(MuMIn) # for r.squaredGLMM	
library(optimx) # for optimx optimizer

## A function to extract standardized beta coefficients from linear mixed models.	
stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

# Load data
home_sample_3 <- read.csv("~/Desktop/Home assignment/Assignment 1/home_sample_3.csv")
home_sample_4 <- read.csv("~/Desktop/Home assignment/Assignment 1/home_sample_4.csv")

# Data check and exploration
home_sample_3 %>% 	
  summary()	
describe(home_sample_3)

home_sample_4 %>% 	
  summary()	
describe(home_sample_4)

home_sample_3 %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)	

home_sample_3 %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)	

# Deal with coding error and outliers
home_sample_3 <- home_sample_3 %>% 
  mutate(sex = recode(sex,
                      "femlae" = "female"))
home_sample_3 <- home_sample_3[-c(121),]
home_sample_4 <- home_sample_4[-c(9),]

# Asign class as a grouping factor	
home_sample_3 = home_sample_3 %>%	
  mutate(hospital= factor(hospital, levels = c("hospital_1",	
                                               "hospital_2",
                                               "hospital_3",
                                               "hospital_4",
                                               "hospital_5",
                                               "hospital_6",
                                               "hospital_7",
                                               "hospital_8",
                                               "hospital_9",
                                               "hospital_10")))
home_sample_4 = home_sample_4 %>%	
  mutate(hospital= factor(hospital))


## First, build a linear mixed model on data file 3, accounting for the clustering of the data at different hospital sites. We have no reason to assume that the effects of the different predictors would be different in the different hospitals, so fit a random intercept model including the random intercept of hospital-ID, and the fixed effect predictors you used in assignment 1. Once the model is built, note the model coefficients and the confidence intervals of the coefficients for all fixed effect predictors, and compare them to the ones obtained in assignment 1.


# Build a random intercept model including the random intercept of hospital-ID
mod_int_1 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + (1|hospital), data = home_sample_3)

# Model coefficients and the confidence intervals 
summary(mod_int_1)
coef_CI = suppressWarnings(confint(mod_int_1))		
stdCoef.merMod(mod_int_1)	

# Model from assignment 1	
mod_fixed = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = home_sample_3)	
summary(mod_fixed)

# marginal R squared with confidence intervals	
r2beta(mod_int_1, method = "nsj", data = home_sample_3)
# marginal and conditional R squared values	
r.squaredGLMM(mod_int_1)	

# Use the regression equation obtained on data file 3 to predict pain in data file 4.
mod_int_2 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + (1|hospital), data = home_sample_4)
summary(mod_int_2)

# Compute the variance explained by the model on data file 4.
RSS_mod_int_2 = sum((home_sample_4$pain - predict(mod_int_2))^2)	
mod_mean <- lm(pain ~ 1, data = home_sample_4)	
TSS_mod_int_2 = sum((home_sample_4$pain - predict(mod_mean))^2)	
R2_mod_int_2 = 1-(RSS_mod_int_2 /TSS_mod_int_2)	
R2_mod_int_2

# Comparing model fit indices	
sum(residuals(mod_int_1)^2)		
sum(residuals(mod_fixed)^2)		
sum(residuals(mod_int_2)^2)		

AIC(mod_fixed)		
cAIC(mod_int_1)$caic		
cAIC(mod_int_2)$caic		

## Build a new linear mixed effects model on dataset 3 predicting pain. However, instead of including all predictors, you should only include the most influential predictor from the previous model. Allow for both random intercept and random slope. 
# **random intercept model**:	
mod_int_3 = lmer(pain ~ cortisol_saliva + (1|hospital), data = home_sample_3)
# **random slope model** (allowing BOTH random intercept and random slope):	
mod_slope_3 = lmer(pain ~ cortisol_saliva + (cortisol_saliva|hospital), data = home_sample_3)

sum(residuals(mod_int_3)^2)		
sum(residuals(mod_slope_3)^2)		
cAIC(mod_int_3)$caic		
cAIC(mod_slope_3)$caic	

# **Singular fit**: This warning tells you that one or more variances estimated for the random effect terms is (very close to) zero, meaning that that random effect predictor seems to be not useful in modeling the data. 

## Now visualize the fitted regression lines for each hospital separately.
# We can plot the regression lines of the two models by saving the predictions of the models into a variable.	
home_sample_3 = home_sample_3 %>% 		
  mutate(pred_int = predict(mod_int_3),		
         pred_slope = predict(mod_slope_3)) 		

# Regression line of the random intercept model	
plot1 <- home_sample_3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_saliva, group = hospital)+		
  geom_point(aes(color = hospital), size = 2) +		
  geom_line(color ='red', aes(y = pred_int, x = cortisol_saliva))+		
  facet_wrap( ~ hospital, ncol = 5)	

plot1 +
  labs(x = "Cortisol_saliva",
         y = "Pain")

# Regression line of the random slope model	
plot2 <- home_sample_3 %>% 		
  ggplot() +	
  aes(y = pain, x = cortisol_saliva, group = hospital)+		
  geom_point(aes(color = hospital), size = 2) +		
  geom_line(color ='red', aes(y = pred_slope, x = cortisol_saliva))+		
  facet_wrap( ~ hospital, ncol = 5)	

plot2 +
  labs(x = "Cortisol_saliva",
       y = "Pain")



