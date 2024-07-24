
#######################################################################################################################
####The relationship between emotional resilience, social competence, family coherence, and word reading proficiency###
#######################################################################################################################                                      

                                                        ##MRes thesis##


#The first thing I will do is load the following packages.
library(languageR)
library(lme4)
library(lmerTest)
library(lattice)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(tidyverse)
library(jtools)
library(plotrix)
library(sjPlot)
library(ggstance)
library(emmeans)
library(r2mlm)
library(MuMIn) # look this up for Rsquared
library(effects)


#I will now set the working directory. I should note that for some strange reason when I upload the data, it all appears in one column. So, to overcome this I will supress the warnings. 
setwd("C:/Users/james/OneDrive/Desktop/Raw data")
data = read.table(file="Final analysis.csv", header=T, sep="\t")
data <- suppressWarnings(read.csv('Final analysis.csv'))


#The dataset has 23 variables. I explored my data extensively to examine trends and relationships. For simplicity, I will delete the variables I have not analysed in my results.
data$Participant <- data$OVERALL_MEAN_IN_C_MS <- data$Mean_RT_All_Words <- data$Mean_RT_All_PsWords <- data$Mean_RT_Correct_Words <- data$Mean_RT_Correct_PsWords <- data$SD_IN_C_MS <- data$Correct_Percentage <- data$Percentage_Correct_Words <- data$Percentage_Correct_PsWords <- data$Percentage_dropped <- data$MEAN_OF_CORRECT_MS <- data$Total_Academic_Resilience_Scale_ARS_30_Score <- data$TAR_Norm <- data$Total_Resilience_Scale_for_Adults__RSA_37_Score <- data$TR_Norm <- NULL


#The dataset now has 6 variables. The variables are: emotional resilience, social competence, family coherence, 
#and age. We decided to split word reading proficiency into two components: 
#efficiency of words and efficiency of pseudowords. 


#I will check that the variables have the right variable type labels: 
data$Efficiency_Of_Words <- as.numeric(as.character(data$Efficiency_Of_Words))
data$Efficiency_Of_Pseudowords <- as.numeric(as.character(data$Efficiency_Of_Pseudowords))
data$Emotional_Resilience <- as.numeric(as.character(data$Emotional_Resilience))
data$Social_Competence <- as.numeric(as.character(data$Social_Competence))
data$Family_Coherence <- as.numeric(as.character(data$Family_Coherence))
data$Age <- as.numeric(as.character(data$Age))
data$Gender <- as.factor(data$Gender)


#Check the structure of the dataset:
head(data)
str(data)
summary(data)

                                             ##############################
                                             ######Correlation Matrix######
                                             ##############################   


#The first set of analyses I will do is a correlation matrix. I will do correlation analyses 
#to ensure that highly correlated predictors are not included in the analysis. 


#I will first determine the correlation coefficients (Pearson’s r values). 
#The values can be retrieved from the table called “data.cor”
data.cor = cor(data, method = c("pearson"))


#Since, all the correlation coefficients are less than 0.8, there are no highly correlated predictors.


#To complete the rest of the correlation analyses, the following needs to be downloaded:
install.packages("Hmisc")
library("Hmisc")


#I will now determine the p values associated with each correlation coefficient: 
data.rcorr = rcorr(as.matrix(data))
data.rcorr
data.coeff = data.rcorr$r
data.p = data.rcorr$P


#For additional support, I will plot a correlation matrix. I will download the required package:
library(corrplot)


#I will now do the actual plotting:
data = cor(data)
corrplot(data, method = 'number')

                                             ################################
                                             #######Multiple regression######
                                             ################################


#The second set of analyses I will do is multiple regression analysis. I will first clear my working space:
rm(list=ls())


#I set the working directory again:
setwd("C:/Users/james/OneDrive/Desktop/Raw data")
data = read.table(file="Final analysis.csv", header=T, sep="\t")
data <- suppressWarnings(read.csv('Final analysis.csv'))


#Again, there are variables that should be removed, since they were not analysed in our results:
data$Gender <- data$Participant <- data$OVERALL_MEAN_IN_C_MS <- data$Mean_RT_All_Words <- data$Mean_RT_All_PsWords <- data$Mean_RT_Correct_Words <- data$Mean_RT_Correct_PsWords <- data$SD_IN_C_MS <- data$Correct_Percentage <- data$Percentage_Correct_Words <- data$Percentage_Correct_PsWords <- data$Percentage_dropped <- data$MEAN_OF_CORRECT_MS <- data$Total_Academic_Resilience_Scale_ARS_30_Score <- data$TAR_Norm <- data$Total_Resilience_Scale_for_Adults__RSA_37_Score <- data$TR_Norm <- NULL


#I check the structure of the dataset:
head(data)
str(data)
summary(data)


#I make sure again that the variables have the right variable type labels:
data$Efficiency_Of_Words <- as.numeric(as.character(data$Efficiency_Of_Words))
data$Efficiency_Of_Pseudowords <- as.numeric(as.character(data$Efficiency_Of_Pseudowords))
data$Emotional_Resilience <- as.numeric(as.character(data$Emotional_Resilience))
data$Social_Competence <- as.numeric(as.character(data$Social_Competence))
data$Family_Coherence <- as.numeric(as.character(data$Family_Coherence))
data$Age <- as.numeric(as.character(data$Age))


#Before doing the regression analysis, I ensured no assumptions were violated. 
#I did this on JASP, and no assumptions were violated. 


#There were two regression models that were tested. 
#In both models, the predictor variables were emotional resilience, social competence, and family coherence. 
#Age was coded as a covariate. However, in model one, efficiency of words was a response variable. 
#In contrast, in model two, the efficiency of pseudowords was a response variable. 


#Model one (efficiency of words) was tested first:
Model1 <- lm(data$Efficiency_Of_Words ~ data$Emotional_Resilience + data$Social_Competence + 
               data$Family_Coherence + data$Age)
summary(model1)


#This model was non-significant, R2 = .104, F(4, 45) = 1.310, p = .281. 
#It was found that total emotional resilience (B = -21.98, p = .477), 
#social competence (B = 43.14, p = .093), family coherence (B = 22.86, p = .176), 
#and age (B = 2.74, p = .755) did not significantly predict inverse efficiency of words.


#Model two (efficiency of pseudowords) was then tested:
model2 <- lm(data$Efficiency_Of_Pseudowords ~ data$Emotional_Resilience + data$Social_Competence + 
               data$Family_Coherence + data$Age)
summary(model2)


#This model was significant, R2 = .218, F(4,45) = 3.145, p = .023. It was found that age 
#significantly predicted inverse efficiency of pseudowords (B = 34.79, p = .004). 
#Total emotional resilience (B = -1.17, p = .977), social competence (B = -17.83, p = .591), 
#and family coherence (B = 32.69, p = .141) did not significantly predict 
#inverse efficiency of pseudowords. 


#I then ran a simple follow up regression, to determine how much variance is accounted for age:
model3 <- lm(data$Efficiency_Of_Pseudowords ~ data$Age)
summary(model3)


### The follow-up simple regression revealed age accounted for 16.2% of the variance (F(1,48) = 9.247, p = .004). 


                                                  ##############################
                                                  ######Mediation analysis######
                                                  ##############################


I did the mediation analysis on JASP. I did this because R would not work.