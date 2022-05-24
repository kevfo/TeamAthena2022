library(tidyverse) ; library(corrplot)

# First, let's read in the data that we cleaned (removing everything else 
# before that first):
rm(list = ls()) ; par(mfrow = c(1, 1))

# Read in the data:
setwd("C:/Users/Kevin/Desktop/Projects/R/TeamAthena2022/analysis/")
data <- read_csv("ImputedData.csv")

# Taking a look at the data:
str(data) ; summary(data)

# Convert everything into a factor first...
data$rbc <- as.factor(data$rbc) ; data$pc <- as.factor(data$pc)
data$pcc <- as.factor(data$pcc) ; data$ba <- as.factor(data$ba)
data$htn <- as.factor(data$htn) ; data$dm <- as.factor(data$dm)
data$cad <- as.factor(data$cad) ; data$appet <- as.factor(data$appet)
data$pe <- as.factor(data$pe) ; data$ane <- as.factor(data$ane)
data$classification <- as.factor(data$classification)

# I think we should take a look to see if any variables are correlated - I'm
# using all numerical values (i.e., so the factors will not be in the correlation
# graph that I'm about to make) - also include this graph in our flexdashboard:
str(data)
data %>% select(c(age, bp, sg, al, su, bgr, bu, sc, sod, pot, hemo, pcv)) %>% 
  cor() %>% corrplot(method = "number")

# We see that hemo and pcv are highly correlated.  I think this implies that 
# if we use hemo or pcv in our model, then we shouldn't use the other (and 
# vice verl)) + geom_bar(aes(fill = classification)) + facet_wrap(~al)
# sa).  A similar argument can probably be made for hemo / al,
# hemo / sc, and hemo / bu.

# Otherwise, I think it could be worth exploring the relationship between
# our dependent variable classification and other categorical variables:
library(ggplot2)

# How about starting off with "rbc"?  The bar plot tells us that those without
# kidney disease have a higher proportion of normal rbc to abnormal rbc.  
# This makes sense - rbc can be used as predictor since there is a marked 
# difference between both groups of individuals.
ggplot(data, aes(x = appet)) + geom_bar(aes(fill = classification)) + 
  facet_wrap(~classification)

# By changing the categorical variable in the above line of code, I noted the following:
#
# 1) pc => proportion of abnormal to normal pc in diseased and healthy patients
#          looks very similar.  
#
# 2) pcc => there's no pcc present in those that are healthy.
#
# 3) ba => ditto.  I'm not sure how pcc and ba can help us out in our aim.
#
# 4) htn => healthy individuals have no htn.  This is definitely a major factor,
#           especially from a physiological standpoint and a data standpoint.
#
# 5) dm => ditto.
#
# 6) cad => ditto (though there's lesser individuals in the teal category).
#
# 7) appet => ditto.
#
# 8) pe => ditto.

# I was also wondering if it makes sense to engineer a new feature based on
# age.  I'm thinking that we can perhaps partition our "age" variable into
# three - four categories: young, middle aged, and elderly for instance.
#
# Based off a source I found on the internet, this is what they have to say:
#
# 1) Middle age => 45 - 65 years old (not inclusive on upper end)
# 2) Elderly => 65 and above
# 3) Young => Anybody below 45 (implied)
#
# I don't know how valid this is in the Singaporean context - this is something
# that we might have to look into in the near future.  Nevertheless:
data$agerange <- case_when(data$age < 45 ~ "Young",
                           data$age < 65 ~ "Middle age",
                           data$age >= 65 ~ "Elderly")
data$agerange <- as.factor(data$agerange)

ggplot(data, aes(x = classification)) + geom_bar(aes(fill = classification)) +
  facet_wrap(~agerange)

# I think that this could be helpful in our model.  The younger we get, 
# the lesser amount of people with kidney failure that we have.  

# Otherwise, I think taking what Edsel mentioned in the group chat into account,
# perhaps we wouldn't need to be so thorough after all.

# I suggest create a "base" model to improve on - perhaps using ALL features
# and then trying to improve upon it...

# Hoong Kai and Edsel - see if there's anything else within the data
# that you want to explore!
#
#
#