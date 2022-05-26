## == Data Import ==

# Load the necessary packages
library(tidyverse)
library(caret)
library(mi)

# Set working directory
setwd("C:/Users/Kevin/Desktop/Projects/R/TeamAthena2022/analysis/")

# Clear the global environment
rm(list = ls())

# Import data and remove "id" column
data <- read_csv("kidney_disease.csv") %>% select(-c(id))


## == Data Pre-processing ==

# View structure and data summary
str(data) ; summary(data)

# Type coercion to numeric
data[, c('pcv', 'wc', 'rc')] <- lapply(data[, c('pcv', 'wc', 'rc')], FUN = as.numeric)

# Type coercion to factor
data[, c('al', 'su', 'rbc', 'pc', 'pcc', 'ba')] <- lapply(data[, c('al', 'su', 'rbc', 'pc', 'pcc', 'ba')], FUN = as.factor)
data[, c('htn', 'dm', 'cad', 'appet', 'pe', 'ane')] <- lapply(data[, c('htn', 'dm', 'cad', 'appet', 'pe', 'ane')], FUN = as.factor)
data$classification <- as.factor(data$classification)

# Check structure again
str(data)

# Explore missing entries
apply(is.na(data), MARGIN = 2, FUN = which)
colSums(is.na(data))
data %>% na.omit() %>% nrow()

# entries appear to be missing at random (i.e., missing data don't seem to follow
# any sort of pattern, but can be accounted for by other variables)

# also not ideal to remove all rows and columns with NAs as only 158 observations
# will be left (about 40% of original data)

# aim to impute as many data points as possible to avoid deleting missing entries

# Analyzing "age" variable
par(mfrow = c(3, 4))
plot(data$age, data$bp) ; plot(data$age, data$sg) ; plot(data$age, data$bgr)
plot(data$age, data$bu) ; plot(data$age, data$sc) ; plot(data$age, data$sod)
plot(data$age, data$pot) ; plot(data$age, data$hemo) ; plot(data$age, data$pcv)
plot(data$age, data$wc) ; plot(data$age, data$rc)

# remove data whose "age" is NA as it is impossible to predict somebody's age
# also remove entries with unknown kidney disease status
data <- data %>% filter(!is.na(age) & !is.na(classification))

# wc and rc have at least 25% missing data, therefore remove them as data might
# become biased as a result of median imputation
data <- data %>% select(-c(wc, rc, rbc))

## == Using MI to impute the missing data == 

missingData <- missing_data.frame(as.matrix(data))

# I think it's wise to explore the data a little bit further before trying 
# to impute anything else

show(missingData)

# Change some of the data that we have:
missingData <- change(missingData, y = c('age', 'bp', 'sg', 'al', 'su',
                          'bgr', 'bu',
                          'sc', 'sod', 'pot',
                          'hemo', 'pcv'),
       what = "type", to = c(rep("continuous", 3),
                             rep('ordered-categorical', 2),
                             rep('continuous', 7)))
show(missingData)

# Now that the data is (mostly) of the correct type, the vignette for mi
# suggests that we call summary() on missingData to get a sense of what 
# our raw data looks like:

summary(missingData)
image(missingData)  # Use this plot for data cleaning section in shinyflexboard!
                    # Use this to justify imputing missing data.

# Then, impute the data as per the vignette (consider parallel computations
# for the sake of speed):
imputations <- mi(missingData, parallel = T, n.iter = 80)

# To verify that our imputations is good, I'm just following the vignette's
# reasoning:
imputations %>% mipply(mean, to.matrix = T) %>% round(3)

# The documentation for the below function does shed some insight into what
# this function does.  "R-hat" appears to be some sort convergence statistic
# that compares the variance between chains.  
#
# As a general guideline, an R-hat approaching 1 is good - anything more than
# 1.1 means that more computation is needed.
Rhats(imputations)

plot(imputations)   # Also use these plots too (perhaps we can take  
                    # three random graphs since they all explain the same 
                    # thing and leave them inside the shinyflexboard document).  


# How does the imputed data look compared to the original image?
image(imputations)  # Also use this plot...

# And then observe the mean of the finished data:
summary(imputations) # Not sure whether or not to use the data from this function

# When we're done, we can write our imputed data to a .csv file that 
# I'll then use:
com <- imputations %>% complete(m = 1) ; write.csv(com[, 1:23], 
                                                   "ImputedData.csv",
                                                   row.names = F)


