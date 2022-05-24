# Load in the necessary packages first:
library(tidyverse) ; library(magrittr) ; library(dplyr)

# Set the working directory:
# PS: do change the directory according to your machine (i.e., the line of 
# code below will cause this script file to crash on your machine)!
setwd("C:/Users/Kevin/Desktop/Projects/R/TeamAthena2022/analysis/")

# First things first, I think it's wise to explore our data.  I know that 
# some entries are missing, but HOW many entries are missing?

# rm(list = ls()) is a shortcut for removing everything from your global 
# environment (great if you were just messing around earlier):
rm(list = ls())

# Then, load in the data and explore it (also get rid of the "id" column
# in the process - we won't need it):
data <- read_csv("kidney_disease.csv") %>% select(-c(id))

# Looking at the structure and the summary of the data:
str(data) ; summary(data)
.  
# There are a lot of NAs (i.e., a lot of missing data), but if we were to 
# delete all rows with missing data, we'd only have 158 rows of data to
# work with - about 40% of our original data (i.e., more than half of our 
# data - this isn't the most ideal approach).
data %>% na.omit() %>% nrow()

# Nevertheless, the data we have appears to be missing entries at random 
# (i.e., our data has missing data that doesn't seem to follow any sort of 
# pattern, but can be accounted for by other variables...)

## == Imputing the data ==

# Because we only have so much to work with, therefore I think we should try
# and avoid deleting as best as possible.  So, we'd have to impute as many
# data points as possible.  Right off the bat, I think we should remove data
# whose "age" is NA - we really cannot predict somebody's age - see these graphs
# for yourself:
par(mfrow = c(3, 3))
plot(data$age, data$bp) ; plot(data$age, data$sg) ; plot(data$age, data$al)
plot(data$age, data$bgr) ; plot(data$age, data$bu) ; plot(data$age, data$sc)
plot(data$age, data$hemo) ; plot(data$age, data$pcv) ; plot(data$age, data$wc)

# So, delete the entries whose age and kidney disease status (for obvious reasons) 
# cannot be known:
data <- data %>% filter(!is.na(age) & !is.na(classification))

# Maybe then, we can construct multiple boxplots:
par(mfrow = c(3, 3))
boxplot(data$bp, ylab = "bp") ; boxplot(data$sg, ylab = "sg") ; boxplot(data$bgr, ylab = "bgr")
boxplot(data$bu, ylab = "bu") ; boxplot(data$sc, ylab = 'sc') ; boxplot(data$sod, ylab = 'sod')
boxplot(data$hemo, ylab = 'hemo') ; boxplot(as.numeric(data$pcv), ylab = 'pcv') ; boxplot(as.numeric(data$rc), ylab = 'rc')

# At this point, I also noted that some variables are not numeric like they
# should be.  So, before moving on, make them numeric:
data$pcv <- as.numeric(data$pcv) %>% suppressWarnings()
data$wc <- as.numeric(data$wc) %>% suppressWarnings()
data$rc <- as.numeric(data$rc) %>% suppressWarnings()

## Let's focus on the bp variable - let's see which data entries are actually
# NAs:
data[which(is.na(data$bp), data$bp), ] %>% View()

# In the above data frame, I see a two year old.  I think it makes sense that
# because the child is so young, that these measurements wouldn't make too 
# much sense.  Let's remove them.
data <- data %>% filter(age > 2)

# Nevertheless, I think we should look closer at the boxplot for bp.  Let's
# also find out the quartiles:
par(mfrow = c(1, 1))
boxplot(bp ~ classification, na.rm = T, data)
summary(data[data$classification == "ckd", ]$bp) ; summary(data[data$classification == "notckd", ]$bp)

# Since we only have two outliers in the ckd type (in classification), I 
# think imputing the NAs with the medians should be fine... this is because
# we have a large range of data:
for (i in which(is.na(data$bp), data$bp)) {
  data[i, ]$bp <- ifelse(data[i, ]$classification == "ckd", 80, 71.29)
} 

## Let's now focus on the sg variable by taking a closer look at its boxplot:
par(mfrow = c(1, 1))
boxplot(sg ~ classification, data)
summary(data[data$classification == "ckd", ]$sg) ; summary(data[data$classification == "notckd", ]$sg)


# Let's also do the same thing as before:
for (i in which(is.na(data$sg), data$sg)) {
  data[i, ]$sg <- ifelse(data[i, ]$classification == "ckd", 1.015, 1.022)
}

## At this point, I decided to run summary(data) again:
summary(data)

# I noticed that the variables wc and rc had at least 25% missing data.  
# I decided that it wasn't going to be worth the trouble trying to impute 
# the missing values for these, so I just deleted them.  I would have tried
# imputing them with the mean / median, but I was afraid that at 25% missing data,
# that the data will be biased if I did:
data <- data %>% select(-c(wc, rc))

## Now, what about bgr?  I made two separate histograms:
par(mfrow = c(1, 2))
hist(data$bgr) ; hist(data[data$classification == 'ckd', ]$bgr)
summary(data[data$classification == "ckd", ]$bgr) ; summary(data[data$classification == "notckd", ]$bgr)


# I think we should impute with the median instead - I also used a box plot:
boxplot(bgr ~ classification, data)
for (i in which(is.na(data$bgr), data$bgr)) {
  data[i, ]$bgr <- ifelse(data[i, ]$classification == "ckd", 144, 108)
}

## What about bu?  Maybe we can do the same thing too - the outliers are those 
#  that already have kidney disease anyways...
boxplot(bu ~ classification, data)
summary(data[data$classification == "ckd", ]$bu) ; summary(data[data$classification == "notckd", ]$bu)

for (i in which(is.na(data$bu), data$bu)) {
  data[i, ]$bu <- ifelse(data[i, ]$classification == "ckd", 53, 33)
}

# Let's do the same thing for sc too:
boxplot(sc ~ classification, data)
summary(data[data$classification == "ckd", ]$sc) ; summary(data[data$classification == "notckd", ]$sc)

for (i in which(is.na(data$sc), data$sc)) {
  data[i, ]$sc <- ifelse(data[i, ]$classification == "ckd", 2.2, 0.9)
}

## What about sodium and potassium?
boxplot(sod ~ classification, data)
summary(data[data$classification == "ckd", ]$sod) ; summary(data[data$classification == "notckd", ]$sod)

for (i in which(is.na(data$sod), data$sod)) {
  data[i, ]$sod <- ifelse(data[i, ]$classification == "ckd", 136, 141)
}

boxplot(pot ~ classification, data)
summary(data[data$classification == "ckd", ]$pot) ; summary(data[data$classification == "notckd", ]$pot)

for (i in which(is.na(data$pot), data$pot)) {
  data[i, ]$pot <- ifelse(data[i, ]$classification == "ckd", 4.3, 4.5)
}

## Perhaps do the same thing too for hemo and pcv:
boxplot(hemo ~ classification, data)
summary(data[data$classification == "ckd", ]$hemo) ; summary(data[data$classification == "notckd", ]$hemo)

for (i in which(is.na(data$hemo), data$hemo)) {
  data[i, ]$hemo <- ifelse(data[i, ]$classification == "ckd", 10.9, 15)
}

boxplot(pcv ~ classification, data)
summary(data[data$classification == "ckd", ]$pcv) ; summary(data[data$classification == "notckd", ]$pcv)

for (i in which(is.na(data$pcv), data$pcv)) {
  data[i, ]$pcv <- ifelse(data[i, ]$classification == "ckd", 33, 46)
}

# Now that all the numerical values are imputed, I think we should opt for 
# model-based imputations for the categorical variables.  We can perhaps opt 
# for a kNN model, but first, how does the data look like?

library(caret)
specialData <- data %>% filter(!is.na(al)) %>% select(-c('rbc', 'pc', 'pcc', 'ba', 'htn', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification', 'su'))
alPredictor <- train(al ~ ., 
      data = specialData,
      method = 'knn')

for (i in 1:nrow(data)) {
  if (is.na(data$al[i])) data$al[i] <- predict(alPredictor, data[i, ])
}

# Let's do the same thing for su, rbc, and pc:
specialData <- data %>% filter(!is.na(su)) %>% select(-c('rbc', 'pc', 'pcc', 'ba', 'htn', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification', 'al'))
suPredictor <- train(su ~ ., 
                     data = specialData,
                     method = 'knn')
for (i in 1:nrow(data)) {
  if (is.na(data$su[i])) data$su[i] <- predict(suPredictor, data[i, ])
}

specialData <- data %>% filter(!is.na(rbc)) %>% select(-c('su', 'pc', 'pcc', 'ba', 'htn', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification', 'al'))
rbcPredictor <- train(rbc ~ ., 
                      data = specialData,
                      method = 'knn')
for (i in 1:nrow(data)) {
  if (is.na(data$rbc[i])) data$rbc[i] <- predict(rbcPredictor, data[i, ])
}

specialData <- data %>% filter(!is.na(rbc)) %>% select(-c('su', 'rbc', 'pcc', 'ba', 'htn', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification', 'al'))
rbcPredictor <- train(rbc ~ ., 
                      data = specialData,
                      method = 'knn')
for (i in 1:nrow(data)) {
  if (is.na(data$rbc[i])) data$rbc[i] <- predict(rbcPredictor, data[i, ])
}
data$rbc <- ifelse(data$rbc == 2, 'abnormal', 'normal')


specialData <- data %>% filter(!is.na(pc)) %>% select(-c('su', 'rbc', 'pcc', 'ba', 'htn', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification', 'al'))
pcPredictor <- train(pc ~ ., 
                      data = specialData,
                      method = 'knn')
for (i in 1:nrow(data)) {
  if (is.na(data$pc[i])) data$pc[i] <- predict(pcPredictor, data[i, ])
}
data$pc <- ifelse(data$pc == 2, 'abnormal', 'normal')

# We still have some hidden NAs:
colSums(is.na(data))

# predicting pcc
data$pcc <- as.factor(data$pcc)
specialData <- data %>% filter(!is.na(pcc)) %>% select(-c('su', 'rbc', 'htn', 'ba', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification', 'al'))
pccPredictor <- train(pcc ~ ., 
                     data = specialData,
                     method = 'knn')
for (i in 1:nrow(data)) {
  if (is.na(data$pcc[i])) data$pcc[i] <- predict(pccPredictor, data[i, ])
}

# predicting ba
data$ba <- as.factor(data$ba)
specialData <- data %>% filter(!is.na(ba)) %>% select(-c('su', 'rbc', 'htn', 'pcc', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification', 'al'))
baPredictor <- train(ba ~ ., 
                      data = specialData,
                      method = 'knn')
for (i in 1:nrow(data)) {
  if (is.na(data$ba[i])) data$ba[i] <- predict(baPredictor, data[i, ])
}

# predicting htn
data$htn <- as.factor(data$htn)
specialData <- data %>% filter(!is.na(htn)) %>% select(-c('su', 'rbc', 'ba', 'pcc', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification', 'al'))
htnPredictor <- train(htn ~ ., 
                     data = specialData,
                     method = 'knn')
for (i in 1:nrow(data)) {
  if (is.na(data$htn[i])) data$htn[i] <- predict(htnPredictor, data[i, ])
}

# predicting dm
data$dm <- as.factor(data$dm)
specialData <- data %>% filter(!is.na(dm)) %>% select(-c('su', 'rbc', 'ba', 'pcc', 'htn', 'cad', 'appet', 'pe', 'ane', 'classification', 'al'))
dmPredictor <- train(dm ~ ., 
                      data = specialData,
                      method = 'knn')
for (i in 1:nrow(data)) {
  if (is.na(data$dm[i])) data$dm[i] <- predict(dmPredictor, data[i, ])
}

# For cad

data$cad <- as.factor(data$cad)
specialData <- data %>% filter(!is.na(cad)) %>% select(-c('su', 'rbc', 'ba', 'pcc', 'htn', 'dm', 'appet', 'pe', 'ane', 'classification', 'al'))
cadPredictor <- train(cad ~ ., 
                     data = specialData,
                     method = 'knn')
for (i in 1:nrow(data)) {
  if (is.na(data$cad[i])) data$cad[i] <- predict(cadPredictor, data[i, ])
}

# For appet
data$appet <- as.factor(data$appet)
specialData <- data %>% filter(!is.na(appet)) %>% select(-c('su', 'rbc', 'ba', 'pcc', 'htn', 'dm', 'cad', 'pe', 'ane', 'classification', 'al'))
appetPredictor <- train(appet ~ ., 
                      data = specialData,
                      method = 'knn')
for (i in 1:nrow(data)) {
  if (is.na(data$appet[i])) data$appet[i] <- predict(appetPredictor, data[i, ])
}

# For Pe
data$pe <- as.factor(data$pe)
specialData <- data %>% filter(!is.na(pe)) %>% select(-c('su', 'rbc', 'ba', 'pcc', 'htn', 'dm', 'cad', 'appet', 'ane', 'classification', 'al'))
pePredictor <- train(pe ~ ., 
                        data = specialData,
                        method = 'knn')
for (i in 1:nrow(data)) {
  if (is.na(data$pe[i])) data$pe[i] <- predict(pePredictor, data[i, ])
}

data$ane <- as.factor(data$ane)
specialData <- data %>% filter(!is.na(ane)) %>% select(-c('su', 'rbc', 'ba', 'pcc', 'htn', 'dm', 'cad', 'appet', 'pe', 'classification', 'al'))
anePredictor <- train(ane ~ ., 
                     data = specialData,
                     method = 'knn')
for (i in 1:nrow(data)) {
  if (is.na(data$ane[i])) data$ane[i] <- predict(anePredictor, data[i, ])
}

write.csv(data, "data.csv")
