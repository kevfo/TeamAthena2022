## == Data Import ==

# Load the necessary packages
library(tidyverse)
library(caret)

# Set working directory
setwd(" ")

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

# remove ages 0 to 5 as it might be inaccurate to impute the other measurements
data <- data %>% filter(age > 5)

# Analyzing "white blood cell count" and "red blood cell count" variables
summary(data$wc) ; summary(data$rc)

# wc and rc have at least 25% missing data, therefore remove them as data might
# become biased as a result of median imputation
data <- data %>% select(-c(wc, rc))


## == Imputing Numerical Variables with Median ==

# Determine variables with NAs
colSums(is.na(data))

# Construct multiple boxplots
# to impute NAs with medians because there is a large range of data
par(mfrow = c(3, 3))
boxplot(bp ~ classification, data) ; boxplot(sg ~ classification, data) ; boxplot(bgr ~ classification, data)
boxplot(bu ~ classification, data) ; boxplot(sc ~ classification, data) ; boxplot(sod ~ classification, data)
boxplot(pot ~ classification, data) ; boxplot(hemo ~ classification, data) ; boxplot(pcv ~ classification, data)

# Split the dataset by classification to compute them separately
ckd <- data[data$classification == "ckd", ]
notckd <- data[data$classification == "notckd", ]

# Analyzing "blood pressure" variable
summary(ckd$bp) ; summary(notckd$bp)

for (i in which(is.na(data$bp))) {
  data[i, ]$bp <- ifelse(data[i, ]$classification == "ckd",
                         median(ckd$bp, na.rm = TRUE),
                         median(notckd$bp, na.rm = TRUE))
}

# Analyzing "specific gravity" variable
summary(ckd$sg) ; summary(notckd$sg)

for (i in which(is.na(data$sg))) {
  data[i, ]$sg <- ifelse(data[i, ]$classification == "ckd",
                         median(ckd$sg, na.rm = TRUE),
                         median(notckd$sg, na.rm = TRUE))
}

# Analyzing "blood glucose random" variable
par(mfrow = c(1, 2))
hist(data$bgr) ; hist(ckd$bgr)
summary(ckd$bgr) ; summary(notckd$bgr)

for (i in which(is.na(data$bgr))) {
  data[i, ]$bgr <- ifelse(data[i, ]$classification == "ckd",
                          median(ckd$bgr, na.rm = TRUE),
                          median(notckd$bgr, na.rm = TRUE))
}

# Analyzing "blood urea" variable
summary(ckd$bu) ; summary(notckd$bu)

for (i in which(is.na(data$bu))) {
  data[i, ]$bu <- ifelse(data[i, ]$classification == "ckd",
                         median(ckd$bu, na.rm = TRUE),
                         median(notckd$bu, na.rm = TRUE))
}

# Analyzing "serum creatinine" variable
summary(ckd$sc) ; summary(notckd$sc)

for (i in which(is.na(data$sc))) {
  data[i, ]$sc <- ifelse(data[i, ]$classification == "ckd",
                         median(ckd$sc, na.rm = TRUE),
                         median(notckd$sc, na.rm = TRUE))
}

# Analyzing "sodium" variable
summary(ckd$sod) ; summary(notckd$sod)

for (i in which(is.na(data$sod))) {
  data[i, ]$sod <- ifelse(data[i, ]$classification == "ckd",
                          median(ckd$sod, na.rm = TRUE),
                          median(notckd$sod, na.rm = TRUE))
}

# Analyzing "potassium" variable
summary(ckd$pot) ; summary(notckd$pot)

for (i in which(is.na(data$pot))) {
  data[i, ]$pot <- ifelse(data[i, ]$classification == "ckd",
                          median(ckd$pot, na.rm = TRUE),
                          median(notckd$pot, na.rm = TRUE))
}

# Analyzing "hemoglobin" variable
summary(ckd$hemo) ; summary(notckd$hemo)

for (i in which(is.na(data$hemo))) {
  data[i, ]$hemo <- ifelse(data[i, ]$classification == "ckd",
                           median(ckd$hemo, na.rm = TRUE),
                           median(notckd$hemo, na.rm = TRUE))
}

# Analyzing "packed cell volume" variable
summary(ckd$pcv) ; summary(notckd$pcv)

for (i in which(is.na(data$pcv))) {
  data[i, ]$pcv <- ifelse(data[i, ]$classification == "ckd",
                          median(ckd$pcv, na.rm = TRUE),
                          median(notckd$pcv, na.rm = TRUE))
}


## == Imputing Categorical Variables using kNN ==

# Determine variables with NAs
colSums(is.na(data))

# Predicting for "albumin" variable
specialData <- data %>%
  filter(!is.na(al)) %>%
  select(-c('su', 'rbc', 'pc', 'pcc', 'ba', 'htn', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification'))

alPredictor <- train(al ~ ., data = specialData, method = 'knn')

for (i in 1:nrow(data)) {
  if (is.na(data$al[i])) data$al[i] <- predict(alPredictor, data[i, ])
}

# Predicting for "sugar" variable
specialData <- data %>%
  filter(!is.na(su)) %>%
  select(-c('al', 'rbc', 'pc', 'pcc', 'ba', 'htn', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification'))

suPredictor <- train(su ~ ., data = specialData, method = 'knn')

for (i in 1:nrow(data)) {
  if (is.na(data$su[i])) data$su[i] <- predict(suPredictor, data[i, ])
}

# Predicting for "red blood cell" variable
specialData <- data %>%
  filter(!is.na(rbc)) %>%
  select(-c('al', 'su', 'pc', 'pcc', 'ba', 'htn', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification'))

rbcPredictor <- train(rbc ~ ., data = specialData, method = 'knn')

for (i in 1:nrow(data)) {
  if (is.na(data$rbc[i])) data$rbc[i] <- predict(rbcPredictor, data[i, ])
}

# Predicting for "pus cell" variable
specialData <- data %>%
  filter(!is.na(pc)) %>%
  select(-c('al', 'su', 'rbc', 'pcc', 'ba', 'htn', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification'))

pcPredictor <- train(pc ~ ., data = specialData, method = 'knn')

for (i in 1:nrow(data)) {
  if (is.na(data$pc[i])) data$pc[i] <- predict(pcPredictor, data[i, ])
}

# Predicting for "pus cell clumps" variable
specialData <- data %>%
  filter(!is.na(pcc)) %>%
  select(-c('al', 'su', 'rbc', 'pc', 'ba', 'htn', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification'))

pccPredictor <- train(pcc ~ ., data = specialData, method = 'knn')

for (i in 1:nrow(data)) {
  if (is.na(data$pcc[i])) data$pcc[i] <- predict(pccPredictor, data[i, ])
}

# Predicting for "bacteria" variable
specialData <- data %>%
  filter(!is.na(ba)) %>%
  select(-c('al', 'su', 'rbc', 'pc', 'pcc', 'htn', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification'))

baPredictor <- train(ba ~ ., data = specialData, method = 'knn')

for (i in 1:nrow(data)) {
  if (is.na(data$ba[i])) data$ba[i] <- predict(baPredictor, data[i, ])
}

# Predicting for "hypertension" variable
specialData <- data %>%
  filter(!is.na(htn)) %>%
  select(-c('al', 'su', 'rbc', 'pc', 'pcc', 'ba', 'dm', 'cad', 'appet', 'pe', 'ane', 'classification'))

htnPredictor <- train(htn ~ ., data = specialData, method = 'knn')

for (i in 1:nrow(data)) {
  if (is.na(data$htn[i])) data$htn[i] <- predict(htnPredictor, data[i, ])
}

# Predicting for "diabetus mellitus" variable
specialData <- data %>%
  filter(!is.na(dm)) %>%
  select(-c('al', 'su', 'rbc', 'pc', 'pcc', 'ba', 'htn', 'cad', 'appet', 'pe', 'ane', 'classification'))

dmPredictor <- train(dm ~ ., data = specialData, method = 'knn')

for (i in 1:nrow(data)) {
  if (is.na(data$dm[i])) data$dm[i] <- predict(dmPredictor, data[i, ])
}

# Predicting for "coronary artery disease" variable
specialData <- data %>%
  filter(!is.na(cad)) %>%
  select(-c('al', 'su', 'rbc', 'pc', 'pcc', 'ba', 'htn', 'dm', 'appet', 'pe', 'ane', 'classification'))

cadPredictor <- train(cad ~ ., data = specialData, method = 'knn')

for (i in 1:nrow(data)) {
  if (is.na(data$cad[i])) data$cad[i] <- predict(cadPredictor, data[i, ])
}

# Predicting for "appetite" variable
specialData <- data %>%
  filter(!is.na(appet)) %>%
  select(-c('al', 'su', 'rbc', 'pc', 'pcc', 'ba', 'htn', 'dm', 'cad', 'pe', 'ane', 'classification'))

appetPredictor <- train(appet ~ ., data = specialData, method = 'knn')

for (i in 1:nrow(data)) {
  if (is.na(data$appet[i])) data$appet[i] <- predict(appetPredictor, data[i, ])
}

# Predicting for "pedal edema" variable
specialData <- data %>%
  filter(!is.na(pe)) %>%
  select(-c('al', 'su', 'rbc', 'pc', 'pcc', 'ba', 'htn', 'dm', 'cad', 'appet', 'ane', 'classification'))

pePredictor <- train(pe ~ ., data = specialData, method = 'knn')

for (i in 1:nrow(data)) {
  if (is.na(data$pe[i])) data$pe[i] <- predict(pePredictor, data[i, ])
}

# Predicting for "anaemia" variable
specialData <- data %>%
  filter(!is.na(ane)) %>%
  select(-c('al', 'su', 'rbc', 'pc', 'pcc', 'ba', 'htn', 'dm', 'cad', 'appet', 'pe', 'classification'))

anePredictor <- train(ane ~ ., data = specialData, method = 'knn')

for (i in 1:nrow(data)) {
  if (is.na(data$ane[i])) data$ane[i] <- predict(anePredictor, data[i, ])
}

# Check variables with NAs again
colSums(is.na(data))

# Export the processed data
write.csv(data, "data.csv")
