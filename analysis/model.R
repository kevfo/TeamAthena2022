library(tidyverse) ; library(caret) ; library(vip) ; library(dplyr)
library(rattle) ; library(ROCR) ; library(car)

setwd("C:/Users/Kevin/Desktop/Projects/R/TeamAthena2022/analysis/")
source("EDA.R")

# This is a function to make an AUC curve: a plot of a classifier's
# TPR versus its FPR.  Feel free to ask any questions that you may have!
# PS: change "color = T" if you want color according to probability cutoffs.
# We can include the ROC curve for the random forest in our 
# shinyflexboard presentation...

makeROC <- function(model, testData, title, color = F) {
  binaryPred <- predict(model, testData) ; binaryPred <- ifelse(binaryPred == 'Diseased', 1, 0) 
  binaryActual <- ifelse(testData$classification == 'Diseased', 1, 0)
  
  # Compute the AUC value:
  AUCval <- binaryPred %>% prediction(binaryActual) %>% 
    performance(measure = 'auc')
  AUCval <- AUCval@y.values[[1]]
  
  # Make the graph:
  binaryPred %>% prediction(binaryActual) %>% 
    performance('tpr', 'fpr') %>% plot(main = paste0("ROC Curve of ", title),
                                       colorize = color)
grid(lty = 'dashed', lwd = 1.5) ; text(paste0("AUC: ", AUCval),
                                       x = 0.7, y = 0.3)
}

# Now that we have our data... I think now's also a good time to begin training 
# our first models...

# I think we can try a 60% / 40% split.  We don't want too much data in either
# our training or testing sets - otherwise, our results will be highly variable.
# There is no hard rule for splitting data - I suppose the only way to do this is 
# via trial and error and intuition.  Nevertheless, I also set a seed to some 
# arbitrary number - this is to ensure reproducibility.

set.seed(123)
index <- createDataPartition(data$classification, p = 0.6, list = FALSE)
training <- data[index, ] ; testing <- data[-index, ]

# And also check the dimensions of our training and our testing set to 
# make sure that everything looks right!
dim(training) ; dim(testing)

# I think that we should also try cross-validation.  From what I know, 
# k = 10 is a pretty common number (don't know why though) - nevertheless,
# cross validation also helps to ensure that our training data isn't biased
# against any one class in particular (i.e., whether somebody has kidney
# disease or not).  Just looking at the data, about 60% of our data represents
# diseased patients.
kfold <- trainControl(method = "cv", number = 10)

# Make the model using caret.  Perhaps what we can (initially) do is to 
# create a random forest model with ALL predictors:
rfInitial <- train(classification ~ ., data = training, method = 'rf', 
                   trControl = kfold)

# And let's evaluate the model's performance (sensitivity 0.9896, accuracy = 0.9871).  
# From the confusion matrix, it seems like our initial random forest model is 
# highly accurate and highly sensitive (in this context, I think that it makes 
# sense to maximize sensitivity - better false positives than false negatives).
#
# However, before we jump to any conclusions, I think that our model's performance
# is suspicious because:
#
# 1) We still gotta take real world factors into account (i.e., Edsel's findings).
#
# 2) I think it helps to compare the model's performance against other models.
#    Random forests are usually the go-to model since they perform so well, but
#    if even other simpler models perform this well, then something could be off.
#
rfPredictions <- predict(rfInitial, testing)
confusionMatrix(testing$classification, rfPredictions)

# Otherwise, I even made a ROC curve to go with the confusion matrix.  It
# had a very high AUC value, hence implying that this random forest model
# of ours is too good.:
# makeROC(rfInitial, testing, "RF", T)

# But while we're here, taking what Edsel found into account, our updated 
# data frame is gonna look like this (15 total predictors):
data <- data %>% select(c(age, agerange, bp, sg, al, su, 
                          bgr, bu, sc, sod, pot,
                          htn, dm, cad, appet, pe, classification
                          ))

# Re-train the model?

set.seed(123)
index <- createDataPartition(data$classification, p = 0.6, list = FALSE)
training <- data[index, ] ; testing <- data[-index, ]

rfNewer <- train(classification ~ ., data = training, method = 'rf', 
                trControl = kfold)

# Now, we have an accuracy of 0.9935 and a sensitivity of 1.  Out of 
# curiosity, I also constructed a variable importance plot to see which
# values are actually contributing to the trees in the random forests:
rfPredictions <- predict(rfNewer, testing)
# confusionMatrix(testing$classification, rfPredictions)
# vip(rfNewer, geom = "point")

# I also decided to make a cumulative variable importance plot in the form 
# of a bar plot:
viStats <- vi_model(rfNewer) %>% arrange(desc(Importance))
(cumsum(viStats$Importance) / sum(viStats$Importance)) %>% 
  barplot(col = 'skyblue', las = 2, ylim = c(0, 1), 
          names = viStats$Variable, main = "Cumulative Variable Importance Plot of Newer Random Forest Model",
          ylab = "Cumulative Importance", xlab = "Variable")
abline(h = 0.9, lty = 'dashed', col = 'red', lwd = 1.5)
text(x = 3, y = 0.97, "Cutoff")  # include this in shinyflexboard

# I also wonder if we need that many predictor variables to begin with.
# At least according to R (see the above block of code), a good portion of 
# features don't contribute much (if anything) to the splits of the 
# decision trees in the random forest.
#
# For no particular reason, I suggest using a cutoff at around 90%.
# Past the sixth bar, the importance doesn't seem to improve by that a very
# substantial amount.  The more features we have, the longer it takes to train
# our model, and I'm not very sure if it's worth that extra training time...

data2 <- data %>% select(c(viStats$Variable[1:4], htn, 
                           viStats$Variable[6:7], dm, bp, 
                           pe, age, su, classification))

set.seed(123)
index <- createDataPartition(data2$classification, p = 0.6, list = F)
training <- data2[index, ] ; testing <- data2[-index, ]

dim(training) ; dim(testing)
rfWithVip <- train(classification ~ ., data = training, method = 'rf', trControl = kfold)

# Accuracy: 96.13%, sensitivity: 1
rfPredictions <- predict(rfWithVip, testing)
# confusionMatrix(testing$classification, rfPredictions)
# makeROC(rfWithVip, testing, "Random Forest", T)

# So far, we have three models:
#
# 1) rfInitial => this is the model that we started off with.
#                 highly accurate, but also not very in line with 
#                 Edsel's findings (huge problem in terms of practicality)
#
# 2) rfNewer   => this is the model with Edsel's chosen parameters.  It has
#                 high sensitivity and accuracy (even more so than rfInitial).
#                 theoretically, this also means more work for the user to do to 
#                 use the model.
#
# 3) rfWithVip => similar to rfNewer, but with only six predictors.  It's fast 
#                 to train (~6 seconds) but comes at a slight cost of accuracy.

# == Testing other models == 

# Like I mentioned in the second limitation, random forests are very popular
# because of their results, but before getting our hopes up too much, I think it's
# wise to compare our random forest's model with other, simpler models using
# Edsel's chosen variables.
#
# I'm thinking of training the following models on the data that rfWithVip
# was trained using:
#
# 1) A single decision tree
# 2) Logistic regression
# 3) kNN (bad idea)

# -- Decision tree -- (96.77 % accuracy, 94.79% sensitive)

rpartBench <- train(classification ~ ., data = training, method = 'rpart',
                    trControl = kfold)
# rpartBench %>% predict(testing) %>% confusionMatrix(testing$classification)

# makeROC(rpartBench, testing, "Decision Tree", T)

# -- Logistic regression -- (97.42% accuracy, 95.83% sensitive)

logBench <- glm(classification ~ ., family = binomial, data = training)

# Is multicolinearity a problem in logBench?
# car::vif(logBench)

# Just to make sure of something...
# contrasts(training$classification)

# So our model performs very well when it comes to its own data.  What 
# about testing data?
predictions <- predict(logBench, type = 'response')
ifelse(predictions < 0.5, "Diseased", 'Healthy') %>% as.factor() %>% confusionMatrix(training$classification)

# Testing data (95.48 % accuracy, 92.71% sensitive):
predictions <- predict(logBench, newdata = testing, type = 'response')
ifelse(predictions < 0.5, 'Diseased', 'Healthy') %>% as.factor() %>% confusionMatrix(testing$classification)

# Make the ROC curve:
# logTestPred <- ifelse(predictions < 0.5, 'Diseased', 'Healthy') 
# logAucValue <- ifelse(logTestPred == 'Diseased', 0, 1) %>% 
#  prediction(ifelse(testing$classification == 'Diseased', 0, 1)) %>% performance(measure = 'auc')
# logAucValue <- logAucValue@y.values[[1]]

# ifelse(logTestPred == 'Diseased', 0, 1) %>% prediction(ifelse(testing$classification == 'Diseased', 0, 1)) %>% 
#   performance('tpr', 'fpr') %>% plot(main = 'ROC Curve of Logistic Regression',
#                                     colorize = T)
# grid(lty = 'dashed', lwd = 1.5) ; text(x = 0.7, y = 0.3, paste0("AUC: ", logAucValue))

# -- kNN -- (bad idea to begin with - 82.58% accuracy, 77.08% sensitive)
knnBench <- train(classification ~ sc + sg + sod + bu + bgr, data = training, method = 'knn', trControl = kfold)
# knnBench

# predict(knnBench, testing) %>% confusionMatrix(testing$classification)

#m akeROC(knnBench, testing, "kNN", T)
