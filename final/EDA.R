## == Data Import ==

# Load the necessary packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(GGally)

# Set working directory
setwd(".")

# Clear the global environment
rm(list = ls())

# Import data
data <- read_csv("ImputedData.csv")


## == Data Pre-processing ==

# Taking a look at the data:
# str(data) ; summary(data)

# Changing some variable values for the plots later
data$pcc <- ifelse(data$pcc == "notpresent", "absent", "present")
data$ba <- ifelse(data$ba == "notpresent", "absent", "present")

# Type coercion to factor
data[, c('rbc', 'pc', 'pcc', 'ba', 'al')] <- lapply(data[, c('rbc', 'pc', 'pcc', 'ba', 'al')], FUN = as.factor)
data[, c('htn', 'dm', 'cad', 'appet', 'pe', 'ane')] <- lapply(data[, c('htn', 'dm', 'cad', 'appet', 'pe', 'ane')], FUN = as.factor)
data$classification <- factor(data$classification, levels = c("ckd", "notckd"), labels = c("Diseased", "Healthy"))

# Check structure of data
# str(data)


## == Exploratory Data Analysis ==

# Exploring the numerical variables
# wc and rc have been removed

num_var <- data[, c('age', 'bp', 'hemo', 'sg', 'pcv', 'bgr', 'bu', 'sc', 'sod', 'pot', 'classification')]
colnames(num_var) <- c("Age", "Blood Pressure", "Hemoglobin", "Specific Gravity", "Packed Cell Volume",
                       "Random Blood Glucose", "Blood Urea", "Serum Creatinine", "Sodium", "Potassium",
                       "Classification")

# Using GGally package to plot scatterplot correlogram:
ggpairs(num_var,
        columns = 1:10,
        mapping = aes(color = Classification, alpha = 0.5),
        title = "Multivariate Analysis of Continuous Variables",
        upper = list(continuous = wrap("cor", method = "pearson", use = "complete.obs", size = 2.5)),
        lower = list(continuous = "points"),
        diag = list(continuous = "densityDiag"),
        axisLabels = "show")

# Alternatively, if a correlation plot works better:
ggcorr(data = NULL, cor_matrix = cor(num_var[1:10], use = "complete.obs", method = "pearson"),
       label = TRUE, label_round = 2, label_color = "black", label_size = 3,
       palette = "RdBu")

# hemo / pcv, hemo / sg, and hemo / bu are highly correlated
# if we use hemo or pcv in our model, then we shouldn't use the other (and vice versa)

# Here is a parallel coordinate plot that I'm not sure whether it will be useful
ggparcoord(num_var,
           columns = 1:10,
           groupColumn = 11,
           scale = "std",
           order = "anyClass",
           showPoints = TRUE,
           alphaLines = 0.3,
           title = "Parallel Coordinate Plot for Numerical Variables") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 13)) +
  xlab("") +
  ylab("Scaled Values")

# Exploring the categorical variables

# Plot the blood test parameters
# set fixed y-limit so that the scales will not be distorted when plots are arranged

# rbc => marked difference between both groups of individuals (can be used as predictor)
p1 <- ggplot(data, aes(x = rbc)) +
      geom_bar(aes(fill = classification)) +
      facet_wrap(~ classification) +
      ylim(0, 250) +
      labs(x = "Red Blood Cell") +
      theme(legend.position = "none")

# pc => proportion of abnormal to normal pc in diseased and healthy patients looks very similar
p2 <- ggplot(data, aes(x = pc)) +
      geom_bar(aes(fill = classification)) +
      facet_wrap(~ classification) +
      ylim(0, 250) +
      labs(x = "Pus Cell") +
      theme(legend.position = "none")

# pcc => all absent in healthy individuals
p3 <- ggplot(data, aes(x = pcc)) +
      geom_bar(aes(fill = classification)) +
      facet_wrap(~ classification) +
      ylim(0, 250) +
      labs(x = "Pus Cell Clumps") +
      theme(legend.position = "none")

# ba => all absent in healthy individuals
p4 <- ggplot(data, aes(x = ba)) +
      geom_bar(aes(fill = classification)) +
      facet_wrap(~ classification) +
      ylim(0, 250) +
      labs(x = "Bacteria") +
      theme(legend.position = "none")

plot1 <- ggarrange(p1, p2, p3, p4,
                   ncol = 2, nrow = 2,
                   labels = c("A", "B", "C", "D"),
                   common.legend = TRUE, legend = "bottom")

annotate_figure(plot1, top = text_grob("Blood Test Parameters",
                              color = "red", face = "bold", size = 14))

# Plot co-morbities
# htn => all absent in healthy individuals
p5 <- ggplot(data, aes(x = htn)) +
      geom_bar(aes(fill = classification)) +
      facet_wrap(~ classification) +
      ylim(0, 250) +
      labs(x = "Hypertension") +
      theme(legend.position = "none")

# dm => all absent in healthy individuals
p6 <- ggplot(data, aes(x = dm)) +
      geom_bar(aes(fill = classification)) +
      facet_wrap(~ classification) +
      ylim(0, 250) +
      labs(x = "Diabetes Mellitus") +
      theme(legend.position = "none")

# cad => all absent in healthy individuals
p7 <- ggplot(data, aes(x = cad)) +
      geom_bar(aes(fill = classification)) +
      facet_wrap(~ classification) +
      ylim(0, 250) +
      labs(x = "Coronary Artery Disease") +
      theme(legend.position = "none")

# appet => all absent in healthy individuals
p8 <- ggplot(data, aes(x = appet)) +
      geom_bar(aes(fill = classification)) +
      facet_wrap(~ classification) +
      ylim(0, 250) +
      labs(x = "Appetite") +
      theme(legend.position = "none")

# pe => all absent in healthy individuals
p9 <- ggplot(data, aes(x = pe)) +
      geom_bar(aes(fill = classification)) +
      facet_wrap(~ classification) +
      ylim(0, 250) +
      labs(x = "Pedal Edema") +
      theme(legend.position = "none")

# ane => all absent in healthy individuals
p10 <- ggplot(data, aes(x = ane)) +
       geom_bar(aes(fill = classification)) +
       facet_wrap(~ classification) +
       ylim(0, 250) +
       labs(x = "Anemia") +
       theme(legend.position = "none")

plot2 <- ggarrange(p5, p6, p7, p8, p9, p10,
                   ncol = 2, nrow = 3,
                   labels = c("A", "B", "C", "D", "E", "F"),
                   common.legend = TRUE, legend = "bottom")

annotate_figure(plot2, top = text_grob("Presence of Co-morbidities",
                                       color = "red", face = "bold", size = 14))

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

#ggplot(data, aes(x = classification)) + geom_bar(aes(fill = classification)) +
#  facet_wrap(~agerange)

# I think that this could be helpful in our model.  The younger we get, 
# the lesser amount of people with kidney failure that we have.  

# Otherwise, I think taking what Edsel mentioned in the group chat into account,
# perhaps we wouldn't need to be so thorough after all.

# I suggest create a "base" model to improve on - perhaps using ALL features
# and then trying to improve upon it...

# Hoong Kai and Edsel - see if there's anything else within the data
# that you want to explore!
