setwd("C:/Users/aholl/Desktop/Data Science Portfolio/Project #1/project#1newdata/Colorado EPA Data")
#Explore 
#Clean
#Manipulate
#Describe
#Visualise 
#Analyse
library(readxl)
Book1 <- read_excel("Book1.xlsx", col_types = c("skip", 
                                                "text", "skip", "skip", "skip", "skip", 
                                                "skip", "skip", "skip", "skip", "skip", 
                                                "skip", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "skip", 
                                                "text", "skip", "text", "text", "text", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "skip", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "skip", "skip", "skip", "text", 
                                                "skip", "text", "text", "text", "skip", 
                                                "skip"))
View(Book1)

dim(hardrock)

str(hardrock)

glimpse(hardrock)


# Project Title: Predicting Water Quality at Hardrock Mines and Consumer versus Commercial Use of Chemicals in Colorado in 2020

# Project Description: This project will use data from the Colorado Department of Public Health and Environment to predict water quality at hardrock mines in Colorado. It will also use data from the Colorado Department of Public Health and Environment to determine the amount of chemicals used in Colorado in 2020 for consumer versus commercial use.

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(cowplot)
library(ggcorrplot)
library(corrplot)
library(caret)

library(rpart)
library(rpart.plot)
library(randomForest)
library(ranger)
library(e1071)
library(class)
library(kknn)
library(kernlab)
library(nnet)
library(neuralnet)
library(ROCR)
library(pROC)

# Read in data
hardrock <- Book1

# View data
View(hardrock)

# Check data structure
str(hardrock)

# Check data summary
summary(hardrock)

#head 
head(hardrock)

#tail
tail(hardrock)

#attach 
attach(hardrock)

names(hardrock)
length(hardrock)
class(hardrock$`CHEMICAL NAME`)
length(hardrock$`CHEMICAL NAME`)
unique(hardrock$`CHEMICAL NAME`)
table(hardrock$`CHEMICAL NAME`)

View(sort(table(hardrock$`CHEMICAL NAME`),decreasing = TRUE))
barplot(sort(table(hardrock$`CHEMICAL NAME`),decreasing = TRUE))

hardrock %>% 
  select(`CHEMICAL NAME`) %>% 
  count(`CHEMICAL NAME`) %>% 
  arrange(desc(n)) %>% 
  View()

(hardrock[is.na(hardrock$`C / C MAXIMUM CONCENTRATION`),])
view(hardrock[is.na(hardrock$`C / C MAXIMUM CONCENTRATION`),])
is.na(hardrock$`C / C MAXIMUM CONCENTRATION`)
-----------------
  # Install and load necessary packages
if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}
library(ggplot2)
library(e1071)

# Generate synthetic data for classification (you should replace this with your actual data)
set.seed(123)
data <- data.frame(
  x1 = rnorm(100, mean = 0, sd = 1),
  x2 = rnorm(100, mean = 0, sd = 1),
  class = factor(sample(0:1, 100, replace = TRUE))
)

# Train an SVM classification model (you should replace this with your model)
svm_model <- svm(class ~ ., data = data, kernel = "linear")

# Create a meshgrid for plotting decision boundaries
x1_range <- seq(min(data$x1), max(data$x1), by = 0.1)
x2_range <- seq(min(data$x2), max(data$x2), by = 0.1)
grid <- expand.grid(x1 = x1_range, x2 = x2_range)
grid$predicted_class <- predict(svm_model, newdata = grid)

# Create a colorful plot to visualize decision boundaries
ggplot(data, aes(x = x1, y = x2, color = class)) +
  geom_point() +
  geom_contour(aes(z = as.numeric(predicted_class)),
               data = grid, color = "black", breaks = c(0, 1),
               alpha = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "SVM Classification Decision Boundaries") +
  theme_minimal()














# Check data
hardrock$`CHEMICAL NAME` <- as.factor(hardrock$`CHEMICAL NAME`)
hardrock$`EPA REGION` <- as.numeric(hardrock$`EPA REGION`)

#Predicting Water Quality at Hardrock Mines and Consumer versus Commercial Use of Chemicals in Colorado in 2020
# predict model for water quality at hardrock mines in Colorado
# create training and testing data sets
set.seed(123)
trainIndex <- createDataPartition(hardrock$`CHEMICAL NAME`, p = .8, list = FALSE)
train <- hardrock[ trainIndex,]
test <- hardrock[-trainIndex,]


ggplot(train, aes(x = `EPA REGION`, y = `EPA REGION`)) + geom_point() + theme_bw()

nurueal network model for water quality at hardrock mines in Colorado  using the neuralnet package    
# create training and testing data sets
set.seed(123)
trainIndex <- createDataPartition(hardrock$`EPA REGION`, p = .8, list = FALSE)
train <- hardrock[ trainIndex,]
test <- hardrock[-trainIndex,]



# create neural network model
set.seed(123)
nnet_model <- neuralnet(`EPA REGION` ~ ., data = train, hidden = 2, linear.output = FALSE)

# Check data
nnet_model

# Check data
plot(nnet_model)

# Check data
nnet_pred <- compute(nnet_model, test[1:2])

# Check data
nnet_pred$net.result

# Check data
nnet_pred$net.result <- round(nnet_pred$net.result)

# Check data
nnet_pred$net.result

# Check data
nnet_pred$net.result <- as.factor(nnet_pred$net.result)

# Check data
nnet_pred$net.result

# Check data
nnet_pred$net.result <- as.numeric(nnet_pred$net.result)

# Check data
nnet_pred$net.result
----------------
Toxicology Data Network model for water quality at hardrock mines in Colorado using the neuralnet package r code for neural network model
# create training and testing data sets
set.seed(123)
trainIndex <- createDataPartition(hardrock$`EPA REGION`, p = .8, list = FALSE)
train <- hardrock[ trainIndex,]
test <- hardrock[-trainIndex,]

# create neural network model
set.seed(123)
nnet_model <- neuralnet(`EPA REGION` ~ ., data = train, hidden = 2, linear.output = FALSE)

# Check data
nnet_model

# Check data
plot(nnet_model)

# Check data
nnet_pred <- compute(nnet_model, test[1:2])

# Check data
nnet_pred$net.result

# Check data
nnet_pred$net.result <- round(nnet_pred$net.result)
----------------------------
geographic information system model for water quality at hardrock mines in Colorado using ggplot2 package and ggcorrplot package r code for geographic information system model
# create training and testing data sets
set.seed(123)
trainIndex <- createDataPartition(hardrock$`EPA REGION`, p = .8, list = FALSE)
train <- hardrock[ trainIndex,]
test <- hardrock[-trainIndex,]

# create heatmap for daily temperature data set using ggplot2 package and ggcorrplot package
ggplot(train, aes(x = `EPA REGION`, y = `EPA REGION`)) + geom_point() + theme_bw() + ggtitle("Daily Temperature Heat Map") + theme(plot.title = element_text(hjust = 0.5))

#extract cynanide data from hardrock data set and create new data set for cynanide data only
cynanide <- hardrock[ , c("CYNANIDE TOTAL","CYNANIDE TOTAL UNITS","CYNANIDE TOTAL DATE","CYNANIDE TOTAL DATE TYPE","CYNANIDE TOTAL QUALIFIER","CYNANIDE TOTAL COMMENT")]

# Check data
cynanide

# Check data
str(cynanide)

# Check data
summary(cynanide)

# Check data
View(cynanide)

# Check data
cynanide$`CYNANIDE TOTAL` <- as.numeric(cynanide$`CYNANIDE TOTAL`)
cynanide$`CYNANIDE TOTAL` <- as.factor(cynanide$`CYNANIDE TOTAL`)

tibble::lst(Book1$`CHEMICAL NAME`)


