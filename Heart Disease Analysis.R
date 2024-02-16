
library(readr)
library(tidyverse)
library(ggplot2)
library(dbplyr)
library(tidyr)
library(data.table)
library(ggthemes)
library(e1071)
library(pROC)
library(blorr)
library(kernlab)

#1. Take a glance into the dataset
df <- read_csv('/Users/mac/Desktop/heart.csv')
head(df)
summary(df)
shape <- c(nrow(df),ncol(df))

#2. Data Transformation

df2 <- df %>% mutate(sex = if_else(sex==1, "Male", "Female"),
                     fbs = if_else(fbs == 1, ">120", "<120"),
                     exang = if_else(exang == 1, "Yes", "No"),
                     cp = if_else(cp == 1, "Atypical Angina",
                                  if_else(cp == 2, "Non-Anginal Pain", "Asymptomatic")),
                     restecg = if_else(restecg == 0, "Normal",
                                       if_else(restecg == 1,"Abnormality", "Probable On Definite")),
                     slope = as.factor(slope),
                     ca = as.factor(thal),
                     target = if_else(target == 1, "Yes", "No")
                     )%>% mutate_if(is.character, as.factor) %>%
  select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())

#3. Data Visualization

#a:Barplot: Target

ggplot(df2, aes(x = target, fill = target)) + geom_bar() +
  xlab("Heart Disease")+
  ylab("Count")+
  ggtitle("Presence & Absence of Heart Disease") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absense", "Presence"))

#b: Proportion of Heart Disease

prob <- table(df2$target)/length(df2$target)
prob

#The dataset is quite balanced

#c: Create frequency plot of age
df2 %>% ggplot(aes(age)) + geom_histogram(fill = 'red', alpha = 0.5) + ggtitle("Age Distribution") + xlab("Age") + ylab("Count")


#d. Compare blood pressure across the chest pain
df2 %>% ggplot(aes(x=sex, y=trestbps)) + geom_boxplot(fill = 'blue') + xlab('Sex') + ylab('Blood Pressure') + facet_grid(.~cp)

#e. Compare  cholesterol level across the chest pain
df2 %>% ggplot(aes(x=sex, y=chol)) + geom_boxplot(fill = 'green') + xlab('Sex') + ylab('Blood Pressure') + facet_grid(.~cp)

#f. Correlation
library(corrplot)

cor_heart <- cor(df2[, 10:14])
cor_heart

corrplot(cor_heart, method = 'square', type = 'upper')
corrplot(cor_heart, method = 'square', type = 'lower')

#4. Data Cleaning

is.null(df2)

#4. Modelling and Prediction

library(caret)
set.seed(10)

#a. Train test split
sample <- sample(c(TRUE, FALSE), nrow(df2), replace=TRUE, prob=c(0.7,0.3))
train.set  <- df2[sample, ]
test.set   <- df2[!sample, ]


AUC = list()
Accuracy = list()

#b. Random Forest
library(randomForest)
set.seed(10)
RF_Model <- randomForest(target ~ .,
                        data=train.set, 
                        importance=TRUE, 
                        ntree=200)
#varImpPlot(RFModel)
RF_Prediction <- predict(RF_Model, test.set)
RF_Prediction_prob = predict(RF_Model,test.set,type="prob")[, 2]

RFConfMat <-table(test.set$target, RF_Prediction_prob > 0.5)

AUC$RF <- roc(as.numeric(test.set$target),as.numeric(as.matrix((RF_Prediction_prob))))

fitted.results <- ifelse(RF_Prediction_prob > 0.5,"Yes","No")
misClasificError <- mean(fitted.results != test.set$target)
print(paste('Accuracy',1-misClasificError))


#c. Support Vector Machine
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)


set.seed(10)
svm_Model <- train(target ~ ., data = train.set,
                  method = "svmRadial",
                  trControl = fitControl,
                  preProcess = c("center", "scale"),
                  tuneLength = 8,
                  metric = "ROC")
svm_Prediction <- predict(svm_Model, test.set)
svm_Predictionprob <- predict(svm_Model, test.set, type='prob')[2]

svmConfMat <-table(test.set$target, svmPredictionprob > 0.5)
#ROC Curve
AUC$svm <- roc(as.numeric(test.set$target),as.numeric(as.matrix((svm_Predictionprob))))


fitted.results2 <- ifelse(svm_Predictionprob > 0.5,"Yes","No")
misClasificError2 <- mean(fitted.results != test.set$target)
print(paste('Accuracy',1-misClasificError))

AUC








