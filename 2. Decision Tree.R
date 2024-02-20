library(caret)
library(rpart)
library(ROSE)
library(rpart.plot)
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)





getwd()
setwd("/Users/ali/Downloads")
df<-read.delim("2.Bank_India_preprocessed_data.csv", sep=",")
df

prop.table(table(df$target)) * 100 # unbalanced
prop.table(table(df$gender)) * 100 # unbalanced


df <- df[, -which(names(df) == "id")]

df$target <- factor(df$target)

#divide data
set.seed(1234)
ind <- sample(2, nrow(df), replace = T, prob = c(0.8, 0.2))
train <- df[ind == 1,]
test <- df[ind == 2,]


#small changes in preprocessing
train <- train[train$job_type != "Religion", ]
train <- train[train$job_type != "Cleaning", ]
train <- train[train$job_type != "Mobile", ]
test <- test[test$job_type != "Religion", ]
test <- test[test$job_type != "Mobile", ]
test <- test[test$job_type != "Cleaning", ]
test <- test[test$job_type != "Insurance", ]


# balancig train
data_balanced_over <- ovun.sample(target ~ ., data = train, method = "over",N=3643*2)$data
table(data_balanced_over$target)

data_balanced_under <- ovun.sample(target ~ ., data = train, method = "under", N = 344*2, seed = 1)$data
table(data_balanced_under$target)

data_balanced_both <- ovun.sample(target ~ ., data = train, method = "both", p=0.5,N=900, seed = 1)$data
table(data_balanced_both$target)


# decision tree model
oj_mdl_cart_full <- rpart(formula = target ~ ., data = data_balanced_both,method = "class",cp=0.007)
oj_mdl_cart_full

# choose cp
printcp(oj_mdl_cart_full)
head(oj_mdl_cart_full$cptable, 10)
xerror <- oj_mdl_cart_full$cptable[,"xerror"]
imin.xerror <- which.min(xerror)
oj_mdl_cart_full$cptable[imin.xerror, ]
upper.xerror <- xerror[imin.xerror] + oj_mdl_cart_full$cptable[imin.xerror, "xstd"]
icp <- min(which(xerror <= upper.xerror))
cp <- oj_mdl_cart_full$cptable[icp, "CP"]
cp
tree <- prune(oj_mdl_cart_full, cp = cp) ##or cp= cp from line 44
# plot
rpart.plot(oj_mdl_cart_full, yesno = TRUE)

#rules
rpart.rules(oj_mdl_cart_full, style = "tall")
# summary
summary(tree)

importance <- oj_mdl_cart_full$variable.importance # Equivalente a caret::varImp(tree) 
importance <- round(100*importance/sum(importance), 1)
importance[importance >= 1]


###Evaluation
#Confusion matrix - train

p <- predict(oj_mdl_cart_full, data_balanced_both, type = 'class')
confusionMatrix(p, data_balanced_both$target, positive="payed")

# predict test
p2 <- predict(oj_mdl_cart_full, test, type = 'class')
confusionMatrix(p2, test$target, positive="payed")



####


# Random forest desbalanceo usando oversampling
set.seed(12345)
trControl = trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final",  
  classProbs = TRUE,  
  summaryFunction = twoClassSummary,
  allowParallel = TRUE,
  sampling = "up")

# Random forest

mod.rf.over <- train(
  target ~ ., 
  data = train, 
  method = "rf",
  tuneGrid = expand.grid(mtry = 3:4),
  trControl = trControl,
  metric = "ROC"
)

mod.rf.over























# validation
pred2 <-  predict(oj_mdl_cart_full, newdata = data_balanced_both, type = "prob")[,"payed"]

roc.car <- roc(data_balanced_both$target, pred2, 
               print.auc=TRUE, 
               ci=TRUE,
               plot=TRUE)

# Bagged tree
trControl = trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",  
  classProbs = TRUE,  
  summaryFunction = twoClassSummary
)

set.seed(1234)
mod.bag <- train(
  target ~ ., 
  data = data_balanced_both, 
  method = "treebag",
  trControl = trControl,
  metric = "ROC"
)

# Importancia
plot(varImp(mod.bag), main="Importancia de variables con Bagging")

# Predicción
preds <- predict(mod.bag, newdata=test)
confusionMatrix(test$target, preds)


set.seed(1234)
mod.rf <- train(
  target ~ ., 
  data = data_balanced_both, 
  method = "rf",
  tuneGrid = expand.grid(mtry = 3:6),
  trControl = trControl,
  metric = "ROC"
)



p2 <- predict(oj_mdl_cart_full, test, type = 'class')

# recall y precision
confusionMatrix(p2, test$target, positive="payed")
### Help for Confusion Matrix ---> https://towardsdatascience.com/understanding-confusion-matrix-a9ad42dcfd62
### Recall,Precision and Accuracy should be high as possible

#Confusion matrix -train
library(dplyr)

oj_preds_cart <- bind_cols(
  predict(oj_mdl_cart_full, newdata = test, type = "prob"),
  predicted = predict(oj_mdl_cart_full, newdata = test, type = "class"),
  actual = test$target
)
# pureza nodos, caminos mas importantes, frequencia nodos .
# comparar arbol grande , 
# modelos supervisds con supervisados i no supervisados 
# comparar patropnes comunes de todos los modelos, 
# cluste
# random forest 

oj_cm_cart <- confusionMatrix(oj_preds_cart$predicted, reference = oj_preds_cart$actual)
oj_cm_cart
