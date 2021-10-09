#################################################################
# Data Split

data$SALE <- as.factor(data$SALE)

# Set seed
set.seed(123)

# Split data, 75% distribution of churn for training
train.index <- createDataPartition(
  y = dataca$SALE, p = 0.75, list = FALSE
)

train <- data[train.index,]
test <- data[-train.index,]

# full logistic model 
data <- data %>%
  dplyr::select(
    -QUOTEINT
  )

####################################################################
# Training & Evaluation
# 3 x 5-fold cross validation
fitCtrl = trainControl(method="repeatedcv", number=5, repeats=3)

# Fit model
tree.fit <- rpart(
  SALE ~ ., 
  data = data, 
  method = "class"
)

# Graph of tree
rpart.plot::rpart.plot(
  tree.fit,
  type = 4,
  extra = 2,
  under = TRUE,
  fallen.leaves = F
)

# Training a Decision Tree, One Rule, Boosting Models, Naive Bayes, GLM, KNN using the metric "Accuracy"
modelDT = train(formula_with_most_important_attributes, data=train, method="J48", trControl=fitCtrl, metric="Accuracy",na.action = na.pass)
#modelOneR = train(formula_with_most_important_attributes, data=train, method="OneR", trControl=fitCtrl, metric="Accuracy",na.action = na.pass)
modelBoost = train(formula_with_most_important_attributes, data=train, method="LogitBoost", trControl=fitCtrl, metric="Accuracy",na.action = na.pass)
#modelnb = train(formula_with_most_important_attributes, data=train, method="nb", trControl=fitCtrl, metric="Accuracy",na.action = na.pass)
modelGLM = train(formula_with_most_important_attributes, data=train, method="glm", family='binomial', trControl=fitCtrl, metric="Accuracy",na.action = na.pass)
modelKNN = train(formula_with_most_important_attributes, data=train, method="knn", trControl=fitCtrl, metric="Accuracy",na.action = na.pass)



#DT GRAPH
# Fit model
tree.fit <- rpart(
  SALE ~ ., 
  data = data, 
  method = "class"
)



options(repr.plot.width = 10, repr.plot.height = 10)

# Graph of tree
rpart.plot(
  tree.fit,
  type = 4,
  extra = 2,
  under = TRUE,
  fallen.leaves = F
)




library(rattle)
fancyRpartPlot(modelDT$finalModel)
plot(modelDT)
install.packages("partykit", repos='http://cran.us.r-project.org')

plot(modelDT$finalModel)

# Compare results of different models
res = resamples(list(dt=modelDT, boost =modelBoost, GLM=modelGLM, KNN=modelKNN))
summary(res)


# Show confusion matrix (in percent)
confusionMatrix(modelDT)
confusionMatrix(modelBoost)
#confusionMatrix(modelOneR)
#confusionMatrix(modelnb)
confusionMatrix(modelGLM)
confusionMatrix(modelKNN)


# Based on the results the top models are the Decision Tree and Boost But it seems slightly higher for the DT and for that reason this model will be chosen. 
# The accuracy of the model is 93.51% 
##########################


####################################################################
## Predict classes in test data

prediction_classes = predict.train(object=modelDT, newdata=test, na.action=na.pass)
predictions = data.frame(id=test$Id, prediction=prediction_classes) ###create ID column


######################################################
# Export the predictions
write.csv(predictions, file="predictions_Unic0rn_2.csv", row.names=FALSE)





# Fit model
tree.fit <- rpart(
  SALE ~ ., 
  data = data, 
  method = "class"
)