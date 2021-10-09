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
# Remove columns we didn't see influence on SALE from above
dataco <- data %>%
  dplyr::select(
    -CATAGE
  )
dataca <- data %>%
  dplyr::select(
    -DRIVERAGE
  )
logisticModelFull <- glm(as.factor(SALE) ~ ., family = "binomial", dataco)
summary(logisticModelFull)

l1 <- glm(as.factor(SALE) ~ ., family = "binomial", dataca)
summary(l1)

probabilities <- logisticModelFull %>% predict(dataco, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "YES", "NO")

probabilities <- l1 %>% predict(dataco, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "YES", "NO")
confusionMatrix(data = as.factor(predicted.classes), as.factor(dataca$SALE))
caret::confusionMatrix(predicted.classes)
confusionMatrix(l1)

# new model (optimize model by finding the min. AIC value)
logisticModelNew <-  stepAIC(logisticModelFull, trace = 0)
summary(logisticModelNew)

logit_1 <- glm(as.factor(SALE)~DRIVERAGE, family = binomial,data = data)
logodds <- logit_1$linear.predictors
summary(logit_1)
plot(logodds ~ data$DRIVERAGE)

logisticModelNew <-  stepAIC(logisticModelFull, trace = 0)
