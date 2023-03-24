# library(ggplot2)
# library(dplyr)
# library(readr)
# library(utils)
# library(stringr)
# library(stats)

# import datasets ----
train <- read_csv("data/train.csv")%>%data.frame()
test <- read_csv("data/test.csv")%>%data.frame()

source("function/imputeMissingValues.R")
source("function/featureEngineering.R")

# Pre-processing ----
preProcessingTrain <- imputeMissingValues(dataIn = train)
preProcessingTest <- imputeMissingValues(dataIn = test)

featuredTrain <- featureEngineering(dataIn = preProcessingTrain)
featuredTest <- featureEngineering(dataIn = preProcessingTest)

# Fitting the model
formula <- formula(Survived ~ Pclass +
                     Age +
                     AgeBreak +
                     Fare +
                     FareBreak +
                     TicketFare +
                     Sex +
                     Embarked +
                     Cabin +
                     Parch +
                     SibSp +
                     Status)

glm <- stats::glm(formula = formula,
                  family = stats::binomial(),
                  data = featuredTrain)

glmPredict <- predict(glm, newdata = featuredTest, type = "response")

submission <- cbind(test, glmPredict) %>%
  mutate(Survived = if_else(glmPredict >= 0.5, 1, 0)) %>%
  select(PassengerId, Survived)

filename <- paste("titanicSubmission", Sys.Date(), sep = "_")

write.csv(submission, paste(filename, ".csv"), row.names = FALSE)                        
