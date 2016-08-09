##
## Thomas Astad Sve
##
##

## -- Load dataset --

file <- "million_songs2.csv"
MSongs <- read.csv(file)

## -- Split into train, val and test --
spec = c(train = .6, test = .2, validate = .2)
g = sample(cut(
  seq(nrow(df)), 
  nrow(df)*cumsum(c(0,spec)),
  labels = names(spec)
))

dataset = split(df, g)
## Train: dataset$train, test: dataset$test, val: dataset$validate

## -- Set train index --

## -- Fit data --
library(randomForest)

## -- Test data --
testPred <- predict(rfFit, test)

## -- Evaluate results --

## Print Sensitivity
sens <- sensitivity(obs, pred)

## Print Confusion matrix
conf.mat <- confusion.matrix(obs, pred, threshold = 0.5)


## -- Save the model for later use --

save(model, file = "random_forrest_m1.rda")

