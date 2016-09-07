##
## Thomas Astad Sve
##
##

## Create new environment, classification
cls <- new.env()

##
## Load dataset
##
## Predict popularity
cls$dt <- read.csv("../datasets/msd_pop.csv")

## Build the training/validate/test datasets.
## Split into 70/15/15 train/test/val
val <- 0.15
test <- 0.15
train <- 0.7

cls$nrow <- nrow(cls$dt)
cat(paste("Number of rows: ", cls$nrow, "\n"))

cls$sample <- cls$train <- sample(cls$nrow, train*cls$nrow)
cls$validate <- sample(setdiff(seq_len(cls$nrow), cls$train), val*cls$nrow)
cls$test <- setdiff(setdiff(seq_len(cls$nrow), cls$train), test*cls$nrow)

## The following variable selections have been noted.

cls$input <- c("loudness", "tempo", "time_signature", "key", "mode", "duration")
cls$numeric <- c("loudness", "tempo", "time_signature", "key", "mode", "duration")

cls$target  <- "popular"
cls$ident   <- "track_id"
cls$ignore  <- c("artist_name", "title", "energy", "danceability", "song_hotttnesss")

##
## Decision Tree 
##

library(rpart, quietly=TRUE)
print("Classifying using decision tree...")
cls$dtfit <- rpart(popular ~ .,
                   data=cls$dt[cls$train, c(cls$input, cls$target)],
                   method="class")

print("Finished classifying using Decision Tree")

##
## SVM
##

library(e1071, quietly=TRUE)
print("Classifying using SVM...")
cls$svmfit <- svm(popular ~ .,
                  data=cls$dt[cls$train, c(cls$input, cls$target)])

print("Finished classifying using SVM")

##
## Random Forest
##

library(randomForest, quietly=TRUE)
print("Classifying using Random Forest...")
cls$rffit <- randomForest::randomForest(popular ~ .,
                                        data=cls$dt[cls$train, c(cls$input, cls$target)],
                                        ntree=500,
                                        mtry=2,
                                        importance=TRUE,
                                        na.action=randomForest::na.roughfix,
                                        replace=FALSE)

print("Finished classifying using Random Forest")

##
## Evaluate results
##

library(ggplot2, quietly=TRUE)
library(plyr, quietly=TRUE)

print("Obtain the response from the classifyers...")
cls$dtpr <- predict(cls$dtfit, newdata=na.omit(cls$dt[cls$test, c(cls$input, cls$target)]))
cls$svmpr <- predict(cls$svmfit, newdata=na.omit(cls$dt[cls$test, c(cls$input, cls$target)]))
cls$rfpr <- predict(cls$rffit, newdata=na.omit(cls$dt[cls$test, c(cls$input, cls$target)]))


## plot tree
cat(paste("Saving a plot of the decision Tree \n"))
jpeg('pop_dt_plot.jpg')
plot(cls$dtfit, uniform=TRUE, 
  	main="Classification Tree for popularity")
text(cls$dtfit, use.n=TRUE, all=TRUE, cex=.8)
dev.off()

## Plot count of popularity distribution

## Plot count of genre distribution
print("Creating a popularity distribution")
jpeg('popularity_distribution.jpg')
g1<-ggplot(cls$dt, aes(x=factor(1), fill=factor(popular))) + geom_bar(width = 1)
plot(g1 + coord_polar(theta="y"))
dev.off()
print("Saved popular distribution plot")
