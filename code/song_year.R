##
## Thomas Astad Sve
##
##
## Create new environment, classification
cls <- new.env()

## Predict year
cls$dt <- read.csv(file = "../datasets/msd_year.csv", header = TRUE)

## Split into train, validate and test set
val <- 0.15
test <- 0.15
train <- 0.7
cls$nrow <- nrow(cls$dt)
print(paste("Number of rows: ", cls$nrow, "\n"))

cls$sample <- cls$train <- sample(cls$nrow, train*cls$nrow)
cls$validate <- sample(setdiff(seq_len(cls$nrow), cls$train), val*cls$nrow)
cls$test <- setdiff(setdiff(seq_len(cls$nrow), cls$train), test*cls$nrow)

## Choose variables
cls$input <- c("loudness", "tempo", "time_signature", "key", "mode", "duration")
cls$numeric <- c("loudness", "tempo", "time_signature", "key", "mode", "duration")
cls$target <- "decade"
cls$ident   <- "track_id"

##
## Statistical distribution of the data set
##
library(ggplot2)
library(plyr)

print("Creating a year distribution pie")
jpeg('year_distribution.jpg')
g1<-ggplot(cls$dt, aes(x=factor(1), fill=factor(decade))) + geom_bar(width = 1)
plot(g1 + coord_polar(theta="y"))
dev.off()
print("Saved year distribution plot")

## Plot correlation between loudness and year
print("Creating a correlation plot between loudness and year")
jpeg('loudness_year.jpg')
g<-ggplot(cls$dt, aes(x=decade, y=loudness))
plot(g+geom_violin(alpha=0.5, color="gray")+geom_jitter(alpha=0.5, aes(color=decade),
                                  position = position_jitter(width = 0.1))+coord_flip())

dev.off()
print("Saved loudness and year correlation")

## Plot correlation between duration and year
print("Creating a correlation plot between duration and year")
jpeg('duration_year.jpg')
g<-ggplot(cls$dt, aes(x=decade, y=duration))

plot(g+geom_violin(alpha=0.5, color="gray")+geom_jitter(alpha=0.5, aes(color=decade),
                                  position = position_jitter(width = 0.1))+coord_flip())
dev.off()
print("Saved duration and year correlation")


##
## Do predictive analysis 
##

library(rpart, quietly=TRUE)
## Build the Decision Tree model.
print("Classifying a decision tree")
cls$dtfit <- rpart(decade ~ .,
    data=cls$dt[cls$train, c(cls$input, cls$target)],
    method="class")


##
## SVM
##

library(e1071, quietly=TRUE)
print("Classifying using SVM...")
cls$svmfit <- svm(decade ~ .,
                   data=cls$dt[cls$train, c(cls$input, cls$target)])
print("Finished classifying using SVM")

##
## Random Forest
##

library(randomForest, quietly=TRUE)
print("Classifying using Random Forest...")
cls$rffit <- randomForest::randomForest(decade ~ .,
                                        data=cls$dt[cls$train, c(cls$input, cls$target)],
                                        ntree=500,
                                        mtry=2,
                                        importance=TRUE,
                                        na.action=randomForest::na.roughfix,
                                        replace=FALSE)

print("Finished classifying using Random Forest")

##
## Evaluate the results
##

print("Obtain the response from the classifyers...")
cls$dtpr <- predict(cls$dtfit, newdata=na.omit(cls$dt[cls$test, c(cls$input, cls$target)]))
cls$svmpr <- predict(cls$svmfit, newdata=na.omit(cls$dt[cls$test, c(cls$input, cls$target)]))
cls$rfpr <- predict(cls$rffit, newdata=na.omit(cls$dt[cls$test, c(cls$input, cls$target)]))


## Plot prediction and observation line on test-set
obs <- subset(cls$dt[cls$test, c(cls$input, cls$target)], select=cls$target)

