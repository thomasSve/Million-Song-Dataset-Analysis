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
#cls$dt <- read.csv("msd_mean_pop.csv")

## Predict genre
cls$dt <- read.table(file = "../datasets/msd_genre_dataset.txt", header = TRUE, sep = ",", comment = "#", quote=NULL, fill=TRUE)

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

#cls$target  <- "popular"
cls$target <- "genre"
cls$ident   <- "track_id"
#cls$ignore  <- c("artist_name", "title", "energy", "danceability", "song_hotttnesss")

##
## Decision Tree 
##

library(rpart, quietly=TRUE)

## Build the Decision Tree model.
cat(paste("Classifying a decision tree \n"))
cls$dtfit <- rpart(genre ~ .,
    data=cls$dt[cls$train, c(cls$input, cls$target)],
    method="class")


## Predict on test-set
printcp(cls$dtfit)
##cls$dtpred <- predict(cls$dtfit, newdata = cls$dt[cls$test, c(cls$input, cls$target)], type = "prob")
##cat(paste("Decision tree results: ", cls$dtpred, "\n"))

##
## Support vector machine. 
##

##library(kernlab, quietly=TRUE)

## Build a Support Vector Machine model.
##cls$ksvm <- ksvm(as.factor(genre) ~ .,
##      data=cls$dt[cls$train,c(cls$input, cls$target)],
##      kernel="rbfdot",
##      prob.model=TRUE)

## Generate a textual view of the SVM model.

##
## Evaluate results
##

library(ggplot2)
library(plyr)


## plot tree
cat(paste("Saving a plot of the decision Tree \n"))
jpeg('dt_plot.jpg')
plot(cls$dtfit, uniform=TRUE, 
  	main="Classification Tree for genre")
text(cls$dtfit, use.n=TRUE, all=TRUE, cex=.8)
dev.off()

## Generate a Confusion Matrix
cat(paste("Saving a confusion Matrix of the decision Tree \n"))
jpeg('dt_confusion_matrix.jpg')
plot(cls$dtfit, uniform=TRUE, 
  	main="Classification Tree for genre")
text(cls$dtfit, use.n=TRUE, all=TRUE, cex=.8)
dev.off()

## Plot count of genre distribution
print("Creating a year distribution pie")
jpeg('genre_distribution.jpg')
g1<-ggplot(cls$dt, aes(x=factor(1), fill=factor(genre))) + geom_bar(width = 1)
plot(g1 + coord_polar(theta="y"))
dev.off()
print("Saved year distribution plot")


## Plot relation between loudness and genre
##g<-ggplot(cls$dt, aes(x=genre, y=loudness))

##plot(g+geom_violin(alpha=0.5, color="gray")+geom_jitter(alpha=0.5, aes(color=genre),
##      position = position_jitter(width = 0.1))+coord_flip())
