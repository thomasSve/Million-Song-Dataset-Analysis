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

cls$target <- "genre"
cls$ident   <- "track_id"

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}

library(ggplot2, quietly=TRUE)
library(plyr, quietly=TRUE)

##
## Decision Tree 
##
library(rpart, quietly=TRUE)
print("Classifying using decision tree...")
cls$dtfit <- rpart(genre ~ .,
                   data=cls$dt[cls$train, c(cls$input, cls$target)],
                   method="class")

print("Finished classifying using Decision Tree")

# Evaluate Result for Decision Tree

cls$dtpr <- predict(cls$dtfit, newdata=cls$dt[cls$test, c(cls$input, cls$target)], type="class")

print(length(cls$dt))
print(length(cls$test))
print(length(cls$dt[cls$test, c(cls$input, cls$target)]))
print(length(cls$dtpr))

perdt <- pcme(cls$dt[cls$test, c(cls$input, cls$target)]$genre, cls$dtpr)
round(perdt, 2)
cat(100*round(1-sum(diag(perdt), na.rm=TRUE), 2))

errormatdt <- table(cls$dt[cls$test, c(cls$input, cls$target)]$genre, cls$dtpr,
                    useNA="ifany",
                    dnn=c("Actual", "Predicted"))
write.table(errormatdt, file = "genre_error_matrix_dt.txt")

##
## Random Forest
##

library(randomForest, quietly=TRUE)
print("Classifying using Random Forest...")
cls$rffit <- randomForest::randomForest(genre ~ .,
                                        data=cls$dt[cls$train, c(cls$input, cls$target)],
                                        ntree=500,
                                        mtry=2,
                                        importance=TRUE,
                                        na.action=randomForest::na.roughfix,
                                        replace=FALSE)

print("Finished classifying using Random Forest")

## Evaluate results for random Forest
cls$rfpr <- predict(cls$rffit, newdata=cls$dt[cls$test, c(cls$input, cls$target)], type="class")


## Plot Error Matrix
errormatrf <- table(cls$dt[cls$test, c(cls$input, cls$target)]$genre, cls$rfpr,
                    useNA="ifany",
                    dnn=c("Actual", "Predicted"))

write.table(errormatrf, file = "genre_error_matrix_rf.txt")
perrf <- pcme(cls$dt[cls$test, c(cls$input, cls$target)]$genre, cls$rfpr)
round(perdt, 2)
cat(100*round(1-sum(diag(perrf), na.rm=TRUE), 2))

##
## SVM
##

library(e1071, quietly=TRUE)
print("Classifying using SVM...")
cls$svmfit <- svm(genre ~ .,
                  data=cls$dt[cls$train, c(cls$input, cls$target)])

print("Finished classifying using SVM")

## Evaluate Results for SVM
cls$svmpr <- predict(cls$svmfit, newdata=cls$dt[cls$test, c(cls$input, cls$target)], type="class")
errormatsvm <- table(cls$dt[cls$test, c(cls$input, cls$target)]$genre, cls$svmpr,
                     useNA="ifany",
                     dnn=c("Actual", "Predicted"))
write.table(errormatsvm, file = "genre_error_matrix_svm.txt")
persvm <- pcme(cls$dt[cls$test, c(cls$input, cls$target)]$genre, cls$svmpr)
round(persvm, 2)
cat(100*round(1-sum(diag(persvm), na.rm=TRUE), 2))

##
## Make plots
##

## Plot tree
print("Saving a plot of the decision Tree...")
jpeg('genre_dt_plot.jpg')
plot(cls$dtfit, uniform=TRUE, 
  	main="Classification Tree for genre")
text(cls$dtfit, use.n=TRUE, all=TRUE, cex=.8)
dev.off()

## Generate a Confusion Matrix
print("Saving a confusion Matrix of the decision Tree...")
jpeg('genre_dt_confusion_matrix.jpg')
plot(cls$dtfit, uniform=TRUE, 
  	main="Classification Tree for genre")
text(cls$dtfit, use.n=TRUE, all=TRUE, cex=.8)
dev.off()

## Plot count of genre distribution
print("Creating a genre distribution pie")
jpeg('genre_distribution.jpg')
g1<-ggplot(cls$dt, aes(x=factor(1), fill=factor(genre))) + geom_bar(width = 1)
plot(g1 + coord_polar(theta="y"))
dev.off()
print("Saved year distribution plot")


## Plot relation between loudness and genre
print("Plot showing relation between loudness and genre")
jpeg('genre_loudness.jpg')
g<-ggplot(cls$dt, aes(x=genre, y=loudness))
plot(g+geom_violin(alpha=0.5, color="gray")+geom_jitter(alpha=0.5, aes(color=genre),
                                  position = position_jitter(width = 0.1))+coord_flip())
dev.off()
print("Saved loudness and genre relation")

