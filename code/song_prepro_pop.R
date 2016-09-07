## 
## Thomas Astad Sve
## Script to create a popular column in the msd dataset, and cleans duplicates
## 

## -- Load dataset --
print("Loading dataset...")
file <- "../datasets/msd_simple.csv"
MSongs <- read.csv(file, header = TRUE)

## Removing duplicates
MSongs <- MSongs[!duplicated(MSongs[,c('artist_name', 'title')]),]

## Sort data by hotness
print("Sorting dataset by hottnesss")
MSongs <- na.omit(MSongs[order(-MSongs$song_hotttnesss),])

## Insert new value with the most popular songs as 1, rest 0
print("Adding new variable, popular")

## Take top 25% of the hottest songs as popular
n = 25
MSongs$popular <- ifelse(MSongs$song_hotttnesss < quantile(MSongs$song_hotttnesss,prob=1-n/100),0,1)

print(paste("Num samples: ", nrow(MSongs)))

numUnPop <- length(MSongs$popular[MSongs$popular == 0])
numPop <- length(MSongs$popular[MSongs$popular == 1])

print(paste("Num Popular: ", numPop, " Num unPopular: ", numUnPop))

## Save dataset
print("Saving the new popular dataset")
write.csv(MSongs, "../datasets/msd_pop.csv")
print("Saved!")
