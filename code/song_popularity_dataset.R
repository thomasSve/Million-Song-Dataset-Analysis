## 
## Thomas Astad Sve
## 
## 

## -- Load dataset --
print("Loading dataset...")
file <- "million_songs.csv"
MSongs <- read.csv(file, header = TRUE)

## Removing duplicates
MSongs <- MSongs[!duplicated(MSongs[,c('artist_name', 'title')]),]

## Sort data by hotness
print("Sorting dataset by hottnesss")
MSongs <- MSongs[order(-MSongs$song_hotttnesss),]

## Insert new value with the most popular songs as 1, rest 0
print("Adding new variable, popular")

## If hottnesss is over .65, consider it popular
MSongs$popular <- ifelse(MSongs$song_hotttnesss<0.65,0,1)
numSamples <- nrow(MSongs)
cat("Num samples: ", numSamples)


## Save dataset
write.csv(MSongs, "million_songs2.csv")
