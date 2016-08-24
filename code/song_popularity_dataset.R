## 
## Thomas Astad Sve
## 
## 

## -- Load dataset --
print("Loading dataset...")
file <- "msd_seg_mean.csv"
MSongs <- read.csv(file, header = TRUE)

## Removing duplicates
MSongs <- MSongs[!duplicated(MSongs[,c('artist_name', 'title')]),]

## Merge in genre dataset
MSongsGenre <- read.table("msd_genre_dataset.txt", header = TRUE)
MSongs <- merge(MSongs, MSongsGenre,by=c("artist_name","title"))
## Sort data by hotness
print("Sorting dataset by hottnesss")
MSongs <- na.omit(MSongs[order(-MSongs$song_hotttnesss),])

## Insert new value with the most popular songs as 1, rest 0
print("Adding new variable, popular")

## If hottnesss is over .65, consider it popular
n = 25
MSongs$popular <- ifelse(MSongs$song_hotttnesss < quantile(MSongs$song_hotttnesss,prob=1-n/100),0,1)
##MSongs$popular <- ifelse(MSongs$song_hotttnesss<0.65,0,1)
numSamples <- nrow(MSongs)
print(paste("Num samples: ", numSamples))

numUnPop <- length(MSongs$popular[MSongs$popular == 0])
numPop <- length(MSongs$popular[MSongs$popular == 1])

print(paste("Num Popular: ", numPop, " Num unPopular: ", numUnPop))

## Save dataset
write.csv(MSongs, "msd_mean_pop.csv")

warnings()
