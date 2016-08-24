## 
## Thomas Astad Sve
## Script to clear the year dataset, so there is no samples with NA or 0 value in year
## 

## -- Load dataset --
print("Loading dataset...")
MSongs <- read.csv("../datasets/msd_simple.csv", header = TRUE)

## Removing duplicates
print("Removing duplicates...")
MSongs <- MSongs[!duplicated(MSongs[,c('artist_name', 'title', 'track_id')]),]

## Remove columns where year is zero, year has column 11
print("Removing columns where year is 0...")
cleanYMS <- MSongs[apply(MSongs[11], 1, function(z) any(z!=0)),]

print("Saving new clean dataset...")
write.csv(cleanYMS, "../datasets/msd_year.csv")
print("Saved!")
