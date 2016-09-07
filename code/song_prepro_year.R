## 
## Thomas Astad Sve
## Script to clear the year dataset, so there is no samples with NA or 0 value in year
## This script also creates a new column to reduce the number of classes
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

## Reducing classes to five, using decades
print("Reducing to five classes")
cleanYMS$decade <- ifelse(cleanYMS$year>=2005, 2005,
                          ifelse(cleanYMS$year>= 1995, 1995,
                                 ifelse(cleanYMS$year>=1985, 1985,
                                        ifelse(cleanYMS$year>= 1975, 1975, 1965))))

print("Saving new clean dataset...")
write.csv(cleanYMS, "../datasets/msd_year.csv")
print("Saved!")

library(ggplot2, quietly=TRUE)
cat(paste("Saving a plot for year distribution \n"))
jpeg('year_distribution_full.jpg')
c <- ggplot(cleanYMS, aes(factor(year)))
plot(c + geom_bar())
dev.off()
