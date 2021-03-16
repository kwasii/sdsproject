##loading packages
library(rtweet)
library(vader)
library(dplyr)
library(maps)

##downloading (non-re-tweeted) tweets with biden/joebiden in the US
biden_tweets <- search_tweets(q ="biden OR joebiden", lang = "en", geocode = lookup_coords("usa"), n = 1000, include_rts = F)

## create lat/lng variables using all available tweet and profile geo-location data
biden_tweets <- lat_lng(biden_tweets)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
## plot lat and lng points onto state map
with(biden_tweets, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

##cleaning data frame with dplyr


##exporting dataframe into .csv file
write.csv(biden_tweets,file = "C:\\Users\\dkwasnits\\Documents\\Repositories\\sdsproject\\biden_tweets.csv", row.names = FALSE)