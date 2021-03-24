library(rtweet)

##downloading biden tweets for manual annotation
biden_tweets_manual <- search_tweets(q ="biden OR joebiden", lang = "en", geocode = lookup_coords("usa"), n = 18000, include_rts = F)

biden_tweets_manual = data.frame(lapply(biden_tweets_manual, as.character), stringsAsFactors=FALSE)
write.csv(biden_tweets_manual,file = "biden_tweets_manual.csv", row.names = FALSE)

##downloading trump tweets for manual annotation
trump_tweets_manual <- search_tweets(q ="trump OR donaldtrump", lang = "en", geocode = lookup_coords("usa"), n = 18000, include_rts = F)

trump_tweets_manual = data.frame(lapply(trump_tweets_manual, as.character), stringsAsFactors=FALSE)
write.csv(biden_tweets_manual,file = "trump_tweets_manual.csv", row.names = FALSE)