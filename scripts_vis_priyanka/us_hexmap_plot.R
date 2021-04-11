

# library
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)

# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

# Load this file. (Note: I stored in a folder called DATA)
spdf <- geojson_read("us_states_hexgrid.json",  what = "sp")

# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# Show it
plot(spdf)

# I need to 'fortify' the data to be able to show it with ggplot2 (we need a data frame format)
library(broom)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()

# Load mariage data
#data <- read.table("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/State_mariage_rate.csv", header=T, sep=",", na.strings="---")
elec_res = read.csv("democratic_vs_republican_votes_by_usa_state_2020.csv", stringsAsFactors = F)


# Merge geospatial and numerical information
spdf_fortified <- spdf_fortified %>%
  left_join(. , elec_res, by=c("id"="state")) 

# Make a first chloropleth map
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill =  percent_democrat, x = long, y = lat, group = group)) +
  scale_fill_gradient(trans = "log") +
  theme_void() +
  coord_map()


# Prepare binning
spdf_fortified$bin <- cut( spdf_fortified$percent_democrat , breaks=seq(0, 100, length.out = 7), labels=c("0-17", "17-33", "33-50", "50-67", "67-83", "83-100"), include.lowest = TRUE )

# Prepare a color scale coming from the viridis color palette
library(viridis)
my_palette <- rev(magma(8))[c(-1,-8)]

# plot
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = bin, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
  theme_void() +
  scale_fill_manual( 
    values=my_palette, 
    name="Percentage votes for Biden in various US states", 
    guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  ) +
  ggtitle( "2020 USA Election: Percent of Population Voting for the Democratic Party" ) +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

########################################################################################################

library(syuzhet)
library(vader)


load("tweets_before_election.RData")

## vader and syuzhet on tweets
biden_vader = vader_df(hashtag_joebidentest$tweet)
hashtag_joebidentest$VADER <- biden_vader$compound

hashtag_joebidentest$Syuzhet <- get_sentiment(hashtag_joebidentest$tweet) 

###################################################################################################

load("StateZipScores.RData")



write.csv(state_scores_income, file = 'state_scores_income.csv')


elec_res = read.csv("democratic_vs_republican_votes_by_usa_state_2020.csv", stringsAsFactors = F)



