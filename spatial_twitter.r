library(twitteR)
library(ggmap)
library(ggplot2)
library(readr)
#get Twitter token at development website
setup_twitter_oauth()

#tweets collecting section
keyword = "#cool"
tweets = searchTwitter(keyword, n = 100)
tweets = twListToDF(tweets)
tweetsUsers = twListToDF(lookupUsers(tweets$screenName))
withLocation = subset(tweetsUsers, location != "" , location)
geoCodes = geocode(withLocation$location)
geoCodesTotalNoNA = na.omit(geoCodes)

#grouping tweets by state geo within 5 degree 
for(i in 1 : 52) {
    v1 = abs(geoCodesTotalNoNA$lon - as.numeric(state_geo[i,1])) <= 5 & abs(geoCodesTotalNoNA$lat - as.numeric(state_geo[i,2])) <= 5
    v2 = length(which(v1))
    state_geo[i,4] = v2   
}
#eliminate Alaska and Hawaii for denser plot, sorry Alaska and Hawaii
land = state_geo[state_geo$state != c("Alaska","Hawaii"), ]

#plot section
#when query beyond the API limit, or use mapCenter = as.numeric(geocode("United States"))
mapCenter = c(-98, 40)
#inspred by Domino,https://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/
USAMap = ggmap(get_googlemap(center = mapCenter, scale = 2, zoom = 4), extent="normal")
USAMap + geom_point(aes(x = lon, y = lat), data = land, col = "red", alpha = 0.5, size = as.numeric(land$num)*0.05) 

keyword = "San Fransisco"
#trendgeo = geocode(keyword), but query beyond API limit
trendgeo = c(38, -122)
found = closestTrendLocations(trendgeo[1],trendgeo[2])
trend = getTrends(found["woeid"])
print(trend[1:20,])
