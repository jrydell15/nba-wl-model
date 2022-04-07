require(here)
require(tidyverse)
require(geosphere)

dfVenues = read_csv(here('data/raw/misc/venueLocs.csv'))

distances = distm(dfVenues %>% select(Longitude, Latitude)) * 0.000621371 # distm outputs in meters, convert to miles
rownames(distances) = dfVenues$venue.id
colnames(distances) = dfVenues$venue.id

venueDists = as.data.frame(as.table(distances))
names(venueDists) = c('fromVenueID', 'toVenueID', 'dist_miles')

write_csv(venueDists, file=here('data/raw/misc/venueDistances.csv'))