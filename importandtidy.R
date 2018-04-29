library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(maps)
library(ggmap)
library(mapdata)

# Import and Tiday

hazeglife <- read_excel("~/Desktop/hazinggreeklife/Hazing Database 4.25 presentation.xlsx")
hazeglife
summary(hazeglife)

greekorg <- hazeglife$'Organization'
fratsoro <- hazeglife$'FraternitySorority'
colluniv <- hazeglife$'CollegeUniversity'	
collst <- hazeglife$'State'
startdate <- hazeglife$'Date'
month <- hazeglife$'Month'
yr <- hazeglife$'Year'
investsanc <- hazeglife$'Sanctioned'
sanction <- hazeglife$'Resulting Action'
enddate <- hazeglife$'End Date'
camplat <- hazeglife$'CampusLat (N/S)'
camplon <- hazeglife$'CampLong (E/W)'
campsize <- hazeglife$'Campus Size'
hazeglife %>% 
  mutate('Suspension Length' = difftime(startdate, enddate))

# Graphing

orgsyr <- tribble(
  ~year, ~noforgs,
  "2015", 16,
  "2016", 4,
  "2017", 239,
  "2018", 54
)
orgsaction <- tribble(
  ~sanction, ~noforgs,
  "cease and desist", 16,
  "disciplinary probation", 5,
  "revocation", 7,
  "social probation", 46,
  "suspension", 239
)
orgssus <- tribble(
  ~organization, ~suslength,
  "Pi Kappa Alpha", 307.75,
  "Sigma Alpha Epsilon", 287.25,
  "Theta Kappa Epsilon", 146.5,
  "Alpha Phi Alpha", 132,
  "Theta Delta Chi", 125
)
ggplot(data = orgssus) +
  geom_bar(mapping = aes(x = organization, y = suslength), stat = "identity")
ggplot(data = orgsaction) +
  geom_bar(mapping = aes(x = sanction, y = noforgs), stat = "identity")
ggplot(data = orgsyr) +
  geom_bar(mapping = aes(x = year, y = noforgs), stat = "identity")

orgscamp <- tribble(
  ~institution, ~norgs,
  "FSU", 52,
  "PSU", 45,
  "OSU", 36,
  "IU", 34,
  "TSU", 30
)

ggplot(data = orgscamp) +
  geom_bar(mapping = aes(x = institution, y = norgs), stat = "identity")

# mapping

usa <- map_data("usa") # we already did this, but we can do it again
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

labs <- data.frame(
  long = c(-122.064873, -122.306417),
  lat = c(36.951968, 47.644855),
  names = c("SWFSC-FED", "NWFSC"),
  stringsAsFactors = FALSE
)  

gg1 + 
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 5) +
  geom_point(data = labs, aes(x = long, y = lat), color = "yellow", size = 4)

#subset of states 
west_coast <- subset(states, region %in% c("california", "oregon", "washington"))

ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)

#Plot the state first but let’s ditch the axes gridlines, and gray background by using the super-wonderful theme_nothing().

ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
ca_base + theme_nothing()

#Now plot the county boundaries in white:
  
  ca_base + theme_nothing() + 
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)