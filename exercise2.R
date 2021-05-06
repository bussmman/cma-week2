################
## Exercise 2 ##
################

## Load the necessary libraries
library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

## Import the downloaded csv
wildschwein_BE = read_delim("wildschwein_BE_2056.csv",",")

wildschwein_BE = st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

## Calculate the time difference between measurements
wildschwein_BE = group_by(wildschwein_BE, TierID)
wildschwein_BE$timelag = as.integer(difftime(lead(wildschwein_BE$DatetimeUTC),wildschwein_BE$DatetimeUTC,units = "secs"))

# How many Indivuiduals were tracked? Three!
unique(wildschwein_BE$TierID)

# For how long were the individuals tracked? 1/2 - 1 year!
Tier1 = wildschwein_BE[wildschwein_BE$TierID == "002A",]
l1 = sum(Tier1$timelag[1:length(Tier1$timelag)-1])/(60*60*24)
Tier2 = wildschwein_BE[wildschwein_BE$TierID == "016A",]
l2 = sum(Tier2$timelag[1:length(Tier2$timelag)-1])/(60*60*24)
Tier3 = wildschwein_BE[wildschwein_BE$TierID == "018A",]
l3 = sum(Tier3$timelag[1:length(Tier3$timelag)-1])/(60*60*24)

# Are there gaps? Yes, there are times, where the sampling
# intervall is significantly higher than 15 min!
plot(Tier1$timelag, ylim=c(0,10000))
plot(Tier2$timelag, ylim=c(0,10000))
plot(Tier3$timelag, ylim=c(0,10000))

# Were all individuals tracked concurrently or sequentially?
# The trackings overlap, but are not of the same duration!
# Two have the same starting point, two the same ending point!
tr1 = c(Tier1$DatetimeUTC[1],Tier1$DatetimeUTC[length(Tier1$TierID)])
tr2 = c(Tier2$DatetimeUTC[1],Tier2$DatetimeUTC[length(Tier2$TierID)])
tr3 = c(Tier3$DatetimeUTC[1],Tier3$DatetimeUTC[length(Tier3$TierID)])

# What is the temporal sampling intervall? ~15min!
quantile(wildschwein_BE$timelag, 0.01, na.rm=TRUE)  # 13min 42s
quantile(wildschwein_BE$timelag, 0.25, na.rm=TRUE)  # 14min 56s
quantile(wildschwein_BE$timelag, 0.5, na.rm=TRUE)   # 15min 3s
quantile(wildschwein_BE$timelag, 0.75, na.rm=TRUE)  # 15min 16s
quantile(wildschwein_BE$timelag, 0.99, na.rm=TRUE)  # 180min 7s

## Calculate the wild boar's steplengths
wildschwein_BE$steplength = sqrt(
  (wildschwein_BE$E-lead(wildschwein_BE$E))^2 + 
  (wildschwein_BE$N-lead(wildschwein_BE$N))^2
)

## Using the timelag and the steplength calculate the wild boar's
## Speed
wildschwein_BE$speed = wildschwein_BE$steplength/wildschwein_BE$timelag

# What speed unit do you get? Meters per second!

## Import the "caro" data
caro = read_delim("caro60.csv",",")
#caro = st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)

## Subset the caro data by different intervalls
caro3 = caro[seq(1,length(caro$TierID),3),]       # 3
caro6 = caro[seq(1,length(caro$TierID),6),]       # 6
caro9 = caro[seq(1,length(caro$TierID),9),]       # 9

## Calculate timelag for the 4 data sets
caro$timelag = as.integer(difftime(lead(caro$DatetimeUTC),caro$DatetimeUTC,units = "secs"))
caro3$timelag = as.integer(difftime(lead(caro3$DatetimeUTC),caro3$DatetimeUTC,units = "secs"))
caro6$timelag = as.integer(difftime(lead(caro6$DatetimeUTC),caro6$DatetimeUTC,units = "secs"))
caro9$timelag = as.integer(difftime(lead(caro9$DatetimeUTC),caro9$DatetimeUTC,units = "secs"))

## Calculate steplength for the 4 data sets
caro$steplength = sqrt(
  (caro$E-lead(caro$E))^2 + (caro$N-lead(caro$N))^2
)

caro3$steplength = sqrt(
  (caro3$E-lead(caro3$E))^2 + (caro3$N-lead(caro3$N))^2
)

caro6$steplength = sqrt(
  (caro6$E-lead(caro6$E))^2 + (caro6$N-lead(caro6$N))^2
)

caro9$steplength = sqrt(
  (caro9$E-lead(caro9$E))^2 + (caro9$N-lead(caro9$N))^2
)

## Calculate speed for the 4 data sets
caro$speed = caro$steplength/caro$timelag
caro3$speed = caro3$steplength/caro3$timelag
caro6$speed = caro6$steplength/caro6$timelag
caro9$speed = caro9$steplength/caro9$timelag

## Define identifiers to group the 4 different data sets
caro$ID = "1"
caro3$ID = "3"
caro6$ID = "6"
caro9$ID = "9"

## Merge the 4 different data sets
caro_merged = rbind(caro, caro3, caro6, caro9)

## Plot the 4 different trajectories
ggplot(caro_merged, aes(x=E, y=N, group=ID)) +
  geom_path(aes(color=ID)) +
  geom_point(aes(color=ID))

## Plot the 4 different speeds
ggplot(caro_merged, aes(x=DatetimeUTC, y=speed, group=ID)) +
  geom_line(aes(color=ID))

# What do the different lines for the different granularities tell
# you? --> the larger the intervall, the more the speed is smoothened,
# meaning that short sprints of the boar cannot be detected, but are
# averaged out!

## 
