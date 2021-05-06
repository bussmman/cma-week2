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
# intervall deviates significantly from 15 min!
plot(Tier1, ylim=c(0,10000))
plot(Tier2, ylim=c(0,10000))
plot(Tier3, ylim=c(0,10000))

# Were all individuals tracked concurrently or sequentially?
# The trackings overlap, but are not of the same duration!
tr1 = c(Tier1$DatetimeUTC[1],Tier1$DatetimeUTC[length(Tier1$TierID)])
tr2 = c(Tier2$DatetimeUTC[1],Tier2$DatetimeUTC[length(Tier2$TierID)])
tr3 = c(Tier3$DatetimeUTC[1],Tier3$DatetimeUTC[length(Tier3$TierID)])

# What is the temporal sampling intervall? ~15min!
quantile(wildschwein_BE$timelag, 0.01, na.rm=TRUE)  # 13min 42s
quantile(wildschwein_BE$timelag, 0.25, na.rm=TRUE)  # 14min 56s
quantile(wildschwein_BE$timelag, 0.5, na.rm=TRUE)   # 15min 3s
quantile(wildschwein_BE$timelag, 0.75, na.rm=TRUE)  # 15min 16s
quantile(wildschwein_BE$timelag, 0.99, na.rm=TRUE)  # 180min 7s

## 



