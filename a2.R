library(dplyr)
library(plyr)
library(reshape2)
library(ggplot2)
setwd('./R/ReproducibleResearch')

file_dl <- './StormData.csv.bz2'
if (!file.exists(file_dl))
{
  file_dl <- download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "StormData.csv.bz2")
}

# Only the following columns are required for our analysis:
## STATE, BGN_DATE, STATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP
## Get only those variables in order to shrink our dataset
fac <- "factor"
nul <- "NULL"
num <- "numeric"
col_classes <- c(nul, fac, rep(nul, 4), fac, fac, rep(nul, 14), rep(num, 3), fac, num, fac, rep(nul, 9))
data <- read.csv("StormData.csv.bz2", header=T, sep=",", strip.white=TRUE, colClasses= col_classes)
dim(data) # 902297

# Convert factor to date
data$BGN_DATE <- as.Date(data$BGN_DATE, format = "%m/%d/%Y")

# Filter out data before 1996, when all event types began to be recorded
data.dateSub <- filter(data, BGN_DATE > as.Date("12/31/1995", format = "%m/%d/%Y"))
dim(data.dateSub) # 653530

# For the purposes of our analysis, we are not concerned with data that does not have an impact
## on public health or economics.  Therefore, we filter out data without fatalities, injuries, 
## property damange, and crop damage
data.dmg <- filter(data.dateSub, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)
dim(data.dmg) # 201318

# Filter to only official EVTYPE values
## First, convert EVTYPE column to uppercase and then compare to official EVTYPEs
data.dmg$EVTYPE <- toupper(data.dmg$EVTYPE)
data.dmg_clean <- data.dmg

# Discover the unofficial EVTYPEs that were used so that we can map them to the correct official type
unique(data.dmg$EVTYPE)

data.dmg_clean$EVTYPE[grepl("LAKE EFFECT SNOW", data.dmg_clean$EVTYPE)] <- "LAKE-EFFECT SNOW"
data.dmg_clean$EVTYPE[grepl("RIP CURRENT", data.dmg_clean$EVTYPE)] <- "RIP CURRENT"
data.dmg_clean$EVTYPE[grepl("LANDSPOUT", data.dmg_clean$EVTYPE)] <- "TORNADO"
data.dmg_clean$EVTYPE[grepl("WHIRLWIND", data.dmg_clean$EVTYPE)] <- "TORNADO"
data.dmg_clean$EVTYPE[grepl("BLOWING DUST", data.dmg_clean$EVTYPE)] <- "DUST STORM"
data.dmg_clean$EVTYPE[grepl("^FOG$", data.dmg_clean$EVTYPE)] <- "DENSE FOG"
data.dmg_clean$EVTYPE[grepl("FIRE", data.dmg_clean$EVTYPE)] <- "WILDFIRE"
data.dmg_clean$EVTYPE[grepl("LANDSLUMP", data.dmg_clean$EVTYPE)] <- "DEBRIS FLOW"
data.dmg_clean$EVTYPE[grepl("SQUALL", data.dmg_clean$EVTYPE)] <- "BLIZZARD"
data.dmg_clean$EVTYPE[grepl("BLOWING SNOW", data.dmg_clean$EVTYPE)] <- "BLIZZARD"
data.dmg_clean$EVTYPE[grepl("(HEAVY|EXCESSIVE) SNOW", data.dmg_clean$EVTYPE)] <- "HEAVY SNOW"
data.dmg_clean$EVTYPE[grepl("C(OA)?ST(A)?L( )+FLOOD", data.dmg_clean$EVTYPE)] <- "COASTAL FLOOD"
data.dmg_clean$EVTYPE[grepl("TIDAL FLOOD", data.dmg_clean$EVTYPE)] <- "COASTAL FLOOD"
data.dmg_clean$EVTYPE[grepl("FLASH", data.dmg_clean$EVTYPE)] <- "FLASH FLOOD"
data.dmg_clean$EVTYPE[grepl("RIVER FLOOD", data.dmg_clean$EVTYPE)] <- "FLOOD"
data.dmg_clean$EVTYPE[grepl("FLD", data.dmg_clean$EVTYPE)] <- "FLOOD"
data.dmg_clean$EVTYPE[grepl("(SLIDE|EROSION)", data.dmg_clean$EVTYPE)] <- "DEBRIS FLOW"
data.dmg_clean$EVTYPE[grepl("SURF", data.dmg_clean$EVTYPE)] <- "HIGH SURF"
data.dmg_clean$EVTYPE[grepl("HIGH WATER", data.dmg_clean$EVTYPE)] <- "HIGH SURF"
data.dmg_clean$EVTYPE[grepl("HIGH TIDE", data.dmg_clean$EVTYPE)] <- "HIGH SURF"
data.dmg_clean$EVTYPE[grepl("HIGH SWELLS", data.dmg_clean$EVTYPE)] <- "HIGH SURF"
data.dmg_clean$EVTYPE[grepl("STORM SURGE", data.dmg_clean$EVTYPE)] <- "STORM TIDE"
data.dmg_clean$EVTYPE[grepl("HEAVY SEAS", data.dmg_clean$EVTYPE)] <- "STORM TIDE"
data.dmg_clean$EVTYPE[grepl("ROUGH SEAS", data.dmg_clean$EVTYPE)] <- "STORM TIDE"
data.dmg_clean$EVTYPE[grepl("HIGH SEAS", data.dmg_clean$EVTYPE)] <- "STORM TIDE"
data.dmg_clean$EVTYPE[grepl("COASTAL( )?STORM", data.dmg_clean$EVTYPE)] <- "STORM TIDE"
data.dmg_clean$EVTYPE[grepl("HURRICANE", data.dmg_clean$EVTYPE)] <- "HURRICANE/TYPHOON"
data.dmg_clean$EVTYPE[grepl("TYPHOON", data.dmg_clean$EVTYPE)] <- "HURRICANE/TYPHOON"
data.dmg_clean$EVTYPE[grepl("EXTREME (COLD|WINDCHILL)", data.dmg_clean$EVTYPE)] <- "EXTREME COLD/WIND CHILL"
data.dmg_clean$EVTYPE[grepl("^COLD WEATHER$", data.dmg_clean$EVTYPE)] <- "COLD/WIND CHILL"
data.dmg_clean$EVTYPE[grepl("^UNSEASONABL(E|Y) COLD$", data.dmg_clean$EVTYPE)] <- "COLD/WIND CHILL"
data.dmg_clean$EVTYPE[grepl("EXTENDED COLD", data.dmg_clean$EVTYPE)] <- "COLD/WIND CHILL"
data.dmg_clean$EVTYPE[grepl("EXPOSURE", data.dmg_clean$EVTYPE)] <- "COLD/WIND CHILL"
data.dmg_clean$EVTYPE[grepl("^COLD", data.dmg_clean$EVTYPE)] <- "COLD/WIND CHILL"
data.dmg_clean$EVTYPE[grepl("SMALL HAIL", data.dmg_clean$EVTYPE)] <- "HAIL"
data.dmg_clean$EVTYPE[grepl("NON[- ]?TSTM WIND", data.dmg_clean$EVTYPE)] <- "STRONG WIND"
data.dmg_clean$EVTYPE[grepl("MARINE TSTM WIND", data.dmg_clean$EVTYPE)] <- "MARINE THUNDERSTORM WIND"
data.dmg_clean$EVTYPE[grepl("^THUNDERSTORM$", data.dmg_clean$EVTYPE)] <- "THUNDERSTORM WIND"
data.dmg_clean$EVTYPE[grepl("TSTM WIND", data.dmg_clean$EVTYPE)] <- "THUNDERSTORM WIND"
data.dmg_clean$EVTYPE[grepl("^THUNDERSTORM WIND", data.dmg_clean$EVTYPE)] <- "THUNDERSTORM WIND"
data.dmg_clean$EVTYPE[grepl("^STRONG WIND", data.dmg_clean$EVTYPE)] <- "STRONG WIND"
data.dmg_clean$EVTYPE[grepl("GUSTY", data.dmg_clean$EVTYPE)] <- "STRONG WIND"
data.dmg_clean$EVTYPE[grepl("^HIGH WIND", data.dmg_clean$EVTYPE)] <- "STRONG WIND"
data.dmg_clean$EVTYPE[grepl("GRADIENT WIND", data.dmg_clean$EVTYPE)] <- "STRONG WIND"
data.dmg_clean$EVTYPE[grepl("DOWNBURST", data.dmg_clean$EVTYPE)] <- "STRONG WIND"
data.dmg_clean$EVTYPE[grepl("MICROBURST", data.dmg_clean$EVTYPE)] <- "STRONG WIND"
data.dmg_clean$EVTYPE[grepl("NON-SEVERE WIND DAMAGE", data.dmg_clean$EVTYPE)] <- "STRONG WIND"
data.dmg_clean$EVTYPE[grepl("^WIND", data.dmg_clean$EVTYPE)] <- "STRONG WIND"
data.dmg_clean$EVTYPE[grepl("(EXCESSIVE|RECORD) HEAT", data.dmg_clean$EVTYPE)] <- "EXCESSIVE HEAT"
data.dmg_clean$EVTYPE[grepl("HEAT WAVE", data.dmg_clean$EVTYPE)] <- "EXCESSIVE HEAT"
data.dmg_clean$EVTYPE[grepl("WARM", data.dmg_clean$EVTYPE)] <- "HEAT"
data.dmg_clean$EVTYPE[grepl("MIXED PRECIP", data.dmg_clean$EVTYPE)] <- "SLEET"
data.dmg_clean$EVTYPE[grepl("FREEZING RAIN", data.dmg_clean$EVTYPE)] <- "SLEET"
data.dmg_clean$EVTYPE[grepl("RAIN/SNOW", data.dmg_clean$EVTYPE)] <- "SLEET"
data.dmg_clean$EVTYPE[grepl("FREEZING DRIZZLE", data.dmg_clean$EVTYPE)] <- "SLEET"
data.dmg_clean$EVTYPE[grepl("GLAZE", data.dmg_clean$EVTYPE)] <- "FREEZING FOG"
data.dmg_clean$EVTYPE[grepl("FROST", data.dmg_clean$EVTYPE)] <- "FROST/FREEZE"
data.dmg_clean$EVTYPE[grepl("FREEZE$", data.dmg_clean$EVTYPE)] <- "FROST/FREEZE"
data.dmg_clean$EVTYPE[grepl("WINTER WEATHER", data.dmg_clean$EVTYPE)] <- "WINTER WEATHER"
data.dmg_clean$EVTYPE[grepl("WINTRY MIX", data.dmg_clean$EVTYPE)] <- "WINTER STORM"
data.dmg_clean$EVTYPE[grepl("LIGHT SNOW", data.dmg_clean$EVTYPE)] <- "WINTER WEATHER"
data.dmg_clean$EVTYPE[grepl("LATE SEASON SNOW", data.dmg_clean$EVTYPE)] <- "WINTER WEATHER"
data.dmg_clean$EVTYPE[grepl("^SNOW$", data.dmg_clean$EVTYPE)] <- "WINTER WEATHER"
data.dmg_clean$EVTYPE[grepl("TORRENTIAL RAINFALL", data.dmg_clean$EVTYPE)] <- "HEAVY RAIN"
data.dmg_clean$EVTYPE[grepl("UNSEASONAL RAIN", data.dmg_clean$EVTYPE)] <- "HEAVY RAIN"
data.dmg_clean$EVTYPE[grepl("^RAIN$", data.dmg_clean$EVTYPE)] <- "HEAVY RAIN"
data.dmg_clean$EVTYPE[grepl("(ICE|ICY)", data.dmg_clean$EVTYPE)] <- "ICE STORM"
data.dmg_clean$EVTYPE[grepl("FREEZING SPRAY", data.dmg_clean$EVTYPE)] <- "ICE STORM"
data.dmg_clean$EVTYPE[grepl("MARINE ACCIDENT", data.dmg_clean$EVTYPE)] <- "OTHER"
data.dmg_clean$EVTYPE[grepl("ROGUE WAVE", data.dmg_clean$EVTYPE)] <- "OTHER"
data.dmg_clean$EVTYPE[grepl("DAM BREAK", data.dmg_clean$EVTYPE)] <- "OTHER"
data.dmg_clean$EVTYPE[grepl("DROWNING", data.dmg_clean$EVTYPE)] <- "OTHER"

data.dmg_clean$EVTYPE <- as.factor(data.dmg_clean$EVTYPE)

data.fatalities <- aggregate(data.dmg_clean$FATALITIES, list(data.dmg_clean$EVTYPE), sum)
data.injuries <- aggregate(data.dmg_clean$INJURIES, list(data.dmg_clean$EVTYPE), sum)

data.fatalities <- data.fatalities[order(-data.fatalities$x),]
rownames(data.fatalities) <- 1:nrow(data.fatalities)
colnames(data.fatalities) <- c("EventType", "Fatalities")
head(data.fatalities, n=10)

data.injuries <- data.injuries[order(-data.injuries$x),]
rownames(data.injuries) <- 1:nrow(data.injuries)
colnames(data.injuries) <- c("EventType", "Injuries")
head(data.injuries, n=10)

data.propDmg <- filter(data.dmg_clean, PROPDMG > 0)
data.propDmg$PROPDMGEXP <- gsub('H', 2, data.propDmg$PROPDMGEXP)
data.propDmg$PROPDMGEXP <- gsub('K', 3, data.propDmg$PROPDMGEXP) 
data.propDmg$PROPDMGEXP <- gsub('M', 6, data.propDmg$PROPDMGEXP) 
data.propDmg$PROPDMGEXP <- gsub('B', 9, data.propDmg$PROPDMGEXP) 
data.propDmg$PROPDMGEXP <- gsub('^$', 0, data.propDmg$PROPDMGEXP)
data.propDmg$PROPDMGEXP <- as.integer(data.propDmg$PROPDMGEXP)
data.propDmg <- mutate(data.propDmg, PROPDMGVAL = PROPDMG * 10^PROPDMGEXP)

data.propDmgSum <- aggregate(data.propDmg$PROPDMGVAL, list(data.propDmg$EVTYPE), sum)
data.propDmgSum <- data.propDmgSum[order(-data.propDmgSum$x),]
rownames(data.propDmgSum) <- 1:nrow(data.propDmgSum)
colnames(data.propDmgSum) <- c("EventType", "PropertyDamage")

data.cropDmg <- filter(data.dmg_clean, CROPDMG > 0)
data.cropDmg$CROPDMGEXP <- gsub('H', 2, data.cropDmg$CROPDMGEXP)
data.cropDmg$CROPDMGEXP <- gsub('K', 3, data.cropDmg$CROPDMGEXP) 
data.cropDmg$CROPDMGEXP <- gsub('M', 6, data.cropDmg$CROPDMGEXP) 
data.cropDmg$CROPDMGEXP <- gsub('B', 9, data.cropDmg$CROPDMGEXP)
data.cropDmg$CROPDMGEXP <- gsub('^$', 0, data.cropDmg$CROPDMGEXP)
data.cropDmg$CROPDMGEXP <- as.integer(data.cropDmg$CROPDMGEXP)
data.cropDmg <- mutate(data.cropDmg, CROPDMGVAL = CROPDMG * 10^CROPDMGEXP)

data.cropDmgSum <- aggregate(data.cropDmg$CROPDMGVAL, list(data.cropDmg$EVTYPE), sum)
data.cropDmgSum <- data.cropDmgSum[order(-data.cropDmgSum$x),]
rownames(data.cropDmgSum) <- 1:nrow(data.cropDmgSum)
colnames(data.cropDmgSum) <- c("EventType", "CropDamage")

economicDamage <- merge(data.propDmgSum, data.cropDmgSum, all = TRUE)
economicDamage[is.na(economicDamage)] <- 0
economicDamage <- mutate(economicDamage, TotalDamage = PropertyDamage + CropDamage)
economicDamage <- economicDamage[order(-economicDamage$TotalDamage),]
rownames(economicDamage) <- 1:nrow(economicDamage)

economicDamage_top <- economicDamage[1:10,]
keeps <- c("EventType","PropertyDamage", "CropDamage")
economicDamage_top = economicDamage_top[keeps]

DF1 <- melt(economicDamage_top, id.var="EventType")
g <- ggplot(DF1, aes(x = EventType, y = value, fill = variable)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + scale_fill_discrete(name="Legend", breaks=c("PropertyDamage", "CropDamage"),
  labels=c("Property Damage", "Crop Damage") )
g + ggtitle("Property and Crop Damage by Weather Event Type") + 
  labs(x="Event Type", y="Damage Amount ($)")



propDmg_top <- data.propDmgSum[1:5,]
cropDmg_top <- data.cropDmgSum[1:5,]

barplot(economicDamage_top, names.arg=economicDamage_top$EventType, ylim=c(0,1.5e11), las=2)

main_text <- "Total Economic Damage ($) by Event Type from Jan 1996 - Oct 2011"
y_lim <- c(0, 1.5e+11)

par(mfrow = c(1, 2), mar = c(11, 5, 1, 0), oma = c( 0, 0, 2, 0 )) 
barplot(propDmg_top$ProperyDamage, names.arg = propDmg_top$EventType, las=2,
        ylim = y_lim, col="blue")
par(mar = c(11, 0, 1, 5))
barplot(cropDmg_top$CropDamage, names.arg = cropDmg_top$EventType, las=2, 
        axes = FALSE, ylim = y_lim, col="red")

title( main_text, outer = TRUE )
legend("topright", c("Property Damage", "Crop Damage"), fill=c("blue", "red"))

print(economicDamage)
