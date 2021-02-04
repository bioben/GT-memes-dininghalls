##---
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
##--

# ###---Load packages
library(readxl)
library(ggplot2)
library(chisq.posthoc.test)

# ###---Read in raw data, clean dataframe
dataname = 'dininghall_mfkv2.xlsx'
diningHallData <- as.data.frame(read_excel(dataname))

# ###---Willage Data
willageData <- diningHallData[which(diningHallData$DiningHall == "Willage"), names(diningHallData)]
willageMarryData <- willageData[which(willageData$Decision == "M"), names(willageData)]
willageMarryTotal <- sum(willageMarryData$VoteCount)
willageKillData <- willageData[which(willageData$Decision == "K"), names(willageData)]
willageKillTotal <- sum(willageKillData$VoteCount)
willageFuckData <- willageData[which(willageData$Decision == "F"), names(willageData)]
willageFuckTotal <- sum(willageFuckData$VoteCount)
willageGrandtotal <- sum(willageMarryTotal, willageKillTotal, willageFuckTotal)

# ###---Nave Data
naveData <- diningHallData[which(diningHallData$DiningHall == "Nave"), names(diningHallData)]
naveMarryData <- naveData[which(naveData$Decision == "M"), names(naveData)]
naveMarryTotal <- sum(naveMarryData$VoteCount)
naveKillData <- naveData[which(naveData$Decision == "K"), names(naveData)]
naveKillTotal <- sum(naveKillData$VoteCount)
naveFuckData <- naveData[which(naveData$Decision == "F"), names(naveData)]
naveFuckTotal <- sum(naveFuckData$VoteCount)
naveGrantTotal<- sum(naveMarryTotal, naveFuckTotal, naveKillTotal)

# ###---Britain Data
britainData <- diningHallData[which(diningHallData$DiningHall == "Britain"), names(diningHallData)]
britainMarryData <- britainData[which(britainData$Decision == "M"), names(britainData)]
britainMarryTotal <- sum(britainMarryData$VoteCount)
britainKillData <- britainData[which(britainData$Decision == "K"), names(britainData)]
britainKillTotal <- sum(britainKillData$VoteCount)
britainFuckData <- britainData[which(britainData$Decision == "F"), names(britainData)]
britainFuckTotal <- sum(britainFuckData$VoteCount)
britainGrandTotal <- sum(britainMarryTotal, britainKillTotal, britainFuckTotal)

# ###---Overall Plots
decisionBreakdown <- ggplot(data=diningHallData, aes(x=DiningHall, y=VoteCount, fill=Decision)) + 
  geom_bar(stat = "identity") +
  ggtitle("Dining Halls") +
  theme_classic()

diningHallBreakdown <- ggplot(data=diningHallData, aes(x=Decision, y=VoteCount, fill=DiningHall)) + 
  geom_bar(stat = "identity") +
  ggtitle("Decision") +
  theme_classic()

# ###---DiningHall Plots, pie charts
navePlot <- ggplot(data=naveData, aes(x="", y = VoteCount, fill=Decision)) + 
  geom_bar(stat="identity") + 
  coord_polar("y", start=0) + 
  ggtitle("Nave") +
  theme_void()
britainPlot <- ggplot(data=britainData, aes(x="", y = VoteCount, fill=Decision)) + 
  geom_bar(stat="identity") + 
  coord_polar("y", start=0) +
  ggtitle("Brittain") +
  theme_void()
willagePlot <- ggplot(data=willageData, aes(x="", y = VoteCount, fill=Decision)) + 
  geom_bar(stat="identity") + 
  coord_polar("y", start=0) +
  ggtitle("Willage") +
  theme_void()


# ###---Statistical Tests
diningHallNames <- c("Nave", "Britain", "Willage")
marryTotals <- c(naveMarryTotal, britainMarryTotal, willageMarryTotal)
fuckTotals <- c(naveFuckTotal, britainFuckTotal, willageFuckTotal)
killTotals <- c(naveKillTotal, britainKillTotal, willageKillTotal)
chisquareTable <- data.frame("Marry" = marryTotals,
                             "Fuck" = fuckTotals,
                             "Kill" = killTotals)
chisq.test(chisquareTable)
chisq.posthoc.test(chisquareTable, method = "bonferroni")
