#runSim4.1.R
#Clara Yip
#3/21/17

#This script runs mapGrid_parameter.R for multiple simulations at varying % forest cover and produces (1) a graph of %forest cover vs % of unused forest and (2) a graph of avg dist between patches vs amount of total unused forest on the landscape

## SetWD Jen
setwd("~/Dropbox/PhD_Bradham/IBM/IBM_2017_03_22")
rm(list=ls())
# 
source("mg4.1_JB.R")
library(RColorBrewer)
#library(ggplot2)

freqHolder <- matrix(0L, nrow = 10, ncol = 30) # matrix to record the frequency of cell hits
timesCrossed <- matrix(0L, nrow = 10, ncol = 30) # matrix to record the times the matrix is crossed
distBWPatches <- matrix(0L, nrow = 10, ncol = 30) # matrix to record the distance between forested patches


for (i in 1:10) {
  print(paste("Simulating", i*10, "percent viable", sep = " "))
  for (j in 1:30) {
    print(".")
    result <- simulateMovement(167,167, 4, i*10, 14600, 83.33, j) #xlength, ylength, number of seeds, % viable land, number of steps, max distance per step, iteration
    freqHolder[i, j] <- result[1]                    
    timesCrossed[i, j] <- result[2]
    distBWPatches[i, j] <- result[3]  
  }
}

#simulateMovement: (xLength, yLength, seeds, viable, steps, maxDist, iterations)

#print/save %viable vs %unused
boxplot.matrix(t(freqHolder), las = 2, ylab = "Percent viable map with tread == 0", xlab = "Percent map viable", 
               col = brewer.pal(9, "Spectral"), 
               names = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"))

dev.copy(jpeg, paste("totalBoxplot", ".jpeg", sep = ""))
dev.off()

write.csv(freqHolder, file = "totalBoxplot.csv")
write.csv(timesCrossed, file = "totalCrossingFreq.csv")
write.csv(distBWPatches, file = "avgDist.csv")

# Measuring connectivity between patches with 10 graphs for each seed 10, one graph per percent
# scatter (w/o line)
for (i in 1:10) {
  plot(distBWPatches[i,], freqHolder[i,], ylab = "Percent of unused forest", xlab = "Avg distance between forest patches")
  dev.copy(jpeg, paste("avg0byDist", i*10, ".jpeg", sep = ""))
  dev.off()
}

