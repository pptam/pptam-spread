#####################################################
# plottingPolygons.R
#####################################################
# plot polygons

###############################################################################################
#FUNCTIONS
###############################################################################################
# Function computeRelativeMass 
#
# Compute the mass for each load and each configuration
###############################################################################################
computeRelativeMass<-function(threshold, avg, mixTemp){
  
  #Check pass/fail for each service. the "mix" value is 0 if fail and mixTemp if pass. 
  #  Compute the relative mass for each configuration
  passCriteria<-avg
  relativeMass<-c()
  mix<-as.data.frame(matrix(nrow=nrow(usedSettings), ncol=ncol(mixTemp)))
  for(j in 1:nrow(passCriteria)){
    mix[j,]<-mixTemp[j,]
    for(i in 6:(5+noMicroServices)){
      if(passCriteria[j,i]>threshold[i]){mix[j,i]<-0}
    }
    relativeMass[j]<-sum(mix[j,6:(5+noMicroServices)])
  }
  return(relativeMass)
}
#################################################################
# end computeRelativeMass
###########################################################################

###############################################################################################
# Function ComputeDomainMetrics
#
#Compute the domain metric for each configuration as a list
###############################################################################################
computeDomainMetrics<-function(usedSettings, relativeMass, aggregatedValues ){
  tempData<-usedSettings
  tempData$relativeMass<-relativeMass
  absoluteMass<-c()
  for(j in 1:nrow(tempData)){	
    absoluteMass[j]<-tempData[j,"relativeMass"]*aggregatedValues[match(tempData[j,"Users"], aggregatedValues[,1]),2]
  }
  tempData$absoluteMass<-absoluteMass
  
  mySettingsUnique<-unique(tempData[3:5])
  set<-list()
  domainMetricList<-list()
  for(i in 1:nrow(mySettingsUnique)){
    set[[i]]<-tempData[which(tempData[,3] == mySettingsUnique[i,1]&tempData[,4] == mySettingsUnique[i,2]&tempData[,5] == mySettingsUnique[i,3]),]
    domainMetricList[[i]]<-set[[i]][,c(2,7)][order(set[[i]][,c(2,7)][,1]),]
  }
  return(domainMetricList)
}
####################################################################################
# end of ComputeDomainMetrics
####################################################################################

#################################################################################
# Function: computeCumulativeDomainMetric
#
#Compute Cumulative Domain Metric: summing up absoluteMass over loads for each configuration. Plot in a table
######################################################################################
computeCumulativeDomainMetric<-function(usedSettings, relativeMass, aggregatedValues){
  tempData<-computeDomainMetricsAll(usedSettings, relativeMass, aggregatedValues )
  mySettingsUnique<-unique(tempData[3:5])
  mySettingsUnique$domainMetric<-0
  for(i in 1:nrow(mySettingsUnique)){
    mySettingsUnique[i,4]<-round(sum(tempData[which(tempData[,3] == mySettingsUnique[i,1]&tempData[,4] == mySettingsUnique[i,2]&tempData[,5] == mySettingsUnique[i,3]),"absoluteMass"]),4)
  }
  domainMetric<-mySettingsUnique
  return(domainMetric)
}
#####################################################
# end of computeCumulativeDomainMetric
###########################################################
############################################################################################
# computeDomainMetricsAll
#
#Compute the domain metric for all configurations
###########################################################################################
computeDomainMetricsAll<-function(usedSettings, relativeMass, aggregatedValues){
  tempData<-usedSettings
  tempData$relativeMass<-relativeMass
  absoluteMass<-c()
  for(j in 1:nrow(tempData)){	
    absoluteMass[j]<-tempData[j,"relativeMass"]*aggregatedValues[match(tempData[j,"Users"], aggregatedValues[,1]),2]
  }
  #head(finalSettings)
  tempData$absoluteMass<-absoluteMass
  return(tempData)
}
##################################################################################################
# end of computeDomainMetricsAll
#############################################################

#END OF FUNCTIONS
###################################################################################
## Options
if(w<4){myLevels<-5}else{myLevels<-10}
#
max.num.users <-myMax
num.load.levels <- myLevels
######################################################


#scale the load
max.requests <- max(project.count.data$requests)
scale.factor <- max.num.users / max.requests
scaled.number.of.users <- scale.factor * project.count.data$requests

#tempData<-usedSettings
#mySettingsUnique<-unique(tempData[3:5])
#print(mySettingsUnique)

#########################################################
# Aggregates frequencies by Scaled number of users
#########################################################
num.users.hist <- hist(scaled.number.of.users,
                       breaks = num.load.levels,
                       col = "darkgray", border = "white",
                       xlab = "scaled number of users", main = "Histogram of scaled number of users", plot=F)     

num.users.hist$frequency<-num.users.hist$counts / sum(num.users.hist$counts)

#reformat the above number into a Table format
frequencies.of.occurrence <- data.frame(
  scaled.number.of.users = num.users.hist$breaks,
  frequency.of.occurrence = c(0,num.users.hist$frequency)
)

aggregatedValuesWikipedia<-frequencies.of.occurrence
#################################################################
# computeThreshold.R
#################################################################

#Identify selected configurations from dataFile
mySettings <-unique(dataFile[,1:5])
#mySettings

############################################
#mySettingsUnique<-unique(tempData[3:5])

# computeThreshold.R
#THRESHOLD
#Define the threshold for each service. The threshold is a vector computed as avg+3*SD for the configuration with Users=2, Memory=4, CPU=1, CartReplica=1   
noMicroServices<-ncol(dataFile)-6

# setting up thresholds for Users = 2 which is avg + 3 * SD for each of the types
tempBench<-dataFile[dataFile$Users==2,]
#tempBench
benchSettings<-mySettings[mySettings$Users==2,]
benchSettings
avgVectorB<-tempBench[tempBench$Metric=="Avg (sec)",][,-c(1:6)]
SDVectorB<- tempBench[tempBench$Metric=="SD (sec)",][,-c(1:6)]
mixB<-tempBench[tempBench$Metric=="Mix % (take failure into account)",][,-c(1:6)]
threshold<-data.frame(benchSettings,avgVectorB+3*SDVectorB)
#threshold
#SELECT DATA FROM FILE
#Exclude case with user = 2 from dataFile and check whether each service pass or fail: avg<threshold (Pass). 
#Select relevant rows
usedSettings<-mySettings[!mySettings$Users==2,]
usedSettings

usedDataFile<-dataFile[!dataFile$Users==2,]
#average number of access
avg<-usedDataFile[usedDataFile$Metric=="Avg (sec)",-6]
#standard deviation of access
SD<-usedDataFile[usedDataFile$Metric=="SD (sec)",-6]
#This is the frequency of access to a microservice
mixTemp<-usedDataFile[usedDataFile$Metric=="Mix % (take failure into account)",-6]
################################################################################
# end of computeThreshold.R
################################################################################

# compute using function relative Mass
relativeMass<-computeRelativeMass(threshold, avg, mixTemp)  
#relativeMass

# compute using function doman
domainMetricList<-computeDomainMetrics(usedSettings, relativeMass, aggregatedValuesWikipedia)
domainMetricList
# makes list of domain metrics

myL<-list()
myValue<-c()
for(i in 1:nrow(unique(usedSettings[,3:5]))){
  #print(domainMetricList[[i]][,2]>=aggregatedValuesWikipedia[,2])
  myL[[i]]<-domainMetricList[[i]][,2]>=aggregatedValuesWikipedia[,2]
  myValue[i]<-min(which(myL[[i]] %in% FALSE))-1
}
bestApproachingLine<-which.max(myValue)

domainMetric<-computeCumulativeDomainMetric(usedSettings, relativeMass, aggregatedValuesWikipedia)
bestDMLine<-which.max(domainMetric[,4])

#######################################################
# SET UP FOR Sensitvity
aggregatedValues<-aggregatedValuesWikipedia
#############################################################
# SensitivityAnalysis.R
#############################################################

originalThreshold <- threshold
domainMatricVariantLower <- list()
relativeMassItem <- list()
scaledThresholdLower <- c()
for (m in c(1:50)) {
  l <- m / 50
  scaledThresholdLower <- l * originalThreshold
  relativeMassItem <-
    computeRelativeMass(scaledThresholdLower, avg, mixTemp)
  domainMatricVariantLower[[m]] <-
    computeDomainMetrics(usedSettings, relativeMassItem, aggregatedValues)
}

domainMatricVariantGreater <- list()
relativeMassItem <- list()
for (m in c(1:49)) {
  l <- (m + 50) / 50
  scaledThresholdGreater <- l * originalThreshold
  relativeMassItem <-
    computeRelativeMass(scaledThresholdGreater, avg, mixTemp)
  domainMatricVariantGreater[[m]] <-
    computeDomainMetrics(usedSettings, relativeMassItem, aggregatedValues)
}

domainMatricVariantIntegers <- list()
relativeMassItem <- list()
for (m in c(1:49)) {
  scaledThresholdIntegers <- (m + 1) * originalThreshold
  relativeMassItem <-
    computeRelativeMass(scaledThresholdIntegers, avg, mixTemp)
  domainMatricVariantIntegers[[m]] <-
    computeDomainMetrics(usedSettings, relativeMassItem, aggregatedValues)
}


scaleLower <- 1 / 50 * c(1:50)
scaleGreater <- 1 / 50 * c(51:99)
scaleIntegers <- c(2:50)

#all in once plots
domainMatricVariant <- list()
domainMatricVariant <- append(domainMatricVariantLower, domainMatricVariantGreater)
domainMatricVariant <- append(domainMatricVariant, domainMatricVariantIntegers)
scale <- c()
scale <- append(scaleLower, scaleGreater)
scale <- append(scale, scaleIntegers)

#FUNCTIONS
#set current directory as in Start.R
#setwd(currentWD)
#print("Definition of computeSensitivityPerConfiguration in SensitivityAnalysis.R")


###########################################################
#  computeSensitivityPerConfiguraton
#############################################################
sortedDomainMetric <-
  domainMetric[order(domainMetric[, 1], domainMetric[, 2], domainMetric[, 3]), ]
k <- which(sortedDomainMetric[, 4] == max(sortedDomainMetric[, 4]))
j<-min(which(domainMetric[,4]==sortedDomainMetric[k[1], 4]))




