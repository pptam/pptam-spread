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
#avg
#     ID Users Memory  CPU CartReplicas createOrder basket getCatalogue getItem getCart  login getOrders catalogue  home
#1    1   150    1.0 0.50            2       0.011  0.002        0.063   0.063   0.044  0.072     0.011     0.002 0.002
#4    2   300    0.5 0.25            4       0.008  0.002        0.014   0.017  16.504  9.274     0.008     0.002 0.001
#mixTemp
#    ID Users Memory  CPU CartReplicas createOrder basket getCatalogue getItem getCart  login getOrders catalogue   home
#3    1   150    1.0 0.50            2      0.0132 0.0132       0.1128  0.0960  0.2259 0.0261    0.0261    0.0303 0.0996
#6    2   300    0.5 0.25            4      0.0130 0.0130       0.1132  0.0957  0.2254 0.0257    0.0259    0.0305 0.1001  
#threshold
#    ID Users Memory CPU CartReplicas createOrder basket getCatalogue getItem getCart login getOrders catalogue  home  tags
#205 69     2      4   1            1        0.03  0.002        0.008   0.027   0.074 0.089     0.026     0.002 0.005 0.016
# if avg <= threshold then mixtemp else 0, then sum the mixtemps in a line
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
    temp2<-tempData[which(tempData[,3] == mySettingsUnique[i,1]&tempData[,4] == mySettingsUnique[i,2]&tempData[,5] == mySettingsUnique[i,3]),]
    set[[i]] <- temp2 %>% add_row(relativeMass=0,absoluteMass=0,Users=0)
    domainMetricList[[i]]<-set[[i]][,c(2,7)][order(set[[i]][,c(2,7)][,1]),]
  }
class(tempData[which(tempData[,3] == mySettingsUnique[i,1]&tempData[,4] == mySettingsUnique[i,2]&tempData[,5] == mySettingsUnique[i,3]),])
  
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
#usedSettings


computeDomainMetricsAll<-function(usedSettings, relativeMass, aggregatedValues){
  tempData<-usedSettings
#tempData
#ID Users Memory  CPU CartReplicas
#1    1   150    1.0 0.50            2
#4    2   300    0.5 0.25            4
#7    3   250    1.0 0.50            2
#10   4    50    0.5 0.50            2
#13   5   300    0.5 0.25            2
#16   6   100    1.0 0.50            2
#19   7   300    1.0 0.50            2
  tempData$relativeMass<-relativeMass
# tempData$relativeMass
#[1] 0.5304 0.6048 0.2262 0.9999 0.2262 1.0001 0.2258 1.0001 1.0000 0.7480 0.3918 0.7474 0.2260 0.5305 0.2262 0.6175 1.0001
#[18] 1.0000 0.5300 0.9999 1.0000 0.2262 0.2260 0.1959 0.5623 0.2259 1.0003 0.9998 0.5304 0.9999 0.2258 0.9999 0.2261 0.9998  
  absoluteMass<-c()
  for(j in 1:nrow(tempData)){	
    # multiply relativemass for point times the aggregatedValue
    absoluteMass[j]<-tempData[j,"relativeMass"]*aggregatedValues[match(tempData[j,"Users"], aggregatedValues[,1]),2]
  }
  # absoluteMass
   #[1] 0.188206452 0.010567742 0.017025806 0.151866532 0.003952419 0.213731048 0.003945430 0.213731048 0.151881720
  #[10] 0.159854839 0.139025806 0.139635215 0.017010753 0.188241935 0.042260484 0.219112903 0.151896909 0.151881720  
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
max.num.users <-myMax  # this is 300
num.load.levels <- myLevels  # either 5 or 10
######################################################
num.load.levels
#scale the load
max.requests <- max(project.count.data$requests)  # from wikipedia-profile-de
scale.factor <- max.num.users / max.requests
# this is a scaled number of users rescaled using the scale.factor
scaled.number.of.users <- scale.factor * project.count.data$requests


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
# this aggregatedValuesWikipedia is a rescaled probability by 0-50,50-100,100-150,150-200,200-250,250-300 as .151, etc.
aggregatedValuesWikipedia<-frequencies.of.occurrence
#################################################################
# computeThreshold.R
#################################################################

#Identify selected configurations from dataFile which is the first 5 columns of benchflow_output.csv
mySettings <-unique(dataFile[,1:5])
nrow(mySettings)
#ID Users Memory  CPU       CartReplicas
#1    1   150    1.0 0.50            2
#4    2   300    0.5 0.25            4
#7    3   250    1.0 0.50            2
#10   4    50    0.5 0.50            2
#13   5   300    0.5 0.25            2
#16   6   100    1.0 0.50            2
#19   7   300    1.0 0.50            2
#....

############################################
#mySettingsUnique<-unique(tempData[3:5])

# computeThreshold.R
#THRESHOLD
#Define the threshold for each service. The threshold is a vector computed as avg+3*SD for the configuration with Users=2, Memory=4, CPU=1, CartReplica=1   
# number of MicroServices are columns minus 6
noMicroServices<-ncol(dataFile)-6

# setting up thresholds for Users = 2 which is avg + 3 * SD for each of the types
tempBench<-dataFile[dataFile$Users==2,]
nrow(tempBench)
#ID Users Memory CPU CartReplicas                            Metric createOrder basket getCatalogue getItem getCart
#205 69     2      4   1            1                         Avg (sec)      0.0120 0.0020       0.0050  0.0060  0.0140
#206 69     2      4   1            1                          SD (sec)      0.0060 0.0000       0.0010  0.0070  0.0200
#207 69     2      4   1            1 Mix % (take failure into account)      0.0129 0.0129       0.1124  0.0952  0.2263
#login getOrders catalogue   home   tags getCustomer viewOrdersPage cataloguePage getRelated addToCart catalogueSize
#205 0.0230    0.0110    0.0020 0.0020 0.0040      0.0060         0.0020        0.0050     0.0070    0.0020        0.0060
#206 0.0220    0.0050    0.0000 0.0010 0.0040      0.0070         0.0010        0.0010     0.0090    0.0010        0.0060
#207 0.0247    0.0252    0.0313 0.0998 0.0313      0.1136         0.0252        0.0313     0.0442    0.0126        0.0313
#getAddress getCard showDetails
#205     0.0070  0.0050      0.0020
#206     0.0090  0.0010      0.0000
#207     0.0129  0.0129      0.0442

benchSettings<-mySettings[mySettings$Users==2,]
benchSettings
#    ID  Users Memory  CPU  CartReplicas
#205 69     2      4   1            1
avgVectorB<-tempBench[tempBench$Metric=="Avg (sec)",][,-c(1:6)]
SDVectorB<- tempBench[tempBench$Metric=="SD (sec)",][,-c(1:6)]
mixB<-tempBench[tempBench$Metric=="Mix % (take failure into account)",][,-c(1:6)]
threshold<-data.frame(benchSettings,avgVectorB+3*SDVectorB)
#threshold
#threshold  # thresholds by microservices
#    ID Users Memory CPU CartReplicas createOrder basket getCatalogue getItem getCart login getOrders catalogue  home  tags
#205 69     2      4   1            1        0.03  0.002        0.008   0.027   0.074 0.089     0.026     0.002 0.005 0.016
#SELECT DATA FROM FILE
#Exclude case with user = 2 from dataFile and check whether each service pass or fail: avg<threshold (Pass). 
#Select relevant rows
nrow(mySettings[!mySettings$Users==2,])
usedSettings<-mySettings[!mySettings$Users==2,]

usedDataFile<-dataFile[!dataFile$Users==2,]
avg<-dataFile[usedDataFile$Metric=="Avg (sec)",]
#nrow(avg)
#nrow(usedDataFile)
# these are the average, sd and mixTemp that are in the BenchOutput dataset
#average number of access
#usedDataFile[usedDataFile$Metric=="Avg (sec)",-6]
avg<-avg[,-6]
#nrow(avg)
#avg
#ID Users Memory  CPU CartReplicas createOrder basket getCatalogue getItem getCart  login getOrders catalogue  home
#1    1   150    1.0 0.50            2       0.011  0.002        0.063   0.063   0.044  0.072     0.011     0.002 0.002
#4    2   300    0.5 0.25            4       0.008  0.002        0.014   0.017  16.504  9.274     0.008     0.002 0.001
#7    3   250    1.0 0.50            2       0.056  0.002        0.432   0.426   0.317  0.530     0.058     0.002 0.002
#10   4    50    0.5 0.50            2       0.007  0.001        0.004   0.005   0.009  0.016     0.008     0.001 0.001
#13   5   300    0.5 0.25            2       0.053  0.002        0.414   0.417   0.512  0.693     0.056     0.002 0.002

#standard deviation of access
SD<-usedDataFile[usedDataFile$Metric=="SD (sec)",-6]
#This is the frequency of access to a microservice
mixTemp<-usedDataFile[usedDataFile$Metric=="Mix % (take failure into account)",-6]
################################################################################
# end of computeThreshold.R
################################################################################
#nrow(mixTemp)
# compute using function relative Mass
relativeMass<-computeRelativeMass(threshold, avg, mixTemp)  
#relativeMass # relative mass for the all the different configuations by the tests
#[1] 0.5304 0.6048 0.2262 0.9999 0.2262 1.0001 0.2258 1.0001 1.0000 0.7480 0.3918 0.7474 0.2260 0.5305 0.2262 0.6175 1.0001
#[18] 1.0000 0.5300 0.9999 1.0000 0.2262 0.2260 0.1959 0.5623 0.2259 1.0003 0.9998 0.5304 0.9999 0.2258 0.9999 0.2261 0.9998
#[35] 0.7484 0.2261 0.2259 0.2134 0.4350 0.6440 0.2262 1.0002 0.2263 0.7485 1.0003 0.5917 0.9999 0.2261 0.7497 0.2261 0.9999
#[52] 0.2262 0.2264 0.2262 1.0000 0.2259 0.2261 0.6434 0.7482 0.9999 1.0000 0.5181 0.2262 1.0001 0.7485 1.0000 0.2130 0.2262
#[69] 0.7481 0.2260 0.2259 0.6435

# compute using function doman
domainMetricList<-computeDomainMetrics(usedSettings, relativeMass, aggregatedValuesWikipedia)
#domainMetricList
# makes list of domain metrics by absoluteMass by the 12 different Configuration
#[[1]]
#    Users absoluteMass
#82     50   0.15185134
#16    100   0.21373105
#1     150   0.18820645
#109   200   0.04220444
#7     250   0.01702581
#19    300   0.00394543

myL<-list()
myValue<-c()

domainMetricList[[1]]

for(i in 1:nrow(unique(usedSettings[,3:5]))){
 #   print(domainMetricList[[i]][,2]>=aggregatedValuesWikipedia[,2])
  myL[[i]]<-domainMetricList[[i]][,2]>=aggregatedValuesWikipedia[,2]
  myValue[i]<-min(which(myL[[i]] %in% FALSE))-1
}
#myValue is 2 2 2 3 2 3 2 3 3 2 2 3
# myL is [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE
bestApproachingLine<-which.max(myValue)
#bestApproachingLine  # this is 4

domainMetric<-computeCumulativeDomainMetric(usedSettings, relativeMass, aggregatedValuesWikipedia)
bestDMLine<-which.max(domainMetric[,4])
#bestDMLine  # this is 4
#######################################################
# SET UP FOR Sensitvity
aggregatedValues<-aggregatedValuesWikipedia
# these are top black line in the polygon
################################################################
##############################################################
# SensitivityAnalysis.R
#############################################################
#aggregatedValues
originalThreshold <- threshold
domainMatricVariantLower <- list()
relativeMassItem <- list()
scaledThresholdLower <- c()
for (m in c(1:50)) {
  l <- m / 50
  scaledThresholdLower <- l * originalThreshold
  relativeMassItem <- computeRelativeMass(scaledThresholdLower, avg, mixTemp)
  domainMatricVariantLower[[m]] <- computeDomainMetrics(usedSettings, relativeMassItem, aggregatedValues)
}

domainMatricVariantGreater <- list()
relativeMassItem <- list()
for (m in c(1:49)) {
  l <- (m + 50) / 50
  scaledThresholdGreater <- l * originalThreshold
  relativeMassItem <-  computeRelativeMass(scaledThresholdGreater, avg, mixTemp)
  domainMatricVariantGreater[[m]] <-computeDomainMetrics(usedSettings, relativeMassItem, aggregatedValues)
}

domainMatricVariantIntegers <- list()
relativeMassItem <- list()
for (m in c(1:49)) {
  
  scaledThresholdIntegers <- (m + 1) * originalThreshold
  relativeMassItem <- computeRelativeMass(scaledThresholdIntegers, avg, mixTemp)
  domainMatricVariantIntegers[[m]] <- computeDomainMetrics(usedSettings, relativeMassItem, aggregatedValues)

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
sortedDomainMetric <- domainMetric[order(domainMetric[, 1], domainMetric[, 2], domainMetric[, 3]), ]
k <- which(sortedDomainMetric[, 4] == max(sortedDomainMetric[, 4]))
# k is the position of the maximum Domain Metric
j<-min(which(domainMetric[,4]==sortedDomainMetric[k[1], 4]))
# j is the position in the unsorted list
#sortedDomainMetric
#Memory  CPU CartReplicas domainMetric
#64    0.5 0.25            1       0.6572
#13    0.5 0.25            2       0.7773
#4     0.5 0.25            4       0.6721
#22    0.5 0.50            1       0.6125
#10    0.5 0.50            2       0.6169
#40    0.5 0.50            4       0.6171
#94    1.0 0.25            1       0.6030
#46    1.0 0.25            2       0.6932
#67    1.0 0.25            4       0.6288
#85    1.0 0.50            1       0.6170
#1     1.0 0.50            2       0.6170
#25    1.0 0.50            4       0.6568

