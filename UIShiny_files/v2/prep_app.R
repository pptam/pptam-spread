# ShinyPPTAMWorking8.R.R

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gridExtra)
library(gridBase)
library(grid)
library(stringr)
library(tidyverse)
library(plotly)
library(here)
rm(list=ls())
# comment out when publishing
#setwd("C:\\jobs\\ESOlabs\\polygon_graph") # set working directory
here::set_here()   # set working directory to current directory for shiny publishing
##############################################
# INPUT FILES wikipedia-profile-XX.csv and benchflow_output.csv
#Operational profile of Wikipedia
# this is from file wikipedia-profile-de.csv
project.count.data <- read.csv2("wikipedia-profile-de.csv")
#########################################################
#working directory
experimentsDirectory <-file.path(getwd(), fsep = .Platform$file.sep)
#experiments' files
#######################################################################################
# this looks for ResponseTimesseconds.csv files.  You need a clean-up directory for this
# Don't fill it with .pdf files and the like.
#######################################################################################
#myFiles <-
#  list.files(
#    experimentsDirectory,
#    recursive = TRUE,
#    full.names = F,
#    pattern = "^ResponseTimesseconds.csv"
#  )
#myFiles
##################################################################################
#Average data Benchflow. look for benchflow_output.csv file.  Should be only 1!!!
##################################################################################
myFileBenchFlow <-
  list.files(experimentsDirectory,
             full.names = F,
             recursive = TRUE,
             pattern = "^benchflow")

myFileBenchFlow
#############################################
# read the benchflow_output.csv file
#######################################

######################################################################
# benchflowoutput had a first line "sep =" ,not anymore, so need 'skip = 0' for that
######################################################################
#BenchFlowTemp <- read.csv("benchflow_output.csv")
tryCatch({
  BenchFlowTemp <-
    as.data.frame(read.csv(
      file.path(experimentsDirectory, myFileBenchFlow),header = T,sep = ",",skip=0))}, 
  error=function(e){stop(safeError(e)) })
BenchFlowTemp
BenchFlow <- BenchFlowTemp
#nrow(BenchFlow)
dataFile <- BenchFlowTemp
#############################################################
#############################################################
# MAKE CHANGES STARTING HERE
#
# CHANGE THIS ONE BELOW  # select statement has to match the column headings
unique_data <- dataFile %>%  filter(Users != 2) %>% select(Memory,CPU,CartReplicas) %>% unique
############END CHANGE###########################################
domain_n <- nrow(unique_data)   # DON'T CHANGE THIS!!!!
#############################################################
#  EXPECTING A HEADING LIKE THIS
#ID	Users	Memory	CPU	Service1Replica	Service2Replica	Metric	  createOrder
#1	150	   1	    0.5	         2	          0	       Avg (sec)	 0.011
#1	150	   1	    0.5	         2	          0	       SD (sec)	   0.066
#############################################################
# CHANGE THESE TWO BELOW - NumMetricCol and choices
#############################################################
NumMetricCol <<- 6  # No. of Metric column -- This column F=6,G=7,H=8


###### here's where you make the labels for the UI polygon page.
choices =paste ("Mem=",format(unique_data$Memory,width=2)," CPU=",format(unique_data$CPU,width=2)," CartReplicas=",unique_data$CartReplicas, sep = "", collapse = NULL)
##########END WITH ALL CHANGES ####################################################
##################################################################################

NumOfLabels <<- NumMetricCol   # << makes it a global variable

NumColConf <<- NumOfLabels - 1

myNames<-c("Time",colnames(BenchFlow[-c(1:NumOfLabels)]))
myNames
mySettings <-unique(dataFile[dataFile$Users > 2,3:NumColConf]) # take out baseline which is Users = 2
mySettings

w=2  # some kind of control variable
myMax<-300   # maximum number of users

# 1   2     3      4      5           6
#ID	Users	Memory	CPU	CartReplicas	Metric

oldw <- getOption("warn")  # turn off warnings
options(warn = -1)


# this is the code to make the polygon and the sensitivity analysis
################################# START OF PolygonSource_working_v2.R#####################

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
    for(i in NumOfLabels:(NumOfLabels -1 +noMicroServices)){
      if(passCriteria[j,i]>threshold[i]){mix[j,i]<-0}
    }
    relativeMass[j]<-sum(mix[j, NumOfLabels:(NumOfLabels -1+noMicroServices)])
  }
  return(relativeMass)
}
#################################################################
# end computeRelativeMass
###########################################################################
##########################################################################################
# aggregateMassByUser
# this is a subroutine that returns the data for a unique setting.
# inputs, idomain = domain index
#         tempData = database of all Users, infor, and probability Masses 
#         startcol = number where col starts of unique info
#         endcol  = number  where col ends of unique info
# return:   for one domainMtricList
#################################################
aggregateMassByUser <- function(idomain,tempData,mySettingsUnique,startcol,endcol,masscol) {
  temp2 <- NULL
  # this statement finds all the matching by unique setting and passes the row numbers as a vector
  vector <- which(apply(tempData[,startcol:endcol],1,function(x) sum( x == mySettingsUnique[idomain,]) == endcol-startcol+1))
  temp2 <- tempData[vector,]
  return(temp2)
}    

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
  
  mySettingsUnique<-unique(tempData[3:NumColConf])
  ########################################################################################################
  ### make a list of the domains based on the n number of possible settings that are in columns 3,4, and 5
  ### There are 12 possibilities, 2 Memories, 2 CPUs, and 3 CartReplicas, hence 12
  ########################################################################################################
  set<-list()
  domainMetricList<-list()
  for(i in 1:nrow(mySettingsUnique)){
    temp2 <- aggregateMassByUser(i,tempData,mySettingsUnique,3,NumColConf,NumOfLabels)  # arguments inputdata,start col, end col and mass column    
    
    # adding an extra row for the 0,0 plot point for the polygon
    set[[i]] <- temp2 %>% add_row(relativeMass=0,absoluteMass=0,Users=0)
    # add to list
    domainMetricList[[i]]<-set[[i]][,c(2,NumOfLabels+1)][order(set[[i]][,c(2,NumOfLabels+1)][,1]),]
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
  
  mySettingsUnique<-unique(tempData[3:NumColConf])
  mySettingsUnique$domainMetric<-0
  mySettingsUnique
  for(i in 1:nrow(mySettingsUnique)){
    # >>>>NumColConf-2)]) == NumColConf-2)  <<<< you substract 2 because we take out -- 1)ID and 2)Users
    mySettingsUnique[i,NumColConf-1]<-round(sum(tempData[which(apply(tempData[,3:NumColConf],1,function(x) sum( x == mySettingsUnique[i,1:(NumColConf-2)]) == NumColConf-2)),"absoluteMass"]),4)
    #    mySett
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
#num.load.levels
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
#num.users.hist
num.users.hist$frequency<-num.users.hist$counts / sum(num.users.hist$counts)

#reformat the above number into a Table format
frequencies.of.occurrence <- data.frame(
  #  scaled.number.of.users = num.users.hist$breaks,
  #  frequency.of.occurrence = c(0,num.users.hist$frequency)
  sampled.load.tests = num.users.hist$breaks,
  contrib.to.domain.metrics = c(0,num.users.hist$frequency)
  
)
# this aggregatedValuesWikipedia is a rescaled probability by 0-50,50-100,100-150,150-200,200-250,250-300 as .151, etc.
aggregatedValuesWikipedia<-frequencies.of.occurrence
#aggregatedValuesWikipedia
#################################################################
# computeThreshold.R
#################################################################

#Identify selected configurations from dataFile which is the first 5 columns of benchflow_output.csv
mySettings <-unique(dataFile[,1:NumColConf])
#nrow(mySettings)
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
#unique(mySettings[3:6])
#mySettingsUnique<-unique(dataFile[,3:6])
#mySettingsUnique
# computeThreshold.R
#THRESHOLD
#Define the threshold for each service. The threshold is a vector computed as avg+3*SD for the configuration with Users=2, Memory=4, CPU=1, CartReplica=1   
# number of MicroServices are columns minus NumOfLabels = 6
noMicroServices<-ncol(dataFile)-NumOfLabels
################################################################################################
# setting up thresholds for Users = 2 which is the no load case, avg + 3 * SD for each of the types
###############################################################################################
tempBench<-dataFile[dataFile$Users==2,]
nrow(tempBench)  # should be 3
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
#benchSettings
#    ID  Users Memory  CPU  CartReplicas
#205 69     2      4   1            1
avgVectorB<-tempBench[tempBench$Metric=="Avg (sec)",][,-c(1:NumOfLabels)]
SDVectorB<- tempBench[tempBench$Metric=="SD (sec)",][,-c(1:NumOfLabels)]
mixB<-tempBench[tempBench$Metric=="Mix % (take failure into account)",][,-c(1:NumOfLabels)]
threshold<-data.frame(benchSettings,avgVectorB+3*SDVectorB)
#threshold
#threshold  # thresholds by microservices
#    ID Users Memory CPU CartReplicas createOrder basket getCatalogue getItem getCart login getOrders catalogue  home  tags
#205 69     2      4   1            1        0.03  0.002        0.008   0.027   0.074 0.089     0.026     0.002 0.005 0.016
#SELECT DATA FROM FILE
#Exclude case with user = 2 from dataFile and check whether each service pass or fail: avg<threshold (Pass). 
#Select relevant rows
#nrow(mySettings[!mySettings$Users==2,])
usedSettings<-mySettings[!mySettings$Users==2,]

usedDataFile<-dataFile[dataFile$Users>2,]
#usedDataFile$Users
avg<-dataFile[usedDataFile$Metric=="Avg (sec)" & dataFile$Users>2,]
#avg

#nrow(avg)
#nrow(usedDataFile)
# these are the average, sd and mixTemp that are in the BenchOutput dataset
#average number of access
#usedDataFile[usedDataFile$Metric=="Avg (sec)",-NumMetricCol]
avg<-avg[,-NumMetricCol]  # -NumMetricCol gets rid of metric column
#nrow(avg)
#avg
#ID Users Memory  CPU CartReplicas createOrder basket getCatalogue getItem getCart  login getOrders catalogue  home
#1    1   150    1.0 0.50            2       0.011  0.002        0.063   0.063   0.044  0.072     0.011     0.002 0.002
#4    2   300    0.5 0.25            4       0.008  0.002        0.014   0.017  16.504  9.274     0.008     0.002 0.001
#7    3   250    1.0 0.50            2       0.056  0.002        0.432   0.426   0.317  0.530     0.058     0.002 0.002
#10   4    50    0.5 0.50            2       0.007  0.001        0.004   0.005   0.009  0.016     0.008     0.001 0.001
#13   5   300    0.5 0.25            2       0.053  0.002        0.414   0.417   0.512  0.693     0.056     0.002 0.002

#standard deviation of access
SD<-usedDataFile[usedDataFile$Metric=="SD (sec)",-NumMetricCol]
#This is the  to a delta number of microservice
mixTemp<-usedDataFile[usedDataFile$Metric=="Mix % (take failure into account)",-NumMetricCol]
################################################################################
# end of computeThreshold.R
################################################################################
#nrow(mixTemp)
# compute using function relativeMass
#relativeMass # relative mass for the all the different configuations by the tests
relativeMass<-computeRelativeMass(threshold, avg, mixTemp)  
#relativeMass
#[1] 0.5304 0.6048 0.2262 0.9999 0.2262 1.0001 0.2258 1.0001 1.0000 0.7480 0.3918 0.7474 0.2260 0.5305 0.2262 0.6175 1.0001
#[18] 1.0000 0.5300 0.9999 1.0000 0.2262 0.2260 0.1959 0.5623 0.2259 1.0003 0.9998 0.5304 0.9999 0.2258 0.9999 0.2261 0.9998
#[35] 0.7484 0.2261 0.2259 0.2134 0.4350 0.6440 0.2262 1.0002 0.2263 0.7485 1.0003 0.5917 0.9999 0.2261 0.7497 0.2261 0.9999
#[52] 0.2262 0.2264 0.2262 1.0000 0.2259 0.2261 0.6434 0.7482 0.9999 1.0000 0.5181 0.2262 1.0001 0.7485 1.0000 0.2130 0.2262
#[69] 0.7481 0.2260 0.2259 0.6435

# compute using function doman
domainMetricList<-computeDomainMetrics(usedSettings, relativeMass, aggregatedValuesWikipedia)
#domainMetricList[[4]]
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

for(i in 1:nrow(unique(usedSettings[,3:NumColConf]))){
  #   print(domainMetricList[[i]][,2]>=aggregatedValuesWikipedia[,2])
  myL[[i]]<-domainMetricList[[i]][,2]>=aggregatedValuesWikipedia[,2]
  myValue[i]<-min(which(myL[[i]] %in% FALSE))-1
}
#myValue is 2 2 2 3 2 3 2 3 3 2 2 3
# myL is [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE
bestApproachingLine<-which.max(myValue)
bestApproachingLine  # this is 4
#myValue
domainMetric<-computeCumulativeDomainMetric(usedSettings, relativeMass, aggregatedValuesWikipedia)
domainMetric
bestDMLine<-which.max(domainMetric[,NumColConf-1])
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
# sort by variables in order no matter the number of variables
sortedDomainMetric <- domainMetric %>% arrange()

# (NumColConf-1) is the last column in 
k <- which(sortedDomainMetric[, ncol(sortedDomainMetric)] == max(sortedDomainMetric[, ncol(sortedDomainMetric)]))
# k is the position of the maximum Domain Metric
j<-min(which(domainMetric[,(NumColConf-1)]==sortedDomainMetric[k[1], (NumColConf-1)]))
# j is the position in the unsorted list
#sortedDomainMetric


#################end of PolygonSource_working_v2.R  ##################################
#save.image("C:/jobs/ESOlabs/polygon_graph/to_shiny_site.RData")
save.image("to_shiny_site.RData")

