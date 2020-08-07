# ShinyPPTAMWorking8.R.R

library(shiny)
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
setwd("C:\\jobs\\ESOlabs\\polygon_graph8_2020\\PPTAM_EXT") # set working directory

##############################################
# INPUT FILES wikipedia-profile-XX.csv and benchflow_output.csv
#Operational profile of Wikipedia
# this is from file wikipedia-profile-de.csv
project.count.data <- read.csv2("wikipedia-profile-de.csv")
#########################################################
#project.count.data
#comment this out before deploy
setwd("C:\\jobs\\ESOlabs\\polygon_graph8_2020\\PPTAM_EXT")

#working directory
experimentsDirectory <-file.path(getwd(), fsep = .Platform$file.sep)
#experiments' files


myFiles <-
  list.files(
    experimentsDirectory,
    recursive = TRUE,
    full.names = F,
    pattern = "^ResponseTimesseconds.csv"
  )
#myFiles

#Average data Benchflow
myFileBenchFlow <-
  list.files(experimentsDirectory,
             full.names = F,
             recursive = TRUE,
             pattern = "^benchflow")
#myFileBenchFlow
#############################################
# read the benchflow_output.csv file
#######################################
tryCatch({
  BenchFlowTemp <-
    as.data.frame(read.delim(
      file.path(experimentsDirectory, myFileBenchFlow),header = T,sep = ",", skip=1))[-1, ]}, 
      error=function(e){stop(safeError(e)) })
#BenchFlowTemp
BenchFlow <- BenchFlowTemp[-1, ]
#nrow(BenchFlow)
dataFile <- BenchFlow
myNames<-c("Time",colnames(BenchFlow[-c(1:6)]))
#myNames


w=2  # some kind of control variable
myMax<-300   # maximum number of users
num_config = 12

oldw <- getOption("warn")  # turn off warnings
options(warn = -1)
##### read in count data for rescaling purpuses
project.count.data <- read.csv2("wikipedia-profile-de.csv")
###############################################################
#Function: Compute benchmark threshold for each service / this is for time series plots
###############################################################
computeBenchmark<-function(BenchFlow){
  BenchFlowAvg<-BenchFlow[BenchFlow$Metric=="Avg (sec)",]
  Config <- unique(BenchFlowAvg[c(2:5)])
  #ConfigThreshold<-Config[Config$Users==2,]
  avgThreshold<- BenchFlow[BenchFlow$Metric=="Avg (sec)" & BenchFlow$Users==2,][,-c(1:6)]
  SDThreshold <- BenchFlow[BenchFlow$Metric=="SD (sec)" & BenchFlow$Users==2,][,-c(1:6)]
  benchmark<-as.data.frame(t(as.numeric(as.matrix(avgThreshold))+as.numeric(as.matrix(SDThreshold))*3))
  myList<-list(Config,benchmark,BenchFlowAvg)
  names(myList)<-list("Config","baseline", "BenchFlowAvg")
  return(myList)
}
##########################################################################
# datasetCreation  / this is for time series plots
##########################################################################

datasetCreation<-function(workload,myFiles,enchFlow,myType,myBehav){
  #Type of behaviour
  currentFilesIndex <- which(str_extract(myFiles, myBehav) == myBehav)
  myFilemyBehav <- myFiles[currentFilesIndex]
  #Type of attack
  currentFilesIndex <- which(str_extract(myFilemyBehav, myType) == myType)
  myFilemyBehav_type <- myFilemyBehav[currentFilesIndex]
  #Users
  currentFilesIndex<-which(str_extract(myFilemyBehav_type, paste("/",workload,"Users", sep="")) == paste("/",workload,"Users", sep=""))
  myFilemyBehav_type_users <- myFilemyBehav_type[currentFilesIndex]
  if(length(myFilemyBehav_type_users)==0){print("no such file") 
    return()}
  tryCatch({
    #reading file
    faithful_myBehav_type_users <-
      as.data.frame(read.delim(
        file.path(experimentsDirectory, myFilemyBehav_type_users),
        header = T,
        sep = ","
      ))[, -1]},
    error=function(e){
      stop(safeError(e))
    })
  
   #Configuration
   myList_myBehav_type <- str_split( myFilemyBehav_type_users, "/")
   configuration_myBehav_type <- unlist(myList_myBehav_type)[which(str_extract(unlist(myList_myBehav_type), paste("^",myBehav, sep="")) == myBehav)]
   configurationItems_myBehav_type <-
   as.numeric(unlist(str_split(configuration_myBehav_type, "-"))[-1])
   names(configurationItems_myBehav_type)<-list("Users","Memory","CPU","CartReplicas")
  #Compute threshold
   myNames <- colnames(faithful_myBehav_type_users)
   benchmark<-computeBenchmark(BenchFlow)[['baseline']]
   colnames(benchmark)<-myNames[-1]
   Config<-computeBenchmark(BenchFlow)[['Config']]
   BenchFlowAvg<- computeBenchmark(BenchFlow)[['BenchFlowAvg']]
   #retrieve experiments' average
   # average mean under no attack
   nr <-
    as.numeric(row.names(Config[Config$Users == configurationItems_myBehav_type[["Users"]] &
                          as.numeric(as.character(Config$Memory)) == configurationItems_myBehav_type[["Memory"]]
                                &
                              as.numeric(as.character(Config$CPU)) == configurationItems_myBehav_type[["CPU"]] &
                              Config$CartReplicas == configurationItems_myBehav_type[["CartReplicas"]], ]))
    predictedAvg_myBehav_type <- BenchFlowAvg[which(row.names(BenchFlowAvg) == nr), ]
    configurationItems_myBehav_type_collapsed<-paste(string=unlist(configurationItems_myBehav_type), sep="", collapse="-")
    myList<-list(faithful_myBehav_type_users,configurationItems_myBehav_type_collapsed,predictedAvg_myBehav_type, benchmark)
    names(myList)<-list("faithful","configItem","predictedAvg", "baseline")
  return(myList)
}  
# this is the code to make the polygon and the sensitivity analysis
source("C:\\jobs\\ESOlabs\\polygon_graph\\PolygonSource_working2.R") 


ui <- fluidPage(title = "PPTAM",
  tabsetPanel(              
    tabPanel(title = "Poly Graph",
      column(8,       
      plotOutput("poly"),
      sliderInput("x_range", "Range:", min = 0, max = max(aggregatedValuesWikipedia[, 1]), value = c(0, max(aggregatedValuesWikipedia[, 1])), step = 50)
      ),
      column(4,
      checkboxInput("show1",  "Mem=1.0  CPU=0.50  CartReplicas=2", value = FALSE),
      checkboxInput("show2",  "Mem=0.5  CPU=0.25  CartReplicas=4", value = FALSE),
      checkboxInput("show3",  "Mem=0.5  CPU=0.50  CartReplicas=2", value = FALSE),
      checkboxInput("show4",  "Mem=0.5  CPU=0.25  CartReplicas=2", value = FALSE),
      ), # column 4
      column(4,
      checkboxInput("show5",  "Mem=0.5  CPU=0.50  CartReplicas=1", value = FALSE),
      checkboxInput("show6",  "Mem=1.0  CPU=0.50  CartReplicas=4", value = FALSE),
      checkboxInput("show7",  "Mem=0.5  CPU=0.50  CartReplicas=4", value = FALSE),
      checkboxInput("show8",  "Mem=1.0  CPU=0.25  CartReplicas=2", value = FALSE),
      ), # column4
      column(4,
      checkboxInput("show9",  "Mem=0.5  CPU=0.25  CartReplicas=1", value = FALSE),
      checkboxInput("show10", "Mem=1.0  CPU=0.25  CartReplicas=4", value = FALSE),
      checkboxInput("show11", "Mem=1.0  CPU=0.50  CartReplicas=1", value = FALSE),
      checkboxInput("show12", "Mem=1.0  CPU=0.25  CartReplicas=1", value = FALSE),
      ) # column 4
      ),
      tabPanel(title = "Raw Microservice Data",
      fluidRow(

                   column(3,
                          fluidRow(
                           selectInput("service", "Choose one service:",
                                       list(`services` = myNames[-1])
                           )),
                          fluidRow(
                            selectInput("anotherService", "Choose another service:",
                                      list(`services` = myNames[-1])
                            )),
                          fluidRow(
                            selectInput("workload", "Choose workload:",
                                      list(`services` = c(50,100,150,200,250,300))
                          )
                        )
                         ),
                 column(4,
                        checkboxInput(
                          inputId = "switch1",
                          label = "First microservice",
                          value = FALSE
                        ),
                        checkboxInput(
                          inputId = "switch5",
                          label = "First microservice with attack",
                          value = FALSE
                        ),
                        checkboxInput(
                          inputId = "switch3",
                          label = "Second microservice",
                          value = FALSE
                        ),
                        checkboxInput(
                          inputId = "switch6",
                          label = "Second microservice with attack",
                          value = FALSE
                        )
                    )
                 ),
    
      fluidRow(
      column(10, 
             sliderInput(
               "bins",
               "Time interval:",
               min = 0,
               max = 373,
               #             max = nrow(faithful_best),
               value = c(0, 373),
               width = 1500
             ))
      ),
      fluidRow(
        width=26,
          column(8,conditionalPanel(condition = "input.switch0", plotlyOutput("polygons", width = "100%", height = "400px")))
                 ),
        fluidRow(
          column(6,
                 conditionalPanel(condition = "input.switch1", plotlyOutput("S1", width = "100%", height = "400px"), textOutput("selected_var1"))),
          column(6,
                 conditionalPanel(condition = "input.switch5", plotlyOutput("S5", width = "100%", height = "400px"), textOutput("selected_var5"))),
          column(6,
                 conditionalPanel(condition = "input.switch3", plotlyOutput("S3",width = "100%", height = "400px"), textOutput("selected_var3"))),
          column(6,
                 conditionalPanel(condition = "input.switch6", plotlyOutput("S6",width = "100%", height = "400px"), textOutput("selected_var6"))),
          column(6,
                 conditionalPanel(condition = "input.switch2", plotlyOutput("S2",width = "100%", height = "400px"), textOutput("selected_var2"))),
          column(6,
                 conditionalPanel(condition = "input.switch4", plotlyOutput("S4",width = "100%", height = "400px"), textOutput("selected_var4"))
                 )
          )
        ),
        
     
      
  
      tabPanel(title = "Grid Table",
        plotOutput("grid1"),
 
       ),
    tabPanel(title = "Sensitivity Analysis",
      plotOutput("sensitivity"),
      numericInput("r_choice", "Which Graph", 1,
                   1,6, 1) 
  
    )
     ),
  )


server <- function(input, output) {
  
 
  output$grid1 <- renderPlot({ 
  #######################################################
  # print the scores to the domainMetric to a grid table
  #  domainMetric
  p<-tableGrob(round(domainMetric,3), rows=NULL)
  p
    grid.arrange(p)  # prints nice formatted table
  })
  
  output$poly <- renderPlot({
    
  ######################################################################
  # plot aggregated line in black and the "large" polygon  
    plot(aggregatedValuesWikipedia,  xlim = c(input$x_range[1], input$x_range[2]), ylim=c(0, max(aggregatedValuesWikipedia[,2])*(1+5/100)),cex.lab=1.3)
    polygon(c(min(aggregatedValuesWikipedia[, 1]),aggregatedValuesWikipedia[,1],max(aggregatedValuesWikipedia[, 1])),c(0,aggregatedValuesWikipedia[,2],0), col=adjustcolor("darkblue",alpha.f = 0.2), lty = 1, lwd = 3, border = "darkblue")
    
    color=heat.colors(nrow(unique(usedSettings[,3:5])))
    color_transparent <- adjustcolor(color, alpha.f = 0.2) 

##########################################################################################################
# this plots the 12 lines within the polygon
#    for(i in c( input$mem ) [-c(bestApproachingLine, bestDMLine)] ){
#      lines(domainMetricList[[1]], type="l",lwd = 2)
#      polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[1]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[i]][2]),0), col=NA, lty = 1, lwd = 2,  border =heat.colors(20)[i])
#    }
    if (input$show1 == TRUE) {  lines(domainMetricList[[1]], type="l",lwd = 2, col=3) 
      if (bestDMLine == 1)  polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
      if (bestApproachingLine == 1) {
        lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
        polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red") }
      
      
      }
    if (input$show2 == TRUE) {  lines(domainMetricList[[2]], type="l",lwd = 2, col=4) 
       if (bestDMLine == 2)  polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
       if (bestApproachingLine == 2) {
         lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
         polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red") }
      }
    if (input$show3 == TRUE) {  lines(domainMetricList[[3]], type="l",lwd = 2, col=5)  
       if (bestDMLine == 3)  polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
       if (bestApproachingLine == 3) {
        lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
        polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red") }
      }
    if (input$show4 == TRUE) {  lines(domainMetricList[[4]], type="l",lwd = 2)
        if (bestDMLine == 4)  polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
        if (bestApproachingLine == 4) {
          lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
          polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red") }
          } 
    if (input$show5 == TRUE) {  lines(domainMetricList[[5]], type="l",lwd = 2, col=6)
        if (bestDMLine == 5)  polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
        if (bestApproachingLine == 5) {
          lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
          polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red") }
      }
    if (input$show6 == TRUE) {  lines(domainMetricList[[6]], type="l",lwd = 2, col=7)
        if (bestDMLine == 6)  polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
        if (bestApproachingLine == 6) {
          lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
          polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red") }
      }
    if (input$show7 == TRUE) {  lines(domainMetricList[[7]], type="l",lwd = 2, col=8)
        if (bestDMLine == 7)  polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
        if (bestApproachingLine == 7) {
          lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
          polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red") }
       }

    if (input$show8 == TRUE) {  lines(domainMetricList[[8]], type="l",lwd = 2, col=10)
        if (bestDMLine == 8)  polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
        if (bestApproachingLine == 8) {
          lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
          polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red") }
         }
    
    if (input$show9 == TRUE) {  lines(domainMetricList[[9]], type="l",lwd = 2, col=11)
        if (bestDMLine == 9)  polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
        if (bestApproachingLine == 9) {
          lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
          polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red") }
         }
    if (input$show10 == TRUE) {  lines(domainMetricList[[10]], type="l",lwd = 2, col=12)
        if (bestDMLine == 10)  polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
        if (bestApproachingLine == 10) {
          lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
          polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red") }
         }
      if (input$show11 == TRUE) {  lines(domainMetricList[[11]], type="l",lwd = 2, col=13)
        if (bestDMLine == 11)  polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
        if (bestApproachingLine == 11) {
          lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
          polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red") }
      }
    if (input$show12 == TRUE) {  lines(domainMetricList[[12]], type="l",lwd = 2, col=14)
        if (bestDMLine == 12)  polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
        if (bestApproachingLine == 12) {
          lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
          polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red") }
        }
  
    # plots labels
    text(aggregatedValuesWikipedia,labels = round(aggregatedValuesWikipedia[1:nrow(aggregatedValuesWikipedia),2],3), pos=3, col="black")
})

output$sensitivity <- renderPlot({
    #run without function
     r = input$r_choice
    # set up boundaries 
    xbottom = 0
    xtop = 50  
    ybottom = 0
    # if first row has a 0 users then get rid of it for the analysis
    if (aggregatedValues[1,1] == 0) { aggregatedValues <- aggregatedValues[-1,]}
  
    #computeSensitivityPerConfiguration <-function(k,domainMatricVariant,scale,xbottom,xtop,ybottom) {
    originalValues <- scale * originalThreshold
    temp <- sum(originalThreshold[-c(1:5)])
    myTicks <- scale * temp

  if (aggregatedValues[r,2]>0){
     mass <- c()
     for (m in c(1:148)) {
       myList <- domainMatricVariant[[m]]
       mass[m] <- myList[[k]][r, 2]
     }
     par(mfrow=c(1,1))
     plot(scale, mass, frame.plot = T, ylab = "", xlab = "", xaxt = "n", yaxt = "n", pch = "*", cex = 0.5, 
      xlim = c(0, 50), ylim = c(0,max(mass)+max(mass)* .05) 
    )
    lines( scale,mass,type = "o", col = "black", pch = "*", cex = 1 )
    abline( h = aggregatedValues[r, 2], col = "blue", lty = 3, cex = 0.1 )
    abline( v = 1, col = "red", lty = 3, cex = 0.1)
    
    myDiff <- c()
    tempDiff <- -mass[148] + aggregatedValues[r, 2]
    if (tempDiff < 0.0001)  tempDiff <- 0
    myDiff <- max( round(100* (tempDiff) / aggregatedValues[r, 2]), 0)
    #                  print(paste(mass[148], " ", aggregatedValues[r, 2], sep = ""))
    bestK <- c()
    bestK <- scale[which(mass == max(mass))[1]]

#    print(bestK)
    legend( "bottomright", legend = paste("Load: ",  aggregatedValues[r,1], ", Final Gap: ", myDiff , ", Scale:", bestK, sep ="") )
    ticklabels <-
      round(myTicks[c(
        which(scale == 1),
        which(scale == 10),
        which(scale == 20),
        which(scale == 30),
        which(scale == 40),
        which(scale == 50)
      )], 2)
    axis(1,at = c(1, 10, 20, 30, 40, 50),labels = ticklabels,line = 1.5,tck = 0.01, padj = -3.8)
    axis(1,at = c(1, 10, 20, 30, 40, 50),labels = c(1, 10, 20, 30, 40, 50), line = 1.5, padj = -1, tck = -0.01 )
    axis(2, tck = -0.01, padj = 1)
    title(xlab = "Scale", mgp = c(1.8, 1, 0))
    title(ylab = "Domain metric", mgp = c(1.5, 1, 0))
  }
  

})

dataInput<-reactive({input$workload})

#First microservice with no attack
output$S1 <- renderPlotly({
  # generate bins based on input$bins from ui.R
  # Best case first microservice
  bins <-
    seq(input$bins[1],
        input$bins[2],
        length.out = (input$bins[2] - input$bins[1]) + 1)
  bins
  faithfulSet_NoAttack_Best<-datasetCreation(dataInput(),myFiles,BenchFlow,"/NoAttack", "Best")
  faithful_NoAttack_best<-faithfulSet_NoAttack_Best[["faithful"]]
  predictedAvg__NoAttack_best<-faithfulSet_NoAttack_Best[["predictedAvg"]]
  benchmark<-faithfulSet_NoAttack_Best[["baseline"]]
  configuration_best_noattack<- faithfulSet_NoAttack_Best[["configItem"]]
  
  plot_ly(
    faithful_NoAttack_best,
    x=~Time[input$bins[1]:input$bins[2]],
    mode = "lines+markers"
  )%>%
    layout(dragmode = "select",
           xaxis=list(range=c(min(faithful_NoAttack_best$Time[input$bins[1]:input$bins[2]]), max(faithful_NoAttack_best$Time[input$bins[1]:input$bins[2]]))),
           yaxis = list(range = c(0, max(max(
             faithful_NoAttack_best[[input$service]][input$bins[1]:input$bins[2]], na.rm = T)+0.001, as.numeric(as.character(benchmark[[input$service]])))))
    )%>%  
    add_trace(
      y =  ~ faithful_NoAttack_best[[input$service]][input$bins[1]:input$bins[2]],
      name = "dataset",
      mode = 'lines+markers',
      marker = list(size=4),
      color  = I('black'),
      size = I(0.50)) %>%
    add_trace(
      y =  ~ round(mean(faithful_NoAttack_best[[input$service]][input$bins[1]:input$bins[2]]),3),
      name = 'mean',
      mode = 'lines', 
      color  = I('green'), size = I(0.90)) %>%
    add_trace(
      y =  ~ as.numeric(as.character(predictedAvg__NoAttack_best[input$service][1, ])),
      name = 'experiments mean',
      mode = 'lines',
      color  = I('blue'), size = I(0.90)) %>%
    add_trace(
      y =  ~ as.numeric(as.character(benchmark[[input$service]])),
      name = 'baseline',
      mode = 'lines',
      color  = I('red'), size = I(0.90)) %>%
    layout(
      title = paste("NoAttack_", configuration_best_noattack, sep=""),
      yaxis = list (title = input$service),
      xaxis = list (title = "Time"),
      legend=list(x=100, y=1)
    )
})

output$selected_var1 <- renderText({
  faithfulSet_NoAttack_Best<-datasetCreation(dataInput(),myFiles,BenchFlow,"/NoAttack", "Best")
  faithful_NoAttack_best<-faithfulSet_NoAttack_Best[["faithful"]]
  predictedAvg_NoAttack_best<-faithfulSet_NoAttack_Best[["predictedAvg"]]
  benchmark<-faithfulSet_NoAttack_Best[["baseline"]]
  configuration_best_noattack<- faithfulSet_NoAttack_Best[["configItem"]]
  
  paste(
    input$service,
    ": mean (green) is ",
    round(mean(faithful_NoAttack_best[[input$service]][input$bins[1]:input$bins[2]]),3),
    ", experiments mean (blue) is ",
    predictedAvg_NoAttack_best[input$service][1, ],
    " benchmark mean + 3SD  (red) is ", benchmark[[input$service]],
    sep = ""
  )
})
#})
#Not visualised yet. It was used for worst case
output$S2 <- renderPlotly({
  # generate bins based on input$bins from ui.R
  # Worst case first microservice
  bins <-
    seq(input$bins[1],
        input$bins[2],
        length.out = (input$bins[2] - input$bins[1]) + 1)
  
  faithfulSet_NoAttack_worst<-datasetCreation(dataInput(),myFiles,BenchFlow,"/NoAttack", "Worst")
  faithful_NoAttack_worst<-faithfulSet_NoAttack_worst[["faithful"]]
  predictedAvg_NoAttack_worst<-faithfulSet_NoAttack_worst[["predictedAvg"]]
  benchmark<-faithfulSet_NoAttack_worst[["baseline"]]
  configuration_worst_noattack<- faithfulSet_NoAttack_worst[["configItem"]]
  
  plot_ly(
    faithful_NoAttack_worst,
    x=~Time[input$bins[1]:input$bins[2]],
    mode = "lines+markers")%>%
    layout(dragmode = "select",
           xaxis= list(range=c(min(faithful_NoAttack_worst$Time[input$bins[1]:input$bins[2]]), max(faithful_NoAttack_worst$Time[input$bins[1]:input$bins[2]]))),
           yaxis = list(range = c(0, max(max(
             faithful_NoAttack_worst[[input$service]][input$bins[1]:input$bins[2]], na.rm = T)+0.001, as.numeric(as.character(benchmark[[input$service]])))))
    )%>%
    add_trace(
      y =  ~ faithful_NoAttack_worst[[input$service]][input$bins[1]:input$bins[2]],
      name = "dataset",
      mode = 'lines+markers',
      marker = list(size=4),
      color  = I('black'),
      size = I(0.50)) %>%
    add_trace(
      y =  ~ round(mean(faithful_NoAttack_worst[[input$service]][input$bins[1]:input$bins[2]]),3),
      name = 'mean',
      mode = 'lines',
      color  = I('green'), size = I(0.90)) %>%
    add_trace(
      y =  ~ as.numeric(as.character(predictedAvg_NoAttack_worst[input$service][1, ])),
      name = 'experiments mean',
      mode = 'lines',
      color  = I('blue'), size = I(0.90)) %>%
    add_trace(
      y =  ~ as.numeric(as.character(benchmark[[input$service]])),
      name = 'baseline',
      mode = 'lines',
      color  = I('red'), size = I(0.90)) %>%
    layout(
      title = paste("Worst_","NoAttack_",configuration_worst_noattack, sep=""),
      yaxis = list (title = input$service),
      xaxis = list (title = "Time"),
      legend=list(x=100, y=1)
    )
})

output$selected_var2 <- renderText({
  faithfulSet_NoAttack_worst<-datasetCreation(dataInput(),myFiles,BenchFlow,"/NoAttack", "Worst")
  faithful_NoAttack_worst<-faithfulSet_NoAttack_worst[["faithful"]]
  predictedAvg_NoAttack_worst<-faithfulSet_NoAttack_worst[["predictedAvg"]]
  benchmark<-faithfulSet_NoAttack_worst[["baseline"]]
  configuration_best_noattack<- faithfulSet_NoAttack_worst[["configItem"]]
  paste(
    input$service,
    ": mean (green) is ",
    round(mean(faithful_NoAttack_worst[[input$service]][input$bins[1]:input$bins[2]]),3),
    ",  experiments mean (blue) is ",
    predictedAvg_NoAttack_worst[input$service][1, ],
    " benchmark mean + 3SD  (violet) is ", benchmark[[input$service]],
    sep = ""
  )
})

#Second microservice with no attack
output$S3 <- renderPlotly({
  # generate bins based on input$bins from ui.R
  # Best case second microservice
  bins <-
    seq(input$bins[1],
        input$bins[2],
        length.out = (input$bins[2] - input$bins[1]) + 1)
  faithfulSet_NoAttack_best<-datasetCreation(dataInput(),myFiles,BenchFlow,"/NoAttack", "Best")
  faithful_NoAttack_best<-faithfulSet_NoAttack_best[["faithful"]]
  predictedAvg_NoAttack_best<-faithfulSet_NoAttack_best[["predictedAvg"]]
  benchmark<-faithfulSet_NoAttack_best[["baseline"]]
  configuration_best_noattack<- faithfulSet_NoAttack_best[["configItem"]]
  
  plot_ly(
    faithful_NoAttack_best,
    x=~Time[input$bins[1]:input$bins[2]]    )%>%
    layout(dragmode = "select",
           xaxis=list(range=c(min(faithful_NoAttack_best$Time[input$bins[1]:input$bins[2]]), max(faithful_NoAttack_best$Time[input$bins[1]:input$bins[2]]))),
           yaxis = list(range = c(0, max(max(
             faithful_NoAttack_best[[input$anotherService]][input$bins[1]:input$bins[2]], na.rm = T)+0.001, as.numeric(as.character(benchmark[[input$anotherService]])))))
    )%>%
    add_trace(
      y =  ~ faithful_NoAttack_best[[input$anotherService]][input$bins[1]:input$bins[2]],
      name = "dataset",
      mode = 'lines+markers',
      marker = list(size=4),
      color  = I('black'),
      size = I(0.50)) %>%
    add_trace(
      y =  ~ round(mean(faithful_NoAttack_best[[input$anotherService]][input$bins[1]:input$bins[2]]),3),
      name = 'mean',
      mode = 'lines',
      color  = I('green'), size = I(0.90)) %>%
    add_trace(
      y =  ~ as.numeric(as.character(predictedAvg_NoAttack_best[input$anotherService][1, ])),
      name = 'experiments mean',
      mode = 'lines',
      color  = I('blue'), size = I(0.90)) %>%
    add_trace(
      y =  ~ as.numeric(as.character(benchmark[[input$anotherService]])),
      name = 'baseline',
      mode = 'lines',
      color  = I('red'), size = I(0.90)) %>%
    layout(
      title = paste("NoAttack_",configuration_best_noattack, sep=""),
      yaxis = list (title = input$anotherService),
      xaxis = list (title = "Time"),
      legend=list(x=100, y=1)
    )
})

output$selected_var3 <- renderText({
  faithfulSet_NoAttack_best<-datasetCreation(dataInput(),myFiles,BenchFlow,"/NoAttack", "Best")
  faithful_NoAttack_best<-faithfulSet_NoAttack_best[["faithful"]]
  predictedAvg_NoAttack_best<-faithfulSet_NoAttack_best[["predictedAvg"]]
  benchmark<-faithfulSet_NoAttack_best[["baseline"]]
  configuration_best_noattack<- faithfulSet_NoAttack_best[["configItem"]]
  paste(
    input$anotherService,
    ": mean (green) is ",
    round(mean(as.numeric(as.character(faithful_NoAttack_best[[input$anotherService]][input$bins[1]:input$bins[2]]))),3),
    ",  experiments mean (blue) is ",
    predictedAvg_NoAttack_best[input$anotherService][1, ],
    " benchmark mean + 3SD  (red) is ", benchmark[[input$anotherService]],
    sep = ""
  )
})

#Not visualised yet. It was used for worst case
output$S4 <- renderPlotly({
  # generate bins based on input$bins from ui.R
  # Worst case second microservice
  bins <-
    seq(input$bins[1],
        input$bins[2],
        length.out = (input$bins[2] - input$bins[1]) + 1)
  faithfulSet_NoAttack_worst<-datasetCreation(dataInput(),myFiles,BenchFlow,"/NoAttack", "Worst")
  faithful_NoAttack_worst<-faithfulSet_NoAttack_worst[["faithful"]]
  predictedAvg_NoAttack_worst<-faithfulSet_NoAttack_worst[["predictedAvg"]]
  benchmark<-faithfulSet_NoAttack_worst[["baseline"]]
  configuration_worst_noattack<- faithfulSet_NoAttack_worst[["configItem"]]
  
  plot_ly(
    faithful_NoAttack_worst,
    x=~Time[input$bins[1]:input$bins[2]]
    #xlim = c(min(faithful_Attack_worst$Time[input$bins[1]:input$bins[2]]), max(faithful_Attack_worst$Time[input$bins[1]:input$bins[2]])),
    #ylim = c(0, max(max(
    # faithful_Attack_worst[[input$anotherService]][input$bins[1]:input$bins[2]], na.rm = T
    #)+0.001, as.numeric(as.character(benchmark[[input$anotherService]]))))
  )%>%
    layout(dragmode = "select",
           xaxis=list(title = "Time",range=c(min(faithful_NoAttack_worst$Time[input$bins[1]:input$bins[2]]), max(faithful_NoAttack_worst$Time[input$bins[1]:input$bins[2]]))),
           yaxis = list(title = input$anotherService, range = c(0, max(max(
             faithful_NoAttack_worst[[input$service]][input$bins[1]:input$bins[2]], na.rm = T)+0.001, as.numeric(as.character(benchmark[[input$service]]))))),
           title = paste("Worst_","NoAttack_",configuration_worst_noattack, sep="")
    )%>%
    add_trace(
      y =  ~ faithful_NoAttack_worst[[input$anotherService]][input$bins[1]:input$bins[2]],
      name = "dataset",
      mode = 'lines+markers',
      marker = list(size=4),
      color  = I('black'),
      size = I(0.50)) %>%
    add_trace(
      y =  ~ round(mean(faithful_NoAttack_worst[[input$anotherService]][input$bins[1]:input$bins[2]]),3),
      name = 'mean',
      mode = 'lines',
      color  = I('green'), size = I(0.90)) %>%
    add_trace(
      y =  ~ as.numeric(as.character(predictedAvg_NoAttack_worst[input$anotherService][1, ])),
      name = 'experiments mean',
      mode = 'lines',
      color  = I('blue'), size = I(0.90)) %>%
    add_trace(
      y =  ~ as.numeric(as.character(benchmark[[input$anotherService]])),
      name = 'baseline',
      mode = 'lines',
      color  = I('red'), size = I(0.90)) 
})

output$selected_var4 <- renderText({
  faithfulSet_NoAttack_worst<-datasetCreation(dataInput(),myFiles,BenchFlow,"/NoAttack", "Worst")
  faithful_NoAttack_worst<-faithfulSet_NoAttack_worst[["faithful"]]
  predictedAvg_NoAttack_worst<-faithfulSet_NoAttack_worst[["predictedAvg"]]
  benchmark<-faithfulSet_NoAttack_worst[["baseline"]]
  configuration_worst_noattack<- faithfulSet_NoAttack_worst[["configItem"]]
  paste(
    input$anotherService,
    ": mean (green) is ",
    round(mean(as.numeric(as.character(faithful_NoAttack_worst[[input$anotherService]][input$bins[1]:input$bins[2]]))),3),
    ",  experiments mean (blue) is ",
    predictedAvg_NoAttack_worst[input$anotherService][1, ],
    #", mean plus 3 SD (red) is ",
    #myCeiling,
    " benchmark mean + 3SD (violet) is ", benchmark[[input$anotherService]],
    sep = ""
  )
})

#First microservice with attack
output$S5 <- renderPlotly({
  # generate bins based on input$bins from ui.R
  # Best case first microservice with attack
  bins <-
    seq(input$bins[1],
        input$bins[2],
        length.out = (input$bins[2] - input$bins[1]) + 1)
  faithfulSet_Attack_best<-datasetCreation(dataInput(),myFiles,BenchFlow,"/Attack", "Best")
  faithful_Attack_best<-faithfulSet_Attack_best[["faithful"]]
  predictedAvg_Attack_best<-faithfulSet_Attack_best[["predictedAvg"]]
  benchmark<-faithfulSet_Attack_best[["baseline"]]
  configuration_best_attack<- faithfulSet_Attack_best[["configItem"]]
  plot_ly(
    faithful_Attack_best,
    x=~Time[input$bins[1]:input$bins[2]]    )%>%
    layout(dragmode = "select",
           xaxis=list(range=c(min(faithful_Attack_best$Time[input$bins[1]:input$bins[2]]), max(faithful_Attack_best$Time[input$bins[1]:input$bins[2]]))),
           yaxis = list(range = c(0, max(max(
             faithful_Attack_best[[input$service]][input$bins[1]:input$bins[2]], na.rm = T)+0.001, as.numeric(as.character(benchmark[[input$service]])))))
    )%>%
    add_trace(
      y =  ~ faithful_Attack_best[[input$service]][input$bins[1]:input$bins[2]],
      name = "dataset",
      mode = 'lines+markers',
      marker = list(size=4),
      color  = I('black'),
      size = I(0.50)) %>%
    add_trace(
      y =  ~ round(mean(faithful_Attack_best[[input$service]][input$bins[1]:input$bins[2]]),3),
      name = 'mean',
      mode = 'lines',
      color  = I('green'), size = I(0.90)) %>%
    add_trace(
      y =  ~ as.numeric(as.character(predictedAvg_Attack_best[input$service][1, ])),
      name = 'experiments mean',
      mode = 'lines',
      color  = I('blue'), size = I(0.90)) %>%
    add_trace(
      y =  ~ as.numeric(as.character(benchmark[[input$service]])),
      name = 'baseline',
      mode = 'lines',
      color  = I('red'), size = I(0.90)) %>%
    layout(
      title = paste("Attack_",configuration_best_attack, sep=""),
      yaxis = list (title = input$service),
      xaxis = list (title = "Time"),
      legend=list(x=100, y=1)
    )
})

output$selected_var5 <- renderText({
  faithfulSet_Attack_best<-datasetCreation(dataInput(),myFiles,BenchFlow,"/Attack", "Best")
  faithful_Attack_best<-faithfulSet_Attack_best[["faithful"]]
  predictedAvg_Attack_best<-faithfulSet_Attack_best[["predictedAvg"]]
  benchmark<-faithfulSet_Attack_best[["baseline"]]
  configuration_best_attack<- faithfulSet_Attack_best[["configItem"]]
  paste(
    input$service,
    ": mean (green) is ",
    round(mean(as.numeric(as.character(faithful_Attack_best[[input$service]][input$bins[1]:input$bins[2]]))),3),
    ",  experiments mean (blue) is ",
    predictedAvg_Attack_best[input$service][1, ],
    " benchmark mean + 3SD (red) is ", benchmark[[input$service]],
    sep = ""
  )
  
})

#Second microservice with attack
output$S6 <- renderPlotly({
  # generate bins based on input$bins from ui.R
  # Best case second microservice with attack
  bins <-
    seq(input$bins[1],
        input$bins[2],
        length.out = (input$bins[2] - input$bins[1]) + 1)
  faithfulSet_Attack_best<-datasetCreation(dataInput(),myFiles,BenchFlow,"/Attack", "Best")
  faithful_Attack_best<-faithfulSet_Attack_best[["faithful"]]
  predictedAvg_Attack_best<-faithfulSet_Attack_best[["predictedAvg"]]
  benchmark<-faithfulSet_Attack_best[["baseline"]]
  configuration_best_attack<- faithfulSet_Attack_best[["configItem"]]
  
  plot_ly(
    faithful_Attack_best,
    x=~Time[input$bins[1]:input$bins[2]])%>%
    layout(dragmode = "select",
           xaxis=list(range=c(min(faithful_Attack_best$Time[input$bins[1]:input$bins[2]]), max(faithful_Attack_best$Time[input$bins[1]:input$bins[2]]))),
           yaxis = list(range = c(0, max(max(
             faithful_Attack_best[[input$anotherService]][input$bins[1]:input$bins[2]], na.rm = T)+0.001, as.numeric(as.character(benchmark[[input$anotherService]])))))
    )%>%
    add_trace(
      y =  ~ faithful_Attack_best[[input$anotherService]][input$bins[1]:input$bins[2]],
      name = "dataset",
      mode = 'lines+markers',
      marker = list(size=4),
      color  = I('black'),
      size = I(0.50)) %>%
    add_trace(
      y =  ~ round(mean(faithful_Attack_best[[input$anotherService]][input$bins[1]:input$bins[2]]),3),
      name = 'mean',
      mode = 'lines',
      color  = I('green'), size = I(0.90)) %>%
    add_trace(
      y =  ~ as.numeric(as.character(predictedAvg_Attack_best[input$anotherService][1, ])),
      name = 'experiments mean',
      mode = 'lines',
      color  = I('blue'), size = I(0.90)) %>%
    add_trace(
      y =  ~ as.numeric(as.character(benchmark[[input$anotherService]])),
      name = 'baseline',
      mode = 'lines',
      color  = I('red'), size = I(0.90)) %>%
    layout(
      title = paste("Attack_",configuration_best_attack, sep=""),
      yaxis = list (title = input$anotherService),
      xaxis = list (title = "Time"),
      legend=list(x=100, y=1)
    )
})

output$selected_var6 <- renderText({
  faithfulSet_Attack_best<-datasetCreation(dataInput(),myFiles,BenchFlow,"/Attack", "Best")
  faithful_Attack_best<-faithfulSet_Attack_best[["faithful"]]
  predictedAvg_Attack_best<-faithfulSet_Attack_best[["predictedAvg"]]
  benchmark<-faithfulSet_Attack_best[["baseline"]]
  configuration_best_attack<- faithfulSet_Attack_best[["configItem"]]
  paste(
    input$anotherService,
    ": mean (green) is ",
    round(mean(as.numeric(as.character(faithful_Attack_best[[input$anotherService]][input$bins[1]:input$bins[2]]))),3),
    ",  experiments mean (blue) is ",
    predictedAvg_Attack_best[input$anotherService][1, ],
    " benchmark mean + 3SD (red) is ", benchmark[[input$anotherService]],
    sep = ""
  )
})




}

shinyApp(server = server, ui = ui)

