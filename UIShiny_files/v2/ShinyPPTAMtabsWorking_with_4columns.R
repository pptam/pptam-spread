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
setwd("C:\\jobs\\ESOlabs\\polygon_graph8_2020\\PPTAM_EXT") # set working directory

##############################################
# INPUT FILES wikipedia-profile-XX.csv and benchflow_output.csv
#Operational profile of Wikipedia
# this is from file wikipedia-profile-de.csv
project.count.data <- read.csv2("wikipedia-profile-de.csv")
#########################################################
#project.count.data
#comment this out before deploy

#working directory
experimentsDirectory <-file.path(getwd(), fsep = .Platform$file.sep)
#experiments' files
#######################################################################################
# this looks for ResponseTimesseconds.csv files.  You need a clean-up directory for this
# Don't fill it with .pdf files and the like.
#######################################################################################
myFiles <-
  list.files(
    experimentsDirectory,
    recursive = TRUE,
    full.names = F,
    pattern = "^ResponseTimesseconds.csv"
  )
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
#BenchFlowTemp <- read.csv("C:\\jobs\\ESOlabs\\polygon_graph8_2020\\PPTAM_EXT\\Data\\100Users\\OriginalData\\benchflow_output.csv")
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

myNames<-c("Time",colnames(dataFile[-c(1:NumOfLabels)]))
myNames
mySettings <-unique(dataFile[dataFile$Users > 2,3:NumColConf]) # take out baseline which is Users = 2
mySettings

w=2  # some kind of control variable
myMax<-300   # maximum number of users

# 1   2     3      4      5           6
#ID	Users	Memory	CPU	CartReplicas	Metric

oldw <- getOption("warn")  # turn off warnings
options(warn = -1)

###############################################################
#Function: Compute benchmark threshold for each service / this is for time series plots
###############################################################
computeBenchmark<-function(BenchFlow){
  BenchFlowAvg<-BenchFlow[BenchFlow$Metric=="Avg (sec)",]
  Config <- unique(BenchFlowAvg[c(2:NumColConf)])
  #ConfigThreshold<-Config[Config$Users==2,]
  avgThreshold<- BenchFlow[BenchFlow$Metric=="Avg (sec)" & BenchFlow$Users==2,][,-c(1:NumOfLabels)]
  SDThreshold <- BenchFlow[BenchFlow$Metric=="SD (sec)" & BenchFlow$Users==2,][,-c(1:NumOfLabels)]
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
   
   configurationItems_myBehav_type <- configurationItems_myBehav_type[(1:NumColConf-1)]
   names(configurationItems_myBehav_type)<-list("Users","Memory","CPU")
   
  #Compute threshold
   myNames <- colnames(faithful_myBehav_type_users)
   benchmark<-computeBenchmark(BenchFlow)[['baseline']]
   colnames(benchmark)<-myNames[-1]
   Config<-computeBenchmark(BenchFlow)[['Config']]
   BenchFlowAvg<- computeBenchmark(BenchFlow)[['BenchFlowAvg']]
   #retrieve experiments' average
   # average mean under no attack
   nr <-
     as.numeric(row.names(Config[Config$Users == configurationItems_myBehav_type[[1]] &
                                   as.numeric(as.character(Config$Memory)) == configurationItems_myBehav_type[[2]]
                                 &
                                   as.numeric(as.character(Config$CPU)) == configurationItems_myBehav_type[[3]] , ]))
    predictedAvg_myBehav_type <- BenchFlowAvg[which(row.names(BenchFlowAvg) == nr), ]
    configurationItems_myBehav_type_collapsed<-paste(string=unlist(configurationItems_myBehav_type), sep="", collapse="-")
    myList<-list(faithful_myBehav_type_users,configurationItems_myBehav_type_collapsed,predictedAvg_myBehav_type, benchmark)
    names(myList)<-list("faithful","configItem","predictedAvg", "baseline")
  return(myList)
}  
# this is the code to make the polygon and the sensitivity analysis
source("C:\\jobs\\ESOlabs\\polygon_graph\\PolygonSource_working_v2.R") 


ui <- fluidPage(title = "PPTAM",
  tabsetPanel(              
    tabPanel(title = "Poly Graph",
      fluidRow(
      column(8,       
      plotOutput("poly"),
      sliderInput("x_range", "Range:", min = 0, max = max(aggregatedValuesWikipedia[, 1]), value = c(0, max(aggregatedValuesWikipedia[, 1])), step = 50)
      ),
      column(4,
             sidebarPanel(
               pickerInput("show","Domain Metrics", choices , options = list(`actions-box` = TRUE),width='450px',multiple = T)
              ) # side panel
             )  # column 4
          ) # column 8
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
               dataTableOutput('grid1'),
 
       )
  # tabPanel(title = "Sensitivity Analysis",
  #    plotOutput("sensitivity"),
  #    numericInput("r_choice", "Which Graph", 1,
  #                 1,6, 1) 
  
#    )
     )
  )


server <- function(input, output) {
  
 
  output$grid1 <- renderDataTable( domainMetric)
  
  output$poly <- renderPlot({
  ##########StartTemp#######################################
#  input <- NULL  
#  input$range[1] <- 0
#  input$range[2] <- 300
  ###########EndTemp############################ 
 
    showlist = which(choices %in% input$show)  
  #  print(showlist)
  ######################################################################
  # plot aggregated line in black and the "large" polygon  
    plot(aggregatedValuesWikipedia,  xlim = c(input$x_range[1], input$x_range[2]), ylim=c(0, max(aggregatedValuesWikipedia[,2])*(1+5/100)),cex.lab=1.3)
    polygon(c(min(aggregatedValuesWikipedia[, 1]),aggregatedValuesWikipedia[,1],max(aggregatedValuesWikipedia[, 1])),c(0,aggregatedValuesWikipedia[,2],0), col=adjustcolor("darkblue",alpha.f = 0.2), lty = 1, lwd = 3, border = "darkblue")
    
    color=heat.colors(nrow(unique(usedSettings[,3:NumColConf])))
    color_transparent <- adjustcolor(color, alpha.f = 0.2) 

##########################################################################################################
# this plots the domain_n number of lines within the polygon
#    for(i in c( input$mem ) [-c(bestApproachingLine, bestDMLine)] ){
#      lines(domainMetricList[[1]], type="l",lwd = 2)
#      polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[1]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[i]][2]),0), col=NA, lty = 1, lwd = 2,  border =heat.colors(20)[i])
#    }
############################### 
  
showline <- function(dm_i) {    if (sum(showlist == dm_i) == 1) {  lines(domainMetricList[[dm_i]], type="l",lwd = 2, col=(dm_i + 2)) 
      if (bestDMLine == dm_i)  polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
      if (bestApproachingLine == dm_i) {
        lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
        polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red") }
        }
}

# show the different         
for (i in 1:domain_n) { showline(i)  }


 
########################  
    # plots labels
    text(aggregatedValuesWikipedia,labels = round(aggregatedValuesWikipedia[1:nrow(aggregatedValuesWikipedia),2],3), pos=3, col="black")
})

#output$sensitivity <- renderPlot({
#    #run without function
#     r = input$r_choice
#    # set up boundaries 
#    xbottom = 0
#    xtop = 50  
#    ybottom = 0
#    # if first row has a 0 users then get rid of it for the analysis
#    if (aggregatedValues[1,1] == 0) { aggregatedValues <- aggregatedValues[-1,]}
#    #computeSensitivityPerConfiguration <-function(k,domainMatricVariant,scale,xbottom,xtop,ybottom) {
#    originalValues <- scale * originalThreshold
#    temp <- sum(originalThreshold[-c(1:5)])
#    myTicks <- scale * temp

#  if (aggregatedValues[r,2]>0){
#     mass <- c()
#     for (m in c(1:148)) {
#       myList <- domainMatricVariant[[m]]
#       mass[m] <- myList[[k]][r, 2]
#     }
#     par(mfrow=c(1,1))
#     plot(scale, mass, frame.plot = T, ylab = "", xlab = "", xaxt = "n", yaxt = "n", pch = "*", cex = 0.5, 
#      xlim = c(0, 50), ylim = c(0,max(mass)+max(mass)* .05) 
#    )
#    lines( scale,mass,type = "o", col = "black", pch = "*", cex = 1 )
#    abline( h = aggregatedValues[r, 2], col = "blue", lty = 3, cex = 0.1 )
#    abline( v = 1, col = "red", lty = 3, cex = 0.1)
    
#    myDiff <- c()
#    tempDiff <- -mass[148] + aggregatedValues[r, 2]
#    if (tempDiff < 0.0001)  tempDiff <- 0
#    myDiff <- max( round(100* (tempDiff) / aggregatedValues[r, 2]), 0)
#    #                  print(paste(mass[148], " ", aggregatedValues[r, 2], sep = ""))
#    bestK <- c()
#    bestK <- scale[which(mass == max(mass))[1]]

##    print(bestK)
#    legend( "bottomright", legend = paste("Load: ",  aggregatedValues[r,1], ", Final Gap: ", myDiff , ", Scale:", bestK, sep ="") )
#    ticklabels <-
#      round(myTicks[c(
#        which(scale == 1),
#        which(scale == 10),
#        which(scale == 20),
#        which(scale == 30),
#        which(scale == 40),
#        which(scale == 50)
#      )], 2)
#    axis(1,at = c(1, 10, 20, 30, 40, 50),labels = ticklabels,line = 1.5,tck = 0.01, padj = -3.8)
#    axis(1,at = c(1, 10, 20, 30, 40, 50),labels = c(1, 10, 20, 30, 40, 50), line = 1.5, padj = -1, tck = -0.01 )
#    axis(2, tck = -0.01, padj = 1)
#    title(xlab = "Scale", mgp = c(1.8, 1, 0))
#    title(ylab = "Domain metric", mgp = c(1.5, 1, 0))
#  }
  

#})

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
#  faithfulSet_NoAttack_Best<-datasetCreation(workload,myFiles,BenchFlow,"/NoAttack", "Best")
  
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

