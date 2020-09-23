# ShinyPPTAMtabs2.R

library(shiny)
library(ggplot2)
library(gridExtra)
library(gridBase)
library(grid)
library(plotly)
rm(list=ls())
setwd("C:\\jobs\\ESOlabs\\polygon_graph") # set working directory

##############################################
# INPUT FILES wikipedia-profile-XX.csv and benchflow_output.csv
#Operational profile of Wikipedia
# this is from file wikipedia-profile-de.csv
project.count.data <- read.csv2("wikipedia-profile-de.csv")
#########################################################
myFileName = "benchflow_output.csv"
dataFile  <- read.csv(myFileName,header=TRUE)
#######################################################
tsdata <- read.csv("timeseries.csv",header=TRUE)
# compute means of the 12 configuations for plots
tsdata$mean1 <-  mean(tsdata$dataset1)
tsdata$mean2 <-  mean(tsdata$dataset2)
tsdata$mean3 <-  mean(tsdata$dataset3)
tsdata$mean4 <-  mean(tsdata$dataset4)
tsdata$mean5 <-  mean(tsdata$dataset5)
tsdata$mean6 <-  mean(tsdata$dataset6)
tsdata$mean7 <-  mean(tsdata$dataset7)
tsdata$mean8 <-  mean(tsdata$dataset8)
tsdata$mean9 <-  mean(tsdata$dataset9)
tsdata$mean10 <-  mean(tsdata$dataset10)
tsdata$mean11 <-  mean(tsdata$dataset11)
tsdata$mean12 <-  mean(tsdata$dataset12)


###################### temp ###########

w=2  # some kind of control variable
myMax<-300   # maximum number of users
num_config = 12

oldw <- getOption("warn")  # turn off warnings
options(warn = -1)

source('PolygonSource_working.R') 


ui <- fluidPage(title = "PPTAM",
  tabsetPanel(              
    tabPanel(title = "Poly Graph",
      plotOutput("poly"),
      sliderInput("x_range", "Range:", min = 0, max = max(aggregatedValuesWikipedia[, 1]), value = c(0, max(aggregatedValuesWikipedia[, 1])), step = 50),
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
      ), # column 4
      fluidRow(
        column(6,  
               selectInput("ts_choice", "Drill down to raw data", 
                           choices = c("Mem=1.0  CPU=0.50  CartReplicas=2"  = 1
                                       ,"Mem=0.5  CPU=0.25  CartReplicas=4" = 2
                                       ,"Mem=0.5  CPU=0.50  CartReplicas=2" = 3 
                                       ,"Mem=0.5  CPU=0.25  CartReplicas=2" = 4
                                       ,"Mem=0.5  CPU=0.50  CartReplicas=1" = 5
                                       ,"Mem=1.0  CPU=0.50  CartReplicas=4" = 6
                                       ,"Mem=0.5  CPU=0.50  CartReplicas=4" = 7
                                       ,"Mem=1.0  CPU=0.25  CartReplicas=2" = 8
                                       ,"Mem=0.5  CPU=0.25  CartReplicas=1" = 9
                                       ,"Mem=1.0  CPU=0.25  CartReplicas=4" = 10
                                       ,"Mem=1.0  CPU=0.50  CartReplicas=1" = 11
                                       ,"Mem=1.0  CPU=0.25  CartReplicas=1" = 12
                                       ))
               
              ),# column,
        column(4,
               checkboxInput("timeseries1", "Show graph", value = FALSE)     
               
           ), # column4
      
           ), # fluid row
      fluidRow(
           plotlyOutput("timeseriesplot"),
#            p("mean")
#           p(paste("mean(green)=",mean," experiments mean(red)=",experiments_mean," baseline(blue)=",baseline))
           
          ) # fluid row
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



output$timeseriesplot <- renderPlotly({
    if (input$ts_choice == 1) { dataset <- tsdata$dataset1 
      mean <- mean(tsdata$mean1) }   
    if (input$ts_choice == 2) { dataset <- tsdata$dataset2 
      mean <- mean(tsdata$mean2) }   
    if (input$ts_choice == 3) { dataset <- tsdata$dataset3 
      mean <- mean(tsdata$mean3) }   
    if (input$ts_choice == 4) { dataset <- tsdata$dataset4 
      mean <- mean(tsdata$mean4) }   
    if (input$ts_choice == 5) { dataset <- tsdata$dataset5 
      mean <- mean(tsdata$mean5) }   
    if (input$ts_choice == 6) { dataset <- tsdata$dataset6 
      mean <- mean(tsdata$mean6) }   
    if (input$ts_choice == 7) { dataset <- tsdata$dataset7 
      mean <- mean(tsdata$mean7) }   
    if (input$ts_choice == 8) { dataset <- tsdata$dataset8 
      mean <- mean(tsdata$mean8) }   
    if (input$ts_choice == 9) { dataset <- tsdata$dataset9 
      mean <- mean(tsdata$mean9) }   
    if (input$ts_choice == 10) { dataset <- tsdata$dataset10 
      mean <- mean(tsdata$mean10) }   
    if (input$ts_choice == 11) { dataset <- tsdata$dataset11 
      mean <- mean(tsdata$mean11) }   
    if (input$ts_choice == 12) { dataset <- tsdata$dataset12 
      mean <- mean(tsdata$mean12) }   
  
    
    baseline <- tsdata$baseline
    experiments_mean <- tsdata$experiments_mean
    
  
    #    mean <- tsdata$mean
  
    time <- tsdata$time
  
    data <- data.frame(time,dataset,baseline,experiments_mean,mean)
 
    if (input$timeseries1 == TRUE) {  
    
        fig <- plot_ly(data, x = ~time) 
        fig <- fig %>% add_trace(y = ~dataset, name = 'dataset',mode = 'lines+markers',line = list(color = 'rgba(67,67,67,1)', width = 2),marker = list(color = 'rgba(67,67,67,1)', size = 8)) 
        fig <- fig %>% add_trace(y = ~baseline, name = 'baseline',mode = 'lines',line = list(color = 'rgba(189,45,45, 1)')) 
        fig <- fig %>% add_trace(y = ~experiments_mean, name = 'experiments mean',mode = 'lines',line = list(color = 'rgba(49,49,189, 1)')) 
        fig <- fig %>% add_trace(y = ~mean, name = 'mean',mode = 'lines',line = list(color = 'rgba(49,189,49, 1)')) 
        fig
        
    }  

})


}

shinyApp(server = server, ui = ui)

