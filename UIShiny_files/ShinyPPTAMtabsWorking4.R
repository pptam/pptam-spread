# ShinyPPTAMtabs2.R

library(shiny)
library(ggplot2)
library(gridExtra)
library(gridBase)
library(grid)
rm(list=ls())
setwd("C:\\jobs\\ESOlabs\\polygon_graph") # set working directory

##############################################
# INPUT FILES wikipedia-profile-XX.csv and benchflow_output.csv
#Operational profile of Wikipedia
# this is from file wikipedia-profile-de.csv
profile.project <- "de"
profile.filename <- paste("wikipedia-profile-", profile.project, ".csv", sep = "")
project.count.data <- read.csv2(profile.filename)

myFileName = "benchflow_output.csv"
dataFile  <- read.csv(myFileName,header=TRUE)

w=2  # some kind of control variable
myMax<-300   # maximum number of users

oldw <- getOption("warn")  # turn off warnings
options(warn = -1)

source('PolygonSource_working.R') 


ui <- fluidPage(title = "Random generator",
  tabsetPanel(              
    tabPanel(title = "Poly Graph",
      plotOutput("poly"),
      sliderInput("x_range", "Range:", min = 0, max = max(aggregatedValuesWikipedia[, 1]), value = c(0, max(aggregatedValuesWikipedia[, 1])), step = 50),
      column(4,
      checkboxInput("show1",  "Mem=1.0  CPU=0.50  CartReplicas=2", value = FALSE),
      checkboxInput("show2",  "Mem=0.5  CPU=0.25  CartReplicas=4", value = FALSE),
      checkboxInput("show3",  "Mem=0.5  CPU=0.50  CartReplicas=2", value = FALSE),
      checkboxInput("show4",  "Mem=0.5  CPU=0.25  CartReplicas=2", value = FALSE),
      ),
      column(4,
      checkboxInput("show5",  "Mem=0.5  CPU=0.50  CartReplicas=1", value = FALSE),
      checkboxInput("show6",  "Mem=1.0  CPU=0.50  CartReplicas=4", value = FALSE),
      checkboxInput("show7",  "Mem=0.5  CPU=0.50  CartReplicas=4", value = FALSE),
      checkboxInput("show8",  "Mem=1.0  CPU=0.25  CartReplicas=2", value = FALSE),
      ),
      column(4,
      checkboxInput("show9",  "Mem=0.5  CPU=0.25  CartReplicas=1", value = FALSE),
      checkboxInput("show10", "Mem=1.0  CPU=0.25  CartReplicas=4", value = FALSE),
      checkboxInput("show11", "Mem=1.0  CPU=0.50  CartReplicas=1", value = FALSE),
      checkboxInput("show12", "Mem=1.0  CPU=0.25  CartReplicas=1", value = FALSE),

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
  )
)

server <- function(input, output) {
  
  
output$grid1 <- renderPlot({ 
#    p<-tableGrob(round(domainMetric[order(-domainMetric[,4]),],3), rows=NULL)
  domainMetric
  
  p<-tableGrob(round(domainMetric,3), rows=NULL)
  p
    grid.arrange(p)  # prints nice formatted table
})
  
output$poly <- renderPlot({
    
    plot(aggregatedValuesWikipedia,  xlim = c(input$x_range[1], input$x_range[2]), ylim=c(0, max(aggregatedValuesWikipedia[,2])*(1+5/100)),cex.lab=1.3)
    
    polygon(c(min(aggregatedValuesWikipedia[, 1]),aggregatedValuesWikipedia[,1],max(aggregatedValuesWikipedia[, 1])),c(0,aggregatedValuesWikipedia[,2],0), col=adjustcolor("darkblue",alpha.f = 0.2), lty = 1, lwd = 3, border = "darkblue")
    
    color=heat.colors(nrow(unique(usedSettings[,3:5])))
    color_transparent <- adjustcolor(color, alpha.f = 0.2) 
    #[-c(bestApproachingLine, bestDMLine)]
    #for(i in c(1:nrow(unique(usedSettings[,3:5])))){
    #  print(i)
    #  }
 
    
    # as.character(domainMetric[1,4])
    # this plots the 12 lines within the polygon
#    for(i in c( input$mem ) [-c(bestApproachingLine, bestDMLine)] ){
#      lines(domainMetricList[[1]], type="l",lwd = 2)
#      polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[1]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[i]][2]),0), col=NA, lty = 1, lwd = 2,  border =heat.colors(20)[i])
#    }
    if (input$show1 == TRUE) {  lines(domainMetricList[[1]], type="l",lwd = 2, col=2)  }
    if (input$show2 == TRUE) {  lines(domainMetricList[[2]], type="l",lwd = 2, col=3)  }
    if (input$show3 == TRUE) {  lines(domainMetricList[[3]], type="l",lwd = 2, col=4)   }
    if (input$show4 == TRUE) {  lines(domainMetricList[[4]], type="l",lwd = 2) 
                lines(domainMetricList[[bestApproachingLine]], type="l",lwd = 2)
           polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestDMLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestDMLine]][2]),0), col=adjustcolor("blue", alpha.f = 0.2), lty = 1, lwd = 2,  border ="blue")
      
            polygon(c(min(aggregatedValuesWikipedia[, 1]),t(domainMetricList[[bestApproachingLine]][1]),max(aggregatedValuesWikipedia[, 1])),c(0,t(domainMetricList[[bestApproachingLine]][2]),0), col=adjustcolor("red", alpha.f = 0.2) , lty = 1, lwd = 2,  border ="red")
      }
    if (input$show5 == TRUE) {  lines(domainMetricList[[5]], type="l",lwd = 2, col=6)
    #   text(250, .17, paste("Strength=" , as.character(domainMetric[5,4])) )
      
      }
    if (input$show6 == TRUE) {  lines(domainMetricList[[6]], type="l",lwd = 2, col=7)}
    if (input$show7 == TRUE) {  lines(domainMetricList[[7]], type="l",lwd = 2, col=8)}
    if (input$show8 == TRUE) {  lines(domainMetricList[[8]], type="l",lwd = 2, col=9)}
    if (input$show9 == TRUE) {  lines(domainMetricList[[9]], type="l",lwd = 2, col=10)}
    if (input$show10 == TRUE) {  lines(domainMetricList[[10]], type="l",lwd = 2, col=11)}
    if (input$show11 == TRUE) {  lines(domainMetricList[[11]], type="l",lwd = 2, col=12)}
    if (input$show12 == TRUE) {  lines(domainMetricList[[12]], type="l",lwd = 2, col=13)}
  
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
    #    t <- par(mfrow = c(4, 2), mar = c(2, 2.5, 2, 1),oma = c(3, 2, 3, 0.5))
    #    par(mar=c(5,4,4,2)+0.1)
    #   for (r in c(1:nrow(domainMatricVariant[[1]][[1]]))) {
  
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


  }

shinyApp(server = server, ui = ui)
