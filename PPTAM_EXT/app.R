#Time series analysis
library(shiny)
library(stringr)
library(plotly)
packageVersion('plotly')
library(here)
rm(list=ls())
# ANALYSED DATASETS according to paper in ECSEA

# Best configuration:
# without attack 
# Configuration: memoria 0.5 CPU 0.25 Replica 2 
# with attack 
# Configuration: memoria 0.5 CPU 0.25 Replica 2 

# Worst configuration:
# without attack 
# Configuration: memoria 1 CPU 0.25 Replica 1 
# with attack 
# Configuration: memoria 1 CPU 0.5 Replica 4 

#comment this out before deploy
#setwd("/Users/barbaramini/Research/Dropbox/OverleafGit/Microservices/SingleExperimentAnalysis/PPTAM_EXT/")

#working directory
experimentsDirectory <-
  file.path(getwd(), fsep = .Platform$file.sep)
#experiments' files

myFiles <-
  list.files(
    experimentsDirectory,
    recursive = TRUE,
    full.names = F,
    pattern = "^ResponseTimesseconds.csv"
  )

# #Best configuration files
# currentFilesIndex <- which(str_extract(myFiles, "Best") == "Best")
# myFileBest <- myFiles[currentFilesIndex]
# 
# #Worst configuration files
# currentFilesIndex <- which(str_extract(myFiles, "Worst") == "Worst")
# myFileWorst <- myFiles[currentFilesIndex]

#Average data Benchflow
myFileBenchFlow <-
  list.files(experimentsDirectory,
             full.names = F,
             recursive = TRUE,
             pattern = "^benchflow")
tryCatch({
BenchFlowTemp <-
  as.data.frame(read.delim(
    file.path(experimentsDirectory, myFileBenchFlow),
    header = T,
    sep = ",", skip=1
  ))[-1, ]}, 
error=function(e){stop(safeError(e))
    })

BenchFlow <- BenchFlowTemp[-1, ]
myNames<-c("Time",colnames(BenchFlow[-c(1:6)]))

#test data
# workload<-'100'
# myType<-"/Attack"
# myFiles<-myFiles
# BenchFlow<-BenchFlow
# myBehav<-"Best"

#Uncomment this for testing
#Best configuration and attack
#faithfulSet_Attack_Best<-datasetCreation(workload,myFiles,BenchFlow,"/Attack", "Best")
#Best configuration and no attack
#faithfulSet_NoAttack_Best<-datasetCreation(workload,myFiles,BenchFlow,"/NoAttack", "Best")
#Worst configuration and no attack
#faithfulSet_NoAttack_Worst<-datasetCreation(workload,myFiles,BenchFlow,"/NoAttack", "Worst")
#  nrow(faithfulSet_Attack_Best[["faithful"]])
 
#Compute benchmark threshold for each service  
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
  predictedAvg_myBehav_type <-
    BenchFlowAvg[which(row.names(BenchFlowAvg) == nr), ]
  configurationItems_myBehav_type_collapsed<-paste(string=unlist(configurationItems_myBehav_type), sep="", collapse="-")
  myList<-list(faithful_myBehav_type_users,configurationItems_myBehav_type_collapsed,predictedAvg_myBehav_type, benchmark)
  names(myList)<-list("faithful","configItem","predictedAvg", "baseline")
  return(myList)
}  


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Comparing Response time for configurations with or without attack"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 12,
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
                                       #list(`services` = c(200))
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
                          #,
                          # checkboxInput(
                          #   inputId = "switch2",
                          #   label = "Worst case first microservice",
                          #   value = FALSE
                          # ),
                          # checkboxInput(
                          #   inputId = "switch4",
                          #   label = "Worst case second microservice",
                          #   value = FALSE
                          # )
                   ),
                   column(4,
                     checkboxInput(
                       inputId = "switch0",
                       label = "Select input files to draw polygons",
                       value = FALSE
                     ),
                     
                      fluidRow(
                            fileInput(
                              "aggregatedValuesVSA", h5(" browse for operational profile")
                            ), 
                            checkboxInput(
                              inputId = "switch7",
                              label = "or select the default",
                              value = FALSE
                            )),
                     fluidRow(
                            fileInput(
                              "bestDMLine", h5(" browse for best DM Curve")
                            ), 
                            checkboxInput(
                              inputId = "switch8",
                              label = "or select the default",
                              value = FALSE
                            )),
                     fluidRow(
                            fileInput(
                              "bestApproachingLine", h5(" browse for best approaching Curve")
                            ), 
                            checkboxInput(
                              inputId = "switch9",
                              label = "or select the default",
                              value = FALSE
                            ))
                   )),
    
   
    column(10, 
           sliderInput(
             "bins",
             "Time interval:",
             min = 0,
             max = 373,
#             max = nrow(faithful_best),
             value = c(0, 373),
             width = 1500
           )
    )),
    # Show a plot of the generated distribution
    mainPanel(
      width=26,
      fluidRow(
        column(8,conditionalPanel(condition = "input.switch0", plotlyOutput("polygons", width = "100%", height = "400px")))),
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
         conditionalPanel(condition = "input.switch4", plotlyOutput("S4",width = "100%", height = "400px"), textOutput("selected_var4"))))
    )
  ))

#graphics.off()
# Define server logic required to draw a plots
server <- function(input, output) {
  
  #Plot of polygons for the total performance measurement
  output$polygons <- renderPlotly({
    #use req()to make the file compulsory
    req(input$aggregatedValuesVSA)
    df <- read.csv(input$aggregatedValuesVSA$datapath,header = TRUE,sep = ",")
    req(input$bestDMLine)
    df2<-read.csv(input$bestDMLine$datapath,header = TRUE,sep = ",")
    req(input$bestApproachingLine)
    df3<-read.csv(input$bestApproachingLine$datapath,header = TRUE,sep = ",")
    plot_ly(
      df,
      x=~df[[2]], 
      y=~df[[3]],
      xlim = c(0,max(df[[2]])),
      ylim = c(0, max(df[[3]]) + 0.05),
      cex.lab = 1.3,
      mode = "lines+markers")%>%
      layout(dragmode = "select")%>%
      add_trace(
        y =  ~ df[[3]],
        name = 'operational profile',
        mode = 'lines+markers') %>%
      add_trace(
        y =  ~ df2[[3]],
        name = 'Best DM',
        mode = 'lines+markers') %>%
      add_trace(
        #y =  ~ as.data.frame(domainMetricList[[bestApproachingLine]])$`absoluteMass`,
        y =  ~ df3[[3]],
        name = 'Best appraoching line',
        mode = 'lines+markers') %>%
      layout(
        title = "Polygons",
        yaxis = list (title = "domain metric"),
        xaxis = list (title = "workload")
      )
  })
  #workload<-reactive(input$workload)
  dataInput<-reactive({input$workload})
  
  #First microservice with no attack
  output$S1 <- renderPlotly({
    # generate bins based on input$bins from ui.R
    # Best case first microservice
    bins <-
      seq(input$bins[1],
          input$bins[2],
          length.out = (input$bins[2] - input$bins[1]) + 1)
    
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

# Run the application
shinyApp(ui = ui, server = server)
