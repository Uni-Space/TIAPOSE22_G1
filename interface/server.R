library(shiny)
library(rjson)
library(lubridate)
library(ggplot2)
library(markdown)
library("readxl")
source("prediction_functions.R")
source("optimization_functions.R", encoding="UTF-8")


target <- c("All","Female","Male","Young","Adult")
modelType <- c("Multivariate", "Univariate", "Hybrid")
forecastModels <- c("Forecast - Holt-Winters" = "forecast - Holt-Winters", "Forecast - Arima" = "forecast - Arima", "Forecast - NN" = "forecast - NN", "Forecast - ETS" = "forecast - ETS")
optimizationModelsObj <- c("Blind Search - Monte Carlo" = "mc", "Local Search - Hill Climbing" = "hc", "Local Search - Simulated Annealing" = "sa", "Local Search - Tabu Search" = "tabu", "Population Search - Genetic Algorithms" = "rbga", "Population Search - Differential Evolution" = "deo", "Population Search - Particle Swarm" = "pco", "Multi-Objective - NSGA-II" = "pareto", "Hybrid Genetic Algorith (HGA)" = "hga")


models <- fromJSON(file="prediction_models/rminer_models.json")

rminerModels = c()

for(i in 1:length(models)){
  key <- paste("Rminer", models[[i]]$name, sep=" - ")
  value <- paste("rminer", models[[i]]$rminer, sep=" - ")
  
  rminerModels[ key ] <- value
}

univariateModels <- c(forecastModels, rminerModels)
multivariateModels <- rminerModels


# -------------------------- Interface -------------------------

about_page <- tabPanel(
  title = "About",
  titlePanel("About"),
  includeMarkdown("add-ons/about_page.md")
)

eda_page <- tabPanel(
  title = "EDA",
  titlePanel("EDA"),
  tags$head(tags$style(HTML("
                               .main-container {
                                  width: 100% !important;
                                  max-width: 100% !important;
                               }

                               "))),
  # Univariate
  selectInput("EDA", "EDA Tool", choices = c("Smart EDA", "Data Explorer", "Data Maid")),
  conditionalPanel(
    condition = "input.EDA == 'Smart EDA'",
    includeCSS("add-ons/EDA/SmartEDA.html"),
  ),
  conditionalPanel(
    condition = "input.EDA == 'Data Maid'",
    includeCSS("add-ons/EDA/dataMaid.html"),
  ),
  conditionalPanel(
    condition = "input.EDA == 'Data Explorer'",
    includeCSS("add-ons/EDA/DataExplorer.html"),
  ),
)

scenarios_page <- tabPanel(
  title = "Scenarios",
  titlePanel("Scenarios"),
  includeMarkdown("add-ons/scenarios_page.md")
)


models_page <- tabPanel(
  title = "Models",
  titlePanel("Models Used"),
  includeMarkdown("add-ons/models_page.md")
)

dataset_page <- tabPanel(
  title = "Our Dataset",
  titlePanel("Our Dataset"),
  h4("In this section is displayed the dataset used for training. Also, it is the dataset that can be used in this application and its called 'our dataset' in the predict section"),
  br(),
  h5("The dataset is composed by 257 samples and 11 features. "),
  br(),
  br(),
  br(),
  DT::dataTableOutput("ourDataset")
)


results_page <- tabPanel(
  title = "Results",
  titlePanel("Results"),
  
  fluidRow(
    column(2,selectInput("results_model_type", "Model Type", choices = modelType)),
    column(2,selectInput("results_target_variab", "Target Variable", choices = target)),
  ),
  
  DT::dataTableOutput("tableResults")
)



main_page <- tabPanel(
  title = "Analysis",
  titlePanel("Prediction"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          title = "Custom Models",
          
          radioButtons("selButt", label = h3("Dataset"),
                       choices = c("Our Dataset", "New Dataset"), 
                       selected = "Our Dataset"),
          
          # --------------------------------------------- Presente
          
          conditionalPanel(
            condition = "input.selButt == 'New Dataset'",
            fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
            selectInput("selModelType", "Model Type", choices = modelType),
            
            # Multivariate
            conditionalPanel(
              condition = "input.selModelType == 'Multivariate'",
              selectInput("selMultivariate", "Multivariate Model", choices=multivariateModels),
            ),
            
            # Univariate
            conditionalPanel(
              condition = "input.selModelType == 'Univariate'",
              selectInput("selUnivariate", "Ultivariate Model", choices=univariateModels),
            ),
            
            # Hybrid
            conditionalPanel(
              condition = "input.selModelType == 'Hybrid'",
              selectInput("selHybridUni", "Univariate Model", choices=univariateModels),
            ),
            conditionalPanel(
              condition = "input.selModelType == 'Hybrid'",
              selectInput("selHybridMulti", "Multivariate Model", choices=multivariateModels),
            ),
            br()
          ),
          
          #--------------------------------------------- Passado
          
          conditionalPanel(
            condition = "input.selButt == 'Our Dataset'",
            
            sliderInput("dateToPredict",
                        "Date to Predict:",
                        min = as.Date("2013-05-07","%Y-%m-%d"),
                        max = as.Date("2013-12-15","%Y-%m-%d"),
                        value=as.Date("2013-12-05"),
                        timeFormat="%Y-%m-%d"),
            
            br(),
            
            selectInput("selModelType2", "Model Type", choices = modelType),
            
            
            # Multivariate
            conditionalPanel(
              condition = "input.selModelType2 == 'Multivariate'",
              selectInput("selMultivariate2", "Multivariate Model", choices=multivariateModels),
            ),
            
            # Univariate
            conditionalPanel(
              condition = "input.selModelType2 == 'Univariate'",
              selectInput("selUnivariate2", "Ultivariate Model", choices=univariateModels),
            ),
            
            # Hybrid
            conditionalPanel(
              condition = "input.selModelType2 == 'Hybrid'",
              selectInput("selHybridUni2", "Univariate Model", choices=univariateModels),
            ),
            conditionalPanel(
              condition = "input.selModelType2 == 'Hybrid'",
              selectInput("selHybridMulti2", "Multivariate Model", choices=multivariateModels),
            ),
            br()
          ),
          actionButton("run_button", "Run Analysis", icon = icon("play")),
        ),
        tabPanel(
          title = "Best Models",
          
          radioButtons("selButtBest", label = h3("Dataset"),
                       choices = c("Our Dataset", "New Dataset"), 
                       selected = "Our Dataset"),
          
          # --------------------------------------------- Presente
          
          conditionalPanel(
            condition = "input.selButtBest == 'New Dataset'",
            fileInput("csv_inputBest", "Select CSV File to Import", accept = ".csv")
          ),
          
          conditionalPanel(
            condition = "input.selButtBest == 'Our Dataset'",
            sliderInput("dateToPredictBest",
                        "Date to Predict:",
                        min = as.Date("2013-05-07","%Y-%m-%d"),
                        max = as.Date("2013-12-15","%Y-%m-%d"),
                        value=as.Date("2013-12-05"),
                        timeFormat="%Y-%m-%d"),
          ),
          actionButton("run_button2", "Run Analysis", icon = icon("play")),
        ),
  ),
),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Predictions",
          tags$head(tags$style("#btnout{color: Black;
                                 font-size: 30px;
                                 }"
          )),
          
          
          conditionalPanel(
            condition = "input.selOptimizationModel == 'Best Algorithms'",
            h2("Best Models:"),
            textOutput("text_best_model_all"),
            textOutput("text_best_model_female"),
            textOutput("text_best_model_male"),
            textOutput("text_best_model_young"),
            textOutput("text_best_model_adult"),
          ),
          

          br(),
          DT::dataTableOutput("sample_table"),
          br(),
          selectInput("plotVariab", "Target Variable", choices=target),
          selectInput("numberDotsPlot", "Number of Days", choices=c(15,30,50,100)),
          br(),
          plotOutput("plot1", click = "plot_click"),
        ),
        
        
        tabPanel(
          title = "Campaigns",
          br(),
          # Optimization
          
          selectInput("selOptimizationModel", "Optimization Algorithm", choices=optimizationModelsObj),
          
          
          tags$head(tags$style("#textObj1, #textObj2, #textObj3 {color: Orange;
                                 font-size: 30px;
                                 }"
          )),
          
          tags$head(tags$style("#costObj1, #costObj2, #costObj3 {color: Red;
                                 font-size: 15px;
                                 }"
          )),
          
          tags$head(tags$style("#profitObj1, #profitObj2, #profitObj3 {color: Green;
                                 font-size: 15px;
                                 }"
          )),
          
          tags$head(tags$style("#titleOptimChart{color: Black;
                                 font-size: 20px;
                                 margin-left: 250px;
                                 font-weight: bold;
                                 }"
          )),
          
          textOutput("textObj1"),
          br(),
          DT::dataTableOutput("optim_obj1"),
          br(),
          
          
          fluidRow(
            column(3, 
                   textOutput("salesObj1"),
                   textOutput("costObj1"),
                   textOutput("profitObj1"),
            ),
            conditionalPanel(
              condition = "input.selOptimizationModel == 'pareto'",
              column(8,br(),
                     textOutput("titleOptimChart"),
                     br(),
                     plotOutput("paretoImage"),
                     br(),
              ),
            ),
            conditionalPanel(
              condition = "input.selOptimizationModel == 'hga'",
              column(6,plotOutput("HGAplot1")),
            ),
          ),
          

          br(),
          
          conditionalPanel(
            condition = "input.selOptimizationModel != 'pareto'",
            textOutput("textObj2"),
            br(),
            DT::dataTableOutput("optim_obj2"),
            br(),
            
            fluidRow(
              column(3,
                     textOutput("salesObj2"),
                     textOutput("costObj2"),
                     textOutput("profitObj2"),),
              
              conditionalPanel(
                condition = "input.selOptimizationModel == 'hga'",
                column(6,plotOutput("HGAplot2")),
              ),
            ),

            
            br(),
            
            textOutput("textObj3"),
            br(),
            DT::dataTableOutput("optim_obj3"),
            fluidRow(
              column(3,
                     textOutput("salesObj3"),
                     textOutput("costObj3"),
                     textOutput("profitObj3")),
              
              conditionalPanel(
                condition = "input.selOptimizationModel == 'hga'",
                column(6,plotOutput("HGAplot3")),
              ),
            ),
            br(),
          ),
        )
      )
    )
  )
)


ui <-  navbarPage(
  title = "TIAPOSE - Grupo 1",
  theme = shinythemes::shinytheme("cosmo"),
  main_page,
  models_page,
  scenarios_page,
  dataset_page,
  eda_page,
  results_page,
  about_page
)


# -------------------------- Interface -------------------------



server <- function(input, output){
  
  df2 <- read.csv("data/sanitizedData.csv", header = TRUE, sep=",")
  df2 <- df2[2:length(df2)]
  
  output$ourDataset = DT::renderDataTable({
    df2
  })
  
  
  
  # ---------- Results Page
  
  results_list = list()
  
  rminer_hybrid_all <-  read_excel("prediction_models/results/hybrid/hybrid_unipackage=rminer_multipackage=rminer_target=all.xlsx",1)
  results_list <- append(results_list, list(list(target="all", mt="hybrid", results=rminer_hybrid_all)))
  rminer_hybrid_female <-  read_excel("prediction_models/results/hybrid/hybrid_unipackage=rminer_multipackage=rminer_target=female.xlsx",1)
  results_list <- append(results_list, list(list(target="female", mt="hybrid", results=rminer_hybrid_female)))
  rminer_hybrid_male <-  read_excel("prediction_models/results/hybrid/hybrid_unipackage=rminer_multipackage=rminer_target=male.xlsx",1)
  results_list <- append(results_list, list(list(target="male", mt="hybrid", results=rminer_hybrid_male)))
  rminer_hybrid_young <-  read_excel("prediction_models/results/hybrid/hybrid_unipackage=rminer_multipackage=rminer_target=young.xlsx",1)
  results_list <- append(results_list, list(list(target="young", mt="hybrid", results=rminer_hybrid_young)))
  rminer_hybrid_adult <-  read_excel("prediction_models/results/hybrid/hybrid_unipackage=rminer_multipackage=rminer_target=adult.xlsx",1)
  results_list <- append(results_list, list(list(target="adult", mt="hybrid", results=rminer_hybrid_adult)))
  
  forecast_hybrid_all <-  read_excel("prediction_models/results/hybrid/hybrid_unipackage=forecast_multipackage=rminer_target=all.xlsx",1)
  results_list <- append(results_list, list(list(target="all", mt="hybrid", results=forecast_hybrid_all)))
  forecast_hybrid_female <-  read_excel("prediction_models/results/hybrid/hybrid_unipackage=forecast_multipackage=rminer_target=female.xlsx",1)
  results_list <- append(results_list, list(list(target="female", mt="hybrid", results=forecast_hybrid_female)))
  forecast_hybrid_male <-  read_excel("prediction_models/results/hybrid/hybrid_unipackage=forecast_multipackage=rminer_target=male.xlsx",1)
  results_list <- append(results_list, list(list(target="male", mt="hybrid", results=forecast_hybrid_male)))
  forecast_hybrid_young <-  read_excel("prediction_models/results/hybrid/hybrid_unipackage=forecast_multipackage=rminer_target=young.xlsx",1)
  results_list <- append(results_list, list(list(target="young", mt="hybrid", results=forecast_hybrid_young)))
  forecast_hybrid_adult <-  read_excel("prediction_models/results/hybrid/hybrid_unipackage=forecast_multipackage=rminer_target=adult.xlsx",1)
  results_list <- append(results_list, list(list(target="adult", mt="hybrid", results=forecast_hybrid_adult)))
  
  
  multivariate_all <- read_excel("prediction_models/results/multivariate/multivariate_target=all.xlsx",1)
  results_list <- append(results_list, list(list(target="all", mt="multivariate", results=multivariate_all)))
  multivariate_female <- read_excel("prediction_models/results/multivariate/multivariate_target=female.xlsx",1)
  results_list <- append(results_list, list(list(target="female", mt="multivariate", results=multivariate_female)))
  multivariate_male <- read_excel("prediction_models/results/multivariate/multivariate_target=male.xlsx",1)
  results_list <- append(results_list, list(list(target="male", mt="multivariate", results=multivariate_male)))
  multivariate_young <- read_excel("prediction_models/results/multivariate/multivariate_target=young.xlsx",1)
  results_list <- append(results_list, list(list(target="young", mt="multivariate", results=multivariate_young)))
  multivariate_adult <- read_excel("prediction_models/results/multivariate/multivariate_target=adult.xlsx",1)
  results_list <- append(results_list, list(list(target="adult", mt="multivariate", results=multivariate_adult)))
  
  rminer_univariate_all <- read_excel("prediction_models/results/univariate/univariate_package=rminer_target=all.xlsx",1)
  results_list <- append(results_list, list(list(target="all", mt="univariate", results=rminer_univariate_all)))
  rminer_univariate_female <- read_excel("prediction_models/results/univariate/univariate_package=rminer_target=female.xlsx",1)
  results_list <- append(results_list, list(list(target="female", mt="univariate", results=rminer_univariate_female)))
  rminer_univariate_male <- read_excel("prediction_models/results/univariate/univariate_package=rminer_target=male.xlsx",1)
  results_list <- append(results_list, list(list(target="male", mt="univariate", results=rminer_univariate_male)))
  rminer_univariate_young <- read_excel("prediction_models/results/univariate/univariate_package=rminer_target=young.xlsx",1)
  results_list <- append(results_list, list(list(target="young", mt="univariate", results=rminer_univariate_young)))
  rminer_univariate_adult <- read_excel("prediction_models/results/univariate/univariate_package=rminer_target=adult.xlsx",1)
  results_list <- append(results_list, list(list(target="adult", mt="univariate", results=rminer_univariate_adult)))
  
  forecast_univariate_all <- read_excel("prediction_models/results/univariate/univariate_package=forecast_target=all.xlsx",1)
  results_list <- append(results_list, list(list(target="all", mt="univariate", results=forecast_univariate_all)))
  forecast_univariate_female <- read_excel("prediction_models/results/univariate/univariate_package=forecast_target=female.xlsx",1)
  results_list <- append(results_list, list(list(target="female", mt="univariate", results=forecast_univariate_female)))
  forecast_univariate_male <- read_excel("prediction_models/results/univariate/univariate_package=forecast_target=male.xlsx",1)
  results_list <- append(results_list, list(list(target="male", mt="univariate", results=forecast_univariate_male)))
  forecast_univariate_young <- read_excel("prediction_models/results/univariate/univariate_package=forecast_target=young.xlsx",1)
  results_list <- append(results_list, list(list(target="young", mt="univariate", results=forecast_univariate_young)))
  forecast_univariate_adult <- read_excel("prediction_models/results/univariate/univariate_package=forecast_target=adult.xlsx",1)
  results_list <- append(results_list, list(list(target="adult", mt="univariate", results=forecast_univariate_adult)))
  
  observe({
    results_model_type <- tolower(input$results_model_type)
    results_target_variab <- tolower(input$results_target_variab)
    
    result = data.frame()
    for(i in 1:length(results_list)){
      if(results_model_type == results_list[[i]]$mt){
        if(results_target_variab == results_list[[i]]$target){
          result <- rbind(result, results_list[[i]]$results)
        }
      }
    }
    
    output$tableResults = DT::renderDataTable({
      result
    })
    
  })
    
  
  observeEvent(input$run_button,{
    
    
    updateSelectInput(inputId = "selOptimizationModel", choices = optimizationModelsObj, selected=optimizationModelsObj[[1]])
    
    predArr = c()
    case = 4
    
    # ------ Graph
    new_col = c()
    value = as.integer(input$numberDotsPlot)
    # ------ Graph
    
    targetVariab <- input$plotVariab
    datasetChoosen <- input$selButt
    DatesMerge<-input$dateToPredict 
    inFile <- input$csv_input
    
    if(datasetChoosen != "Our Dataset")
      df <- getDf(datasetChoosen , DatesMerge, inFile) 
    else
      df <- getDf(datasetChoosen , DatesMerge) 
    
    dates_df <- df$dates_df
    days <- df$days
    df <- df$df
    
    
    
    
    # --------------------------------------------------------------  New Dataset
    
    
    if(input$selButt == "New Dataset"){
      
      if(input$selModelType == 'Multivariate'){
        modelToPredict = input$selMultivariate
        splited = unlist(strsplit(modelToPredict, split = " - "))
        package = splited[[1]]
        modelToPredict = splited[[2]]

        predArr <- getModelUniMulti("Multivariate", modelToPredict, df, target)
        
        results <- formatPred(predArr, days)
        
        # Predictions
        
      }
      if(input$selModelType == 'Univariate'){
        modelToPredict = input$selUnivariate
        
        splited = unlist(strsplit(modelToPredict, split = " - "))
        package = splited[[1]]
        modelToPredict = splited[[2]]
        
        predArr <- getModelUniMulti("Univariate", modelToPredict, df, target, package)
        
        results <- formatPred(predArr, days)
        
        
      }
      if(input$selModelType == 'Hybrid'){
        modelToPredictUni = input$selHybridUni
        modelToPredictMulti = input$selHybridMulti
        splited = unlist(strsplit(modelToPredictUni, split = " - "))
        packageUni = splited[[1]]
        modelToPredictUni = splited[[2]]
        
        splited2 = unlist(strsplit(modelToPredictMulti, split = " - "))
        modelToPredictMulti = splited2[[2]]
        
        
        predArr <- getModelHybrid(modelToPredictUni, modelToPredictMulti, df, target, packageUni)
        
        results <- formatPred(predArr, days)
   
      }
    }
    
    
    # --------------------------------------------------------------  Our Dataset
      
    if(input$selButt == "Our Dataset"){
      if(input$selModelType2 == 'Multivariate'){
        modelToPredict = input$selMultivariate2
        splited = unlist(strsplit(modelToPredict, split = " - "))
        package = splited[[1]]
        modelToPredict = splited[[2]]
        
        predArr <- getModelUniMulti("Multivariate", modelToPredict, df, target)

        
        results <- formatPred(predArr, days)
        
      }
      
      if(input$selModelType2 == 'Univariate'){
        modelToPredict = input$selUnivariate2
        
        splited = unlist(strsplit(modelToPredict, split = " - "))
        package = splited[[1]]
        modelToPredict = splited[[2]]
        
        predArr <- getModelUniMulti("Univariate", modelToPredict, df, target, package)
        
        results <- formatPred(predArr, days)
        
        
        # ------------------------------
        
      }
      if(input$selModelType2 == 'Hybrid'){
        modelToPredictUni = input$selHybridUni2
        modelToPredictMulti = input$selHybridMulti2
        splited = unlist(strsplit(modelToPredictUni, split = " - "))
        packageUni = splited[[1]]
        modelToPredictUni = splited[[2]]
        
        splited2 = unlist(strsplit(modelToPredictMulti, split = " - "))
        modelToPredictMulti = splited2[[2]]
        
        predArr <- getModelHybrid(modelToPredictUni, modelToPredictMulti, df, target, packageUni)
        
        results <- formatPred(predArr, days)

      }
    }
    
    # ----- Display Table -------
    
    output$sample_table = DT::renderDataTable({
      DT::datatable(results, options = list(c(5, 15, -1)))
    })
    
    # ----- Optimization
    
    observe({
      optimizationModel = input$selOptimizationModel
      if(length(predArr) != 0){
        
        if((optimizationModel != "pareto") && (optimizationModel != "Best Algorithms" )){ 
          dfOptimResults = list()
          for(i in 1:3){
            results2 <- optimize(optimizationModel, i, predArr, days)
            dfOptimResults <- c(dfOptimResults, list(results2))
          }
          

          output$textObj1= renderText({"Objective 1"})
          output$optim_obj1 = DT::renderDataTable({
            DT::datatable(dfOptimResults[[1]]$strDf,options = list(c(5, 15, -1)))
          })
          output$salesObj1 = renderText({paste("Total Sales:", dfOptimResults[[1]]$sales, "euros")})
          output$costObj1 = renderText({paste("Total Cost:", dfOptimResults[[1]]$costs, "euros")})
          output$profitObj1 = renderText({paste("Total Profit:", dfOptimResults[[1]]$profit, "euros")})
          if(optimizationModel == "hga"){
            output$HGAplot1 <- renderPlot({
              plot(dfOptimResults[[1]]$GA)
            })
          }
          
          
          output$textObj2 = renderText({"Objective 2"})
          output$optim_obj2 = DT::renderDataTable({
            DT::datatable(dfOptimResults[[2]]$strDf,options = list(c(5, 15, -1)))
          })
          output$salesObj2 = renderText({paste("Total Sales:", dfOptimResults[[2]]$sales, "euros")})
          output$costObj2 = renderText({paste("Total Cost:", dfOptimResults[[2]]$costs, "euros")})
          output$profitObj2 = renderText({paste("Total Profit:", dfOptimResults[[2]]$profit, "euros")})
          if(optimizationModel == "hga"){
            output$HGAplot2 <- renderPlot({
              plot(dfOptimResults[[2]]$GA)
            })
          }
          
          
          output$textObj3 = renderText({"Objective 3"})
          output$optim_obj3 = DT::renderDataTable({
            DT::datatable(dfOptimResults[[3]]$strDf,options = list(c(5, 15, -1)))
          })
          output$salesObj3 = renderText({paste("Total Sales:", dfOptimResults[[3]]$sales, "euros")})
          output$costObj3 = renderText({paste("Total Cost:", dfOptimResults[[3]]$costs, "euros")})
          output$profitObj3 = renderText({paste("Total Profit:", dfOptimResults[[3]]$profit, "euros")})
          if(optimizationModel == "hga"){
            output$HGAplot3 <- renderPlot({
              plot(dfOptimResults[[3]]$GA)
            })
          }
          
        }
        
        if(optimizationModel == "pareto"){
          results2 <- paretoApproach(predArr, days)
          
          output$textObj1 = renderText({"Objective 3"})
          output$optim_obj1 = DT::renderDataTable({
            DT::datatable(results2$strDf,options = list(c(5, 15, -1)))
          })
          output$salesObj1 = renderText({paste("Total Sales:", results2$sales, "euros")})
          output$costObj1 = renderText({paste("Total Cost:", results2$costs, "euros")})
          output$profitObj1 = renderText({paste("Total Profit:", results2$profit, "euros")})
          
          
          output$titleOptimChart = renderText({"Pareto Chart"})
          
          #$paretoImage <- renderImage({tempfile(fileext=img[1])}, deleteFile = TRUE)
          output$paretoImage <- renderImage({
            filename <- normalizePath(file.path(results2$filePath))
            
            # Return a list containing the filename
            list(src = filename, alt = "Alternate text")
          }, deleteFile = FALSE)
        }
      }
    })
    
    
    # ----- Plot Graph -------
     
    observe({
      value = as.integer(input$numberDotsPlot)
      target_variables <- input$plotVariab
      optimizationModel = input$paretoImage
      if(nrow(df)>0){
        if(value > nrow(df)) {
          value = nrow(df)
          dd <- data.frame(index = c((1:nrow(df))))
        } else {
          dd <- data.frame(index = c((nrow(df)-value):nrow(df)))
        }
        
        plotSeries <- seriesToPlot(df, predArr, target_variables, value)
        
        needs <- graphPlotPrev(plotSeries, dd, value)
        dd$highlight <- needs
        
        dates_df <- dates_df[dd$index]
        
        output$plot1 <- renderPlot({
          plotPredictions(dd, target_variables, plotSeries, dates_df)
        })
      }
    })
    
    
    # ----- End Plot Graph -------
    
  })
  
  
  observeEvent(input$run_button2,{
    
    updateSelectInput(inputId = "selOptimizationModel", choices = c("Best Algorithms"), selected = "Best Algorithms")

    datasetChoosen <- input$selButtBest
    DatesMerge<-input$dateToPredictBest 
    inFile <- input$csv_inputBest
    
    if(datasetChoosen != "Our Dataset")
      df <- getDf(datasetChoosen , DatesMerge, inFile) 
    else
      df <- getDf(datasetChoosen , DatesMerge) 
    
    dates_df <- df$dates_df
    days <- df$days
    df <- df$df
    
    case = 4
    predArr = c()
    
    
    path_all_uni_hybrid <- "prediction_models/best_params/univariate/univariate_target=all_case=4.json"
    path_all_multi_hybrid <- "prediction_models/best_params/multivariate/multivariate_target=all_case=4_lags=TRUE.json"
    path_female_uni_hybrid <- "prediction_models/best_params/univariate/univariate_target=female_case=4.json"
    path_female_multi_hybrid <- "prediction_models/best_params/multivariate/multivariate_target=female_case=4_lags=TRUE.json"
    path_male_multi_hybrid <- "prediction_models/best_params/multivariate/multivariate_target=male_case=4_lags=TRUE.json"
    path_young_multi <- "prediction_models/best_params/multivariate/multivariate_target=young_case=4_lags=TRUE.json"
    path_adult_multi_hybrid <- "prediction_models/best_params/multivariate/multivariate_target=adult_case=4_lags=TRUE.json"
    
    best_models <- list()
    all_best <- list(type="hybrid", target="all", path_uni=path_all_uni_hybrid, path_multi=path_all_multi_hybrid, package="rminer", model_uni_best="randomForest", model_multi_best="knn")
    female_best <- list(type="hybrid", target="female", path_uni=path_female_uni_hybrid, path_multi=path_female_multi_hybrid, package="forecast", model_uni_best="ETS", model_multi_best="randomForest")
    male_best <- list(type="hybrid", target="male", path_multi=path_male_multi_hybrid, package="forecast", model_uni_best="Arima", model_multi_best="randomForest")
    young_best <- list(type="multi", target="young", path=path_young_multi, package="rminer", model_best="cv.glmnet")
    adult_best <- list(type="hybrid", target="adult", path_multi=path_adult_multi_hybrid, package="forecast", model_uni_best="Arima", model_multi_best="randomForest")
    
    best_models <- list(all_best,female_best,male_best,young_best,adult_best)
    
    for(i in 1:length(best_models)){
      if(best_models[[i]]$type == "hybrid"){
        if(best_models[[i]]$package == 'forecast'){
          modelsUni <- c("Arima", "Holt-Winters", "NN", "ETS")
        } else {
          modelsUni <- fromJSON(file=best_models[[i]]$path_uni)
        }
        
        modelsMulti <- fromJSON(file=best_models[[i]]$path_multi)
        
        preds <- hybridPred(best_models[[i]]$target, df, best_models[[i]]$model_uni_best, best_models[[i]]$model_multi_best, modelsUni, modelsMulti, best_models[[i]]$package)
      } else {
        modelsMulti <- fromJSON(file=best_models[[i]]$path)
        
        preds <- multivariatePred(best_models[[i]]$target, best_models[[i]]$model_best, df, modelsMulti)
      }
    
      predArr = c(predArr,preds)
    }
    
    predArr <- floor(predArr)
    predArr = list(all=predArr[1:7],female=predArr[8:14],male=predArr[15:21],young=predArr[22:28],adult=predArr[29:35])
    results <- formatPred(predArr, days)

    # ----- Display Table -------
    
    output$sample_table = DT::renderDataTable({
      DT::datatable(results, options = list(c(5, 15, -1)))
    })
    
    
    output$text_best_model_all <- renderText({"All - Hybrid Model: Random Forest (Univariate) + KNN (Multivariate)"})
    output$text_best_model_female <-renderText({"Female - Hybrid Model: ETS (Univariate) + Random Forest (Multivariate)"})
    output$text_best_model_male <- renderText({"Male - Hybrid Model: Arima (Univariate) + Random Forest (Multivariate)"})
    output$text_best_model_young <- renderText({"Young - Multivariate Model: GLM"})
    output$text_best_model_adult <-renderText({"Adult - Hybrid Model: Arima (Univariate) + Random Forest (Multivariate)"})
    
    # ----- Plot Graph -------
    
    observe({
      value = as.integer(input$numberDotsPlot)
      target_variables <- input$plotVariab
      
      if(nrow(df)>0){
        if(value > nrow(df)) {
          value = nrow(df)
          dd <- data.frame(index = c((1:nrow(df))))
        } else {
          dd <- data.frame(index = c((nrow(df)-value):nrow(df)))
        }
        
        plotSeries <- seriesToPlot(df, predArr, target_variables, value)
        
        needs <- graphPlotPrev(plotSeries, dd, value)
        dd$highlight <- needs
        
        dates_df <- dates_df[dd$index]
        
        
        output$plot1 <- renderPlot({
          plotPredictions(dd, target_variables, plotSeries, dates_df)
        })
      }
    })
    
    
    
    # ----- Optimization
    
    dfOptimResults = list()
    bestModelsOptimization = c("tabu","tabu","tabu")

      for(i in 1:3){
        results2 <- optimize(bestModelsOptimization[[i]], i, predArr, days)
        dfOptimResults <- c(dfOptimResults, list(results2))
      }
    
      output$textObj1= renderText({"Objective 1 - Tabu Search"})
      output$optim_obj1 = DT::renderDataTable({
        DT::datatable(dfOptimResults[[1]]$strDf,options = list(c(5, 15, -1)))
      })
      output$salesObj1 = renderText({paste("Total Sales:", dfOptimResults[[1]]$sales, "euros")})
      output$costObj1 = renderText({paste("Total Cost: -", dfOptimResults[[1]]$costs, "euros")})
      output$profitObj1 = renderText({paste("Total Profit:", dfOptimResults[[1]]$profit, "euros")})
          
          
      output$textObj2 = renderText({"Objective 2 - Tabu Search"})
      output$optim_obj2 = DT::renderDataTable({
        DT::datatable(dfOptimResults[[2]]$strDf,options = list(c(5, 15, -1)))
      })
      output$salesObj2 = renderText({paste("Total Sales:", dfOptimResults[[2]]$sales, "euros")})
      output$costObj2 = renderText({paste("Total Cost: -", dfOptimResults[[2]]$costs, "euros")})
      output$profitObj2 = renderText({paste("Total Profit:", dfOptimResults[[2]]$profit, "euros")})
          
          
      output$textObj3 = renderText({"Objective 3 - Tabu Search"})
      output$optim_obj3 = DT::renderDataTable({
        DT::datatable(dfOptimResults[[3]]$strDf,options = list(c(5, 15, -1)))
      })
      output$salesObj3 = renderText({paste("Total Sales:", dfOptimResults[[3]]$sales, "euros")})
      output$costObj3 = renderText({paste("Total Cost: -", dfOptimResults[[3]]$costs, "euros")})
      output$profitObj3 = renderText({paste("Total Profit:", dfOptimResults[[3]]$profit, "euros")})
  })

}



shinyApp(ui = ui, server = server)