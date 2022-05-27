library(forecast)
library(rminer)
source("data_preparation_functions.R")

multivariatePred <- function(target_variable, modelToPredict, df,models, case=3, NPRED=7){
  
  v <- getVariables(target_variable, df)
  del_cols <- v$del_cols
  df <- df[,del_cols]
  
  lags = c(1:3, 5:7)
  D=CasesSeries(df[,1],lags)
  df[8:nrow(df),c("lag7","lag6", "lag5", "lag3","lag2","lag1")] <- D
  df <- df[-c(0:7),]
  
  
  index = -1
  for(i in 1:length(models)){
    if(models[[i]]$rminer == modelToPredict){
      index = i
    }
  }
  
  if (models[[index]]$rminer_best_search$mparheuristic) {
    s=list(search=mparheuristic(models[[index]]$rminer,models[[index]]$rminer_best_search$algorithm),method=c("holdoutorder",2/3))
  } else {
    s=models[[index]]$rminer_best_search$algorithm
  }
  
  HD=holdout(df[,1],ratio=NPRED,mode="order")
  M=fit(v$target_form, df[HD$tr,], model=models[[index]]$rminer, search=s)
  #------------------------------------
  predArr <- c()
  count <- 7
  for(m in 1:length(HD$ts)){
    xArr <- df[HD$ts - count, ][1:count, 1]
    lag <- append(xArr, predArr)
    lag <- append(lag, NaN)
    D2 <- CasesSeries(lag,lags)
    y <- cbind(df[HD$ts[[m]],(1:(length(df)-length(lags)))], D2[1:length(lags)])
    PRED <- predict(M,y)
    predArr <- append(predArr, PRED)
    count <- count - 1
  }
  #----------------------------------
  predArr
}


# -------------------------------------------- Univariate ------------------------------------------


univariatePred <- function(target_variable, modelToPredict, df,models, package, case=3, NPRED=7){
  
  target_form = y~.
  Test = 7

  if (target_variable == "all") TS = df$all
  if (target_variable == "male") TS = df$male
  if (target_variable == "female") TS = df$female
  if (target_variable == "adult") TS = df$adult
  if (target_variable == "young") TS = df$young
  
  YR=diff(range(TS))

  if (package == "rminer"){
    timelags = c(1:3, 5:7)
    D=CasesSeries(TS,timelags)
    
    index = -1
    for(i in 1:length(models)){
      if(models[[i]]$rminer == modelToPredict){
        index = i
      }
    }
    
    if (models[[index]]$rminer_best_search$mparheuristic) {
      s=list(search=mparheuristic(models[[index]]$rminer,models[[index]]$rminer_best_search$algorithm),method=c("holdoutorder",2/3))
    } else {
      s=models[[index]]$rminer_best_search$algorithm
    }
    
    HD=holdout(D$y,ratio=NPRED,mode="order")
    M=fit(target_form, D[HD$tr,], model=modelToPredict, search=s)
    
    predArr=lforecast(M,D,start=(length(HD$tr)+1),Test)

  } else {

    HD=holdout(TS,ratio=NPRED,mode="order")
    dtr=ts(TS[HD$tr],frequency=Test)

    if (modelToPredict == "Arima"){
      M=suppressWarnings(auto.arima(dtr))
    }
    if (modelToPredict == "Holt-Winters"){
      M=suppressWarnings(HoltWinters(dtr)) 
    } 
    if (modelToPredict == "NN") {
      M=suppressWarnings(nnetar(dtr))
    } 
    if (modelToPredict == "ETS") {
      M=suppressWarnings(ets(dtr))
    }

    predArr=forecast(M,h=length(HD$ts))$mean[1:Test] 
  }
}



# -------------------------------------------- Hybrid ------------------------------------------


hybridPred <- function(target_variable, df, modelToPredictUni, modelToPredictMulti, modelsUni, modelsMulti, packageUni, case=3, NPRED=7){
  
  df <- getCaseDf(df, case)
  v <- getVariables(target_variable, df)
  target_form = y~.
  Test = 7
  
  if (target_variable == "all") TS = df$all
  if (target_variable == "male") TS = df$male
  if (target_variable == "female") TS = df$female
  if (target_variable == "adult") TS = df$adult
  if (target_variable == "young") TS = df$young
  
  YR=diff(range(TS))
  
  if (packageUni == "rminer"){
    timelags = c(1:3, 5:7)
    D=CasesSeries(TS,timelags)
    
    index = -1
    for(i in 1:length(modelsUni)){
      if(modelsUni[[i]]$rminer == modelToPredictUni){
        index = i
      }
    }
    
    if (modelsUni[[index]]$rminer_best_search$mparheuristic) {
      s=list(search=mparheuristic(modelsUni[[index]]$rminer,modelsUni[[index]]$rminer_best_search$algorithm),method=c("holdoutorder",2/3))
    } else {
      s=modelsUni[[index]]$rminer_best_search$algorithm
    }
    
    HD=holdout(D$y,ratio=Test,mode="order")
    M=fit(target_form, D[HD$tr,], model=modelToPredictUni, search=s)
    pred=lforecast(M,D,start=(length(HD$tr)+1),Test)
    
    trainPred=predict(M,D[HD$tr,])
    
    TRSIZE=length(trainPred)
    LPRED=length(pred)
    predictions=c(trainPred,pred)
    
    new_D <- D$y[1:(TRSIZE+LPRED)]
    NHD=cbind(pred=predictions,(df[8:(length(predictions)+7),v$del_cols][2:length(df[,v$del_cols])]),y=new_D)
    NHD=data.frame(NHD)
    
    index2 = -1
    for(x in 1:length(modelsMulti)){
      if(modelsMulti[[x]]$rminer == modelToPredictMulti){
        index2 = x
      }
    }
    
    if (modelsMulti[[index2]]$rminer_best_search$mparheuristic) {
      searchh2=list(search=mparheuristic(modelsMulti[[index2]]$rminer,modelsMulti[[index2]]$rminer_best_search$algorithm),method=c("holdoutorder",2/3))
    } else {
      searchh2=modelsMulti[[index2]]$rminer_best_search$algorithm
    }
    
    M3=fit(target_form, NHD[1:TRSIZE,], model=modelToPredictMulti, search = searchh2)
    
    predArr=predict(M3,NHD[(TRSIZE+1):(TRSIZE+LPRED),])
  } else {
    
    HD=holdout(TS,ratio=Test,mode="order")
    dtr=ts(TS[HD$tr],frequency=Test)
    
    if (modelToPredictUni == "Arima") {
      M=suppressWarnings(auto.arima(dtr)) 
      Mtrpred = fitted(M)
    }
    if (modelToPredictUni == "Holt-Winters") {
      M = suppressWarnings(HoltWinters(dtr))
      Mtrpred = M$fitted[1:nrow(M$fitted)]
    }
    if (modelToPredictUni == "NN") {
      M=suppressWarnings(nnetar(dtr, p=7)) 
      Mtrpred = M$fitted[8:length(M$fitted)]
    }
    if (modelToPredictUni == "ETS") {
      M=suppressWarnings(ets(dtr))
      Mtrpred = fitted(M)
    }
    
    
    Pred=forecast(M,h=length(HD$ts))$mean[1:Test]
    predForecast=c(Mtrpred, Pred)
    
    NHD=cbind(predictions=predForecast, df[1:length(predForecast),v$del_cols][2:length(df[,v$del_cols])], y=TS[1:length(predForecast)])
    NHD=data.frame(NHD)
    
    index2 = -1
    for(x in 1:length(modelsMulti)){
      if(modelsMulti[[x]]$rminer == modelToPredictMulti){
        index2 = x
      }
    }
    
    if (modelsMulti[[index2]]$rminer_best_search$mparheuristic) {
      searchh2=list(search=mparheuristic(modelsMulti[[index2]]$rminer,modelsMulti[[index2]]$rminer_best_search$algorithm),method=c("holdoutorder",2/3))
    } else {
      searchh2=modelsMulti[[index2]]$rminer_best_search$algorithm
    }
    
    if (modelToPredictUni == "NN") {
      M3=fit(target_form, NHD[8:length(Mtrpred),], model=modelToPredictMulti, search = searchh2)
    } else {
      M3=fit(target_form, NHD[1:length(Mtrpred),], model=modelToPredictMulti, search = searchh2)
    }    
    
    predArr=predict(M3, NHD[(length(Mtrpred) + 1):(length(Mtrpred)+length(Pred)),])
  }
}


# -------------------------------------------- Create Dataframe ------------------------------------------

formatPred <- function(pred, days){
  mat = matrix(ncol = 8, nrow = 0)
  results=data.frame(mat)
  results[nrow(results) + 1,] = c("all",pred$all)
  results[nrow(results) + 1,] = c("female",pred$female)
  results[nrow(results) + 1,] = c("male",pred$male)
  results[nrow(results) + 1,] = c("young",pred$young)
  results[nrow(results) + 1,] = c("adult",pred$adult)
  
  colnames(results) <- c("Target",days)
  
  results
}


graphPlotPrev <- function(df ,dd ,value){
  dd$index <- factor(dd$index)
  
  index.highlight <- dd$index[(length(dd$index)-6):length(dd$index)]
  dd$highlight <- ifelse(dd$index %in% index.highlight, "red", "black")
  
  
  return(dd$highlight)
}

seriesToPlot <- function(df, predArr, target_variables, value){
  if (target_variables == "All") new_col = c(df[(nrow(df)-value):(nrow(df)-7),]$all, predArr$all)
  if (target_variables == "Male") new_col = c(df[(nrow(df)-value):(nrow(df)-7),]$male, predArr$male)
  if (target_variables == "Female") new_col = c(df[(nrow(df)-value):(nrow(df)-7),]$female, predArr$female)
  if (target_variables == "Adult") new_col = c(df[(nrow(df)-value):(nrow(df)-7),]$adult, predArr$adult)
  if (target_variables == "Young") new_col = c(df[(nrow(df)-value):(nrow(df)-7),]$young, predArr$young)
  
  return(new_col)
}


getDf <- function(datasetChoosen , DatesMerge, inFile){
  if(datasetChoosen == "Our Dataset"){
    
    df <- read.csv("data/sanitizedData.csv", header = TRUE, sep=",")
    df <- df[2:length(df)]
    
    dates_df <- as.Date(df$date, format="%Y-%m-%d")
    
    # Training data set
    df = df[1:(which(df[,1]==DatesMerge)+6),2:length(df)]
    
    days_to_predict = seq.Date(DatesMerge, length=7, by='1 days')
    
    days = weekdays(as.Date(days_to_predict))
    
  } else {
    
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath, header = TRUE,sep = ",")
    
    if(length(df)==1){
      df <- read.csv(inFile$datapath, header = TRUE,sep = ";")
      
      df$all <- round(df$all, 0)
      df$female <- round(df$female, 0)
      df$male <- round(df$male, 0)
      df$young <- round(df$young, 0)
      df$adult <- round(df$adult, 0)
      
      df$all <- sapply(df$all, as.integer)
      df$female <- sapply(df$female, as.integer)
      df$male <- sapply(df$male, as.integer)
      df$adult <- sapply(df$adult, as.integer)
      df$young <- sapply(df$young, as.integer)
      
      df$weather[df$weather=="Rain"]<-0
      df$weather[df$weather=="Sunny"]<-1
      df$weather <- sapply(df$weather, as.integer)
      
      df$date <- gsub('/', '-', df$date)
      DateSplited = unlist(strsplit(df$date, split = "-"))
      
      
      if(nchar(DateSplited[[1]])==4)
        df$date <- as.Date(df$date, format="%Y-%m-%d")
      else
        df$date <- as.Date(df$date, format="%d-%m-%Y")
      
      dates_df <- df$date
      
      df$day <- wday(df$date, week_start=1)
      
    } else {
      
      dates_df <- as.Date(df$date, format="%Y-%m-%d")
      df <- df[2:length(df)]
    }
    
    days <- weekdays(as.Date(df[(nrow(df)-6):nrow(df),1]))
    df <- df[,2:length(df)]
    
  }
  return(list(df=df, days=days, dates_df=dates_df))
}
  
  

getModelUniMulti <- function(type, modelToPredict, df, target, package="None"){
  case = 4
  predArr = c()
  df <- getCaseDf(df, case)
  
  if(type == "Multivariate"){
    for(i in 1:length(target)){
      target_variable = tolower(target[[i]])
      path <- paste0("prediction_models/best_params/multivariate/multivariate_target=",target_variable,"_case=",case,"_lags=TRUE.json")
      models <- fromJSON(file=path)
      
      preds <- multivariatePred(target_variable, modelToPredict, df, models)
      predArr<- c(predArr, preds)
    }
  } else {
    for(i in 1:length(target)){
      target_variable = tolower(target[[i]])
      path <- paste0("prediction_models/best_params/univariate/univariate_target=",target_variable,"_case=",case,".json")
      models <- fromJSON(file=path)
      
      preds <- univariatePred(target_variable, modelToPredict, df, models, package)
      predArr= c(predArr,preds)
    }
  }
  
  predArr <- floor(predArr)
  predArr = list(all=predArr[1:7],female=predArr[8:14],male=predArr[15:21],young=predArr[22:28],adult=predArr[29:35])
}




getModelHybrid <- function(modelToPredictUni, modelToPredictMulti, df, target, packageUni){
  case = 4
  predArr = c()
  
  for(i in 1:length(target)){
    target_variable = tolower(target[[i]])
    path <- paste0("prediction_models/best_params/univariate/univariate_target=",target_variable,"_case=",case,".json")
    modelsUni <- fromJSON(file=path)
    path2 <- paste0("prediction_models/best_params/multivariate/multivariate_target=",target_variable,"_case=",case,"_lags=TRUE.json")
    modelsMulti <- fromJSON(file=path2)
    
    preds <- hybridPred(target_variable, df, modelToPredictUni, modelToPredictMulti, modelsUni, modelsMulti, packageUni)
    predArr = c(predArr,preds)
  }
  
  predArr <- floor(predArr)
  
  predArr = list(all=predArr[1:7],female=predArr[8:14],male=predArr[15:21],young=predArr[22:28],adult=predArr[29:35])
}
  



plotPredictions <- function(dd, target_variables, plotSeries, dates_df){
  p <- ggplot(dd, aes(x = dates_df, y = plotSeries, colour = highlight, group = 1)) +
    geom_line() +
    geom_point() +
    scale_colour_identity(dd$highlight)
  p + xlab("Date") + ylab(target_variables)
}

