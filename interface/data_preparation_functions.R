# define Min-Max normalization function
min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}

# return a df with the selected case data
getCaseDf <- function(df, case) {
    if (case=="1") {
        # CASE 1: original dataset
        # remove day col (previously added in the Data Preparation phase)
        df$day <- NULL
    } else if (case=="2") {
        # CASE 2: dataset with normalized data
        # apply Min-Max normalization
        df[7:9] <- as.data.frame(lapply(df[7:9], min_max_norm))
        # remove day col (previously added in the Data Preparation phase)
        df$day <- NULL
    } else if (case=="3") {
        # CASE 3: dataset with normalized data and day col
        # apply Min-Max normalization
        df[7:9] <- as.data.frame(lapply(df[7:9], min_max_norm))
    } else if (case=="4") {
        # CASE 4: dataset with normalized data, day col and changed outliers (value equal to mean)
        # apply Min-Max normalization
        df[7:9] <- as.data.frame(lapply(df[7:9], min_max_norm))
        # change outliers value to mean
        # all col
        upper_bound <- median(df$all) + 3 * mad(df$all, constant=1)
        lower_bound <- median(df$all) - 3 * mad(df$all, constant=1)
        outlier_ind <- which(df$all < lower_bound | df$all > upper_bound)
        df[outlier_ind,]$all <- round(mean(df[-outlier_ind,]$all),0)
        # female col
        upper_bound <- median(df$female) + 3 * mad(df$female, constant=1)
        lower_bound <- median(df$female) - 3 * mad(df$female, constant=1)
        outlier_ind <- which(df$female < lower_bound | df$female > upper_bound)
        df[outlier_ind,]$female <- round(mean(df[-outlier_ind,]$female),0)
        # male col
        upper_bound <- median(df$male) + 3 * mad(df$male, constant=1)
        lower_bound <- median(df$male) - 3 * mad(df$male, constant=1)
        outlier_ind <- which(df$male < lower_bound | df$male > upper_bound)
        df[outlier_ind,]$male <- round(mean(df[-outlier_ind,]$male),0)
        # young col
        upper_bound <- median(df$young) + 3 * mad(df$young, constant=1)
        lower_bound <- median(df$young) - 3 * mad(df$young, constant=1)
        outlier_ind <- which(df$young < lower_bound | df$young > upper_bound)
        df[outlier_ind,]$young <- round(mean(df[-outlier_ind,]$young),0)
        # adult col
        upper_bound <- median(df$adult) + 3 * mad(df$adult, constant=1)
        lower_bound <- median(df$adult) - 3 * mad(df$adult, constant=1)
        outlier_ind <- which(df$adult < lower_bound | df$adult > upper_bound)
        df[outlier_ind,]$adult <- round(mean(df[-outlier_ind,]$adult),0)
    }
    # return df
    df
}

# return useful variables used in the Modeling phase
getVariables <- function(target, df) {
    if (target == "all") { 
        del_cols <- !colnames(df) %in% c("date", "female", "male", "young", "adult")
        target_form <- all~.
    } else if (target == "female") {
        del_cols <- !colnames(df) %in% c("date", "all", "male", "young", "adult")
        target_form <- female~.
    } else if  (target == "male") {
        del_cols <- !colnames(df) %in% c("date", "female", "all", "young", "adult")
        target_form <- male~.
    } else if  (target == "young") {
        del_cols <- !colnames(df) %in% c("date", "female", "male", "all", "adult")
        target_form <- young~.
    } else if  (target == "adult") {
        del_cols <- !colnames(df) %in% c("date", "female", "male", "young", "all")
        target_form <- adult~.
    }
    list(del_cols=del_cols, target_form=target_form)
}

# return a list (JSON propose) with the best parameters of each rminer model
# rminer package needed
# models param is a list with each rminer model
gridSearch <- function(df, models, del_cols, target_form, time_lags=FALSE, NPRED=7) {
    df <- df[,del_cols]
    if (time_lags) {
        lags = c(1:3, 5:7)
        D=CasesSeries(df[,1],lags)
        df[8:nrow(df),c("lag7","lag6", "lag5", "lag3","lag2","lag1")] <- D
        df <- df[-c(0:7),]
    }

    HD=holdout(df[,1],ratio=NPRED,mode="order")
    
    rminer_search_default <- c("heuristic", "heuristic5", "heuristic10", "UD", "UD1", "UD2", "vector")
    rminer_search_mlp <- c("heuristic", "heuristic5", "heuristic10")
    for(i in 1:length(models)){
        # Setup variables
        arr_MAE=c()
        if (grepl("mlp", models[[i]]$rminer, fixed=TRUE)) 
            rminer_search = rminer_search_mlp
        else 
            rminer_search = rminer_search_default
        #
        for(n in 1:length(rminer_search)){
            M=fit(target_form, df[HD$tr,], model=models[[i]]$rminer, search=rminer_search[[n]])
            if (time_lags) {
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
            } else {
                predArr=predict(M,df[HD$ts,])
            }
            Y=df[HD$ts,1]

            MAE =  mmetric(Y,predArr,metric="MAE")
            arr_MAE <- c(arr_MAE, MAE)
        }

        for(n in 1:length(models[[i]]$rminer_mparheuristic)){
            s=list(search=mparheuristic(models[[i]]$rminer,models[[i]]$rminer_mparheuristic[n]),method=c("holdoutorder",2/3))
            M=fit(target_form, df[HD$tr,], model=models[[i]]$rminer, search=s)
            if (time_lags) {
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
            } else {
                predArr=predict(M,df[HD$ts,])
            }
            Y=df[HD$ts,1]

            MAE =  mmetric(Y,predArr,metric="MAE")
            arr_MAE <- c(arr_MAE, MAE)
        }

        max_index <- which(arr_MAE == min(arr_MAE))[1]
        arr_algorithm <- c(rminer_search, models[[i]]$rminer_mparheuristic)
        algorithm <- arr_algorithm[[max_index]]
        mparheuristic <- length(rminer_search)<max_index
        result <- arr_MAE[max_index]
        
        models[[i]] <- append(models[[i]], list(rminer_best_search = list(mparheuristic=mparheuristic, algorithm=algorithm, MAE=result)))
    }
    models
}