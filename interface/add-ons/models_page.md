As far as the models used, we decided to choose the ones that best fit the objective of our project and that were studied during the classes using the Rminer and Forecast packages.

It should be noted that the Rminer package was developed by the UC teacher Paulo Cortez.
The Forecast package has the participation of a number of developers, and the current representative of its maintenance and patent holder is the author Rob Hyndman.

The information about the packages and their models were obtained with the help of the online documentation provided through the link "https://www.rdocumentation.org/".
<br>

## <b>List of models used:</b>
- [Prediction](#Prediction)
  - [Rminer](#Rminer)
      - [Random Forest](#randomForest)
      - [Support Vector Machines](#ksvm)
      - [Multi Linear Regression](#lm)
      - [Multilayer Perceptron](#mlp)
      - [Multilayer Perceptron Ensemble](#mlpe)
      - [Naive](#naive)
      - [Cross-validation for glmnet](#cv.glmnet)
      - [Recursive Partitioning and Regression Trees](#rpart)
      - [k-Nearest Neighbour Classification](#knn)
      - [eXtreme Gradient Boosting](#xgboost)
  - [Forecast](#Forecast)
      - [Holt Winters](#holtWinters)
      - [Arima](#Arima)
      - [Neural Networks](#neuralNetworks)
      - [ETS](#ets)
- [Optimization](#Optimization)
  - [Blind Search](#blindSearch)
      - [Monte Carlo](#monteCarlo)
  - [Local Search](#localSearch)
      - [Hill Climbing](#hillClimbing)
      - [Simulated Annealing](#simulatedAnnealing)
      - [Tabu](#tabu)
  - [Population Based](#populationBased)
      - [Differential Evolution](#differentialEvolution)
      - [Genetic Algorithm](#geneticAlgorithm)
      - [Particle Swarm](#particleSwarm)
      - [NSGA-II](#NSGA-II)
  - [Híbrida](#híbrida)
      - [Genetic Algorithm / Simulated Annealing](#geneticAlgorithmSimulatedAnnealing)

## <b>Prediction</b> <a name = "Prediction"></a>
### <b>Rminer:</b> <a name = "Rminer"></a>
The developed package facilitates the use of machine learning algorithms in classification and regression tasks through a set of functions.
<br>

<b>Author:</b> Paulo Cortez 2015@ University of Minho
<br>
Learn more: http://www3.dsi.uminho.pt/pcortez/rminer.html

#### <u>Random Forest (randomForest)</u> <a name = "randomForest"></a>
The Random Forest algorithm implements the Breiman's random forest algorithm for classification and regression tasks.
<br>
The Random Forest algorithm is a way to average multiple decision trees, each trained on different parts of the same training set, with the goal of overcoming over-fitting problem of individual decision tree.
<br>
Learn more: https://www.rdocumentation.org/packages/randomForest/versions/4.7-1/topics/randomForest

#### <u>Support Vector Machines (ksvm)</u> <a name = "ksvm"></a>
Support Vector Machines are a tool for classification, novelty detection, and regression tasks. Ksvm model supports C-svc, nu-svc, (classification) one-class-svc (novelty) eps-svr, nu-svr (regression) formulations along with native multi-class classification formulations and the bound-constraint SVM formulations.
<br>
It can also suppor class-probabilities output and confidence intervals for regression.
<br>
Learn more: https://www.rdocumentation.org/packages/kernlab/versions/0.6-1/topics/ksvm

#### <u>Multi Linear Regression (lm)</u> <a name = "lm"></a>
Multiple linear regression is an extension of linear regression and it is used to predict an outcome variable, on the basis of multiple distinct predictor variables.
<br>
Multiple linear regression is used to assess the relationship between two variables, taking into account the effect of other variables. With this, we cancel out the effect of these other variables with the objective to isolate and measure the relationship between the two variables of interest. This point is the main difference between this model and simple linear regression.
<br>
Learn more: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm

#### <u>Multilayer Perceptron (mlp)</u> <a name = "mlp"></a>
MLPs are fully connected feedforward networks. It consists of three types of layers: the input layer, output layer and hidden layer. The input layer receives the input signal to be processed. The output layer performs the prediction or classification task.
<br>
A certain number of hidden layers that are placed in between the input and output layer are the true computational engine of the MLP. In this network, the data flows in the forward direction from input to output layer. The neurons in the MLP are trained with the back propagation learning algorithm.
<br>
Learn more: https://www.rdocumentation.org/packages/RSNNS/versions/0.4-14/topics/mlp

#### <u>Multilayer Perceptron Ensemble (mlpe)</u> <a name = "mlpe"></a>
Multilayer perceptron ensemble (classification and regression, uses nnet from nnet package).
<br>
Learn more: https://www.rdocumentation.org/packages/rminer/versions/1.4.6/topics/fit

#### <u>Naive (naive)</u> <a name = "naive"></a>
Function that selects the most common class (classification) or mean output value (regression).
<br>
Learn more: https://www.rdocumentation.org/packages/rminer/versions/1.4.6/topics/fit

#### <u>Cross-validation for glmnet (cv.glmnet)</u> <a name = "cv.glmnet"></a>
Glmnet is a package that fits generalized linear and similar models via penalized maximum likelihood. It fits linear, logistic and multinomial, poisson, and Cox regression models. It can also fit multi-response linear regression, generalized linear models for custom families, and relaxed lasso regression models.
<br>
The cv.glmnet model does k-fold cross-validation for glmnet, wich produces a plot and returns a value for lambda.
<br>
Learn more: https://www.rdocumentation.org/packages/glmnet/versions/4.1-4/topics/cv.glmnet

#### <u>Recursive Partitioning and Regression Trees (rpart)</u> <a name = "rpart"></a>
Rpart is a machine learning library in R that implements recursive partitioning and is used for building classification and regression trees.
<br>
Learn more: https://www.rdocumentation.org/packages/rpart/versions/4.1.16/topics/rpart

#### <u>k-Nearest Neighbour Classification (knn)</u> <a name = "knn"></a>
K Nearest Neighbour is a supervised learning algorithm that classifies a new data point into the target class, depending on the features of its neighboring data points.
<br>
Learn more: https://www.rdocumentation.org/packages/class/versions/7.3-20/topics/knn

#### <u>eXtreme Gradient Boosting (xgboost)</u> <a name = "xgboost"></a>
XGBoost is an efficent implimentation of gradient boosting and it is used in classification and regression tasks.
<br>
Extreme Gradient Boosting is similar to gradient boosting framework but more efficient. It has both linear model solver and tree learning algorithms. Is capacity to do parallel computation on a single machine is what makes this model very fast.
<br>
Learn more: https://www.rdocumentation.org/packages/xgboost/versions/0.4-4/topics/xgboost

### <b>Forecast:</b> <a name = "Forecast"></a>
The R package forecast provides methods and tools for displaying and analysing univariate time series forecasts including exponential smoothing via state space models and automatic ARIMA modelling.
<br>

<b>Author, maintainer and copyright holder:</b> Rob Hyndman
<br>
<b>Other authors:</b> George Athanasopoulos, Christoph Bergmeir, Gabriel Caceres, Leanne Chhay, Mitchell O'Hara-Wild, Fotios Petropoulos, Slava Razbash, Earo Wang and Farah Yasmeen
<br>
<b>Other contributors:</b> R Core Team. Contributor (copyright holder), Ross Ihaka (copyright holder), Daniel Reid, David Shaub, Yuan Tang and Zhenyu Zhou
<br>
Learn more: https://www.rdocumentation.org/packages/forecast/versions/8.16

#### <u>Holt Winters</u> <a name = "holtWinters"></a>
Holt-Winters forecasting allows us to model and predict the behavior of a sequence of values over time (time series), being known as one of the most popular forecasting techniques for time series.
<br>
The Holt-Winters method uses exponential smoothing to encode lots of values from the past and use them to predict “typical” values for the present and future. Exponential smoothing refers to the use of an exponentially weighted moving average (EWMA) to “smooth” a time series.
<br>
Learn more: https://www.rdocumentation.org/packages/forecast/versions/8.16/topics/forecast.HoltWinters

#### <u>Arima</u> <a name = "Arima"></a>
ARIMA is the abbreviation for AutoRegressive Integrated Moving Average. Auto Regressive (AR) terms refer to the lags of the differenced series, Moving Average (MA) terms refer to the lags of errors and I is the number of difference used to make the time series stationary.
<br>
Learn more: https://www.rdocumentation.org/packages/forecast/versions/8.16/topics/Arima

#### <u>Neural Networks</u> <a name = "neuralNetworks"></a>
Feed-forward neural networks with a single hidden layer and lagged inputs for forecasting univariate time series. The nnetar function in the forecast package for R fits a neural network model to a time series with lagged values of the time series as inputs.
<br>
Learn more: https://www.rdocumentation.org/packages/forecast/versions/8.16/topics/forecast.nnetar

#### <u>ETS</u> <a name = "ets"></a>
Exponential smoothing state space methods constitute a broad family of approaches to univariate time series forecasting. ets is able to make the most of all of the members of its family and automatically choose the most effective method for a given dataset.
<br>
Learn more: https://www.rdocumentation.org/packages/forecast/versions/8.16/topics/forecast.ets

## <b>Optimization</b> <a name = "Optimization"></a>
### <b>Blind Search:</b> <a name = "blindSearch"></a>
#### <u>Monte Carlo</u> <a name = "monteCarlo"></a>
<br>

### <b>Local Search:</b> <a name = "localSearch"></a>
#### <u>Hill Climbing</u> <a name = "hillClimbing"></a>
#### <u>Simulated Annealing</u> <a name = "simulatedAnnealing"></a>
#### <u>Tabu</u> <a name = "tabu"></a>
<br>

### <b>Population Based:</b> <a name = "populationBased"></a>
#### <u>Differential Evolution</u> <a name = "differentialEvolution"></a>
#### <u>Genetic Algorithm</u> <a name = "geneticAlgorithm"></a>
#### <u>Particle Swarm</u> <a name = "particleSwarm"></a>
#### <u>NSGA-II</u> <a name = "NSGA-II"></a>
<br>

### <b>Híbrida:</b> <a name = "híbrida"></a>
#### <u>Genetic Algorithm / Simulated Annealing</u> <a name = "geneticAlgorithmSimulatedAnnealing"></a>