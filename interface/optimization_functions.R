source("optimization_models/hill.R")
source("optimization_models/blind.R")
source("optimization_models/montecarlo.R")
library(tabuSearch)
library(genalg)
library(DEoptim)
library(pso)
library(mco)
library(GA)
set.seed(125)

sales=function(pred, x)
{
  s = c()
  if (length(pred$all)>0) {
    s = c(s, ifelse(pred$all<5000, pred$all*0.06, pred$all*0.09))
  }
  if (length(pred$female)>0) {
    s = c(s, ifelse(pred$female<1800, pred$female*0.08, pred$female*0.13))
  }
  if (length(pred$male)>0) {
    s = c(s, ifelse(pred$male<1800, pred$male*0.04, pred$male*0.07))
  }
  if (length(pred$young)>0) {
    s = c(s, ifelse(pred$young<3000, pred$young*0.04, pred$young*0.05))
  }
  if (length(pred$adult)>0) {
    s = c(s, ifelse(pred$adult<800, pred$adult*0.08, pred$adult*0.12))
  }
  s = x*s
  return(s)
}

costs=function(pred, x)
{
  cost = c(rep(350,length(pred$all)), rep(150,length(pred$female)), rep(100,length(pred$male)), rep(100,length(pred$young)), rep(120,length(pred$adult)))
  cost = x*cost
  return(cost)
}

# example: optimize("hc", 3, pred)
optimize <- function(model, obj, pred, days) {
  D = length(pred$all) + length(pred$female) + length(pred$male) + length(pred$young) + length(pred$adult) # get the size of the solution array
  lower=rep(0,D) # lower bounds
  upper=rep(1,D) #  upper bounds
  solution=sample(c(0,0,1), replace=TRUE, size=D) # generate random initial solution
  
  # evaluation functions: ------------------------------
  if (obj == "1") {
    profitObj1=function(x) 
    { 
      x = round(x)
      s = sum(sales(pred, x))
      cost = sum(costs(pred, x))
      p=s-cost
      return(p)
    }
    eval = function(x) profitObj1(x)
    evalN = function(x) -profitObj1(x)
  }
  else if (obj == "2") {
    # repair method
    repairMethod = function(x) {
      counter = 0
      for(i in 1:length(x)){
        if(x[i] == 1 & counter < 10)
          counter = counter + 1
        else 
          x[i] = 0  
      }
      return(x)
    }
    profitObj2Repair=function(x) 
    { 
      x = round(x)
      x = repairMethod(x)
      s = sum(sales(pred, x))
      cost = sum(costs(pred, x))
      p=s-cost
      return(p)
    }
    eval = function(x) profitObj2Repair(x)
    evalN = function(x) -profitObj2Repair(x)
    solution=lower
  }
  else if (obj == "3") {
    profitObj3=function(x) 
    { 
      x = round(x)
      s = sum(sales(pred, x))
      cost = sum(costs(pred, x))
      p=s-cost
      camp=sum(x==1)
      pcamp=0
      if(camp != 0) 
        pcamp=p/camp
      
      score = (p * 0.30) + (pcamp * 0.7)
      return(score)
    }
    eval = function(x) profitObj3(x)
    evalN = function(x) -profitObj3(x)
  }
  
  GA = FALSE
  results = FALSE
  if (model == "mc") {
    # Monte Carlo: ------------------------------
    N=1500 # 1500 searches
    
    MC=mcsearch(fn=eval, lower=lower, upper=upper, N=N, type="max")
    cat("best solution:",round(MC$sol),"evaluation function",MC$eval,"\n")
    results = round(MC$sol)
  } else if (model == "hc") {
    # Hill Climbing: ------------------------------
    N=500 # 500 searches
    REPORT=0 # report results

    # slight change of a real par under a normal u(0,0.3) function:
    rchange1=function(par,lower,upper) # change for hclimbing
    { hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.3,round=TRUE) }
    
    HC=hclimbing(par=solution,fn=eval,change=rchange1,lower=lower,upper=upper,type="max",
                 control=list(maxit=N,REPORT=REPORT,digits=2))
    cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
    results = round(HC$sol)
  } else if (model == "sa") {
    # Simulated Annealing: ------------------------------
    Runs=50 # 50 searches
    
    bestR= -Inf
    
    for(i in 1:Runs)
    {
      # slight change of a real par under a normal u(0,0.3) function:
      rchange2=function(par)
      { hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.3,round=TRUE) }
      sa= optim(solution,fn=evalN,method="SANN", gr=rchange2, control=list(maxit=100,temp=10,tmax=1))
      
      L=abs(sa$value)
      if(L>bestR){
        BESTSA=sa;
        bestR=L;}
    }
    
    cat("Best Solution", BESTSA$par, "Best Profit:", abs(BESTSA$value))
    results = round(BESTSA$par)
  } else if (model == "tabu") {
    # Tabu: ------------------------------
    maxit=100 # 100 searches
    
    b=tabuSearch(size=D,iters=maxit,objFunc=eval,config=solution,neigh=D,listSize=5, nRestarts=5)
    ib=which.max(b$eUtilityKeep) # best index
    cat("tabu b:",b$configKeep[ib,],"f:",b$eUtilityKeep[ib],"\n")
    results = round(b$configKeep[ib,])
  } else if (model == "deo") {
    # Differential Evolution Optimization: -------------------------
    popSize=20 # population size
    iter=100 # maximum number of iterations
    report=FALSE
    
    #CR -> crossover probability 
    #F -> differential weighting factor 
    #trace -> int as report results
    C=DEoptim.control(strategy=2,NP=popSize,itermax=iter,CR=0.9,F=0.8,trace=report,storepopfrom=1,storepopfreq=1)
    
    de=suppressWarnings(DEoptim(fn=evalN,lower=lower,upper=upper,control=C))
    
    # show result:
    showbest("DEoptim",de$optim$bestmem,de$optim$bestval)
    results = round(de$optim$bestmem)
  } else if (model == "rbga") {
    # Genetic Algorithm: ------------------------------
    popSize=20 # population size
    iter=100 # maximum number of iterations
    
    # call to rbga: Genetic Algorithm:
    # note: you can add a monitor in the params
    rga=rbga(lower,upper,popSize=popSize,evalFunc=evalN,iter=iter) 
    # get the best solution:
    PMIN=which.min(rga$evaluations)
    showbest("rbga",rga$population[PMIN,],rga$evaluations[PMIN])
    results = round(rga$population[PMIN,])
  } else if (model == "pco") {
    # Particle Swarm Optimization: ---------------------------------
    popSize=20 # population size
    iter=100 # maximum number of iterations
    report=iter+1
    
    ps=psoptim(par=lower,fn=evalN,lower=lower,upper=upper,control=list(trace=1,REPORT=report,maxit=iter,s=popSize))
    
    # get the best solution:
    showbest("psoptim",ps$par,ps$value)
    results = round(ps$par)
  } else if (model == "hga") {
    popSize=100 # population size
    iter=30 # maximum number of iterations{
    optimArgs = list(method = "SANN", 
                     poptim = 0.5,
                     pressel = 0.8,
                     control = list(maxit=100,temp=10,tmax=1,fnscale = -1)) #fnscale = -1 maximização
    
    GA <- ga(type = "binary", 
             fitness = eval,
             lower = lower, upper = upper, nBits = 35,
             popSize = popSize, maxiter = iter,
             optim = TRUE, optimArgs = optimArgs, elitism=10, seed=10)
    
    results <- as.vector(GA@solution)
  }
  
  if (obj == "2") {
    results =  repairMethod(results)
  }
  
  boolDf <- formatCamp(pred, results, days)
  strDf <- formatCamp2(pred, results, days)
  s <- sum(sales(pred, results))
  c <- sum(costs(pred, results))
  p <- s-c
  return(list(sol=results, profit=round(p,2), sales=round(s,2), costs=round(c,2), boolDf=boolDf, strDf=strDf, GA=GA))
}

#example: paretoApproach(pred, days, "pdf")
paretoApproach = function(pred, days, output="image") {
  profitObj1=function(x) 
  { 
    x = round(x)
    s = sum(sales(pred, x))
    cost = sum(costs(pred, x))
    p=s-cost
    return(p)
  }
  evalM=function(x) c(sum(round(x)), -profitObj1(round(x)))
  
  D = length(pred$all) + length(pred$female) + length(pred$male) + length(pred$young) + length(pred$adult) # get the size of the solution array
  lower=rep(0,D) # lower bounds
  upper=rep(1,D) #  upper bounds
  
  m=2 # there are 2 objectives
  G=nsga2(fn=evalM,idim=D,odim=m, lower.bounds=lower,upper.bounds=upper,popsize=20,generations=1:100)
  
  # show best individuals:
  I=which(G[[100]]$pareto.optimal)
  for(i in I) {
    x=round(G[[100]]$par[i,],digits=0); cat(x)
    cat(" f=(",round(evalM(x)[1],2),",",abs(round(evalM(x)[2],2)),")", "\n",sep="")
  }
  
  # create file with Pareto front evolution:
  if (output == "pdf") {
    filePath = "add-ons/images/nsga_pareto_graph.pdf"
    pdf(file=filePath,paper="special",height=5,width=5)
  } else {
    filePath = "add-ons/images/nsga_pareto_graph.jpg"
    jpeg(filePath, width = 600, height = 600)
  }
  
  par(mar=c(4.0,4.0,0.1,0.1))
  I=1:100
  for(i in I){ 
    G[[i]]$value[,2] <- abs(G[[i]]$value[,2])
    P=G[[i]]$value # objectives f1 and f2
    
    #color from light gray (75) to dark (1):
    COL=paste("gray",round(76-i*0.75),sep="")
    if(i==1) plot(P,xlim=c(0,35),ylim=c(0,3000), xlab="Campaigns",ylab="Profit",cex=1,col=COL)
    Pareto=P[G[[i]]$pareto.optimal,]
    
    # sort Pareto according to x axis:
    points(P,type="p",pch=1,cex=1,col=COL)
    if(is.matrix(Pareto)){ # if Pareto has more than 1 point
      I=sort.int(Pareto[,1],index.return=TRUE)
      Pareto=Pareto[I$ix,]
      lines(Pareto,type="l",cex=1,col=COL)
    }
  }
  dev.off()
  
  lastGen <- G[[100]]
  index <- which.max(lastGen$value[,2])
  sol <- round(lastGen$par[index,],digits=0)
  boolDf <- formatCamp(pred, sol, days)
  strDf <- formatCamp2(pred, sol, days)
  s <- sum(sales(pred, sol))
  c <- sum(costs(pred, sol))
  p <- s-c
  return(list(sol=sol, profit=round(p,2), sales=round(s,2), costs=round(c,2), boolDf=boolDf, strDf=strDf, filePath=filePath))
}

showbest=function(method,par,eval){ 
  cat("method:",method,"\n > par:",round(par),"\n > eval:",abs(eval),"\n") }

formatCamp = function (pred, solution, days) {
  sol <- solution
  mat = matrix(ncol = 8, nrow = 0)
  results=data.frame(mat)
  colnames(results) <- c("Campaigns", days)
  
  init = 1
  end = 7
  if (length(pred$all)>0) {
    results[nrow(results) + 1,] = c("all",sol[init:end])
    init = init + 7
    end = end + 7
  }
  if (length(pred$female)>0) {
    results[nrow(results) + 1,] = c("female",sol[init:end])
    init = init + 7
    end = end + 7
  }
  if (length(pred$male)>0) {
    results[nrow(results) + 1,] = c("male",sol[init:end])
    init = init + 7
    end = end + 7
  }
  if (length(pred$young)>0) {
    results[nrow(results) + 1,] = c("young",sol[init:end])
    init = init + 7
    end = end + 7
  }
  if (length(pred$adult)>0) {
    results[nrow(results) + 1,] = c("adult",sol[init:end])
    init = init + 7
    end = end + 7
  }
  results
}

formatCamp2 = function (pred, solution, days) {
  sol <- solution
  s <- sales(pred, sol)
  c <- costs(pred, sol)
  mat = matrix(ncol = 8, nrow = 0)
  results=data.frame(mat)
  colnames(results) <- c("campaign", days)
  
  getStr = function(){
    sol2 = sol[init:end]
    s2 = s[init:end]
    c2 = c[init:end]
    row = c()
    for (i in 1:length(sol2)) {
      prof = round(s2[i]-c2[i],2)
      
      if (sol2[i] == 1)
        if (prof > 0)
          cell = paste("+",prof, "€")
        else
          cell = paste(prof, "€")
      else 
        cell = "N/A"
      
      row = c(row, cell)
    }
    return(row)
  }
  
  init = 1
  end = 7
  if (length(pred$all)>0) {
    results[nrow(results) + 1,] = c("all",getStr())
    init = init + 7
    end = end + 7
  }
  if (length(pred$female)>0) {
    results[nrow(results) + 1,] = c("female",getStr())
    init = init + 7
    end = end + 7
  }
  if (length(pred$male)>0) {
    results[nrow(results) + 1,] = c("male",getStr())
    init = init + 7
    end = end + 7
  }
  if (length(pred$young)>0) {
    results[nrow(results) + 1,] = c("young",getStr())
    init = init + 7
    end = end + 7
  }
  if (length(pred$adult)>0) {
    results[nrow(results) + 1,] = c("adult",getStr())
    init = init + 7
    end = end + 7
  }
  return(results)
}