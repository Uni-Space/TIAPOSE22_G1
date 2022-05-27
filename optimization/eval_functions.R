# all, female, male, young, adult
campCosts = c(350, 150, 100, 100, 120)
minSales = c(0.06, 0.08, 0.04, 0.04,0.08)
maxSales = c(0.09, 0.13, 0.07, 0.05, 0.12)

# Evaluation Functions: ------------------------------
profitObj1=function(x) 
{ 
  x = round(x)
  s = sum(sales(pred, x))
  cost = sum(costs(pred, x))
  p=s-cost
  return(p)
}

# death penalty method
profitObj2=function(x) 
{ 
  x = round(x)
  s = sum(sales(pred, x))
  cost = sum(costs(pred, x))
  p=s-cost
  camp=sum(x==1)
  if (camp>10) {
    p=-999
  }

  return(p)
}

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

sales=function(pred, x)
{
  s = c()
  if (length(pred$all)>0) {
    s = c(s, ifelse(pred$all<5000, pred$all*minSales[1], pred$all*maxSales[1]))
  }
  if (length(pred$female)>0) {
    s = c(s, ifelse(pred$female<1800, pred$female*minSales[2], pred$female*maxSales[2]))
  }
  if (length(pred$male)>0) {
    s = c(s, ifelse(pred$male<1800, pred$male*minSales[3], pred$male*maxSales[3]))
  }
  if (length(pred$young)>0) {
    s = c(s, ifelse(pred$young<3000, pred$young*minSales[4], pred$young*maxSales[4]))
  }
  if (length(pred$adult)>0) {
    s = c(s, ifelse(pred$adult<800, pred$adult*minSales[5], pred$adult*maxSales[5]))
  }
  s = x*s
  return(s)
}

costs=function(pred, x)
{
  cost = c(rep(campCosts[1],length(pred$all)), rep(campCosts[2],length(pred$female)), rep(campCosts[3],length(pred$male)), rep(campCosts[4],length(pred$young)), rep(campCosts[5],length(pred$adult)))
  cost = x*cost
  return(cost)
}

# Report Function: ------------------------------
formatCampaigns = function (pred, solution, days) {
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