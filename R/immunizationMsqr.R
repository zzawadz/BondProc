immunizationMsqr<-function(basket,r,H)
{
  require(lpSolve)
  
  basket.size = length(basket)
  
  Msqr = 1:basket.size
  prices = 1:basket.size
  duration = 1:basket.size
  
  for(i in 1:basket.size)
  {
    prices[i]   = getBondPrice(basket[[i]],r)
    Msqr[i]     = getMsqr(r,basket[[i]],H,prices[i])
    duration[i] = getDuration(r,basket[[i]])
 #   print(paste(Msqr[i],prices[i],duration[i]))
  }
  
  objective.in = Msqr
  const.mat = rbind(rep(1,length(Msqr)),as.numeric(duration))
  const.rhs = c(1,H)
  const.dir = c("==","==")
  
  optim.solution = lp(objective.in=objective.in,const.mat=const.mat,const.dir=const.dir,const.rhs=const.rhs)
  
  weights = optim.solution$solution
  pMsqr = optim.solution$objval
  result = list(weights = weights,portfolio.Msqr = pMsqr)
  
}