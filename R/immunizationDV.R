immunizationDV<-function(basket,r,H,m = 5)
{
  dur.mat = getVectorDurationMatrix(r,basket,m)
  k = nrow(dur.mat)
  
  
  t.dur.mat = cbind(t(dur.mat), mat.or.vec(m,m),0)
  dur.mat = cbind(diag(k)*2,dur.mat,1)
  last.row = c(rep(1,k),rep(0,m+1))
  
  mat = rbind(dur.mat,t.dur.mat, last.row)
  
  h.mat = c(rep(0,k),rep(H,m)^(1:m),1)
  
  head(solve(mat)%*%h.mat,k)  
}
