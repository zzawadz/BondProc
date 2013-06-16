getYTM <- function(bond ,bond.price = NULL, r = NULL)
{
  if(is.null(bond.price))
  {
    if(!is.null(r)) bond.price = bondPrice(r,bond)
    else bond.price = bond@price
    
    if(length(bond.price)==0) stop("Nie podano ceny obligacji, ani stopy do jej obliczenia w YTM")
  }
  
  YTM <- function(ytm)
  {
    bondPrice(ytm,bond)-bond.price
  }
  
  uniroot(YTM,interval=c(0,1))$root
}

