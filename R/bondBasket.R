setClass(Class="bondBasket",
    contains = "list"
)

newBondBasket<-function() return(new("bondBasket"))
# Operacje na basketach - dodawanie nowej obligacji do koszyka to
# zwyczajne bondBasket + bond

setMethod("+",signature(e1="bondBasket", e2 = "bond"), 
  function(e1,e2)
  {
    e1[[(length(e1)+1)]] = e2
    return(e1)
  }
)

setMethod("+",signature(e1="bond", e2 = "bondBasket"), 
          function(e1,e2)
          {
            e2[[(length(e2)+1)]] = e1
            return(e2)
          }
)

setMethod("+",signature(e1="bond", e2 = "bond"), 
          function(e1,e2)
          {
            basket = new("bondBasket")
            basket = basket + e1
            basket = basket + e2
            return(basket)
          }
)

setMethod("+",signature(e1="bondBasket", e2 = "bondBasket"), 
          function(e1,e2)
          {
            c(e1,e2)
          }
)

