

#year = "2013"
#month = "06"  
#day = "27"

getBondSpotData <-function(day,month,year)
{
  library(XML)
  
  url = paste("http://www.bondspot.pl/notowania_TBSPoland?date=",year,month,day,"&month=",month,"&year=",year, sep = "")
  tables <- readHTMLTable(url)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  
  result = list()
  
  length(tables)
  
  tab = tables[[5]]
  
  start = which(tab[,1]=="(%)")+1
  end   = which(tab[,1]=="RAZEM")-1
  
  col.names = tab[start-2,]
  tab = tab[start:end,]
  colnames(tab) <- c("Nazwa","ISIN",col.names[!is.na(col.names)])
  
  result[[1]] = tab
  return(tab)
}
    
#getBondSpotData("26",month,year)
