monthsdays <- function(month){
  
  combos <- data.frame(month = format(ISOdate(2004,1:12,1),"%b"),
                       days = c(31,29,31,30,31,30,31,31,30,31,30,31))
  
  return(combos$days[tolower(combos$month) == tolower(month)])
  
}