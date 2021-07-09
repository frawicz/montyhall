rm(list = ls())

library(tidyverse)


set.seed(91)

montyhall <- function(n, change){
  choicefin <- NULL
  carplacefin <- NULL
  dooropenedfin <- NULL
  finaldoorfin <- NULL
  
  number <- 0
  
  while (number < n) {
    choice <- sample(c(1:3), 1)
    carplace <- sample(c(1:3), 1)

    if(choice == carplace){
    
    dooropened <- sample(dplyr::setdiff(c(1:3),carplace),1)
  
    } else {
      dooropened <- dplyr::setdiff(c(1:3), c(carplace, choice))
    }
    
    if(change == T){
      
      finaldoor <- dplyr::setdiff(c(1:3), union(choice, dooropened))
      
    } else {
      
      finaldoor <- choice
    }
    
    if(dooropened != choice){
    choicefin <- c(choice, choicefin)
    carplacefin <- c(carplace, carplacefin)
    dooropenedfin <- c(dooropened, dooropenedfin)
    finaldoorfin <- c(finaldoor, finaldoorfin)
    
    number <- number + 1
    }
  }
  finaldf <- data.frame(choicefin, carplacefin, dooropenedfin , finaldoorfin) %>% 
    mutate(Winner = ifelse(carplacefin == finaldoorfin, "Winner", "Looser"))
  
  return(finaldf)
}



dontchange <- montyhall(10000, F)

table(dontchange$Winner)



change <- montyhall(10000, T)

table(change$Winner)


