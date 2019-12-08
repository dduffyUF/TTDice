##' Roll a dice of your choosing (Description)
##'
##' Roll a dice of your choosing and get a randomly generated number as the result (Details)
##' @title Roll dice of your choosing
##' @param m Dice of your choosing of "D20" - 20 sided dice, "D12" - 12 sided dice, "D10_1" 10 sided dice of values 0-9, "D10_10" 10 sided dice of values 00 - 90 by multiples of 10, "D8" 8 sided dice, "D6" 6 sided dice, "D4" 4 sided dice of values {1 2 3}, {1 2 4}, {1 3 4}, or {2 3 4}
##' @return DiceRoll - a value of a selected dice
##' @author Dustin Duffy
##' @export
##' @examples
##' DiceRoll("D20")
DiceRoll <- function(m) {if(m=='D4'){
  return(sample(c(123, 124, 134, 234), 1, replace=TRUE)) #Returns a roll of the 4 sided die
} else {
  if(m =='D6'){
    return(sample(1:6, 1, replace=TRUE)) #Returns a roll of the 6 sided die
  } else {
    if(m =='D8'){
      return(sample(1:8, 1, replace=TRUE)) #Returns a roll of the 8 sided die
    }
    else {
      if(m =='D10_1'){
        return(sample(0:9, 1, replace=TRUE)) #Returns a roll of the 10 sided die values 0-9
      }
      else {
        if(m =='D10_10'){
          return(sample(c(00, 10, 20, 30, 40, 50, 60, 70, 80, 90), 1, replace=TRUE)) #Returns a roll of the 10 sided die values 00-90 by 10
        }
        else {
          if(m =='D12'){
            return(sample(1:12, 1, replace=TRUE)) #returns a value of the 12 sided die
          }
          else {
            if(m =='D20'){
              return(sample(1:20, 1, replace=TRUE)) #returns a value of the 20 sided die
            }
          }
        }
      }

    }
  }
}
}
