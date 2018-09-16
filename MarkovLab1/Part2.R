LongestTail <- function(n, q) {
  i <- 0
  longestStreak <- 0
  curStreak <- 0
  while(i < n){
    flip <- rbinom(1,1,0.5)
    if (flip == 0){
      #Tails
      curStreak <- curStreak + 1
      if(curStreak > longestStreak){
        longestStreak <- curStreak
      }
    }else{
      #Heads
      curStreak <- 0
    }
    i <- i + 1
  }
  return(longestStreak)
}

LongestTail(250, 0.6)
