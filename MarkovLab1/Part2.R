simulateFlips <- function(n, p) {
  sspace <- c("H","T")
  simFlips <- sample(sspace, 
                     size = n, 
                     replace = TRUE, 
                     prob = c(p, 1 - p)) 
  return(simFlips)
}

LongestTail <- function(n, q) {
  simFlips <- simulateFlips(n, 1 - q)
  
  longestStreak <- 0
  curStreak <- 0
  for (i in simFlips) {
    if (i == "H") { # Heads
      if (curStreak > longestStreak) {
        longestStreak <- curStreak
      }
      curStreak <- 0
    }
    else { # Tails
      curStreak <- curStreak + 1
    }
  }
  return(longestStreak)
}

Part3 <- function(){
  sim <- replicate(10000,LongestTail(250,0.6))
  
  hist(sim, 
       main="10K Simulations of Tn", 
       xlab="Longest Tail Run", 
       border="black", 
       col="white",
       xlim=c(0,35),
       ylim=c(0,2000),
       las=1, 
       breaks=20)
}
