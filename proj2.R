Pone <- function(n, k, strategy, nreps){
  # Function to estimate the probability a single prisoner succeeding in finding their number.
  # Input:  n: a natural number
  #         k: the prisoner’s number
  #         strategy: integer, taking values {1,2,3} 
  #         nreps.nreps: the number of replicate simulations to run in order to estimate the probability
  # Output: prob: brobability of a single prisoner succeeding in finding their number.
  if (strategy == 1) {
    initial_box <- k
  }
  else if (strategy ==2) {
    initial_box <- sample(1:2*n, size=1)
  }
  else if (strategy == 3){
    number_found <- 0
    for (i in 1:nreps){
      boxes <- sample(1:(2*n), n)
      if (k %in% boxes){
        number_found <- number_found + 1
      }
    }
    return(number_found / nreps)
  }
}

  