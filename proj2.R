# Aidan Garrity (s1997567), Eleni Michaelidou (s2226022), Meshari Alrafidi (s2319759)

# Brief description of what each team member contributed to the project:

# Proportion of the work was undertaken by each team member: Aidan Garrity: 
#                                                            Eleni Michaelidou: 
#                                                            Meshari Alrafidi: 


# -------------------- Overview of the code --------------------

# We simulate the following puzzle: 2n prisoners are given a natural number to identify them, they enter a room 
# with 2n boxes, each box containing a number inside it and a number on it's lid. The prisoners' goal is to select the 
# box with their number in it. They are allowed to choose a maximum of n boxes. Three strategies are simulated here:
# Strategy 1. A prisoner selects the box with their number on the lid. If the number of the card inside the lid, k, is not their 
# number they go to box k and open it. They continue doing this until they either have found their number or have chosen 
# n boxes. 
# Strategy 2. The prisoner chooses a random box then repeats the steps in strategy 1.
# Strategy 3. n boxes are checked randomnly 

#---------------------Part One----------------------------------

Pone <- function(n, k, strategy, nreps){
  # This function estimates the probability a single prisoner succeeding in finding their number.
  # Input:  n: the maximum number of boxes the prisoner is allowed to open
  #         k: the prisoner’s number
  #         strategy: which strategy to implement {1,2,3}
  #         nreps: the number of iterations of the simulations to run (in order to estimate the probability)
  # Output: prob: probability of a single prisoner succeeding in finding their number.
  if (strategy == 1) {
    initial_box <- k
  }
  else if (strategy ==2) {
    initial_box <- sample(1:2*n, size=1)
  }
  
  # strategy three: n boxes are opened randomly
  else if (strategy == 3){
    
    # number_found represents the number found inside of the box
    number_found <- 0 
    for (i in 1:nreps){ 
      
      # we create 2n boxes, n of which can be opened
      boxes <- sample(1:(2*n), n)
      
      # if the prisoner's number is selected... 
      if (k %in% boxes){
        
        # counter increases
        number_found <- number_found + 1
      }
    }
    # probability estimate of success
    return(number_found / nreps)
  }
}

  