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
# Strategy 3. n boxes are checked randomly 

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
    prob <- following_numbers(n, k, initial_box, nreps)
    
  }
  else if (strategy ==2) {
    
    initial_box <- sample(1:(2*n), size=1)
    prob <- following_numbers(n, k, initial_box, nreps)
    
  }
  else if (strategy == 3){
    
    prob <- picking_randomly(n, k, nreps)
    
  }
  return(prob)
}

Pall <- function(n, strategy, nreps){
  # This function estimates the probability of all prisoners finding their numbers.
  # Input:  n: the maximum number of boxes a prisoner is allowed to open
  #         strategy: which strategy to implement {1,2,3}
  #         nreps: the number of iterations of the simulations to run (in order to estimate the probability)
  # Output: prob: probability of all prisoners succeeding in finding their number.
  
  # Assigning a unique prisoner number to each of the 2n prisoners.
  prisoners <- c(1:(2*n))
  
  
  if (strategy == 1) {
    
    initial_box <- prisoners
    prob <- following_numbers(n, prisoners, initial_box, nreps)
    
  }
  else if (strategy ==2) {
    
    initial_box <- sample(1:(2*n), size=(2*n))
    prob <- following_numbers(n, prisoners, initial_box, nreps)
    
  }
  else if (strategy == 3){
    
    prob <- picking_randomly(n, prisoners, nreps)
    
  }
  return(prob)
}


#######################################################################################

following_numbers <- function(n, prisoners, initial_box, nreps){
  # This function estimates the probability of a single prisoner succeeding in finding their number.
  # The prisoner opens boxes by following the numbers on the card within the boxes.
  # Input:  n: the maximum number of boxes the prisoner is allowed to open
  #         k: the prisoner’s number
  #         nreps: the number of iterations of the simulations to run (in order to estimate the probability)
  # Output: prob: probability of a single prisoner succeeding in finding their number.

  
  # success_trial represents the number of times all prisoners (participating in the experiment) finding their number.
  # Initialization of "success_trial"
  success_trial <- 0  
  

  # Running the experiment nreps times
  for (i in 1:nreps){
    
    # number_found represents the number that a card found inside of the box.
    # Initialization of "number_found"
    number_found <- 0 
    
    # Assign numbers (1 to 2*n) randomly within the 2*n boxes before starting the experiment.
    # Random placement of numbers within the boxes in each simulation.
    boxes <- sample(1:(2*n), 2*n, replace=F)

    pick <- c()
    
    for (k in 1:length(prisoners)){

      # Initial box for the experiment based on the strategy for each prisoner
      pick <- initial_box[k]

      # The prisoner repeat the process of opening boxes until they have either found the card
      # with their number on it (break the loop) or opened n boxes without finding it.
      for (trial in 1:n){
        
        # Exit the loop when the prisoner finds the card in the box with their number.
        if (boxes[pick] == prisoners[k]){
          number_found <- number_found + 1
          break
        }else{
          pick = boxes[pick]
        }
      }
    }
    
    # A trial considers as success if all prisoners participating in the experiment find their number.
    if (number_found == length(prisoners)){
      success_trial <- success_trial + 1
    }
  }
  
  # probability of success (the prisoner(s) find(s) the card with their number on it).
  prob = success_trial / nreps
  return(prob)
}

picking_randomly <- function(n, prisoners, nreps){
  # This function estimates the probability of a single prisoner succeeding in finding their number.
  # The prisoner opens n boxes at random, checking each card for their number.
  # Input:  n: the maximum number of boxes the prisoner is allowed to open
  #         k: the prisoner’s number
  #         nreps: the number of iterations of the simulations to run (in order to estimate the probability)
  # Output: prob: probability of a single prisoner succeeding in finding their number.
  
  # success_trial represents the number of times all prisoners (participating in the experiment) finding their number.
  # Initialization of "success_trial"
  success_trial <- 0  
  

  # Running the experiment nreps times
  for (i in 1:nreps){
    
    # number_found represents the number that a card found inside of the box.
    # Initialization of "number_found" in each repetition
    number_found <- 0
    
    for (k in 1:length(prisoners)){
      
      # we create 2n boxes, n of which can be opened
      boxes <- sample(1:(2*n), n)
      
      # if the prisoner's number is selected... 
      if (k %in% boxes){
        
        # counter increases
        number_found <- number_found + 1
      }
    }
    
    # A trial considers as success if all prisoners participating in the experiment find their number.
    if (number_found == length(prisoners)){
      success_trial <- success_trial + 1
    }
  }  
  # probability of success (the prisoner(s) find(s) the card with their number on it).
  prob = number_found / nreps
  return(prob)
}


# Example estimating the individual success probability under Strategy 1, n =100
Pone(n = 100, k = 4, strategy = 1, nreps = 1000)

# Example estimating the individual success probability under Strategy 2, n =100
Pone(n = 100, k = 4, strategy = 2, nreps = 1000)

# Example estimating the individual success probability under Strategy 3, n =100
Pone(n = 100, k = 4, strategy = 3, nreps = 1000)


# Example estimating the joint success probability under Strategy 1, n =5
Pall(n = 100, strategy = 1, nreps = 1000)

# Example estimating the joint success probability under Strategy 2, n =5
Pall(n = 100, strategy = 2, nreps = 1000)

# Example estimating the joint success probability under Strategy 3, n =5
Pall(n = 100, strategy = 3, nreps = 1000)


###########################################################

#Pone <- function(n, k, strategy, nreps){
  # This function estimates the probability a single prisoner succeeding in finding their number.
  # Input:  n: the maximum number of boxes the prisoner is allowed to open
  #         k: the prisoner’s number
  #         strategy: which strategy to implement {1,2,3}
  #         nreps: the number of iterations of the simulations to run (in order to estimate the probability)
#  # Output: prob: probability of a single prisoner succeeding in finding their number.
#  if (strategy == 1) {
#    number_found <- 0
#    next_box <- k
#    for (i in 1:nreps){
#      boxes <- sample(1:(2*n), size = 1)
#      if (which( boxes == k ) == k){
#        number_found <- number_found + 1
#      } else {
#        next_box <- boxes 
#      }
#    }
#  }
#  else if (strategy ==2) {
#    initial_box <- sample(1:2*n, size=1)
#  }
#  
#  # strategy three: n boxes are opened randomly
#  else if (strategy == 3){
#    
#    # number_found represents the number found inside of the box
#    number_found <- 0 
#    for (i in 1:nreps){ 
#      
#      # we create 2n boxes, n of which can be opened
#      boxes <- sample(1:(2*n), n)
#      
#      # if the prisoner's number is selected... 
#      if (k %in% boxes){
#        
#        # counter increases
#        number_found <- number_found + 1
#      }
#    }
#    # probability estimate of success
#    return(number_found / nreps)
#  }
#}

dloop <- function(n, nreps){
  # Each box is connected to another box by the number inside it, forming a chain or loop of boxes.
  # This function estimates the probability of all possible loops of length  1 to 2n occuring
  # at least once in random permutatation of boxes and and cards. 
  # Inputs: n: maximum number of boxes a prisoner is allowed to open
  #         nreps: number of iterations
  # Outputs: 2n-vector of probabilities
  
  # create 2n boxes 
  u <- sample(1:(2*n))
  
  # create 2n cards
  k <- sample(1:(2*n))
  
  for (i in (1:(2*n))){
    
    depth <- 0
    # checking first box
    if (u[i] == k){
      depth <- depth + 1
    }
    #checking next box
    else{
      while (u[i] != k){
        nxt <- u[i]
      }
    }
  }
}


  