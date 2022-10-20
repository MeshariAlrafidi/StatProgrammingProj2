# Aidan Garrity (s1997567), Eleni Michaelidou (s2226022), Meshari Alrafidi (s2319759)

# Brief description of what each team member contributed to the project:

# Proportion of the work was undertaken by each team member: 
#     Aidan Garrity: dloop final version, plotting, strategy 3, dloop explanations
#     Eleni Michaelidou: strategy_selection function, following_numbers function, Pone, Pall, example code, Pall explanations
#     Meshari Alrafidi: previous iterations of dloop, explanations, 


# -------------------- Overview of the code --------------------

# In this code we implement a stochastic simulation to investigate the following  puzzle:
# 2n prisoners are given a natural number to identify them. They enter a room 
# with 2n boxes, each box containing a number inside it and a number on it's lid. The prisoners' goal is to select the 
# box with their number in it. They are allowed to choose a maximum of n boxes.

# Three strategies are simulated here:
    # Strategy 1. A prisoner selects the box with their number on the lid. If the number of the card inside the lid, k, is not their 
    # number, they go to box k and open it. They continue doing this until they either have found their number or have chosen 
    # n boxes.
    # Strategy 2. The prisoner chooses a random box then repeats the steps in strategy 1.
    # Strategy 3. n boxes are checked randomly

# If all prisoners succeed in finding their number, they are all released.

# The prisoner who attempted to find their number is not allowed to communicate with prisoners yet to have their go.
# After each prisoner’s go, the room is returned exactly to its original state.

# -------------------------------------------------------------


Pone <- function(n, k, strategy, nreps=10000){
  # This function estimates the probability a single prisoner succeeding in finding their number.
  # Input:  n: the maximum number of boxes the prisoner is allowed to open.
  #         k: the prisoner’s number.
  #         strategy: which strategy to implement {1,2,3}.
  #         nreps (10000 by default): the number of iterations of the simulations to run (in order to estimate the probability)
  # Output: prob: probability of a single prisoner succeeding in finding their number under the given strategy.
  
  # Apply function "strategy_selection" to estimate the individual success probability under the given strategy.
  prob <- strategy_selection(n, k, strategy, nreps)
  
  return(prob)
}

Pall <- function(n, strategy, nreps=10000){
  # This function estimates the probability of all prisoners finding their numbers.
  # Input:  n: the maximum number of boxes a prisoner is allowed to open.
  #         strategy: which strategy to implement {1,2,3}.
  #         nreps (10000 by default): the number of iterations of the simulations to run (in order to estimate the probability).
  # Output: prob: probability of all prisoners succeeding in finding their number.
  
  # Assigning a unique prisoner number to each of the 2n prisoners.
  prisoners <- c(1:(2*n))
  
  # Apply function "strategy_selection" to estimate the joint success probability under the given strategy.
  prob <- strategy_selection(n, prisoners, strategy, nreps)
  
  return(prob)
}


strategy_selection <- function(n, prisoners, strategy, nreps){
  # This function is responsible to call the corresponding function based on the given strategy. It returns the probability derived from the given strategy.
  # Input:  n: the maximum number of boxes a prisoner is allowed to open.
  #         prisoners: a vector of prisoners' numbers
  #                    (It can be a vector length 1 -> prisoners = the prisoner's number k (call from Pone function)
  #                    or a vector length 2n -> all prisoners' number (call from Pall function).
  #         strategy: which strategy to implement {1,2,3}.
  #         nreps: the number of iterations of the simulations to run (in order to estimate the probability).
  # Output: prob: probability of success derived from the given strategy.
  
  if (strategy == 1) {
    
    # The prisoner(s) select(s) the box with their number on the lid as their first box.
    initial_box <- prisoners
    
    # Apply function "following_numbers" to estimate the success probability under strategy 1 (initial_box = prisoners).
    prob <- following_numbers(n, prisoners, initial_box, nreps)
    
  }
  else if (strategy ==2) {
    
    # The prisoner(s) select(s) the first box randomly.
    # Two prisoners can start from the same box (replace = TRUE)
    initial_box <- sample(1:(2*n), size=length(prisoners), replace = TRUE)
    
    # Apply function "following_numbers" to estimate the success probability under strategy 2 (initial_box = randomly selected).
    prob <- following_numbers(n, prisoners, initial_box, nreps)
    
  }
  else if (strategy == 3){
    
    # Apply function "picking_randomly" to estimate the success probability under strategy 3.
    prob <- picking_randomly(n, prisoners, nreps)
    
  }
  return(prob)
}


following_numbers <- function(n, prisoners, initial_box, nreps){
  # This function estimates the probability of the prisoner(s) succeeding in finding their number.
  # (Each) prisoner opens boxes by following the numbers on the card within the boxes, entering a closed loop.
  # Input:  n: the maximum number of boxes the prisoner is allowed to open.
  #         prisoners: prisoners' numbers.
  #         initial_box: box number that (each) prisoner will open first.
  #         nreps: the number of iterations of the simulations to run (in order to estimate the probability)
  # Output: prob: the probability of (all) the prisoner(s) succeeding in finding their number, by opening up to n boxes in a closed loop.
  
  
  # success_simulation represents the number of times all prisoners (participating in the experiment) find their boxes.
  # Initialization of "success_simulation".
  success_simulation <- 0  
  
  
  # Running the experiment nreps times.
  for (i in 1:nreps){
    
    # boxes_found represents the number of prisoners found their boxes in each simulation.
    # Reset of "boxes_found" in each repetition.
    boxes_found <- 0 
    
    # Assign numbers (1 to 2*n) randomly within the 2*n boxes before starting the experiment.
    # Random placement of numbers within the boxes in each simulation.
    boxes <- sample(1:(2*n), 2*n, replace=FALSE)
    
    # Loop through the prisoners vector (all prisoners try to find their box).
    for (k in 1:length(prisoners)){
      
      # Boolean to terminate the run of a single simulation if one prisoner fails finding their number.
      prisoner_succ <- FALSE
      
      # Initial box for the prisoner running the experiment.
      next_box <- initial_box[k]

      # The prisoner repeat the process of opening boxes until they have either found the card
      # with their number on it (break the loop) or opened n boxes without finding it.
      for (trial in 1:n){
        
        # Exit the loop when the prisoner finds the card with their number in the boxes.
        if (boxes[next_box] == prisoners[k]){
          
          # counter increases by 1, since the prisoner running the experiment succeeds in finding their number. 
          boxes_found <- boxes_found + 1
          
          # Set to TRUE since the prisoner found their number.
          prisoner_succ <- TRUE
          break
          
        }else{
          
          # The prisoner goes to the next box, by following the card number. 
          next_box = boxes[next_box]
        }
      }
      
      # Exit the loop when one prisoner fails in finding their box.
      # prisoner_succ will be FALSE only if a prisoner fails to find their number in the boxes.
      if (!(prisoner_succ)){
        break
      }
    }
    
    # A simulation is successful if all prisoners participating in the experiment find their number.
    if (boxes_found == length(prisoners)){
      success_simulation <- success_simulation + 1
    }
  }
  
  # probability of success (the prisoner(s) succeed(s) in finding their number).
  prob = success_simulation / nreps # number of successful simulations / total number of simulations.
  
  return(prob)
}


picking_randomly <- function(n, prisoners, nreps){
  # This function estimates the probability of the prisoner(s) succeeding in finding their number.
  # (Each) prisoner opens n boxes at random, checking each card for their number.
  # Input:  n: number of boxes (each) prisoner opens.
  #         prisoners: prisoners' numbers.
  #         initial_box: box number that (each) prisoner will open first.
  #         nreps: the number of iterations of the simulations to run (in order to estimate the probability)
  # Output: prob: the probability of (all) the prisoner(s) succeeding in finding their number, by opening n boxes randomly.
  
  
  # success_simulation represents the number of times all prisoners (participating in the experiment) finding their boxes.
  # Initialization of "success_simulation"
  success_simulation <- 0  
  
  # Running the experiment (simulation) nreps times.
  for (i in 1:nreps){
    
    # boxes_found represents the number of prisoners found their boxes in each simulation.
    # Reset of "boxes_found" in each repetition.
    boxes_found <- 0 
    
    for (k in 1:length(prisoners)){
      
      prisoner_succ <- FALSE
      
      # we create 2n boxes, n of which can be opened.
      boxes <- sample(1:(2*n), n)
      
      # if the prisoner's number is in the randomly selected boxes... 
      if (k %in% boxes){
        
        # counter increases
        boxes_found <- boxes_found + 1
        
        prisoner_succ <- TRUE
      }
      
      # Exit the loop when one prisoner fails in finding their box.
      if (!(prisoner_succ)){
        break
      }
    }
    
    # A simulation is successful if all prisoners participating in the experiment find their box.
    if (boxes_found == length(prisoners)){
      success_simulation <- success_simulation + 1
    }
  }
  
  # probability of success (the prisoner(s) succeed(s) in finding their number).
  prob = success_simulation / nreps # number of successful simulations / total number of simulations.
  
  return(prob)
}


# ---------- Example code ----------

# ---- Individual success probability ----

# Example estimating the individual success probability under Strategy 1, n =5 (nreps = 10000 by default)
Pone(n = 5, k = 4 , strategy = 1)

# Example estimating the individual success probability under Strategy 1, n =50 (nreps = 10000 by default)
Pone(n = 50, k = 4, strategy = 1)

# Example estimating the individual success probability under Strategy 2, n =5 (nreps = 10000 by default)
Pone(n = 5, k = 4, strategy = 2)

# Example estimating the individual success probability under Strategy 2, n =50 (nreps = 10000 by default)
Pone(n = 50, k = 4, strategy = 2)

# Example estimating the individual success probability under Strategy 3, n =5 (nreps = 10000 by default)
Pone(n = 5, k = 4, strategy = 3)

# Example estimating the individual success probability under Strategy 3, n =50 (nreps = 10000 by default)
Pone(n = 50, k = 4, strategy = 3)

# Computing the individual success probability under strategies 1 and 3 for a different number of 
# boxes (2n boxes): n = 5, and n = 50, the probability of success is almost 1/2 in all those cases.
# On the other hand, the individual success probability under strategy 2
# for n = 5 and n = 50 is less than or equal to the prob. of success under strategies 1 and 3.

# ---- Joint success probability ----

# Example estimating the joint success probability under Strategy 1, n =5 (nreps = 10000 by default)
Pall(n = 5, strategy = 1)

# Example estimating the joint success probability under Strategy 1, n =50 (nreps = 10000 by default)
Pall(n = 50, strategy = 1)

# Example estimating the joint success probability under Strategy 2, n =5 (nreps = 10000 by default)
Pall(n = 5, strategy = 2)

# Example estimating the joint success probability under Strategy 2, n =50 (nreps = 10000 by default)
Pall(n = 50, strategy = 2)

# Example estimating the joint success probability under Strategy 3, n =5 (nreps = 10000 by default)
Pall(n = 5, strategy = 3)

# Example estimating the joint success probability under Strategy 3, n =50 (nreps = 10000 by default)
Pall(n = 50, strategy = 3)

# Computing the joint success probability under strategy 1 for a different number of boxes (2n boxes):
# n = 5, and n = 50, the probability of success is close to 35% and 31% respectively. 
# On the other hand, the joint probability under strategies 2 and 3 for different number of boxes (2n boxes):
# n = 5 and n = 50 is close to 0.

# So, what is surprising is that it is more likely that 100 (n = 50) prisoners
# will release (all prisoners find their number) under Strategy 1
# than even 10 (n = 5) prisoners open boxes under Strategies 2 and 3.

# -----

# As explained above (individual success probability under strategy 1), given 
# that each box is a part of a closed loop and each prisoner starts at the box 
# with their number on it, the prisoner will succeed if their number is a part 
# of a loop no longer than n, and that is because each prisoner can open up to n
# of the 2n boxes. The probability of success for prisoners who their box are 
# part of the same loop are not independent, since if a prisoner succeed to find
# their box, then all prisoners on that loop will find their box with the same probability.

# We also know that the "the prisoner is not allowed to communicate with prisoners yet to have their go".
# In addition, under strategy 3, each prisoner opens n boxes randomly. 
# So, the probability of success for each prisoner is independent.
# Individual success probability under Strategy 3 = 1/2  and attempts are 
# independent => P((2n) prisoners succeed)) = (1/2)^(2n)
# Running the code for n = 5 and and n = 50, we find that:
# n=50: P(100 prisoners succeed) = (1/2)^100  = close to 0.
# Even for small n: e.g. n = 5: P(10 prisoners succeed) = (1/2)^10 = extremely small.



dloop <- function(n, nreps){
  # This function estimates the probability that a cycle of any length will occur 
  # in a permutation of length 2n.
  # Input: n: The size of the permutation to be considered (i.e. number of prisoners, so 2n boxes are considered)
  #        nreps: The amount of simulations to be run to estimate the probability vector.
  # Output: a vector of length 2n where the ith entry is the estimated probability that a cycle of length i will occur
  #         in a randomly generated permutation of length 2n.
  # Runtime: O(n * nreps)
  
  # Tracker for all cycles found.
  totalcount <- rep(0, 2*n)
  
  # Running nreps simulations.
  for (rep in 1:nreps){
    
    # Generating the boxes and cards for this simulation.
    boxes <- sample(1:(2*n))
    # Tracker for the cycles of this loop only.
    loopcount <- rep(0, 2*n)
    # Vector containing which boxes have NOT been opened
    left_to_check <- 1:(2*n)
    
    # Starting with the first box, checking all boxes for a cycle.
    # After a box has been found to be part of a cycle, it is removed from left_to_check.
    i<-1
    while (length(left_to_check) > 0) {
      
      # counter tracks the length of the current cycle.
        counter <- 1
      # j is a placeholder for opening the next box in a cycle.
        j <- i
      # tracking which boxes were part of this cycle, to be removed from left_to_check
        checked <- c()
      # iterating over all boxes in a cycle, incrementing the counter and updating the checked boxes list.
        while(i != boxes[j]){
          counter <- counter + 1
          checked <- c(checked, j)
          j <- boxes[j]
        }
        
        checked <- c(checked, j)
        
        # Indicating that a cycle of length counter was found
        # To instead enumerate all cycles in the boxes, this can be changed to 
        # loopcount[counter] <- loopcount[counter] + 1
        loopcount[counter] <- 1
        
        # Removing the checked boxes.
        left_to_check <- left_to_check[-match(checked, left_to_check)]
       
    # Setting the next box to start with for the next cycle. 
      i <- left_to_check[1]
  
    }
    
    totalcount <- totalcount + loopcount
  }
  
  return(totalcount / sum(totalcount))
}

plot(dloop(50,10000), xlab = 'Cycle Length', ylab = 'Probability of Appearing')

# IMPORTANT: The probability of loops larger than 50 appearing are NOT independent events. 
# We cannot take the naive approach and multiply the probability of each cycle 
# length appearing to get the result.
# Instead, we can run many simulations, and for each one track if a long cycle appears.
nreps <- 10000
over50cycles <- 0

# Like running dloop(50, 10000), except at each simulation we track if there were any cycles over 50.
for (i in 1:nreps){
  probs <- dloop(50,1)
  if (1 %in% (probs[51:100]) ){
    over50cycles <- over50cycles + 1
  }
}

none_over_50_prob <- 1 - (over50cycles / nreps)

# Note that the probability of no loop longer than 50 appearing is about the same
# as the probability of all prisoners being freed under strategy 1.
# The difference can be explained by random noise in the box generation.
none_over_50_prob

# End of code