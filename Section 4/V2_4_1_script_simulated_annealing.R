##############################
#### Simulated Annealing #####
##############################

# load library
library(ReinforcementLearning)

# An alternative to Q-learning with "e-greedy"
# action selection is simulated annealing method.

# We summarized the method and will change
# the method learnEpisode() accordingly. 

# We run the new method with the existing
# simulateEnvironment() function.

# Q-Learning in R with previous
# learnEpisode user-defined function
learnEpisode <- function(s_0, s_terminal, epsilon, 
                         learning_rate, Q) {
  state <- s_0 # set cursor to initial state
  while (state != s_terminal) {
    # epsilon-greedy action selection
    if (runif(1) <= epsilon) {
      # pick random action
      action <- sample(actions, 1) 
    } else {
      # pick first best action
      action <- which.max(Q[state, ]) 
    }
    # get next state and reward from environment
    response <- simulateEnvironment(state, action)
    # update rule for Q-learning, updates Q
    Q[state, action] <- Q[state, action] + learning_rate *
      (response$reward + max(Q[response$state, ]) - Q[state, action])
    state <- response$state # move to next state
  }
  # returns updated Q value
  return(Q)
}

# Change the function learnEpisode() to:
learnEpisodeSA <- function(s_0, s_terminal, epsilon, 
                           # n, i are new formal parameters.
                           # See new Qlearning function below
                           learning_rate, Q, n, i) {
  # set cursor to initial state as before
  state <- s_0 
  # instead of e-greedy, incorporate T with 20 episodes
  T <- n / (20*i)
  # while state is not terminal state
  while (state != s_terminal) {
    # Sample action according to boltzmann distribution
    boltzmann_distribution <- exp(Q[state,]/T)
    # Set probabilities using Boltzmann distribution
    probabilities <- boltzmann_distribution / sum(boltzmann_distribution)
    # ?sample
    # probs selecting an action based on Boltzmann distribution
    action <- sample(actions, 1, replace=FALSE, probabilities)
    # get next state and reward from environment
    response <- simulateEnvironment(state, action)
    # update rule for Q-learning
    Q[state, action] <- Q[state, action] + learning_rate *
      (response$reward + max(Q[response$state, ]) - Q[state, action])
    # move to next state
    state <- response$state
  }
  return(Q)
}

# Use much previous R code:

# set actions
actions <- c("up", "left", "down", "right")

# set states
states <- c("s0", "s1", "s2", "s3")

# Same simulate environment function
simulateEnvironment <- function(state, action) {
  # Calculate next state (according to sample grid with wall)
  # Default: remain in a state if action tries to leave grid
  next_state <- state
  if (state == "s0" && action == "down") next_state <- "s1"
  if (state == "s1" && action == "up") next_state <- "s0"
  if (state == "s1" && action == "right") next_state <- "s2"
  if (state == "s2" && action == "left") next_state <- "s1"
  if (state == "s2" && action == "up") next_state <- "s3"
  if (state == "s3" && action == "down") next_state <- "s2"
  # Calculate reward
  if (next_state == "s3") {
    reward <- 10
  } else {
    reward <- -1
  }
  return(list(state=next_state, reward=reward))
}

# Q-learning function is changed
Qlearning <- function(n, s_0, s_terminal,
                      epsilon, learning_rate) {
  # Same initialize state-action function Q to zero
  Q <- matrix(0, nrow=length(states), 
              ncol=length(actions),
              dimnames=list(states, actions))
  # Perform n episodes/iterations using
  # T and Boltzmann distribution
  for (i in 1:n) {
    # Note: are using new learnEpisodeSA() function
    Q <- learnEpisodeSA(s_0, s_terminal,
                        epsilon, learning_rate, 
                        # parameters n, i are added
                        Q, n, i)
  }
  return(Q)
}

# original epsilon value
epsilon <- 0.1

# original learning rate
learning_rate <- 0.1

# set seed
set.seed(0)

# run new Qlearning function
Q <- Qlearning(1000, "s0", "s3", epsilon, learning_rate)
Q
## Note that "best" state-action values are
## much more prominent and definitive
##            up       left      down      right
## s0 -0.7593843 -0.9477923 8.0000000 -0.6028605
## s1 -0.7779349  1.0321205 0.8951468  9.0000000
## s2 10.0000000  1.4174411 4.1484734  2.5758564
## s3  0.0000000  0.0000000 0.0000000  0.0000000

# same policy but is more definitive
actions[max.col(Q)]
## [1] "down" "right" "up" "left"
