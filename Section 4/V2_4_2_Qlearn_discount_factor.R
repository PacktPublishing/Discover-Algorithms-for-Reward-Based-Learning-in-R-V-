###########################################
##### Q-Learning with Discount Factor #####
###########################################

# We must first load the ReinforcementLearning package
library(ReinforcementLearning)

# define actions
actions <- c("up", "left", "down", "right")

# define states
states <- c("s0", "s1", "s2", "s3")

# Add a function that mimics the environment.
# Function returns a list with two entries: 
# the next state and the corresponding reward 
# given the current state and an intended action
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

# Previous Q-Learning function
Qlearning.OLD <- function(n, s_0, s_terminal,
                      epsilon, learning_rate) {
  # Initialize state-action function Q to zero
  Q <- matrix(0, nrow=length(states), 
              ncol=length(actions),
              dimnames=list(states, actions))
  # Perform n episodes/iterations of Q-learning
  for (i in 1:n) {
    Q <- learnEpisode(s_0, s_terminal,
                      epsilon, learning_rate, Q)
  }
  return(Q)
}

# Add a new function that performs
# Q-learning with discount factor
# for a given number of n episodes
Qlearning <- function(n, s_0, s_terminal,
                      epsilon, learning_rate, 
                      # add discount factor parameter
                      discount_factor) {
  # Initialize state-action function Q to zero
  Q <- matrix(0, nrow=length(states), 
              ncol=length(actions),
              dimnames=list(states, actions))
  # Store total reward of each episode
  # This is new
  rewards <- rep(NA, n)
  # Perform n episodes/iterations of Q-learning
  for (i in 1:n) {
    # No longer store in Q
    l <- learnEpisode(s_0, s_terminal,
                      epsilon, learning_rate,
                      # we add discount factor
                      discount_factor, Q)
    # new
    Q <- l$Q
    # new
    rewards[i] <- l$reward
  }
  return(list(Q=Q, rewards=rewards))
}


# Q-Learning in R with discount factor
learnEpisode <- function(s_0, s_terminal, epsilon, 
                         learning_rate, 
                         # add discount factor
                         discount_factor, Q) {
  state <- s_0 # set cursor to initial state
  reward <- 0 # stores total reward of episode
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
    # update rule for Q-learning
    Q[state, action] <- Q[state, action] + learning_rate *
      # note discount factor added below
      (response$reward + discount_factor * max(Q[response$state, ]) - Q[state, action])
    # accumulate reward
    reward <- reward + response$reward
    # move to next state
    state <- response$state
  }
  return(list(Q=Q, reward=reward))
}

# set value for random
# versus epsilon-greedy
epsilon <- 0.1

# set learning rate alpha
learning_rate <- 0.1

# set discount factor
discount_factor <- 0.9 

# set seed for replicability
set.seed(0)

# perform Q-Learning
Q <- Qlearning(1000, "s0", "s3", epsilon, 
               learning_rate, 
               discount_factor)$Q
Q
##            up      left      down     right
## s0  -9.749126 -9.788826 -9.416937 -9.730261
## s1  -9.555764 -8.994670 -9.025922 -8.188748
## s2  -1.807418 -8.371684 -5.144344 -5.998057
## s3   0.000000   0.00000   0.00000   0.00000

# note: problematic for states with ties
actions[max.col(Q)]
## [1] "down" "right" "up" "up"
# ----------------------------------------
# set larger discount factor
discount_factor <- 0.8 

# set seed for replicability
set.seed(0)

# perform Q-Learning
Q <- Qlearning(1000, "s0", "s3", epsilon, 
               learning_rate, discount_factor)$Q
Q
##            up      left      down     right
## s0 -4.9702466 -4.978735 -4.904354 -4.965885
## s1 -4.9238473 -4.814098 -4.826303 -4.405092
## s2 -0.9439783 -4.536720 -3.193259 -3.734741
## s3  0.0000000  0.000000  0.000000  0.000000

# policy is unchanged but ability to
# distinguish is changed in some circumstances
actions[max.col(Q)]
## [1] "down" "right" "up" "up"
# ----------------------------------------
# Increase alpha (learning rate) to 0.3

# set learning rate alpha
learning_rate <- 0.3

# set seed for replicability
set.seed(0)

# perform Q-Learning
Q <- Qlearning(1000, "s0", "s3", epsilon, 
               learning_rate, discount_factor)$Q
Q

## the Q action values are virtually unchanged
## but are slightly "less negative"
##           up      left      down     right
## s0 -4.9135090 -4.910843 -4.784819 -4.836398
## s1 -4.7979206 -4.748873 -4.826028 -3.139114
## s2  0.6898972 -4.324841 -3.225587 -3.855996
## s3  0.0000000  0.000000  0.000000  0.000000

# however, policy is unchanged
actions[max.col(Q)]
## [1] "down" "right" "up" "up"

# ----------------------------------------
# Increase alpha (learning rate) to 0.5

# set learning rate alpha
learning_rate <- 0.5

# set seed for replicability
set.seed(0)

# perform Q-Learning
Q <- Qlearning(1000, "s0", "s3", epsilon, 
               learning_rate, discount_factor)$Q
Q

## the Q action values are more "less negative"
##           up      left      down     right
## s0 -4.883625 -4.886154 -4.532381 -4.550296
## s1 -4.636489 -4.670275 -4.745707 -1.113055
## s2  3.008770 -4.603647 -3.905624 -4.022956
## s3  0.000000  0.000000  0.000000   0.00000

# policy is unchanged
actions[max.col(Q)]
## [1] "down" "right" "up" "up"
