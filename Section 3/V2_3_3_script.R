##############################
#####  Q-Learning with R #####
##############################

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

# Add a function that performs
# Q-learning for a given number
# of n episodes
Qlearning <- function(n, s_0, s_terminal,
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

# Q-Learning in R
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

# set value for random
# versus epsilon-greedy
epsilon <- 0.1

# set learning rate alpha
learning_rate <- 0.1

# set seed for replicability
set.seed(0)

# perform Q-Learning
Q <- Qlearning(1000, "s0", "s3", epsilon, learning_rate)
Q
##            up      left      down     right
## s0 -79.962619 -81.15445 -68.39532 -79.34825
## s1 -73.891963 -52.43183 -52.67565 -47.91828
## s2  -8.784844 -46.32207 -17.97360 -20.29088
## s3   0.000000   0.00000   0.00000   0.00000

# note: problematic for states with ties
actions[max.col(Q)]
## [1] "down" "right" "up" "up"
# ----------------------------------------

# Increase alpha (learning rate) to 0.3

# set learning rate alpha
learning_rate <- 0.3

# set seed for replicability
set.seed(0)

# perform Q-Learning
Q <- Qlearning(1000, "s0", "s3", epsilon, learning_rate)
Q

## the Q action values are "less negative"
##           up      left      down     right
## s0 -57.91025 -63.31566 -45.546401 -55.06790
## s1 -45.95492 -39.33741 -40.477688 -27.78543
## s2  -2.23475 -26.56137  -8.039424 -14.86325
## s3   0.00000   0.00000   0.000000   0.00000

# policy is unchanged
actions[max.col(Q)]
## [1] "down" "right" "up" "up"

# ----------------------------------------
# Increase alpha (learning rate) to 0.5

# set learning rate alpha
learning_rate <- 0.5

# set seed for replicability
set.seed(0)

# perform Q-Learning
Q <- Qlearning(1000, "s0", "s3", epsilon, learning_rate)
Q

## the Q action values are even more "less negative"
##           up      left      down     right
## s0 -39.26690 -45.95787 -30.560015 -34.69330
## s1 -30.30722 -29.24382 -29.805325 -12.93627
## s2   1.60159 -17.83603  -6.978764 -10.81220
## s3   0.00000   0.00000   0.000000   0.00000

# policy is unchanged
actions[max.col(Q)]
## [1] "down" "right" "up" "up"

# ---------------------------------
# Now we introduce the possibility
# of random actions. This is a bit
# different from the stochasticity
# examples we looked at with MDPs.

# Consider the 2x2 grid example. 
# That example was deterministic,
# the environment always executed the 
# desired action correctly. 

# However, many practical applications
# are subject to disturbances and the
# intended action does not result into the
# anticipated state.

# We adjust for that by changing the code
# such that a random action is taken in 
# 20% of all cases (to begin with). This
# introduces variability (stochasticity)
# in the actual action outcome. But the
# variability is not limited to a lateral
# (or any) definable patterns.

# We again use R to compute the optimal 
# policy from the new state-action table.

simulateEnvironment <- function(state, action) {
  # add noise (20% variability) in
  # actual action selected. For example,
  # if the agent chooses "right" it will
  # move right 80% of the time, but 20%
  # of the time, agent will randomly move
  # left, up or down.
  if (runif(1) <= 0.2) {
    action <- sample(actions, 1)
  }
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

# Now we use the same functions as
# before to calculate the new policy:
# Add a function that performs
# Q-learning for a given number
# of n episodes
Qlearning <- function(n, s_0, s_terminal,
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

# Q-Learning in R
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

# set value for random
# versus epsilon-greedy
epsilon <- 0.1

# set learning rate alpha
learning_rate <- 0.1

# set seed for replicability
set.seed(0)

# Perform Q-Learning
Q <- Qlearning(1000, "s0", "s3", epsilon, learning_rate)
Q
##            up       left       down     right
## s0  -45.96078  -45.90610  -44.71164 -45.90129
## s1  -38.04752  -33.44277  -32.83281 -29.32637
## s2  -10.24966  -28.07133  -11.18242 -11.61152
## s3    0.00000    0.00000    0.00000   0.00000

# recommended policy actions (Note:
# problematic for ties)
actions[max.col(Q)]
## [1] "down" "right" "up" "down"

# The optimal policy with 20% variability
# is the same compared to the previous
# deterministic example. 

#----------------------------------

# Now we adjust by changing the code
# such that a random action is taken in 
# 30% of all cases. This introduces
# more random variability in
# the actual action outcome.

# We again use R to compute the optimal 
# policy from the new state-action table.

simulateEnvironment <- function(state, action) {
  # add noise (30% variability)
  # in actual action selected. The
  # policy should be 'more' random.
  if (runif(1) <= 0.3) { 
    action <- sample(actions, 1)
  }
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

# Now we use the same functions as
# before to calculate the new policy:
# Add a function that performs
# Q-learning for a given number
# of n episodes
Qlearning <- function(n, s_0, s_terminal,
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

# Q-Learning in R
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

# set value for random
# versus epsilon-greedy
epsilon <- 0.1

# set learning rate alpha
learning_rate <- 0.1

# set seed for replicability
set.seed(0)

# Perform Q-Learning for 1000 episodes
Q <- Qlearning(1000, "s0", "s3", epsilon, learning_rate)
Q

# recommended policy actions (Note:
# problematic for ties)
actions[max.col(Q)]

# The optimal policy with 30% variability
# may differ (even be counter-intuitive)
# for some states.

#---------------------------------

