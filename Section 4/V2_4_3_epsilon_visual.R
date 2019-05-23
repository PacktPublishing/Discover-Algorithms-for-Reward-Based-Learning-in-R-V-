##############################
##### Q-Learning Visuals #####
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
# Notice what you get when you
# perform 1000 episodes of Q-Learning
l <- Qlearning(1000, "s0", "s3", epsilon, 
               learning_rate, 
               discount_factor)

# examine structure of l
str(l)

# summary Q-value state-action table
l$rewards

#------------------------------------------

# You might need to install ggplot2:
# install.packages(ggplot2, dependencies=TRUE)
library(ggplot2)

# make sure this learnEpisode() function is in memory:
learnEpisode <- function(s_0, s_terminal, epsilon, 
                         learning_rate, discount_factor, Q) {
  state <- s_0 # set cursor to initial state
  reward <- 0 # stores total reward of episode
  while (state != s_terminal) {
    # epsilon-greedy action selection
    if (runif(1) <= epsilon) {
      action <- sample(actions, 1) # pick random action
    } else {
      action <- which.max(Q[state, ]) # pick first best action
    }
    # get next state and reward from environment
    response <- simulateEnvironment(state, action)
    # update rule for Q-learning
    Q[state, action] <- Q[state, action] + learning_rate *
      (response$reward + discount_factor * max(Q[response$state, ]) - Q[state, action])
    # accumulate reward
    reward <- reward + response$reward
    state <- response$state # move to next state
  }
  return(list(Q=Q, reward=reward))
}

# make sure this Qlearning function is in memory:
Qlearning <- function(n, s_0, s_terminal,
                      epsilon, learning_rate, 
                      discount_factor) {
  # Initialize state-action function Q to zero
  Q <- matrix(0, nrow=length(states), ncol=length(actions),
              dimnames=list(states, actions))
  # Total reward of each episode
  rewards <- rep(NA, n)
  # Perform n episodes/iterations of Q-learning
  for (i in 1:n) {
    l <- learnEpisode(s_0, s_terminal,
                      epsilon, learning_rate, discount_factor, Q)
    Q <- l$Q
    rewards[i] <- l$reward
  }
  return(list(Q=Q, rewards=rewards))
}

#-------------------------------------------------------------
# Here we look at all Qlearning episode
# rewards while varying alpha only
set.seed(13)
# are just focusing on rewards for each of 1000 episodes:
rewards_alpha_rate_0.1 <- Qlearning(1000, "s0", "s3", 
                                       0.1, 0.1, 1.0)$rewards
set.seed(123)
rewards_alpha_rate_0.3 <- Qlearning(1000, "s0", "s3", 
                                       0.1, 0.3, 1.0)$rewards
set.seed(1234)
rewards_alpha_rate_0.5 <- Qlearning(1000, "s0", "s3", 
                                       0.1, 0.5, 1.0)$rewards

# put 1000 episodes in a data frame
df.alpha <- data.frame(index=1:1000,
                 alpha_0.1=rewards_alpha_rate_0.1,
                 alpha_0.3=rewards_alpha_rate_0.3,
                 alpha_0.5=rewards_alpha_rate_0.5)

# look at first 30 episodes of rewards
df.alpha[1:30,]

# sum all of the rewards 
# in the individual columns
sum(df.alpha$alpha_0.1)
sum(df.alpha$alpha_0.3)
sum(df.alpha$alpha_0.5)

# plot the first 30 episodes
ggplot(df.alpha[1:30, ]) +
  geom_line(aes(x=index, y=alpha_0.1, color="Learning Rate = 0.1")) +
  geom_line(aes(x=index, y=alpha_0.3, color="Learning Rate = 0.3")) +
  geom_line(aes(x=index, y=alpha_0.5, color="Learning Rate = 0.5")) +
  xlab("n-th Episode") +
  ylab("Reward of n-th Episode") +
  scale_color_manual("", values=c("lightgreen", "red", "blue")) +
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0))

# Notice you do not see
# a pattern by episode
#----------------------------------------------------
# Here we look at all Qlearning episode
# rewards while varying epsilon only
set.seed(1)
rewards_epsilon_0.3 <- Qlearning(1000, "s0", "s3", 
                                 0.3, 0.1, 1.0)$rewards
set.seed(13)
rewards_epsilon_0.5 <- Qlearning(1000, "s0", "s3", 
                                 0.5, 0.1, 1.0)$rewards
set.seed(123)
rewards_epsilon_0.7 <- Qlearning(1000, "s0", "s3", 
                                 0.7, 0.1, 1.0)$rewards
# put 1000 episodes in a data frame
df.eps <- data.frame(index=1:1000,
                 epsilon_0.3=rewards_epsilon_0.3,
                 epsilon_0.5=rewards_epsilon_0.5,
                 epsilon_0.7=rewards_epsilon_0.7)

# look at first 30 episodes of rewards
df.eps[1:30,]

# sum all of the rewards 
# in the individual columns
sum(df.eps$epsilon_0.3)
sum(df.eps$epsilon_0.5)
sum(df.eps$epsilon_0.7)

# plot the first 30 episodes
ggplot(df.eps[1:30, ]) +
  geom_line(aes(x=index, y=epsilon_0.3, color="Epsilon = 0.3")) +
  geom_line(aes(x=index, y=epsilon_0.5, color="Epsilon = 0.5")) +
  geom_line(aes(x=index, y=epsilon_0.7, color="Epsilon = 0.7")) +
  xlab("n-th Episode") +
  ylab("Reward of n-th Episode") +
  scale_color_manual("", values=c("lightgreen", "red", "blue")) +
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0))

#----------------------------------------------------
# Here we look at all Qlearning episode
# rewards while varying gamma only
set.seed(7)
rewards_gamma_0.3 <- Qlearning(1000, "s0", "s3", 
                                 0.1, 0.1, 0.3)$rewards
set.seed(29)
rewards_gamma_0.5 <- Qlearning(1000, "s0", "s3", 
                                 0.1, 0.1, 0.5)$rewards
set.seed(47)
rewards_gamma_0.7 <- Qlearning(1000, "s0", "s3", 
                                 0.1, 0.1, 0.7)$rewards

# collect rewards for 1000 episodes
df.gamma <- data.frame(index=1:1000,
                     gamma_0.3=rewards_gamma_0.3,
                     gamma_0.5=rewards_gamma_0.5,
                     gamma_0.7=rewards_gamma_0.7)

# look at first 30 episodes of rewards
df.gamma[1:30,]

# sum all of the rewards 
# in the individual columns
sum(df.gamma$gamma_0.3)
sum(df.gamma$gamma_0.5)
sum(df.gamma$gamma_0.7)

# plot the first 30 episodes
ggplot(df.gamma[1:30, ]) +
  geom_line(aes(x=index, y=gamma_0.7, color="Discount = 0.7")) +
  geom_line(aes(x=index, y=gamma_0.5, color="Discount = 0.5")) +
  geom_line(aes(x=index, y=gamma_0.3, color="Discount = 0.3")) +
  xlab("n-th Episode") +
  ylab("Reward of n-th Episode") +
  scale_color_manual("", values=c("lightgreen", "red", "blue")) +
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0))
