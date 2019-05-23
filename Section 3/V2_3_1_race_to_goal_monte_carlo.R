##########################################
### Monte Carlo Update to Race-to-Goal ###  
##########################################

# We did this first part already

# Load the package
library(ReinforcementLearning)

# Here we define our "Race-to-Goal" environment
RACE.env <- function(state, action) {
  next_state <- state
  ## define all possible state-action-next state triples
  if(state == state("s0") && action == "right") next_state <- state("s1")
  if(state == state("s0") && action == "up") next_state <- state("s4")
  ## Note: no need to define being in s0 and choosing action
  ## to move left as the next_state would still be s0.
  ## You only need to define possible movements to a new state.
  if(state == state("s1") && action == "right") next_state <- state("s2")
  if(state == state("s1") && action == "left") next_state <- state("s0")
  if(state == state("s1") && action == "up") next_state <- state("s5")
  if(state == state("s2") && action == "right") next_state <- state("s3")
  if(state == state("s2") && action == "left") next_state <- state("s1")
  if(state == state("s2") && action == "up") next_state <- state("s6")
  if(state == state("s3") && action == "left") next_state <- state("s2")
  if(state == state("s3") && action == "up") next_state <- state("s7")
  if(state == state("s4") && action == "down") next_state <- state("s0")
  if(state == state("s4") && action == "right") next_state <- state("s5")
  if(state == state("s5") && action == "down") next_state <- state("s1")
  if(state == state("s5") && action == "left") next_state <- state("s4")
  if(state == state("s5") && action == "right") next_state <- state("s6")
  if(state == state("s6") && action == "left") next_state <- state("s5")
  if(state == state("s6") && action == "right") next_state <- state("s7")
  if(state == state("s6") && action == "down") next_state <- state("s2")
  if(state == state("s7") && action == "up") next_state <- state("s11")
  if(state == state("s7") && action == "left") next_state <- state("s6")
  if(state == state("s7") && action == "down") next_state <- state("s3")
  if(state == state("s8") && action == "up") next_state <- state("s12")
  if(state == state("s8") && action == "right") next_state <- state("s9")
  if(state == state("s9") && action == "left") next_state <- state("s8")
  if(state == state("s9") && action == "right") next_state <- state("s10")
  if(state == state("s9") && action == "up") next_state <- state("s13")
  ## There is no need to define action movements out of state s10
  ## or out of state s12 as those are end (or absorbing) states.
  if(state == state("s11") && action == "up") next_state <- state("s15")
  ## But you do need to define actions into states s10 or s12:
  if(state == state("s11") && action == "left") next_state <- state("s10")
  if(state == state("s11") && action == "down") next_state <- state("s7")
  if(state == state("s13") && action == "right") next_state <- state("s14")
  if(state == state("s13") && action == "down") next_state <- state("s9")
  if(state == state("s13") && action == "left") next_state <- state("s12")
  if(state == state("s14") && action == "right") next_state <- state("s15")
  if(state == state("s14") && action == "left") next_state <- state("s13")
  if(state == state("s14") && action == "down") next_state <- state("s10")
  if(state == state("s15") && action == "up") next_state <- state("s19")
  if(state == state("s15") && action == "down") next_state <- state("s11")
  if(state == state("s15") && action == "left") next_state <- state("s14")
  if(state == state("s16") && action == "right") next_state <- state("s17")
  if(state == state("s17") && action == "left") next_state <- state("s16")
  if(state == state("s17") && action == "right") next_state <- state("s18")
  if(state == state("s18") && action == "left") next_state <- state("s17")
  if(state == state("s18") && action == "right") next_state <- state("s19")
  if(state == state("s19") && action == "down") next_state <- state("s15")
  if(state == state("s19") && action == "left") next_state <- state("s18")
  
  ## define rewards in each state
  ## make them all 0 initially:
  reward <- 0
  ## Then define the exceptions: Entering
  ## Goal state s12 has reward of +10;
  ## Are only two ways to enter s12
  ## from s13 or from s8:
  if (next_state == state("s12") && (state == state("s13"))) reward <- 10
  if (next_state == state("s12") && (state == state("s8"))) reward <- 10
  ## Negative Reward End state s10 has reward of -10.
  ## Can enter state s10 from s9, from s11, and from s14
  if (next_state == state("s10") && (state == state("s9"))) reward <- -10
  if (next_state == state("s10") && (state == state("s11"))) reward <- -10
  if (next_state == state("s10") && (state == state("s14"))) reward <- -10
  
  ## Function returns a list of next_state and reward
  out <- list("NextState" = next_state, "Reward" = reward)
  return(out)
}

# Define state and action sets
states <- c("s0", "s1", "s2", "s3", "s4",
            "s4", "s5", "s6", "s7", "s8",
            "s9", "s10", "s11", "s12", "s13",
            "s14", "s15", "s16", "s17", "s18", "s19")
states # twenty states
actions <- c("up", "down", "left", "right")
actions # four actions

# Sample N = 5000 random sequences from the
# Race-to-Goal environment function above.
# Data format must be (s,a,r,s_new) tuples,
# each as rows in a dataframe structure.

# Set seed for replicability
set.seed(1234)
# ?sampleExperience
data <- sampleExperience(N = 5000, 
                         env = RACE.env, 
                         states = states, 
                         actions = actions)

# Show first 250 records of data
data

## Performing Reinforcement Learning

# Define reinforcement learning parameters
control <- list(alpha = 0.1, # low learning rate
                gamma = 0.5, # middle discount factor
                # epsilon only relevant when sampling
                # new experience based on known rewards
                epsilon = 0.1) # low exploration factor
control

# Perform reinforcement learning
# ?ReinforcementLearning
RACE.model <- ReinforcementLearning(data, 
                                    s = "State", 
                                    a = "Action", 
                                    r = "Reward", 
                                    s_new = "NextState",
                                    actionSelection = "random",
                                    control = control)

# Print result
print(RACE.model)

#--------------------------------
# We had already run an intial data set
# and found a policy

# Now we fine-tune" the existing policy
# with a new data set and we deliberately
# choose "epsilon-greedy" action selection
# Define reinforcement learning parameters
control <- list(alpha = 0.1, # low learning rate
                gamma = 0.5, # middle discount factor
                # epsilon only relevant when sampling
                # new experience based on existing policy
                epsilon = 0.1) # low exploration factor
control

# Set seed for replicability
set.seed(123)

# Sample N = 5000 sequences from the environment 
# using epsilon-greedy action selection
data_new <- sampleExperience(N = 5000, 
                             # use same environment
                             env = RACE.env, 
                             states = states, 
                             actions = actions, 
                             # note we are using the
                             # existing model from before
                             model = RACE.model, 
                             actionSelection = "epsilon-greedy", 
                             control = control)

# view first 250 records
data_new

# Update the existing policy using new training data
model_new <- ReinforcementLearning(data_new, 
                                   s = "State", 
                                   a = "Action", 
                                   r = "Reward", 
                                   s_new = "NextState", 
                                   control = control,
                                   model = RACE.model)

# Print result
print(model_new)
## State-Action function Q

## 
## Reward (last iteration)
## [1] 4410

summary(model_new)
## Model details
## Learning rule:           experienceReplay
## Learning iterations:     2
## Number of states:        20
## Number of actions:       4
## Total Reward:            4410
## 
## Reward details (per iteration)
## Min:                     -470
## Max:                     4410
## Average:                 1970
## Median:                  1970
## Standard deviation:      3450.681

# Plot reinforcement learning curve
plot(model_new)

