###########################################
### Model-Free Race-to-Goal Environment ###  
###########################################

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

## Summary of Video:

#  Drew some distinctions between model-free and
#  model-based Reinforcement Learning paradigms
#  and the impacts on how to solve them.

#  Walked through building an environment for
#  a model-free 'Race-to-Goal' RL problem
#  example using R software.
