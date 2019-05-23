##############################
#####  Discount Factors  #####
##############################
# Using MDPtoolbox, create a MDP for 
# a 1 x 4 grid. In this grid, the 
# left two positions give rewards
# of -5 and -1, respectively. The 
# third and fourth left positions 
# give rewards of +10 and -10 respectively.
#
# The agent can choose between the actions
# of moving left or right but cannot cross
# the left or right boundaries of the grid.
#
# An action does not succeed all the time
# but is subject to random perturbations
# and only succeeds with 80% probability.
#
# With 20% probability, the agent stays in
# its place.

# Apply a policy iteration to your MDP with
# a discount factor of 0.9. What are the 
# recommended actions for each state?

library(MDPtoolbox)

left <- matrix(c(   1,   0,   0,   0,
                  0.8, 0.2,   0,   0,
                    0, 0.8, 0.2,   0,
                    0,   0, 0.8, 0.2),
               nrow=4, ncol=4, byrow=TRUE)
left

right <- matrix(c(0.2, 0.8,   0,   0,
                    0, 0.2, 0.8,   0,
                    0,   0, 0.2, 0.8,
                    0,   0,   0,   1),
                nrow=4, ncol=4, byrow=TRUE)
right

T <- list(left=left, right=right)
T

R <- matrix(c(  -5,  -5,
                -1,  -1,
               +10, +10,
               -10, -10),
            nrow=4, ncol=2, byrow=TRUE)
R

# Check if is a well-formed MDP
mdp_check(T, R)
## [1] ""

# Solve the MDP with discount factor of 0.9
m <- mdp_policy_iteration(P=T, R=R, discount=0.9)

# computed policy
m

m$policy
## [1] 2 2 2 1

names(T)[m$policy]
## [1] "right" "right" "right" "left" 
##         s0      s1      s2     s3

## Optimal policy recommends moving from
## state s2 with a reward of +10 to the
## right to state s3 with a reward of -10
## instead of to the left to state s2 with
## a better reward of -1

# value function
m$V
## [1] -2.162056  4.482103  6.493506 -6.493506
#--------------------------------------------

# Notice this different MDPtoolbox function
# which just outputs optimal value
# function given a certain policy:
out1 <- mdp_eval_policy_iterative(P=T, R=R, 
                                  discount=0.9, 
                                  policy <- c(2,2,2,1))
out1 # optimal policy

# different policy evaluated:
out2 <- mdp_eval_policy_iterative(P=T, R=R, 
                                  discount=0.9, 
                                  policy <- c(2,1,2,1))
out2 # non-optimal policy

#--------------------------------------------
# Run it again, with the original discount factor,
# but increase the negative reward in state s3
# to -1000. What is the resulting policy ?

R <- matrix(c(  -5,     -5,
                -1,     -1,
                +10,   +10,
              -1000, -1000),
            nrow=4, ncol=2, byrow=TRUE)
R

# Check if is a well-formed MDP
mdp_check(T, R)
## [1] ""

# Solve the MDP with same discount factor of 0.9
m <- mdp_policy_iteration(P=T, R=R, discount=0.9)

# computed policy
m

m$policy
## [1] 2 2 1 1

names(T)[m$policy]
## [1] "right" "right" "left" "left" 
##         s0      s1     s2     s3

## Now the optimal policy recommends moving
## left from state s2 with a reward of +10
## to state s1 with a reward of -1 instead
## of right to state s3 with a reward of -10

## Evidently, even with the large original
## discount factor of 0.9, with a very large
## negative reward in the far right state,
## the MDP solution arrived at value functions
## that drove the 'most intuitive' optimal 
## policy to avoid state s3.

# value function
m$V
## [1]    30.27875    41.42857    48.57143 -1176.86411
#--------------------------------------------

# In this video we:

# Examined the discount factor gamma;

# We walked through an R model-based
# program demonstrating how the
# discount factor and the relative
# rewards can affect policy