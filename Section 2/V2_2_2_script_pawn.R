##############################
##### MDP: Moving a Pawn #####
##############################

# We must first load the MDPtoolbox package
library(MDPtoolbox)

## 80% probability will move
## in selected action direction

# Probably most difficult part of setting
# MDP up is representing Pr(s'|s,a)

# Note that these transition probability matrices
# must always be square and that the cumulative
# probabilities in any row must sum 1.

# How read? 
# If you are in  s0   s1   s2   s3   s4   s5   s6   s7
up <- matrix(c( 0.1, 0.1,   0, 0.8,   0,   0,   0,   0,
                0.1, 0.8, 0.1,   0,   0,   0,   0,   0,
                  0,   0, 0.1, 0.1, 0.8,   0,   0,   0,
                  0,   0,   0, 0.2,   0, 0.8,   0,   0,
                  0,   0,   0,   0, 0.2,   0,   0, 0.8,
                  0,   0,   0,   0,   0, 0.9, 0.1,   0,
                  0,   0,   0,   0,   0, 0.1, 0.8, 0.1,
                  0,   0,   0,   0,   0,   0, 0.1, 0.9),
               nrow=8, ncol=8, byrow=TRUE)
up

right <- matrix(c( 0.1, 0.8,   0, 0.1,   0,   0,   0,   0,
                     0, 0.2, 0.8,   0,   0,   0,   0,   0,
                     0,   0, 0.9,   0, 0.1,   0,   0,   0,
                   0.1,   0,   0, 0.8,   0, 0.1,   0,   0,
                     0,   0, 0.1,   0, 0.8,   0,   0, 0.1,
                     0,   0,   0, 0.1,   0, 0.1, 0.8,   0,
                     0,   0,   0,   0,   0,   0, 0.2, 0.8,
                     0,   0,   0,   0, 0.1,   0,   0, 0.9),
                    nrow=8, ncol=8, byrow=TRUE)
right

down <- matrix(c( 0.9, 0.1,   0,   0,   0,   0,   0,   0,
                  0.1, 0.8, 0.1,   0,   0,   0,   0,   0,
                    0, 0.1, 0.9,   0,   0,   0,   0,   0,
                  0.8,   0,   0, 0.2,   0,   0,   0,   0,
                    0,   0, 0.8,   0, 0.2,   0,   0,   0,
                    0,   0,   0, 0.8, 0.1, 0.1,   0,   0,
                    0,   0,   0,   0,   0, 0.1, 0.8, 0.1,
                    0,   0,   0,   0, 0.8,   0, 0.1, 0.1),
                 nrow=8, ncol=8, byrow=TRUE)
down

left <- matrix(c(0.9,     0,   0, 0.1,   0,   0,   0,   0,
                 0.8,   0.2,   0,   0,   0,   0,   0,   0,
                   0,   0.8,   0,   0, 0.2,   0,   0,   0,
                 0.1,     0,   0, 0.8,   0, 0.1,   0,   0,
                   0,     0, 0.1,   0, 0.8,   0,   0, 0.1,
                   0,     0,   0, 0.1,   0, 0.9,   0,   0,
                   0,     0,   0,   0, 0.1, 0.8, 0.1,   0,
                   0,     0,   0,   0, 0.1,   0, 0.8, 0.1),
                nrow=8, ncol=8, byrow=TRUE)
left

###########
# Put transition probabilities in a list
T <- list(up=up, right=right, down=down, left=left)
T

# Dimensions of reward matrix:
# number of columns = number of actions
# number of rows = number of states
R <- matrix(c( 0,    0,  0,    0,  # reward in s0
               0,    0,  0,    0,  # s1
               0,    0,  0,    0,  # s2
               0,    0,  0,    0,  # s3
             -10,  -10,-10,  -10,  # s4
               0,    0,  0,    0,  # s5
               0,    0,  0,    0,  # s6
              10,   10, 10,   10), # s7
            nrow=8, ncol=4, byrow=TRUE)
R
#######

# Check to see if it a well-defined MDP
mdp_check(T, R)
## [1] ""

# We use the mdp_policy_iteration function
m <- mdp_policy_iteration(P=T, R=R, discount=0.5)

# computed policy is a list
m

m$policy # optimal policy
## [1] 1 4 3 1 1 2 2 1

names(T)[m$policy] # action names for optimal policy
## [1] "up" "left" "down" "up" "up" "right" "right" "up"   
##      s0     s1     s2   s3   s4      s5      s6   s7

# optimal value function convergence
# Note negative value for s4 and strong positive value for s7
m$V
# [1] 0.6956902 0.3091957 0.0281087 1.6136149 -2.6900585 3.6306335 8.4210526 18.9473684
#            s0        s1        s2        s3         s4        s5        s6         s7
#-------------------------------
# In this video we:

# Looked at the state-value and 
# state-action value functions;

# Described the evaluate-improve
# policy iteration cycle; and

# Walked through a model-based R
# program MDP example moving a pawn

#-----------------------------------
# We change the gamma discount parameter to 0.1

# Note that the optimal policy is sensitive 
# to the discount factor gamma:
# We use the mdp_policy_iteration function
m <- mdp_policy_iteration(P=T, R=R, discount=0.1) # larger discount

# computed policy is a list
m

m$policy # optimal policy
## [1] 4 4 3 1 1 2 2 1

names(T)[m$policy] # action names for optimal policy
## s0 has a different optimal policy which is counter-intuitive
## [1] "left" "left" "down" "up" "up" "right" "right" "up"   
##        s0     s1     s2   s3   s4      s5      s6   s7

m$V
# Also, the value function changes
#-------------------------------

# If you want to play around with it,
# here are alternative transition probabilities
# with a 90% probability of the action direction
# selected and a 10% probability the agent
# moves in a right angle to the selected action direction
####### 90%
up <- matrix(c( 0, 0.1,   0, 0.9,   0,   0,   0,   0,
                0, 0.9, 0.1,   0,   0,   0,   0,   0,
                0,   0, 0.1,   0, 0.9,   0,   0,   0,
                0,   0,   0, 0.1,   0, 0.9,   0,   0,
                0,   0,   0,   0, 0.1,   0,   0, 0.9,
                0,   0,   0,   0,   0, 0.9, 0.1,   0,
                0,   0,   0,   0,   0,   0, 0.9, 0.1,
                0,   0,   0,   0,   0,   0,   0,   1),
             nrow=8, ncol=8, byrow=TRUE)
up

right <- matrix(c(   0.1, 0.9,   0,   0,   0,   0,   0,   0,
                     0,   0.1, 0.9,   0,   0,   0,   0,   0,
                     0,     0,   1,   0,   0,   0,   0,   0,
                     0.1,   0,   0, 0.9,   0,   0,   0,   0,
                     0,     0, 0.1,   0, 0.9,   0,   0,   0,
                     0,     0,   0, 0.1,   0,   0, 0.9,   0,
                     0,     0,   0,   0,   0,   0, 0.1, 0.9,
                     0,     0,   0,   0,   0,   0,   0,   1),
                nrow=8, ncol=8, byrow=TRUE)
right

down <- matrix(c(   1,   0,   0,   0,   0,   0,   0,   0,
                  0.1, 0.9,   0,   0,   0,   0,   0,   0,
                    0, 0.1, 0.9,   0,   0,   0,   0,   0,
                  0.9,   0,   0, 0.1,   0,   0,   0,   0,
                    0,   0, 0.9,   0, 0.1,   0,   0,   0,
                    0,   0,   0, 0.9,   0, 0.1,   0,   0,
                    0,   0,   0,   0,   0, 0.1, 0.9,   0,
                    0,   0,   0,   0, 0.9,   0, 0.1,   0),
               nrow=8, ncol=8, byrow=TRUE)
down

left <- matrix(c(0.9,   0,   0, 0.1,   0,   0,   0,   0,
                 0.9, 0.1,   0,   0,   0,   0,   0,   0,
                 0,   0.9,   0,   0, 0.1,   0,   0,   0,
                 0,     0,   0, 0.9,   0, 0.1,   0,   0,
                 0,     0,   0,   0, 0.9,   0,   0, 0.1,
                 0,     0,   0,   0,   0,   1,   0,   0,
                 0,     0,   0,   0,   0, 0.9, 0.1,   0,
                 0,     0,   0,   0,   0,   0, 0.9, 0.1),
               nrow=8, ncol=8, byrow=TRUE)
left

# Put transition probabilities in a list
T <- list(up=up, right=right, down=down, left=left)
T

# And here is an alternative reward matrix
# Dimensions of reward matrix:
# number of columns = number of actions
# number of rows = number of states
R <- matrix(c( 0,    0,  0,    0,  # reward in s0
               0,    0,  0,    0,  # s1
               0,    0,  0,    0,  # s2
               0,    0,  0,    0,  # s3
               0,    0,  0,    0,  # s4
               0,    0,  0,    0,  # s5
              10,   10, 10,   10,  # s6 is now destination
             -10,  -10,-10,  -10), # s7 has a negative value
            nrow=8, ncol=4, byrow=TRUE)
R

# Check to see if it a well-defined MDP
mdp_check(T, R)
## [1] ""

# We use the mdp_policy_iteration function
m <- mdp_policy_iteration(P=T, R=R, discount=0.1)

# computed policy is a list
m

m$policy # optimal policy
## [1] 1 4 3 1 1 2 2 1

names(T)[m$policy] # action names for optimal policy
## [1] "up" "left" "down" "up" "down" "right" "down" "left"   
##      s0     s1     s2   s3     s4      s5     s6     s7

# optimal value function convergence
# Note negative value for s7 and strong positive value for s6
m$V
#---------------------------------------
# In this video we:

# Walked through a model-based R
# program MDP example moving a pawn
# with changed parameters
