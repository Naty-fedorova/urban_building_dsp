### final payoffs

# when assigning payoffs - consider maximum over all states to be gained, and then maximum within each segment

## states
states_h <- 2            # number of states for house (ger, bashin)
states_s <- 2            # number of states for savings (no have savings, have savings) 
states_l <- 2            # number of states for land (no land, own land)
states_f <- 3            # number of states for family composition(single, pair, with dependents)

## initialise payoff array
init_payoffs_array <- function(values, dim, dimnames) {
  
  data <- c()
  for ( i in 1:length(values) ) { 
    data <- c(data, t(array(values[[i]], c(dim[1], dim[2]))))
  }
  
  dimnames_expanded <- list()
  for ( i in 1:length(dim) )
  { dimnames_expanded[[i]] <- sprintf("%s=%d", dimnames[i], 1:dim[i]) }
  
  
  return(array(
    data=data,
    dim=dim,
    dimnames=dimnames_expanded
  ))
}


## function for running different payoff structures
# NOTE: rows of mini arrays are house state, columns are saving state

final_payoff_func <- function(scenario = "baseline") {
  
  # flat
  if(scenario == "flat"){
    final_payoffs <- init_payoffs_array(
      values = list(
        #l=1, f=1
        c( 1,  1,
           1,  1),
        #l=2, f=1
        c( 1,  1,
           1,  1),
        
        #l=1, f=2
        c( 1,  1,
           1,  1),
        #l=2, f=2
        c( 1,  1,
           1,  1),
        
        #l=1, f=3
        c( 1,  1,
           1,  1),
        #l=2, f=3
        c( 1,  1,
           1,  1)
      ),
      dim = c(states_h, states_s, states_l, states_f),
      dimnames = c("h", "s", "l", "f")
    )
  }
  
  ## additive, each increase in state has payoff gains, saving more valuable when no tenure, building more valuable when have tenure
  if(scenario == "baseline"){
    final_payoffs <- init_payoffs_array(
      values = list(
        #l=1, f=1
        c( 1,  3,
           2,  4),
        #l=2, f=1
        c( 4,  5,
           6,  7),
        
        #l=1, f=2
        c( 6,  8,
           7,  9),
        #l=2, f=2
        c( 9,  10,
           11, 12),
        
        #l=1, f=3
        c( 12,  14,
           13,  15),
        #l=2, f=3
        c( 15,  16,
           17,  18)
      ),
      dim = c(states_h, states_s, states_l, states_f),
      dimnames = c("h", "s", "l", "f")
    )
  }
  
  
  # fam growth is valued overall, but also tailoring to build env.
  # big fam with tenure and house is better off, house is overkill for single person, and would take away mobility options
  if(scenario == "fam priority"){
    final_payoffs <- init_payoffs_array(
      values = list(
        #l=1, f=1
        c( 2,  2,
           1,  1),
        #l=2, f=1
        c( 2,  2,
           1,  1),
        
        #l=1, f=2
        c( 3,  3,
           3,  3),
        #l=2, f=2
        c( 3,  3,
           3,  3),
        
        #l=1, f=3
        c( 4,  4,
           4,  4),
        #l=2, f=3
        c( 4,  4,
           5,  5)
      ),
      dim = c(states_h, states_s, states_l, states_f),
      dimnames = c("h", "s", "l", "f")
    )
    
  }
  
  # house priority
  # having a house is more valuable than the other conditions
  # having a house better with tenure though, risk averse
  # all the other conditions treated equally
  
  if(scenario == "house priority"){
    final_payoffs <- init_payoffs_array(
      values = list(
        #l=1, f=1
        c( 1,  1,
           2,  2),
        #l=2, f=1
        c( 1,  1,
           3,  3),
        
        #l=1, f=2
        c( 1,  1,
           2,  2),
        #l=2, f=2
        c( 1,  1,
           3,  3),
        
        #l=1, f=3
        c( 1,  1,
           2,  2),
        #l=2, f=3
        c( 1,  1,
           3,  3)
      ),
      dim = c(states_h, states_s, states_l, states_f),
      dimnames = c("h", "s", "l", "f")
    )
  }
  
  # family fit to house priority
  # payoffs of house are dependent on family situation
  # house is not more valuable defacto, it is only valuable if you are a pair, or fam
  # if you are fam, ger living costs you
  if(scenario == "fam and house"){
    final_payoffs <- init_payoffs_array(
      values = list(
        #l=1, f=1
        c( 2,  2,
           2,  2),
        #l=2, f=1
        c( 2,  2,
           2,  2),
        
        #l=1, f=2
        c( 2,  2,
           2,  2),
        #l=2, f=2
        c( 2,  2,
           3,  3),
        
        #l=1, f=3
        c( 2,  2,
           3,  3),
        #l=2, f=3
        c( 1,  1,
           4,  4)
      ),
      dim = c(states_h, states_s, states_l, states_f),
      dimnames = c("h", "s", "l", "f")
    )
    
  }
  
 
  print(final_payoffs)
  return(final_payoffs)
  
}


# TODO: 



