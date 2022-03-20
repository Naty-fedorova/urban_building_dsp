# options(error = quote({
#   dump.frames(to.file=T, dumpto='last.dump')
#   load('last.dump.rda')
#   print(last.dump)
#   q()
# }))
# ^ code for vscode to show more error message


# set up for UB system dsp following Clark&Mangel (2000)

### 1. Specify the basic time interval and time horizon

# A person's adult life, time interval is years

### 2. Specify the state variables and constraints

# savings: how much capital you have

# family situation: family size and dependency ration

# tenure: whether you have land or not

# house: whether you are in a ger or bashin


### 3. Specify the decision variables

# 3 behaviors:
# build
# save
# move


### 4. Specify the state dynamics, including stochastic aspects

# payoffs to build:
# are higher for larger families
# are higher for families with tenure
# reduce savings
# are low if you already have a house

# payoffs to save:
# increase savings

# payoffs to move:
# are lower for larger families
# are lower for families with tenure
# are lower for families with a house

### 5. Specify the characterization of fitness, including the terminal fitness function

### 6. On the basis of 1-5, write the dynamic programming equation

### 7. Parametrize the model on the by specifying parameter values and functional forms

### 8. Write computer code and perform test runs

#### best sentence ever: "where to simplify by omitting details is always a matter of delicacy and style" p21


# what affects family state
get_fam_probs <- function(
  house,
  savings,
  land
) {
  p_base <- (house+savings+land-3) / 3
  
  # prob of new person joining the family (marriage or birth)
  # when we have nothing this is:
  p_f_up_nothing <- 0.1
  # when we have everything this is:
  p_f_up_everything <- 0.2
  # inbetween it's linear
  p_f_up <- p_base * p_f_up_everything + (1-p_base) * p_f_up_nothing
  
  # prob of someone dying
  # when we have nothing this is:
  p_f_down_nothing <- 0.01
  # when we have everything this is:
  p_f_down_everything <- 0.001
  # inbetween it's linear
  p_f_down <- p_base * p_f_down_everything + (1-p_base) * p_f_down_nothing
  
  
  return(c(p_f_down, p_f_up))
}

# fam state and saving
propensity_to_save <- function(fam_state) {
  if(fam_state ==1){
    return(0.5)
  }
  if(fam_state ==2){
    return(1)
  }
  if(fam_state ==3){
    return(0.75)
  }
}


# final payoffs

states_h <- 2            # number of states for house (ger, bashin)
states_s <- 2            # number of states for savings (no have savings, have savings) 
states_l <- 2            # number of states for land (no land, own land)
states_f <- 3            # number of states for family composition(single, pair, with dependents)


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

# when assigning payoffs - consider maximum over all states to be gained, and then maximum within each segment
final_payoffs <- init_payoffs_array(
  values = list(
    #l=1, f=1
    c( 1,  2,
       1,  2),
    #l=2, f=1
    c( 1,  2,
       2,  4),
    
    #l=1, f=2
    c( 1,  2,
       1,  2),
    #l=2, f=2
    c( 1,  2,
       2,  4),
    
    #l=1, f=3
    c( 1,  2,
       2,  4),
    #l=2, f=3
    c( 2,  4,
       4,  8)
  ),
  dim = c(states_h, states_s, states_l, states_f),
  dimnames = c("h", "s", "l", "f")
)
final_payoffs



# TODO:




UB_optimal <- function( 
  
  states_h,            # number of states for house (ger, bashin)
  states_s,            # number of states for savings (no have savings, have savings) 
  states_l,            # number of states for land (no land, own land)
  states_f,            # number of states for family (single, pair, with dependents)
  final_payoffs,       # payoffs in final time period 
  
  maxt = 3,            # number of time step
  
  p_s_save = 0.7,      # prob gain save when saving
  p_h_build = 1.0,     # prob gain house when building
  p_l_move = 0.7,      # prob gain tenure when moving (essentially finding free land)
  
  build_condition = 1, # do you need savings to build? (1 = yes, 0 = no)
  
  p_s_loss = 0.1,      # prob of losing savings at each t
  p_force_move = 0.1   # prob of having to move from land, if you don't have tenure
  
  
) {
  if( p_s_save < 0 | p_s_save > 1) stop(c('p_s_save is not a probability: ', p_s_save))
  if( p_h_build < 0 | p_h_build > 1) stop(c('p_h_build is not a probability: ', p_h_build))
  if( p_l_move < 0 | p_l_move > 1) stop(c('p_l_move is not a probability: ', p_l_move))
  if( p_s_loss < 0 | p_s_loss > 1) stop(c('p_s_loss is not a probability: ', p_s_loss))
  if( p_force_move < 0 | p_force_move > 1) stop(c('p_force_move is not a probability: ', p_force_move))
  
  if( any(dim(final_payoffs) != c(states_h, states_s, states_l, states_f)) ) stop(c('final_payoffs dimensions do not correspond to states: ', dim(final_payoffs)))
  
  if( !(build_condition %in% c(0, 1)) )  stop(c('build_condition is invalid: ', build_condition))
  
  # array for solution
  dim <- c(states_h, states_s, states_l, states_f, maxt)
  dimnames <- c("h", "s", "l", "f", "t")
  dimnames_expanded <- list()
  for ( i in 1:length(dim) )
  { dimnames_expanded[[i]] <- sprintf("%s=%d", dimnames[i], 1:dim[i]) }
  
  strat <- array(
    data = NA, 
    dim = dim,
    dimnames = dimnames_expanded
  ) 
  
  # init payoffs in each state
  payoff <- final_payoffs
  new_payoff <- payoff # used for updating
  
  ## loop backwards through time
  for ( t in maxt:1 ) {
    new_payoff[] <- -1
    print(payoff)
    for ( h in 1:(states_h) ) {
      
      ## define payoffs when we move states
      # must bound maximum and minimum payoff
      h_up <- h+1
      if(h_up > states_h) h_up <- states_h
      h_down <- h-1
      if(h_down < 1) h_down <- 1
      
      for ( s in 1:(states_s)) { 
        
        ## define payoffs when we move states
        # must bound maximum and minimum payoff
        s_down <- s-1
        if(s_down < 1) s_down <- 1
        s_up <- s+1
        if(s_up > states_s) s_up <- states_s
        
        for ( l in 1:(states_l)) { 
          
          ## define payoffs when we move states
          # must bound maximum and minimum payoff
          l_up <- l+1
          if(l_up > states_l) l_up <- states_l
          
          # condition for stochastic forced moves
          # can only happen to agents with no tenure
          if(l == 1){
            effective_p_force_move <- p_force_move
          }else(
            effective_p_force_move <- 0
          )
          
          for( f in 1:(states_f)) {
            
            ## define payoffs when we move states
            # must bound maximum and minimum payoff
            f_down <- f-1
            if(f_down < 1) f_down <- 1
            f_up <- f+1
            if(f_up > states_f) f_up <- states_f
            
            ## calculate expected payoffs
            
            fam_probs <- get_fam_probs(h, s, l)
            p_f_down <- fam_probs[1]
            p_f_up <- fam_probs[2]
            
            if( p_f_down+p_f_up > 1 ) stop('p_f_down+p_f_up > 1')
            
            p_f_static <- 1 - p_f_down - p_f_up
            
            payoff_build <- (
              (1-effective_p_force_move) * (
                (1-p_s_loss) * (
                  p_f_down * (p_h_build * payoff[h_up, s_down, l, f_down] + (1-p_h_build) * payoff[h, s, l, f_down]) +
                    p_f_static * (p_h_build * payoff[h_up, s_down, l, f] + (1-p_h_build) * payoff[h, s, l, f]) +
                    p_f_up * (p_h_build * payoff[h_up, s_down, l, f_up] + (1-p_h_build) * payoff[h, s, l, f_up]))
                +
                  (p_s_loss) * (
                    # NOTE: if you build, you already lose savings, so you can't really re-lose them
                    p_f_down * (p_h_build * payoff[h_up, s_down, l, f_down] + (1-p_h_build) * payoff[h, s_down, l, f_down]) +
                      p_f_static * (p_h_build * payoff[h_up, s_down, l, f] + (1-p_h_build) * payoff[h, s_down, l, f]) +
                      p_f_up * (p_h_build * payoff[h_up, s_down, l, f_up] + (1-p_h_build) * payoff[h, s_down, l, f_up])))
              + 
                (effective_p_force_move) * (
                  # when forced to move, payoff is payoff to move
                  (1-p_s_loss) * (
                    p_f_down * (p_l_move * payoff[h_down, s, l_up, f_down] + (1-p_l_move) * payoff[h, s, l, f_down]) +
                      p_f_static * (p_l_move * payoff[h_down, s, l_up, f] + (1-p_l_move) * payoff[h, s, l, f]) +
                      p_f_up * (p_l_move * payoff[h_down, s, l_up, f_up] + (1-p_l_move) * payoff[h, s, l, f_up]))
                  +
                    (p_s_loss) * (
                      p_f_down * (p_l_move * payoff[h_down, s_down, l_up, f_down] + (1-p_l_move) * payoff[h, s_down, l, f_down]) +
                        p_f_static * (p_l_move * payoff[h_down, s_down, l_up, f] + (1-p_l_move) * payoff[h, s_down, l, f]) +
                        p_f_up * (p_l_move * payoff[h_down, s_down, l_up, f_up] + (1-p_l_move) * payoff[h, s_down, l, f_up]))
                )
            )
            
            
            effective_p_s_save <- propensity_to_save(f) * p_s_save
            payoff_save <- (
              (1 - effective_p_force_move) * (
                (1-p_s_loss) * (
                  p_f_down * (effective_p_s_save * payoff[h, s_up, l, f_down] + (1-effective_p_s_save) * payoff[h, s, l, f_down]) +
                    p_f_static * (effective_p_s_save * payoff[h, s_up, l, f] + (1-effective_p_s_save) * payoff[h, s, l, f]) +
                    p_f_up * (effective_p_s_save * payoff[h, s_up, l, f_up] + (1-effective_p_s_save) * payoff[h, s, l, f_up]))
                +
                  (p_s_loss) * (
                    p_f_down * (effective_p_s_save * payoff[h, s, l, f_down] + (1-effective_p_s_save) * payoff[h, s_down, l, f_down]) +
                      p_f_static * (effective_p_s_save * payoff[h, s, l, f] + (1-effective_p_s_save) * payoff[h, s_down, l, f]) +
                      p_f_up * (effective_p_s_save * payoff[h, s, l, f_up] + (1-effective_p_s_save) * payoff[h, s_down, l, f_up])))
              +
                (effective_p_force_move) * (
                  # when forced to move, payoff is payoff to move
                  (1-p_s_loss) * (
                    p_f_down * (p_l_move * payoff[h_down, s, l_up, f_down] + (1-p_l_move) * payoff[h, s, l, f_down]) +
                      p_f_static * (p_l_move * payoff[h_down, s, l_up, f] + (1-p_l_move) * payoff[h, s, l, f]) +
                      p_f_up * (p_l_move * payoff[h_down, s, l_up, f_up] + (1-p_l_move) * payoff[h, s, l, f_up]))
                  +
                    (p_s_loss) * (
                      p_f_down * (p_l_move * payoff[h_down, s_down, l_up, f_down] + (1-p_l_move) * payoff[h, s_down, l, f_down]) +
                        p_f_static * (p_l_move * payoff[h_down, s_down, l_up, f] + (1-p_l_move) * payoff[h, s_down, l, f]) +
                        p_f_up * (p_l_move * payoff[h_down, s_down, l_up, f_up] + (1-p_l_move) * payoff[h, s_down, l, f_up]))
                )
            )
            
            payoff_move <- (
              (1-p_s_loss) * (
                p_f_down * (p_l_move * payoff[h_down, s, l_up, f_down] + (1-p_l_move) * payoff[h, s, l, f_down]) +
                  p_f_static * (p_l_move * payoff[h_down, s, l_up, f] + (1-p_l_move) * payoff[h, s, l, f]) +
                  p_f_up * (p_l_move * payoff[h_down, s, l_up, f_up] + (1-p_l_move) * payoff[h, s, l, f_up]))
              +
                (p_s_loss) * (
                  p_f_down * (p_l_move * payoff[h_down, s_down, l_up, f_down] + (1-p_l_move) * payoff[h, s_down, l, f_down]) +
                    p_f_static * (p_l_move * payoff[h_down, s_down, l_up, f] + (1-p_l_move) * payoff[h, s_down, l, f]) +
                    p_f_up * (p_l_move * payoff[h_down, s_down, l_up, f_up] + (1-p_l_move) * payoff[h, s_down, l, f_up]))
            )
            
            
            # check payoffs are between min and max of final_payoffs
            if( min(final_payoffs) > payoff_build | payoff_build > max(final_payoffs) ) stop(c('payoff_build outside of bounds ', payoff_build))
            if( min(final_payoffs) > payoff_save | payoff_save > max(final_payoffs) ) stop(c('payoff_save outside of bounds ', payoff_save))
            if( min(final_payoffs) > payoff_move | payoff_move > max(final_payoffs) ) stop(c('payoff_move outside of bounds ', payoff_move))
            
            
            # condition: can only build if you have enough savings
            if(s > build_condition){   # if you have enough savings to build
              
              # which is better?
              # record optimal behavior 
              
              possible_current_payoffs <- c(payoff_build, payoff_save, payoff_move) # there is order of preference here, if payoffs end up being equal
              # make note if payoffs are equal 
              if((length(unique(possible_current_payoffs))== 3) == FALSE) print(c(possible_current_payoffs, h = h, s = s, l = l, f = f ))
              best_payoff_ind <- which(possible_current_payoffs==max(possible_current_payoffs))[1]
              
              if ( best_payoff_ind == 1 ) { # payoff_build
                strat[h, s, l, f, t] <- "build"
                new_payoff[h, s, l, f] <- payoff_build
              }
              if ( best_payoff_ind == 2 ) { # payoff_save
                strat[h, s, l, f, t] <- "save"
                new_payoff[h, s, l, f] <- payoff_save
              }
              if ( best_payoff_ind == 3 ) { # payoff_move
                strat[h, s, l, f, t] <- "move"
                new_payoff[h, s, l, f] <- payoff_move
              }
            }else{  # can only save or move because you don't have enough savings to build 
              
              # which is better?
              # record optimal behavior 
              # and store expected payoff when behaving optimally
              
              possible_current_payoffs <- c(payoff_save, payoff_move) # there is order of preference here, if payoffs end up being equal
              best_payoff_ind <- which(possible_current_payoffs==max(possible_current_payoffs))[1]
              
              if ( best_payoff_ind == 1 ) { # payoff_save
                strat[h, s, l, f, t] <- "save"
                new_payoff[h, s, l, f] <- payoff_save
              }
              if ( best_payoff_ind == 2 ) { # payoff_move
                strat[h, s, l, f, t] <- "move"
                new_payoff[h, s, l, f] <- payoff_move
              }
            } # end of ifelse s>1
          }#f
        }#l
      }#s
    }#h
    
    #check that all new_payoffs have been assigned a value in expected range
    for ( h in 1:(states_h) ) {
      for ( s in 1:(states_s)) {
        for ( l in 1:(states_l)) {
          for( f in 1:(states_f)) {
            if( min(final_payoffs) > new_payoff[h, s, l, f] | new_payoff[h, s, l, f] > max(final_payoffs) ) stop(c('new_payoff outside of bounds ', new_payoff[h, s, l, f]))
          }
        }
      }
    }
    
    # update payoffs to expected payoffs under optimal strategy
    payoff <- new_payoff
    
  }#t
  
  return( list(strat, c(maxt, states_h, states_s, states_l, states_f, p_h_build, p_s_save, p_l_move, build_condition, p_s_loss, p_force_move )))
}


optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs, maxt = 10)

#optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs, maxt = 10, p_s_save = 0)


#names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move", "build_condition", "p_s_loss", "p_force_move")








# qs for Richard
# what to do when payoffs are equal? random choice? have preferences? 



