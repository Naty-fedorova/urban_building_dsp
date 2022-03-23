## simulating from optimal strategy

sim_strat <- function(
  N = 50,                                                      # number of agents                               
  optimal_strat = optimal_strategy_output[[1]],                # optimal strategy
  maxt  = optimal_strategy_output[[2]]["maxt"],                # timescale
  
  states_h  = optimal_strategy_output[[2]]["states_h"],        # states of house (ger, bashin)
  states_s  = optimal_strategy_output[[2]]["states_s"],        # states of savings (savings, no savings)
  states_l  = optimal_strategy_output[[2]]["states_l"],        # states of land (tenure, no tenure)
  states_f  = optimal_strategy_output[[2]]["states_f"],        # states of family (single, pair, with dependents)
  
  p_h_build  = optimal_strategy_output[[2]]["p_h_build"],      # prob of gaining house state when building
  p_s_save  = optimal_strategy_output[[2]]["p_s_save"],        # prob of gaining save state when saving
  p_l_move  = optimal_strategy_output[[2]]["p_l_move"],        # prob of gaining tenure when moving
  
  build_condition  = optimal_strategy_output[[2]]["build_condition"],  # do you need savings to build?
  
  
  p_s_loss  = optimal_strategy_output[[2]]["p_s_loss"],        # prob of losing savings each t
  p_force_move  = optimal_strategy_output[[2]]["p_force_move"],# prob of having to move each t
  
  agent_init = "sample"                                        # "sample" or "start", sample means agents have sample of state values, start means agents all begin with lowest of states
  
){
  
  # initialize array for agents * time
  c_names <- list(1:N , c("agent", "h_state", "s_state", "l_state","f_state", "beh"), rep("t", maxt))
  agent_prop <- array(NA, c(N, length(c_names[[2]]), maxt), dimnames = c_names )
  
  # initialize states level
  states_of_h <- 1:states_h
  states_of_s <- 1:states_s
  states_of_l <- 1:states_l
  states_of_f <- 1:states_f
  
  # intialize agents in first time step
  # atm just random sample from statespace
  if(agent_init == "sample"){
  agent_prop[ , "agent", ] <- c(1:N)
  agent_prop[ , "h_state", 1] <- sample(states_of_h, N, replace = TRUE) 
  agent_prop[ , "s_state", 1] <- sample(states_of_s, N, replace = TRUE)
  agent_prop[ , "l_state", 1] <- sample(states_of_l, N, replace = TRUE)
  agent_prop[ , "f_state", 1] <- sample(states_of_f, N, replace = TRUE)
  }
  
  if(agent_init == "start"){
    agent_prop[ , "agent", ] <- c(1:N)
    agent_prop[ , "h_state", 1] <- rep(1, N)
    agent_prop[ , "s_state", 1] <- rep(1, N)
    agent_prop[ , "l_state", 1] <- rep(1, N)
    agent_prop[ , "f_state", 1] <- rep(1, N)
  }
  
  for(t in 1: maxt){
    
    ## stochastic changes to states
    for(agent in 1:N){
      
      ## stochastic change to fam state
      fam_probs <- get_fam_probs(
        as.numeric(agent_prop[agent ,"h_state", t]), 
        as.numeric(agent_prop[agent ,"s_state", t]), 
        as.numeric(agent_prop[agent ,"l_state", t]))
      
      p_f_down <- fam_probs[1]
      p_f_up <- fam_probs[2]
      
      if( p_f_down+p_f_up >= 1 ) stop('p_f_down+p_f_up > 1')
      
      p_f_static <- 1 - p_f_down - p_f_up
      
      fam_change <- sample(c(-1,0,1), 1, prob = c(p_f_down, p_f_static, p_f_up))
      
      f <- as.numeric(agent_prop[agent ,"f_state", t])
      f_down <- f-1
      if(f_down < 1) f_down <- 1
      f_up <- f+1
      if(f_up > states_f) f_up <- states_f
      
      if(fam_change == -1){
        # fam state decrease
        agent_prop[agent ,"f_state", t] <- f_down
      }
      if(fam_change == 1){
        # fam state increase
        agent_prop[agent ,"f_state", t] <- f_up
      }
      
      # otherwise it stays the same 
      
      #-----------------------------------------
      
      ## stochastic change to savings 
      
      if(rbinom(1,1, prob = p_s_loss) == 1){
        # saving state decreases
        s <- as.numeric(agent_prop[agent ,"s_state", t])
        s_down <- s-1
        if(s_down < 1) s_down <- 1
        
        agent_prop[agent ,"s_state", t] <- s_down
      }
      
      #-----------------------------------------
      
      ## decide on behavior
      agent_prop[agent ,"beh", t] <- optimal_strat[
        as.numeric(agent_prop[agent ,"h_state", t]), 
        as.numeric(agent_prop[agent ,"s_state", t]), 
        as.numeric(agent_prop[agent ,"l_state", t]), 
        as.numeric(agent_prop[agent ,"f_state", t]),
        t]
      
      #-----------------------------------------
      
      ## if agent doesn't have tenure, they might be forced to move
      
      if(agent_prop[agent ,"l_state", t] == 1){
        if(rbinom(1,1, prob = p_force_move) == 1){
          # forced to move
          agent_prop[agent ,"beh", t] <- "move"
        }
      }
    }
    
    
    
    
    
    
    ## materialize state change | intialize states for t+1
    
    if(t < maxt){
      for(agent in 1:N){
        
        # define movements in state space
        # house state
        h <- as.numeric(agent_prop[agent, "h_state", t])
        h_up <- h+1
        if(h_up > states_h) h_up <- states_h
        h_down <- h-1
        if(h_down < 1) h_down <- 1
        
        # save state
        s <- as.numeric(agent_prop[agent, "s_state", t])
        s_up <- s+1
        if(s_up > states_s) s_up <- states_s
        s_down <- s-1
        if(s_down < 1) s_down <- 1
        
        # land state
        l <- as.numeric(agent_prop[agent, "l_state", t])
        l_up <- l+1
        if(l_up > states_l) l_up <- states_l
        l_down <- l-1
        if(l_down < 1) l_down <- 1
        
        # fam state
        f <- as.numeric(agent_prop[agent, "f_state", t])
        f_up <- f+1
        if(f_up > states_f) f_up <- states_f
        f_down <- f-1
        if(f_down < 1) f_down <- 1
        
        # if agent builds
        if(agent_prop[agent, "beh", t] == "build"){
          if(rbinom(1, 1, prob = p_h_build) == 1){
            
            # state changes if agent builds "successfully"
            # increase in house state
            # decrease in saving state
            
            agent_prop[agent, "h_state", t+1] <- h_up
            agent_prop[agent, "s_state", t+1] <- s_down
            agent_prop[agent, "l_state", t+1] <- l
            agent_prop[agent, "f_state", t+1] <- f
            
          }else{
            
            # state changes if agent builds "unsuccessfully"
            # everything stays the same
            agent_prop[agent, "h_state", t+1] <- h
            agent_prop[agent, "s_state", t+1] <- s
            agent_prop[agent, "l_state", t+1] <- l
            agent_prop[agent, "f_state", t+1] <- f
          }
        }#
        
        # if agent saves
        if(agent_prop[agent, "beh", t] == "save"){
          if(rbinom(1, 1, prob = p_s_save) == 1){
            
            # state changes if agent saves "successfully
            # increase in save state
            agent_prop[agent, "h_state", t+1] <- h
            agent_prop[agent, "s_state", t+1] <- s_up
            agent_prop[agent, "l_state", t+1] <- l
            agent_prop[agent, "f_state", t+1] <- f
            
          }else{
            
            # state changes if agent saves "unsuccessfully"
            # everything stays the same
            agent_prop[agent, "h_state", t+1] <- h
            agent_prop[agent, "s_state", t+1] <- s
            agent_prop[agent, "l_state", t+1] <- l
            agent_prop[agent, "f_state", t+1] <- f
          }
        }#
        
        # if agent moves
        if(agent_prop[agent, "beh", t] == "move"){
          if(rbinom(1, 1, prob = p_l_move) == 1){
            
            # state changes if agent moves "successfully"
            # increase in land state
            agent_prop[agent, "h_state", t+1] <- h_down
            agent_prop[agent, "s_state", t+1] <- s
            agent_prop[agent, "l_state", t+1] <- l_up
            agent_prop[agent, "f_state", t+1] <- f
            
          }else{
            
            # state changes if agent moves "unsuccessfully"
            # everything stays the same
            agent_prop[agent, "h_state", t+1] <- h
            agent_prop[agent, "s_state", t+1] <- s
            agent_prop[agent, "l_state", t+1] <- l
            agent_prop[agent, "f_state", t+1] <- f
            
          }
        }#
      }
    }
  }#t
  
  return(agent_prop)
  
}#function

sim_strat(N = 10)

sim_output <- sim_strat(N = 1000, maxt = 10)


# TODO:
# add unit tests
# add test scenarios



