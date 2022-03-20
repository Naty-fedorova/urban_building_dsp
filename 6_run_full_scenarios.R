### Running full scenarios

## baseline (additive and asymmetric)
final_payoffs <- final_payoff_func(scenario = "baseline")
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
sim_output <- sim_strat(N = 1000, maxt = 10)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output)



## predominantly value fam growth
final_payoffs <- final_payoff_func(scenario = "fam priority")
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")

#plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, f = 1)
#plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, f = 2)
#plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, f = 3)

sim_output <- sim_strat(N = 1000, maxt = 10)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output)

# if we run this one with build_condition = 0, then it looks like the others, because it is the optimisation of savings that causes the difference


## fam and house
final_payoffs <- final_payoff_func(scenario = "fam and house")
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")

#plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, f = 1)
#plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, f = 2)
#plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, f = 3)

sim_output <- sim_strat(N = 1000, maxt = 10)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output)


