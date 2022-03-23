### Running full scenarios

## baseline (savings and tenure are good things)
final_payoffs <- final_payoff_func(scenario = "baseline")
payoff_tree(final_payoffs = final_payoffs)
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
sim_output_base <- sim_strat(N = 1000, maxt = 10)
sim_output_base_start <- sim_strat(N = 1000, maxt = 10, agent_init = "start")
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output_base)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output_base_start)

# if we compare baseline to fam, then here there is absolutely no building, even though in the fam only condition there was
# and it was because of the implied benefit of houses on survival, so why not here?

final_payoffs_add <- final_payoff_func(scenario = "additive")
payoff_tree(final_payoffs = final_payoffs_add)
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs_add, maxt = 10, build_condition = 0)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
sim_output_add <- sim_strat(N = 1000, maxt = 10)
sim_output_add_start <- sim_strat(N = 1000, maxt = 10, agent_init = "start")
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output_add)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output_add_start)



## predominantly value fam growth
final_payoffs_fam <- final_payoff_func(scenario = "family priority")
payoff_tree(final_payoffs = final_payoffs_fam)
optimal_strategy_output_fam <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs_fam, maxt = 10, build_condition = 0)
names(optimal_strategy_output_fam[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")

plot_optimal_strat(optimal_strategy_output = optimal_strategy_output_fam)
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output_fam, f = 1)
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output_fam, f = 2)
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output_fam, f = 3)

optimal_strategy_output <- optimal_strategy_output_fam
sim_output_fam <- sim_strat(N = 1000, maxt = 10)
sim_output_fam_start <- sim_strat(N = 1000, maxt = 10)

plot_dsp_sim(optimal_strategy_output = optimal_strategy_output_fam, sim_output = sim_output_fam)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output_fam, sim_output = sim_output_fam_start)

# this one is tricky - there is no final payoff advantage to having a house, yet it is optimal to build sometimes, even under both build conditions
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output_fam, h = 1, l = 2) # why do they build?
# because of family survival rates - if all conditions are maxed out, you are more likely to gain fam state
# this means that if we change that, then they will no longer build - checked it by changing the get_fam_probs function, and it is true

# this is cool, because even if there is no final payoff, there is still risk averse incentive to build house

## fam and house
final_payoffs_fh <- final_payoff_func(scenario = "fam and house")
payoff_tree(final_payoffs = final_payoffs)
optimal_strategy_output_fh <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs_fh, maxt = 10, build_condition = 0)
names(optimal_strategy_output_fh[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")

plot_optimal_strat(optimal_strategy_output = optimal_strategy_output_fh)
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output_fh, f = 1)
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output_fh, f = 2)
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output_fh, f = 3) 

optimal_strategy_output <- optimal_strategy_output_fh
sim_output_fh <- sim_strat(N = 1000, maxt = 10)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output_fh, scenario = "fam and house")


## house priority
final_payoffs <- final_payoff_func(scenario = "house priority")
payoff_tree(final_payoffs = final_payoffs)
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10, build_condition = 1)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")

sim_output_house <- sim_strat(N = 1000, maxt = 10)
sim_output_house_start <- sim_strat(N = 1000, maxt = 10, agent_init = "start")
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output_house)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output_house_start)

# in the house priority condition, the strategies end up always having the same payoffs, particularly when build conditon is 0
# but that is sort of important too, regardless of what the house costs, it is optimal to build it 





# prioritisation of house or fam, or addition doesn't change optimal strategy
# it does a little for fam subsets tho
# but in general it is in the conditions, move and build only at the start, save always 

# TODO:
# will also want plots of total houses and gers over time, to connect it back to rq and data





