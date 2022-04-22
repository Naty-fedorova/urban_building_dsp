#dir.create("Figures")


## payoff trees

# baseline family
png(file = "Figures/payoff_tree_base_fam.png", height = 15, width = 15, units = "cm", res = 400)
final_payoffs <- final_payoff_func(scenario = "baseline fam") 
payoff_tree(final_payoffs = final_payoffs, scenario = "baseline family")
dev.off()

# family priority
png(file = "Figures/payoff_tree_family.png", height = 15, width = 15, units = "cm", res = 400)
final_payoffs <- final_payoff_func(scenario = "family priority") 
payoff_tree(final_payoffs = final_payoffs, scenario = "family priority")
dev.off()

# additive 
png(file = "Figures/payoff_tree_additive.png", height = 15, width = 15, units = "cm", res = 400)
final_payoffs <- final_payoff_func(scenario = "additive") 
payoff_tree(final_payoffs = final_payoffs, scenario = "additive" )
dev.off()


## sim runs

# baseline fam 
png(file = "Figures/sim_run_base_fam.png", height = 10, width = 15, units = "cm", res = 400)
final_payoffs <- final_payoff_func(scenario = "baseline fam")
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10, build_condition = 1)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
sim_output <- sim_strat(N = 1000, maxt = 10)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output, scenario = "baseline family", legend_x = 0, legend_y = 1000)
dev.off()

png(file = "Figures/sim_run_base_fam_bc0.png", height = 10, width = 15, units = "cm", res = 400)
final_payoffs <- final_payoff_func(scenario = "baseline fam")
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10, build_condition = 0)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
sim_output <- sim_strat(N = 1000, maxt = 10)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output, scenario = "baseline family", legend_x = 0, legend_y = 1000)
dev.off()

# very little change along family categories
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, f = 1)
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, f = 2)
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, f = 3)

plot_optimal_strat(optimal_strategy_output = optimal_strategy_output)

# land leads to dif behavior
png(file = "Figures/optimal_fambase_land.png", height = 10, width = 15, units = "cm", res = 400)
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, l = 1, lty = 2, legend_x = 6, legend_y = 25)
par(new = TRUE)
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, l = 2, legend_x = 6, legend_y = 25)
dev.off()

plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, h = 1, lty = 2)
par(new = TRUE)
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, h = 2)


# family
png(file = "Figures/sim_run_family.png", height = 10, width = 15, units = "cm", res = 400)
final_payoffs <- final_payoff_func(scenario = "family priority")
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10, build_condition = 1)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
sim_output <- sim_strat(N = 1000, maxt = 10)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output, scenario = "family priority", tr = 1, legend_x = 8, legend_y = 700)
dev.off()

plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, h = 1, l = 2, f = 3, s = 2)


# family bc = 0
# plot overlapping family bc = 1
png(file = "Figures/sim_run_family_bc0.png", height = 10, width = 15, units = "cm", res = 400)
final_payoffs <- final_payoff_func(scenario = "family priority")
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10, build_condition = 1)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
sim_output <- sim_strat(N = 1000, maxt = 10)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output, scenario = "family priority, bc = 0", tr = 0.3, lty = 3, legend_x = 7, legend_y = 700)


final_payoffs <- final_payoff_func(scenario = "family priority")
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10, build_condition = 0)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
sim_output <- sim_strat(N = 1000, maxt = 10)
par(new=TRUE)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output, scenario = "family priority, bc = 0", legend_x = 7, legend_y = 700)
dev.off()

# remember in this scenario there is only survival benefit of house, so building is driven by that, and the trade-off is that you need to save to build, but savings also hurt ur fam growth

plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, l = 1) # those without tenure are always moving
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, l = 2) # pattern is essentially all for those with tenure

# TODO: consider including this plot - if you have a family, building is better early on
# because for a family house is more valuable than savings 
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, l = 2, f = 2)
plot_optimal_strat(optimal_strategy_output = optimal_strategy_output, l = 2, f = 3)

# additive
# overlapping with family plot
png(file = "Figures/sim_run_additive.png", height = 10, width = 15, units = "cm", res = 400)
final_payoffs <- final_payoff_func(scenario = "family priority")
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10, build_condition = 1)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
sim_output <- sim_strat(N = 1000, maxt = 10)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output, scenario = "additive", tr = 0.3, lty = 3, legend_y = 700)

final_payoffs <- final_payoff_func(scenario = "additive")
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10, build_condition = 0)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
sim_output <- sim_strat(N = 1000, maxt = 10)
par(new=TRUE)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output, scenario = "additive", lty = 1, legend_y = 700)
dev.off()


# additive bc = 0
# overlapping with additive plot
png(file = "Figures/sim_run_additive_bc0.png", height = 10, width = 15, units = "cm", res = 400)
final_payoffs <- final_payoff_func(scenario = "additive")
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10, build_condition = 1)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
sim_output <- sim_strat(N = 1000, maxt = 10)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output, scenario = "additive, bc = 0", tr = 0.3, lty = 3, legend_x = 0, legend_y = 1000)

final_payoffs <- final_payoff_func(scenario = "additive")
optimal_strategy_output <- UB_optimal(states_h, states_s, states_l, states_f, final_payoffs = final_payoffs, maxt = 10, build_condition = 0)
names(optimal_strategy_output[[2]]) <- c("maxt", "states_h", "states_s", "states_l", "states_f", "p_h_build", "p_s_save", "p_l_move","build_condition", "p_s_loss", "p_force_move")
sim_output <- sim_strat(N = 1000, maxt = 10)
par(new=TRUE)
plot_dsp_sim(optimal_strategy_output = optimal_strategy_output, sim_output = sim_output, scenario = "additive, bc = 0", tr = 1, lty = 1, legend_x = 0, legend_y = 1000)
dev.off()
 






