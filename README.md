When is it optimal to build a house?

This repository centers around a dynamic stochastic programming model, 2_UB_dsp.R, which calculates the optimal strategy for when to build a house. 
The model is built around the case study of the ger districs in Ulaanbaatar, Mongolia, where families have the choice between building gers and houses, 
a choice that is informed by their family and economic situation. 

The repository also contains:
- code for initializing the final payoffs for the dsp, 1_theory_to_payoff.R
- code for plotting the optimal strategy, 3_dsp_plot.R
- code for simulating from the optimal strategy with a forward simulation of agents through time, 4_dsp_forward_simulation.R
- code for plotting the behavioral frequency output of the simulation, 5_sim_output_plot.R
- code for running full scenarios with different payoffs, 6_run_full_scenarios.R

The script EHBEA_plots.R contains the scenarios that were explored for a talk given at EHBEA 2022

In general, the scrips contain functions and should be run in full before running the scenarios
