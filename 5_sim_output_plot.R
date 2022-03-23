# summarise sim output to table

plot_dsp_sim <- function(optimal_strategy_output, sim_output = sim_output, legend_x = 8, legend_y = 200, scenario = "default"){
  
  maxt <- optimal_strategy_output[[2]]["maxt"]
  
  beh_freq <- matrix(data = 0, nrow = maxt, ncol = 3)
  colnames(beh_freq) <- c("build", "save", "move")
  
  for(t in 1:maxt){
    sim_output_t <- table(sim_output[ ,"beh", t])
    
    beh_freq[t,"build"] <- sim_output_t["build"]
    beh_freq[t,"save"] <- sim_output_t["save"]
    beh_freq[t,"move"] <- sim_output_t["move"]
  }
  beh_freq[is.na(beh_freq)] <- 0
  
  
  # plot frequency of each beh in each timestep
  colors <- c("#3F3430", "#7DCFF7","#AE7A7C")
  plot(x = 1, y = 1, 
       main = paste("Simulated behavioral frequencies, scenario = ", scenario),
       xlab = "Time", 
       ylab = "Behavioral frequency",
       type = "n", 
       xlim = c(0, maxt), 
       ylim = c(0, max(beh_freq)),
       bty = "n"
  )
  for (e in 1:ncol(beh_freq)) {
    xs <- c()
    ys <- c()
    for(t in 1:maxt){
      xs <- c(xs, t)
      y <- beh_freq[t,e]
      ys <- c(ys, y)
    }
    lines(xs, ys, col=colors[e], lwd = 1.5)
  }
  
  legend(x = legend_x, y = legend_y, 
         legend = c("build", "save", "move"),
         col = colors, 
         cex = 0.5, 
         lty = 1, 
         box.lty = 0, 
         bg = "transparent"
         )
  
  
  
  
}#end function


plot_dsp_sim(optimal_strategy_output = optimal_strategy_output)



# # bar plot
# barplot(t(beh_freq), 
#         col = colors, 
#         border = NA, 
#         xlab = "Time",
#         ylab = "Behavioral frequency"
#         )







# plot possibility tree for 3 behaviors, going through each

# # calculate number of possibilities in final t
# # this is y axis limit
# 
# pos_maxt <- 3^maxt
# 
# 
# 
# plot(10,1, xlim = c(0, maxt), ylim = c(0, pos_maxt))
# 
# for(t in maxt:1){
#   #lines for each possibility
#   # from maxt to maxt - 1
#   n_pos <- 3^(t-1)
#   
#   gap <- pos_maxt / ((3^t))
#   offset <- (gap - 1) / 2
#   print("#########")
#   print(c(t, n_pos, gap, offset))
#     for(twig in 0:(n_pos-1)){
#       y <- offset + gap + 3*twig * gap
#       points(t-1, y)
#       
#       x1 <- t-1
#       y1 <- y
#       
#       # build
#       x2 <- t
#       y2 <- y + gap  # this is the thing that will have to be angled 
#       lines(c(x1,x2), c(y1,y2), type = "l", lwd = 0.5)
#       
#       # save
#       x2 <- t
#       y2 <- y   # this is the thing that will have to be angled 
#       lines(c(x1,x2), c(y1,y2), type = "l", lwd = 0.5)
#       
#       # move
#       x2 <- t
#       y2 <- y - gap  # this is the thing that will have to be angled 
#       lines(c(x1,x2), c(y1,y2), type = "l", lwd = 0.5)
#     }
# 
# }
# 

# TODO:
# comment code
# make plot axes nicer








