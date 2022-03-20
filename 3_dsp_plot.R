

plot_optimal_strat <- function(optimal_strategy_output,
                               h = 1:2,
                               s = 1:2,
                               l = 1:2,
                               f = 1:3){
  maxt <- optimal_strategy_output[[2]]["maxt"]
  strat <- optimal_strategy_output[[1]]
  events <- c("build", "save", "move")
  colors <- c("#3F3430", "#7DCFF7","#AE7A7C")
  
  # build = black
  # save = light blue
  # move = brick

  plot(x = c(1), y = c(1), 
       main = "Optimal strategy",
       xlab = "time", 
       ylab = "Behavioral frequency", 
       type = "n", 
       xlim = c(1, maxt), 
       ylim = c(0, dim(strat)[1]*dim(strat)[2]*dim(strat)[3]*dim(strat)[4] + 10),
       bty = "n"
  )
  for (e in 1:length(events)) {
    xs <- c()
    ys <- c()
    for(i in 1:maxt){
      xs <- c(xs, i)
      t <- table(strat[h, s, l, f, i])
      if (events[e] %in% names(t)) {
        y <- t[[events[e]]]
      } else {
        y <- 0
      }
      ys <- c(ys, y)
    }
    lines(xs, ys, col=colors[e])
  }
  
  legend(x = 8, y = 20, 
         legend = events,
         col = colors, 
         cex = 0.5, 
         lty = 1, 
         box.lty = 0,
         bg = "transparent"
         )
  
}


# TODO:
