plot.sir <- function(res){
  require('ggplot2')
  ggplot(data = res, mapping = aes(x = time)) +
    geom_line(aes(y = S, colour = "susceptibles ratio"), lwd = 1.2) +
    geom_line(aes(y = I, colour = "infectious ratio"), lwd = 1.2) +
    geom_line(aes(y = R, colour = "recovered ratio"), lwd = 1.2) +
    labs(title = "SIR model" , y = "Proportion of population", x = "Time sequence") +
    theme(plot.title = element_text(hjust = 0.5))


}
