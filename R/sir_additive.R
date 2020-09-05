
sir_additive <- function (pars = NULL, init = NULL, time = NULL, step = 1, ...) 
{
 
  function1 <- function(pars = NULL, init = NULL, time = NULL) {
    function2 <- function(time, init, pars) {
      with(as.list(c(init, pars, step = step)), {
        Psi <- psi * rnorm(1)/sqrt(step)
        dS <- N * mu  - beta * S * S/N - Psi - mu * I
        dI <- S * beta * I/N + Psi - mu * I - gamma * I
        dR <- gamma * I - mu * R
        list(c(dS, dI, dR))
      })
    }
    init <- c(init["S"], init["I"], init["R"])
    output <- matrix(0, nrow = ceiling(time[length(time)]/step), 
                     ncol = 3)
    output <- rbind(as.numeric(init), output)
    t <- 1
    while (t <= time[length(time)] & init[1] > 0 & init[2] > 
           0) {
      sqrt.step <- sqrt(step)
      output.tmp <- ode(times = time[1]:step, func = function2, 
                        y = init, parms = pars, ...)
      init <- output.tmp[nrow(output.tmp), -1]
      output[t + 1, ] <- output.tmp[nrow(output.tmp), -1]
      t <- t + 1
    }
    
    output <- cbind(time, output)
    colnames(output) <- c("time", "S", "I", "R")
    return(output)
  }
  output <- function1(pars = pars, init = init, time = time)
  output = as.data.frame(output)
  class(output) = c('sir',class(output))
  return(list(pars = pars, init = init, 
              time = time, results = output))
}