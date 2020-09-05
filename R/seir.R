
seir<- function (pars = NULL, init = NULL, time = NULL){
  require("deSolve")

  if (init[1] < 0) {
    stop("not valid 'S'")
  }
  if (init[2] < 0) {
    stop("not valid 'E'")
  }
  if (init[3] < 0) {
    stop("not valid 'I'")
  }
  if (init[4] < 0){
    stop("not valid 'R'")
  }



  function2 <- function(time, init, pars) {


    with(as.list(c(init, pars)), {
      dS <- mu - (beta * I + mu) * S
      dE <- beta * S * I - (mu + sigma) * E
      dI <- sigma * E -(mu + gamma) * I
      dR <- gamma * I - mu * R
      list(c(dS, dE, dI, dR))
    })
  }
  init <- c(init["S"], init["E"], init["I"], init["R"])

  output <- ode(times = time, func = function2, y = init,
                parms = pars)
  output = as.data.frame(output)
  class(output) = c('seir',class(output))
  return(list(pars = pars, init = init,
              time = time, results = output))
}

