
sir_birthsdeaths<- function (pars = NULL, init = NULL, time = NULL){
  require("deSolve")

  if (init[1] < 0) {
    stop("not valid 'S'")
  }

  if (init[2] < 0) {
    stop("not valid 'I'")
  }
  if (init[3] < 0){
    stop("not valid 'R'")
  }


  function2 <- function(time, init, pars) {

    with(as.list(c(init, pars)), {
      dS <- mu - (beta * S * I) - (mu * S)
      dI <- beta * S * I - gamma * I - mu * I
      dR <- gamma * I - mu * R
      list(c(dS, dI, dR))
    })
  }
  init <- c(init["S"], init["I"], init["R"])

    output <- ode(times = time, func = function2, y = init,
                parms = pars)
  output = as.data.frame(output)
  class(output) = c('sir',class(output))
  return(list(pars = pars, init = init,
              time = time, results = output))
}




