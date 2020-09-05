sirs_nsimul <- function(x, params, maxstep = 1000, nsims = 5) {

  require(shiny)
  require(plyr)

  SIRS.onestep <- function (x, params) {
    S <- x[2]
    I <- x[3]
    R <- x[4]
    N <- S+I+R
    beta <- params["beta"]
    mu <- params["mu"]
    gamma <- params["gamma"]
    xi <- params["xi"]
    ## each individual rate
    rates <- c(
      birth=mu*N,
      infection=beta*S*I/N,
      recovery=gamma*I,
      susceptible=xi*R,
      sdeath=mu*S,
      ideath=mu*I,
      rdeath=mu*R
    )
    ## what changes with each event?
    transitions <- list(
      birth=c(1,0,0),
      infection=c(-1,1,0),
      recovery=c(0,-1,1),
      susceptible=c(1,0,-1),
      sdeath=c(-1,0,0),
      ideath=c(0,-1,0),
      rdeath=c(0,0,-1)
    )
    ## total event rate
    total.rate <- sum(rates)
    ## waiting time
    if (total.rate==0)
      tau <- Inf
    else
      tau <- rexp(n=1,rate=total.rate)
    ## which event occurs?
    event <- sample.int(n=7,size=1,prob=rates/total.rate)
    x+c(tau,transitions[[event]])
  }
  SIRS.simul <- function (x, params) {
    output <- array(dim=c(maxstep+1,4))
    colnames(output) <- names(x)
    output[1,] <- x
    k <- 1
    ## loop until either k > maxstep or
    ## there are no more infectives
    while ((k <= maxstep) && (x["I"] > 0)) {
      k <- k+1
      output[k,] <- x <- SIRS.onestep(x,params)
    }
    as.data.frame(output[1:k,])
  }

  require(plyr)
  simdat <- rdply(
    nsims,
    SIRS.simul(x,params)
  )
  return(simdat)

}
