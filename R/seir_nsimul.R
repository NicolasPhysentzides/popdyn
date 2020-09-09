seir_nsimul <- function(x, params, maxstep = 1000, nsims = 5) {

  require(plyr)


  SEIR.onestep <- function (x, params) {
    S <- x[2]
    E <- x[3]
    I <- x[4]
    R <- x[5]
    N <- S+E+I+R
    beta <- params["beta"]
    mu <- params["mu"]
    gamma <- params["gamma"]
    sigma <- params["sigma"]
    ## each individual rate
    rates <- c(
      birth=mu*N,
      exposed = beta*S*I/N,
      infection=sigma*E,
      recovery=gamma*I,
      sdeath=mu*S,
      edeath = mu*E,
      ideath=mu*I,
      rdeath=mu*R
    )
    ## what changes with each event?
    transitions <- list(
      birth=c(1,0,0,0),
      exposed = c(-1,1,0,0),
      infection=c(0,-1,1,0),
      recovery=c(0,0,-1,1),
      sdeath=c(-1,0,0,0),
      edeath = c(0,-1,0,0),
      ideath=c(0,0,-1,0),
      rdeath=c(0,0,0,-1)
    )
    ## total event rate
    total.rate <- sum(rates)
    ## waiting time
    if (total.rate==0)
      tau <- Inf
    else
      tau <- rexp(n=1,rate=total.rate)
    ## which event occurs?
    event <- sample.int(n=8,size=1,prob=rates/total.rate)
    x+c(tau,transitions[[event]])
  }
  SEIR.simul <- function (x, params) {
    output <- array(dim=c(maxstep+1,5))
    colnames(output) <- names(x)
    output[1,] <- x
    k <- 1
    ## loop until either k > maxstep or
    ## there are no more infectives
    while ((k <= maxstep) && (x["I"] > 0)) {
      k <- k+1
      output[k,] <- x <- SEIR.onestep(x,params)
    }
    as.data.frame(output[1:k,])
  }

  require(plyr)
  simdat <- rdply(
    nsims,
    SEIR.simul(x,params)
  )
  return(simdat)

}



