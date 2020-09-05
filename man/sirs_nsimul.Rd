\name{sirs_nsimul}
\alias{sirs_nsimul}
\title{Stochastic SIRS model with equal birth and deaths for population dynamics using Gillespie’s direct method
}
\description{
A stochastic SIRS model with 3 compartments solved using Gillespie’s direct method.
}
\usage{
sirs_nsimul(x, params, maxstep = 1000, nsims = 5)
}
\arguments{\item{x}{\code{\link{vector}} of 4 parameters \code{time}, \code{S}, \code{I} and \code{R}. \code{time} is the initial starting time, \code{S} is the Susceptibles, \code{I} is the Infecteds and \code{R} is the Recovereds.}
\item{params}{\code{\link{vector}} of 4 parameters \code{beta}, \code{gamma}, \code{mu} and \code{xi}. \code{beta} is the transmission rate, \code{gamma} is the recovery rate, \code{mu} is the per capita death rate, and the population level birth rate adn \code{xi} is the is the rate which recovered individuals return to the susceptible statue}
\item{maxstep}{\code{\link{numeric}} value that indicates the maximum number of transiction between the population. Defaults to 1000.}
\item{nsims}{\code{\link{numeric}} value that indicates the number of simulations. Defaults to 5.}
}

\value{\code{\link{data.frame}}{The data.frame contains 5 columns. Second, third,fourth and fifth are the arguments of the input vector \code{x}. The first column indicates the specific number of the simulation. The column takes (\code{.n}) values from 1 up to \code{nsims} increasing by 1.}
}

\details{
\code{S},\code{I},\code{R} must be positive and \code{S + I + R = N}. \code{N} is the whole population. Every simulation stops when there are no Infecteds or when the maximum number of steps is reached.
}

\examples{
\code{
library('plyr')
set.seed(0)

nsims <- 8
xstart <- c(time=0,S=595,I=5,R=0) #initial conditions
params <- c(mu=0.015,beta=20,gamma=13, xi=5) #parameters

res <- sirs_nsimul(x = xstart, params = params, nsims = nsims)

par(mfrow=c(3,1), mar=c(2,4,1,1))

plot(S~time,data=res,type='n')
d_ply(res,".n",function(x)lines(S~time,data=x,col=.n))

plot(I~time,data=res,type='n')
d_ply(res,".n",function(x)lines(I~time,data=x,col=.n))

plot(R~time,data=res,type='n')
d_ply(res,".n",function(x)lines(R~time,data=x,col=.n))

}

}

