\name{sir_additive}
\alias{sir_additive}
\title{SIR model with equal births and deaths and constant additive noise}
\description{
A basic SIR model with equal births and deaths; 3 compartments susceptible, infection and recovery and additive noise added to the transimtion rate. The processes is solved using \code{\link{ode}}
}
\usage{
sir(pars = NULL, init = NULL, time = NULL)
}
\arguments{\item{pars}{\code{\link{vector}} of 5 parameters \code{beta}, \code{gamma}, \code{mu}, \code{psi} and \code{N}. \code{beta} is the transmission rate, \code{gamma} is the recovery rate, \code{mu} is the per capita death rate, and the population level birth rate, \code{psi} is the noise term and \code{N} the entire population}
\item{init}{\code{\link{vector}} of 3 parameters \code{S} (Number of Susceptibles), \code{I} (Number of Infections) and \code{R} (Number of Recoeverds). These values are the starting values}
\item{time}{time sequence}
}

\value{\code{\link{list}}{The list contains 4 elements.First, second and third are the arguments of the function \code{$pars},\code{$init}, \code{$time}. The fourth argument \code{results}, is a \code{\link{data.frame}} and S3 element (class type 'sir'). The results for each stage across a specific time-frame are displayed in this dataframe}
}

\details{
\code{S},\code{I},\code{R} must be positive and S + I + R <= N.
}

\examples{
\code{# Input parameters and initial stages.
parameters <- c(mu = 0.003,
                beta = 1, gamma = 0.1, psi = 0.9, N = 40000)
initials <- c(S = 20000, I = 500, R =0)
times <- 0:30

# Solve
sir.additive <- sir_additive(pars = parameters, init = initials, time = times)

#Plot
plot(sir.additive$results)



}

}

