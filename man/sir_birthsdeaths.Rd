\name{sir_birthsdeaths}
\alias{sir_birthsdeaths}
\title{SIR model with equal births and deaths for population dynamics}
\description{
A basic SIR mode with equal births and deaths; 3 compartments susceptible, infection and recovery processes solved using \code{\link{ode}}
}
\usage{
sir(pars = NULL, init = NULL, time = NULL)
}
\arguments{\item{pars}{\code{\link{vector}} of 3 parameters \code{beta}, \code{gamma} and \code{mu}. \code{beta} is the transmission rate, \code{gamma} is the recovery rate and \code{mu} is the per capita death rate, and the population level birth rate.}
\item{init}{\code{\link{vector}} of 3 parameters \code{S} (Susceptible proportion), \code{I} (Infected proportion) and \code{R} (Recovered proportion). These values are the starting values.}
\item{time}{time sequence}
}

\value{\code{\link{list}}{The list contains 4 arguments.First, second and third are the arguments of the function \code{$pars},\code{$init}, \code{$time}. The fourth argument \code{results}, is a \code{\link{data.frame}} and S3 element (class type 'sir'). The results for each stage across a specific time-frame are displayed in this dataframe.}
}

\details{
\code{S},\code{I},\code{R} must be positive and S + I + R <= 1.
}

\examples{
\code{# Input parameters and initial stages.
parameters <- c(mu = 0.013,
                beta = 1, gamma = 0.35)
initials <- c(S = 0.1, I = 1 - 1e-4, R = 1 - 0.1 - 1e-4)
times <- 0:365

# Solve
sir.birthsdeaths <- sir_birthsdeaths(pars = parameters, init = initials, time = times)

#Plot
plot(sir.birthsdeaths$results)



}

}

