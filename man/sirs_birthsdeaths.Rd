\name{sirs_birthsdeaths}
\alias{sirs_birthsdeaths}
\title{SIRS model with equal births and deaths for population dynamics}
\description{
A basic SIRS mode with equal births and deaths; 3 compartments susceptible, infection and recovery processes solved using \code{\link{ode}}
}
\usage{
sir(pars = NULL, init = NULL, time = NULL)
}
\arguments{\item{pars}{\code{\link{vector}} of 4 parameters \code{beta}, \code{gamma},\code{mu} and \code{xi}. \code{beta} is the transmission rate, \code{gamma} is the recovery rate, \code{mu} is the per capita death rate, and the population level birth rate and \code{xi}  is the rate which recovered individuals return to the susceptible statue.}
\item{init}{\code{\link{vector}} of 3 parameters \code{S} (Susceptible proportion), \code{I} (Infected proportion) and \code{R} (Recovered proportion). These values are the starting values}
\item{time}{time sequence}
}

\value{\code{\link{list}}{The list contains four arguments.First, second and third are the arguments of the function \code{$pars},\code{$init}, \code{$time}. The fourth argument \code{results}, is a \code{\link{data.frame}} and S3 element (class type 'sir'). The results for each stage across a specific time-frame are displayed in this dataframe}
}

\details{
\code{S},\code{I},\code{R} must be positive and S + I + R <= 1.
}

\examples{
\code{# Input parameters and initial stages.
parameters <- c(mu = 0.2,
                beta = 0.03, gamma = 0.5, xi = 0.05)
initials <- c(S = 0.3, I = 0.4, R = 0.3)
times <- 0:30

# Solve
sirs.birthsdeaths <- sirs_birthsdeaths(pars = parameters, init = initials, time = times)$results

#Plot
plot(sirs.birthsdeaths)



}

}

