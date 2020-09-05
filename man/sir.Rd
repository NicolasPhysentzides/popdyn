\name{sir}
\alias{sir}
\title{SIR model for population dynamics}
\description{
A basic SIR model with 3 compartments; susceptible, infection and recovery processes solved using \code{\link{ode}}
}
\usage{
sir(pars = NULL, init = NULL, time = NULL)
}
\arguments{\item{pars}{\code{\link{vector}} of 2 parameters \code{beta} and \code{gamma}. \code{beta} is the transmission rate and \code{gamma} is the recovery rate. }
\item{init}{\code{\link{vector}} of 3 parameters \code{S} (Susceptible proportion), \code{I} (Infected proportion) and \code{R} (Recovered proportion). These values are the starting values}
\item{time}{time sequence}
}

\value{\code{\link{list}}{The list contains four arguments.First, second and third are the arguments of the function \code{$pars},\code{$init}, \code{$time}. The fourth argument \code{results}, is a \code{\link{data.frame}} and S3 element (class type 'sir'). The results for each stage across a specific time-frame are displayed in this dataframe}
}

\details{
\code{S},\code{I},\code{R} must be positive and S + I + R <= 1.
}

\examples{
\code{# Input parameters and initials stages
parameters <- c(beta = 1.4, gamma = 0.15)
initials <- c(S = 1 - 1e-06, I = 1e-06, R = 0)
times <- seq(from = 0, to = 100, by = 1)

# Solve
sir_model <- sir(pars = parameters, init = initials, time = times)

#Plot
plot(sir_model$results)

}

}

