\name{seir}
\alias{seir}
\title{SEIR model for population dynamics}
\description{
A basic SEIR model with 4 compartments; susceptible, exposed, infection and recovery processes solved using \code{\link{ode}}
}
\usage{
sir(pars = NULL, init = NULL, time = NULL)
}
\arguments{\item{pars}{\code{\link{vector}} of 4 parameters \code{beta},  \code{gamma}, \code{mu} and \code{sigma}. \code{beta} is the transmission rate,\code{gamma} is the recovery rate, \code{mu} is the per capita death rate, and the population level birth rate and \code{sigma} is the rate at which individuals move from the exposed to the infectious classes}
\item{init}{\code{\link{vector}} of 4 parameters \code{S} (Susceptible proportion), \code{E} (Exposed proportion), \code{I} (Infected proportion) and \code{R} (Recovered proportion). These values are the starting values}
\item{time}{time sequence}
}

\value{\code{\link{list}}{The list contains 4 elements. First, second and third are the arguments of the function \code{$pars},\code{$init}, \code{$time}. The fourth argument \code{results}, is a \code{\link{data.frame}} and S3 element (class type 'seir'). The results for each stage across a specific time-frame are displayed in this dataframe}
}

\details{
\code{S},\code{E},\code{I},\code{R} must be positive and S + E + I + R <= 1.
}

\examples{
\code{# Parameters and initial conditions.
parameters <- c(mu = 0.01, beta = 0.5,
                sigma = 1 / 14, gamma = 1 / 7)
initials <- c(S = 0.94, E = 0.01, I = 0.05, R = 0)

# Solve
seir_1 <- seir(pars = parameters, init = initials, time = 0:365)

# Plot
plot(seir_1$results)


}

}
