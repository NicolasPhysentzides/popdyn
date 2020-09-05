\name{plot.sir}
\alias{plot.sir}
\title{Plots for sir objects}
\description{
Plot the output of \code{S}, \code{I}, \code{R} for the specified time sequence
}
\usage{
## S3 method for class 'sir'
plot(res)
}
\arguments{\item{res}{\code{\link{data.frame}} of results that were generated. Element can be found using \code{$results}}
}

\value{
A \code{\link{plot}} with specified settings
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

