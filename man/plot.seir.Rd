\name{plot.seir}
\alias{plot.seir}
\title{Plots for seir objects}
\description{
Plot the output of \code{S}, \code{E}, \code{I}, \code{R} for the specified time sequence
}
\usage{
## S3 method for class 'seir'
plot(res)
}
\arguments{\item{res}{\code{\link{data.frame}} of results that were generated. Element can be found using \code{$results}}
}

\value{
A \code{\link{plot}} with specified settings
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

