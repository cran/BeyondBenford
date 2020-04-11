\name{Blon.val.sd}
\alias{Blon.val.sd}

\title{
Blondeau Da Silva's standard deviations
}
\description{
The function returns the Blondeau Da Silva's standard deviation of the frequency of a digit at a given position (once the associated lower and upper bounds have been specified).
}
\usage{
Blon.val.sd(lwbound, upbound, fig, dig = 1)
}

\arguments{
  \item{lwbound}{
A positive integer, which characterizes the data. All (or most) of the data are greater than this "lower bound".
}
  \item{upbound}{
A positive integer, which characterizes the data. All (or most) of the data are lower than this "upper bound".
}
  \item{fig}{
The considered figure.
}
  \item{dig}{
The chosen position of the digit (from the left).
}
}

\value{
The function returns Blondeau Da Silva's standard deviations of digit frequencies.
}

\author{
Blondeau Da Silva St\'ephane
}

\examples{
Blon.val.sd(171,825, 5, dig = 3)
}
