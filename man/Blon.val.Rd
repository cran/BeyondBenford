\name{Blon.val}
\alias{Blon.val}
\title{
Blondeau Da Silva's values
}
\description{
The function returns Blondeau Da Silva's probability that a figure is at a given position (once the associated upper bound has been specified) and, if requested, the associated standard deviation.
}
\usage{
Blon.val(upbound, fig, dig = 1, sd = 0)
}

\arguments{
  \item{upbound}{
A positive integer, which characterizes the data. All (or most) of the data are lower than this "upper bound".
}
  \item{fig}{
The considered figure.
}
  \item{dig}{
The chosen position of the digit (from the left).
}
\item{sd}{
If sd=0, only the probability is returned. Else, the function returns a dataframe containing the probability and the standard deviation of the expected digit frequency.
}
}

\value{
The function returns Blondeau Da Silva's probability and, if requested, its standard deviation.
}
\references{
S. Blondeau Da Silva (2019). Benford or not Benford: a systematic but not always well-founded use of an elegant law in experimental fields. Communications in Mathematics and Statistics. \doi{10.1007/s40304-018-00172-1}.

S. Blondeau Da Silva (2018). Benford or not Benford: new results on digits beyond the first. \url{https://arxiv.org/abs/1805.01291}.
}
\author{
Blondeau Da Silva St\'ephane
}

\examples{
Blon.val(825, 5, dig = 3)
}

