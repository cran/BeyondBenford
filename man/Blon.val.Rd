\name{Blon.val}
\alias{Blon.val}
\title{
Blondeau Da Silva's values
}
\description{
The function returns Blondeau Da Silva's probability that a figure is at a given position (once the associated upper bound has been specified).
}
\usage{
Blon.val(upbound, fig, dig = 1)
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
}

\value{
The function returns Blondeau Da Silva's probability.
}
\references{
S. Blondeau Da Silva (2018). Benford or not Benford: a systematic but not always well-founded use of an elegant law in experimental fields. \url{https://arxiv.org/abs/1804.06186}.

S. Blondeau Da Silva (2018). Benford or not Benford: new results on digits beyond the first. \url{https://arxiv.org/abs/1805.01291}.
}
\author{
Blondeau Da Silva Stephane
}

\examples{
Blon.val(825, 5, dig = 3)

}

