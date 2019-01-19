\name{theor.distr.val}
\alias{theor.distr.val}
\title{
Theoretical distribution
}

\description{
The function returns the theorical probability distribution described by Blondeau Da Silva for data. If the dataset follows this particular distribution well enough, it enables not to use Benford's values of first (second, third or fourth) digit distribution but rather Blondeau Da Silva's ones. The distribution depends on an upper bound, which characterizes the data.
}

\usage{
theor.distr.val(upbound, dig = 1)
}

\arguments{
  \item{upbound}{
A positive integer, which characterizes the data. All (or most) of the data are lower than this "upper bound".
}
  \item{dig}{
The chosen position of the digit (from the left).
}
}

\value{
The function returns a vector contening the probability distribution of the model determined by the upper bound value.
}

\references{
S. Blondeau Da Silva (2019). Benford or not Benford: a systematic but not always well-founded use of an elegant law in experimental fields. Communications in Mathematics and Statistics. In press.

S. Blondeau Da Silva (2018). Benford or not Benford: new results on digits beyond the first. \url{https://arxiv.org/abs/1805.01291}.
}

\author{
Blondeau Da Silva Stephane
}



\examples{
theor.distr.val(10)

}

