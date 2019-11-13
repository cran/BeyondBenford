\name{Benf.val}
\alias{Benf.val}
\title{
Benford's values
}
\description{
The function returns Benford's probability that a figure is at a given position.
}
\usage{
Benf.val(fig, dig = 1)
}

\arguments{
  \item{fig}{
The considered figure.
}
  \item{dig}{
The chosen position of the digit (from the left).
}
}

\value{
The function returns Benford's probability.
}
\references{
F. Benford (1938). The law of anomalous numbers. Proceedings of the American Philosophical Society, 78:127-131. 

T. Hill (1995). The significant-digit phenomenon. The American Mathematical Monthly, 102(4):322-327.

R. Newcomb (1881). Note on the frequency of use of the different digits in natural numbers. American Journal of Mathematics, 4:39-40. 
}
\author{
Blondeau Da Silva St\'ephane
}


\examples{

Benf.val(7, dig = 2)

}

