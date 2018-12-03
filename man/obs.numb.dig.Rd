\name{obs.numb.dig}
\alias{obs.numb.dig}
\title{
Frequency of each figure at a given position
}
\description{
The function returns the frequencies of each figure at a given position in the considered dataset.
}
\usage{
obs.numb.dig(dat, dig = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
The considered dataset, a data frame containing non-zero real numbers.
}
  \item{dig}{
The chosen position of the digit (from the left).
}
}

\value{
A vector containing the frequencies of each figure in ascending order. Its length is 9 if dig=1 (the figures ranging from 1 to 9) and 10 if dig>1 (the figures ranging from 0 to 9).
}

\author{
Blondeau Da Silva Stephane
}

\note{This warning message can appear: NAs introduced during the automatic conversion.
This is due to the fact that some data are not numerical in the entered dataset. Non numerical values and zeros are not counted.
}

\examples{
data(census)
obs.numb.dig(census, dig=2)

}

