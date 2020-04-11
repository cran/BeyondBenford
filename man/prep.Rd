\name{prep}
\alias{prep}

\title{
Data set preparation
}

\description{
The function returns a prepared data frame that can be used by the other functions of the package.
}

\usage{prep(dat)}

\arguments{
  \item{dat}{
The considered dataset, a data frame.
}
}

\value{
The prepared dataset, a data frame containing only numerical values: character strings and NA values are all replaced by 0.
}

\author{
Blondeau Da Silva St\'ephane
}


