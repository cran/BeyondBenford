\name{chi2}
\alias{chi2}
\title{
Pearson's chi-squared test
}

\description{
It is a test of goodness of fit to find out whether the distribution of first (second, third or fourth) digit in the studied data differs from two theoretical distributions (that of Benford and that of Blondeau Da Silva) or not. The null hypothesis states that the studied distribution is consistent with the considered theoretical distribution. 
}

\usage{
chi2(dat, mod = "ben", upbound = ceiling(max(dat)), dig = 1, pval = 0)
}

\arguments{
  \item{dat}{
The considered dataset, a data frame containing non-zero real numbers.
}
  \item{mod}{
A character string. If mod="ben", the theorical distribution considered is that of Benford, else it is Blondeau Da Silva's ones which is chosen.
}
  \item{upbound}{
A positive integer, which characterizes the data. All (or most) of the data are lower than this "upper bound".
}
  \item{dig}{
The chosen position of the digit (from the left).
}
  \item{pval}{
If pval=0, the p-value is not returned, else it is available.
}
}

\value{
A data frame containing the Pearson chi-squared statistic (and the associated p-value if requested).
}

\references{
K. Pearson (1900). On the criterion that a given system of deviations from the probable in the case of a correlated system of variables is such that it can be reasonably supposed to have arisen from random sampling. Philosophical Magazine, 50(302):157-175. 
}

\author{
Blondeau Da Silva St\'ephane
}

\note{This warning message can appear: NAs introduced during the automatic conversion.
This is due to the fact that some data are not numerical in the entered dataset. Non numerical values and zeros are not counted.
}


\examples{
data(address_PierreBuffiere)
chi2(address_PierreBuffiere,dig=2,pval=1)
chi2(address_PierreBuffiere,dig=2,pval=1,mod="blo")

}

