\name{digit.distr}
\alias{digit.distr}

\title{
Distribution of figures in a given position
}
\description{
The function returns histograms of distribution of figures in a given position: (i) in the dataset, (ii) due to Benford, (iii) due to Blondeau Da Silva. Error bars can be added to the plotted histogram.
}
\usage{
digit.distr(dat, mod = "ben", lwbound = max(floor(min(abs(dat))) + 1, (10^(dig - 1))), 
upbound = ceiling(max(dat)), dig = 1, col = c("#E69F00", "#999999"), 
colbl = c("#AAFFAA", "#999999"), colbebl = c("#E69F00", "#AAFFAA", "#999999"), 
main = "Distribution of digits", No.sd = 0, Sd.pr = 0)
}

\arguments{
  \item{dat}{
The considered dataset, a data frame containing non-zero real numbers.
}
  \item{mod}{
A character string. If mod="ben", the data histogram and that of Benford are displayed, if mod="ben&blo", the data histogram, that of Benford and that of Blondeau Da Silva are plotted, and otherwise the data histogram and that of Blondeau Da Silva are given.
}
  \item{lwbound}{
A positive integer, which characterizes the data. All (or most) of the data are greater than this "lower bound".
}
  \item{upbound}{
A positive integer, which characterizes the data. All (or most) of the data are lower than this "upper bound".
}
  \item{dig}{
The chosen position of the digit (from the left).
}
  \item{col}{
A vector containing two colors used to fill the bars of the histogram, if mod="ben".
}
  \item{colbl}{
A vector containing two colors used to fill the bars of the histogram, if both the data histogram and Blondeau Da Silva's histogram are plotted.
}
  \item{colbebl}{
A vector containing three colors used to fill the bars of the histogram, if mod="ben&blo".
}
  \item{main}{
The title of the graph.
}
  \item{No.sd}{
The positive decimal number of standard deviation that defines the confidence intervals i.e. the error bars. If No.sd=0, no error bars are drawn.
}
  \item{Sd.pr}{
If Sd.pr=1, error bars for proportions are plotted  (with No.sd standard deviation confidence intervals). If Sd.pr=0, they are not plotted.
}
}

\value{
Histograms of distribution of figures in a given position: (i) in the dataset, (ii) due to Benford, (iii) due to Blondeau Da Silva.
}
\references{
F. Benford (1938). The law of anomalous numbers. Proceedings of the American Philosophical Society, 78:127-131.

S. Blondeau Da Silva (2020). Benford or not Benford: a systematic but not always well-founded use of an elegant law in experimental fields. Communications in Mathematics and Statistics, 8:167-201. \doi{10.1007/s40304-018-00172-1}.

S. Blondeau Da Silva (2018). Benford or not Benford: new results on digits beyond the first. \url{https://arxiv.org/abs/1805.01291}.

T. Hill (1995). The significant-digit phenomenon. The American Mathematical Monthly, 102(4):322-327.

R. Newcomb (1881). Note on the frequency of use of the different digits in natural numbers. American Journal of Mathematics, 4:39-40.
}
\author{
Blondeau Da Silva St\'ephane
}
\note{
This warning message can appear: NAs introduced during the automatic conversion.
This is due to the fact that some data are not numerical in the entered dataset. Non numerical values and zeros are not counted.
}


\examples{
data(address_AixesurVienne)
digit.distr(address_AixesurVienne,mod="ben&blo",lwbound=2,No.sd=1, Sd.pr=1)

data(address_PierreBuffiere)
digit.distr(address_PierreBuffiere,mod="blo",dig=2)
}

