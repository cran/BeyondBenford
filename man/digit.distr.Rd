\name{digit.distr}
\alias{digit.distr}
\title{
Distribution of figures in a given position
}
\description{
The function returns histograms of distribution of figures in a given position: (i) in the dataset, (ii) due to Benford, (iii) due to Blondeau Da Silva.
}
\usage{
digit.distr(dat, mod = "ben", upbound = ceiling(max(dat)), dig = 1, 
col = c("#FFFFAA", "#AAFFAA"), colbl = c("#FFFFAA", "#AAFFFF"), 
colbebl = c("#FFFFAA", "#AAFFAA", "#AAFFFF"), main = "Distribution of digits",
legend = TRUE, leg = c("Observed", "Benford"), 
legbebl = c("Observed", "Benford", "Blondeau"), legbl = c("Observed", "Blondeau"))
}

\arguments{
  \item{dat}{
The considered dataset, a data frame containing non-zero real numbers.
}
  \item{mod}{
If mod="ben", the data histogram and that of Benford are displayed, if mod="ben&blo", the data histogram, that of Benford and that of Blondeau Da Silva are plotted, and otherwise the data histogram and that of Blondeau Da Silva are given.
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
A vector containing three colors used to fill the bars of the histogram, if mod="ben&blo".
}
  \item{colbebl}{
A vector containing two colors used to fill the bars of the histogram, if the latter case.
}
  \item{main}{
The title of the graph.
}
  \item{legend}{
If legend=TRUE, the legend is displayed.
}
  \item{leg}{
A two-components vector containing text appearing in the legend, if mod="ben".
}
  \item{legbebl}{
A three-components vector containing text appearing in the legend, if mod="ben&blo".
}
  \item{legbl}{
A two-components vector containing text appearing in the legend, if the latter case.
}
}

\value{
Histograms of distribution of figures in a given position: (i) in the dataset, (ii) due to Benford, (iii) due to Blondeau Da Silva.
}
\references{
F. Benford (1938). The law of anomalous numbers. Proceedings of the American Philosophical Society, 78:127-131.

S. Blondeau Da Silva (2018). Benford or not Benford: a systematic but not always well-founded use of an elegant law in experimental fields. \url{https://arxiv.org/abs/1804.06186}.

S. Blondeau Da Silva (2018). Benford or not Benford: new results on digits beyond the first. \url{https://arxiv.org/abs/1805.01291}.

T. Hill (1995). The significant-digit phenomenon. The American Mathematical Monthly, 102(4):322-327. \doi{10.2307/2974952}.

R. Newcomb (1881). Note on the frequency of use of the different digits in natural numbers. American Journal of Mathematics, 4:39-40. \doi{10.2307/2369148}.
}
\author{
Blondeau Da Silva Stephane
}
\note{This warning message can appear: NAs introduced during the automatic conversion.
This is due to the fact that some data are not numerical in the entered dataset. Non numerical values and zeros are not counted.
}


\examples{
data(address_AixesurVienne)
digit.distr(address_AixesurVienne,mod="ben&blo",upbound=75)

data(address_PierreBuffiere)
digit.distr(address_PierreBuffiere,mod="blo",dig=2)

}

