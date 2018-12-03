\name{dat.distr}
\alias{dat.distr}
\title{
Data distribution
}
\description{
The function returns the histogram of the data. It can also plot one of the Blondeau Da Silva's theoretical distributions (thanks to an "upper bound"): this ideal theoretical distribution must be at least approximately followed by the data for the use of Blondeau Da Silva's model to be well-founded. A specific chi-squared statistic can also be computed to find out whether the data distribution is consistent with the theoretical distribution or not. 
}
\usage{
dat.distr(dat, xlab = "data", ylab = "Frequency", main = "Distribution of data", 
theor = TRUE, nclass = 50, col = "lightblue", conv = 0, upbound = ceiling(max(dat)),
dig = 1, colt = "red", ylim = NULL, border = "blue", nchi = 0, legend = TRUE, 
bg.leg = "gray85")
}

\arguments{
  \item{dat}{
The considered dataset, a data frame containing non-zero real numbers.
}
  \item{xlab}{
The x-axis label.
}
  \item{ylab}{
The y-axis label.
}
  \item{main}{
The title of the graph.
}
  \item{theor}{
If theor=TRUE Blondeau Da Silva's theoretical distribution is plotted, otherwise only the histogram is represented.
}
  \item{nclass}{
A strictly positive integer: the number of classes in the histogram.
}
  \item{col}{
The color used to fill the bars of the histogram. NULL yields unfilled bars.
}
  \item{conv}{
If conv=1, all values of the dataset are multiplied by 10^k where k is the smallest positive integer such that all non-zero numerical values in the newly multiplied data frame have an absolute value greater than or equal to 1.
}
  \item{upbound}{
A positive integer, which characterizes the data. All (or most) of the data are lower than this "upper bound".
}
  \item{dig}{
The chosen position of the digit (from the left).
}
  \item{colt}{
The color used to plot Blondeau Da Silva's theoretical distribution.
}
  \item{ylim}{
A two-components vector: the range of y values.
}
  \item{border}{
The color of the border around the bars.
}
  \item{nchi}{
A positive integer: the number of classes for values from 10^(p-1) to max(max(data),upbound). If nchi>0, the function returns the chi-squared statistic (with nchi-1 degrees of freedom) of goodness of fit determined by the different classes. The null hypothesis states that the studied distribution is consistent with the considered theoretical distribution. 
}
  \item{legend}{
If legend=TRUE, the legend is displayed.
}
  \item{bg.leg}{
The background color for the legend box.
}
}

\value{
The histogram of the data along with optional Blondeau Da Silva's theoretical distributions and a data frame containing the chi-squared statistic and its associated p-value if requested.
}
\references{
S. Blondeau Da Silva (2018). Benford or not Benford: a systematic but not always well-founded use of an elegant law in experimental fields. \url{https://arxiv.org/abs/1804.06186}.

S. Blondeau Da Silva (2018). Benford or not Benford: new results on digits beyond the first. \url{https://arxiv.org/abs/1805.01291}.

K. Pearson (1900). On the criterion that a given system of deviations from the probable in the case of a correlated system of variables is such that it can be reasonably supposed to have arisen from random sampling. Philosophical Magazine, 50(302):157-175.
}
\author{
Blondeau Da Silva Stephane
}
\note{This warning message can appear: NAs introduced during the automatic conversion.
This is due to the fact that some data are not numerical in the entered dataset. Non numerical values and zeros are not counted.
}


\examples{
data(address_PierreBuffiere)
dat.distr(address_PierreBuffiere,nchi=6)

data(census)
dat.distr(census,theor=0,nclass=100,dig=3)

data(address_AixesurVienne)
dat.distr(address_AixesurVienne,upbound=75)

}

