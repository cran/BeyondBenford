\name{BeyondBenford-package}
\alias{BeyondBenford-package}
\alias{BeyondBenford}
\docType{package}
\title{
Compare the goodness of fit of Benford's and Blondeau Da Silva's digit distributions to a given dataset
}
\description{
The purpose of this package is to compare the goodness of fit of Benford's and Blondeau Da Silva's digit distributions in a dataset. The package is used to check whether the data distribution is consistent with theoretical distributions highlighted by Blondeau Da Silva or not (through the function 'dat.distr'): this ideal theoretical distribution must be at least approximately followed by the data for the use of Blondeau Da Silva's model to be well-founded. It also enables to plot histograms of digit distributions, both observed in the dataset and given by the two theoretical approaches (with the function 'digit.ditr'). Finally, it proposes to quantify the goodness of fit via Pearson's chi-squared test (with the function 'chi2').
}

\author{
Blondeau Da Silva 

Maintainer: Blondeau Da Silva
}
\references{
F. Benford (1938). The law of anomalous numbers. Proceedings of the American Philosophical Society, 78:127-131.

A. Berger and T. Hill (2015). An introduction to Benford's Law. Princeton University Press, Princeton, NJ. ISSN/ISBN: 978-0-691-16306-2.

S. Blondeau Da Silva (2019). Benford or not Benford: a systematic but not always well-founded use of an elegant law in experimental fields. Communications in Mathematics and Statistics. In press.

S. Blondeau Da Silva (2018). Benford or not Benford: new results on digits beyond the first. \url{https://arxiv.org/abs/1805.01291}.

T. Hill (1995). The significant-digit phenomenon. The American Mathematical Monthly, 102(4):322-327.

S. J. Miller, editor (2015). Benford's Law: Theory and Applications. Princeton University Press, Princeton, NJ. ISSN/ISBN: 978-0-691-14761-1.

R. Newcomb (1881). Note on the frequency of use of the different digits in natural numbers. American Journal of Mathematics, 4:39-40.

K. Pearson (1900). On the criterion that a given system of deviations from the probable in the case of a correlated system of variables is such that it can be reasonably supposed to have arisen from random sampling. Philosophical Magazine, 50(302):157-175.
}

\examples{
data(address_PierreBuffiere)
data(census)
data(address_AixesurVienne)

dat.distr(address_PierreBuffiere,nchi=6)
dat.distr(census,theor=0,nclass=100,dig=3)
dat.distr(address_AixesurVienne,upbound=75)

digit.distr(address_AixesurVienne,mod="ben&blo",upbound=75)
digit.distr(address_PierreBuffiere,mod="blo",dig=2)

chi2(address_PierreBuffiere,dig=2,pval=1)
chi2(address_PierreBuffiere,dig=2,pval=1,mod="blo")

}
