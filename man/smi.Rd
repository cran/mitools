\name{smi}
\alias{smi}
\docType{data}
\title{Multiple imputations}
\description{
  An \code{imputationList} object containing five imputations of data
  from the Victorian Adolescent Health Cohort Study.
}
\usage{data(smi)}
\format{
  The underlying data are in a data frame with 1170 observations on the following 12 variables.
  \describe{
    \item{id}{a numeric vector}
    \item{wave}{a numeric vector}
    \item{mmetro}{a numeric vector}
    \item{parsmk}{a numeric vector}
    \item{drkfre}{a factor with levels \code{Non drinker} \code{not in last wk} \code{<3 days last wk} \code{>=3 days last wk}}
    \item{alcdos}{a factor with levels \code{Non drinker} \code{not in last wk} \code{av <5units/drink_day} \code{av =>5units/drink_day}}
    \item{alcdhi}{a numeric vector}
    \item{smk}{a factor with levels \code{non/ex-smoker} \code{<6 days} \code{6/7 days}}
    \item{cistot}{a numeric vector}
    \item{mdrkfre}{a numeric vector}
    \item{sex}{a numeric vector}
    \item{drinkreg}{a logical vector}
  }
}

\source{
  Carlin, JB, Li, N, Greenwood, P, Coffey, C. (2003) "Tools for
  analysing multiple imputed datasets" The Stata Journal 3; 3: 1-20.
}

\examples{
data(smi)
with(smi, table(sex, drkfre))
model1<-with(smi, glm(drinkreg~wave*sex, family=binomial()))
MIcombine(model1)
summary(MIcombine(model1))
}
\keyword{datasets}
