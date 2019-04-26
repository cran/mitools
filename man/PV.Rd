\name{withPV}
\alias{withPV}
\alias{withPV.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Analyse plausible values in surveys
}
\description{
Repeats an analysis for each of a set of 'plausible values' in a data
set, returning a list suitable for \code{MIcombine}. That is, the data
set contains some sets of columns where each set are multiple
imputations of the same variable. With
\code{rewrite=TRUE}, the \code{action} is rewritten to reference each
plausible value in turn; with code{rewrite=FALSE} a new data set is
constructed for each plausible value, which is slower but more general.
}
\usage{
withPV(mapping, data, action, rewrite=TRUE, ...)
\S3method{withPV}{default}(mapping, data, action, rewrite=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
A formula or list of formulas describing each variable in the analysis that has plausible values. The left-hand side of the formula is the name to use in the analysis; the right-hand side gives the names in the dataset.
}
  \item{data}{
A data frame. Methods for \code{withPV} dispatch on this argument, so
can be written for, eg, survey designs or out-of-memory datasets.
}
\item{action}{
  With \code{rewrite=TRUE}, a quoted expression specifying the analysis,
  or a function taking a data frame as its only argument.  With  \code{rewrite=FALSE},
A function taking a data frame as its only argument, or a quoted
expression with \code{.DATA} referring to the newly-created data frame to be used.
}
\item{rewrite}{
Rewrite \code{action} before evaluating it (versus constructing new data
sets)
  }

  \item{\dots}{
For methods
}
}
\value{
A list of the results returned by each evaluation of \code{action}, with the call as an attribute.
}

\note{I would be interested in seeing naturally-occurring examples where
\code{rewrite=TRUE} does not work}
\seealso{
\code{\link{pisamaths}}
  
\code{\link{with.imputationList}}
}
\examples{
data(pisamaths)

models<-withPV(list(maths~PV1MATH+PV2MATH+PV3MATH+PV4MATH+PV5MATH), data=pisamaths,
       action= quote(lm(maths~ ST04Q01*(PCGIRLS+SMRATIO)+MATHEFF+OPENPS,
       data=.DATA)),
       rewrite=FALSE
)

summary(MIcombine(models))

## equivalently
models2<-withPV(list(maths~PV1MATH+PV2MATH+PV3MATH+PV4MATH+PV5MATH), data=pisamaths,
       action=quote( lm(maths~ST04Q01*(PCGIRLS+SMRATIO)+MATHEFF+OPENPS)), rewrite=TRUE)


summary(MIcombine(models2))



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
