\name{MIcombine}
\alias{MIcombine}
\alias{MIcombine.default}
\alias{MIcombine.imputationResultList}
\alias{print.MIresult}
\alias{summary.MIresult}
\alias{vcov.MIresult}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multiple imputation inference}
\description{
Combines results of analyses on multiply imputed data sets. A generic
function with methods for \code{imputationResultList} objects and a
default method.  In addition to point estimates and variances,
\code{MIcombine} computes Rubin's degrees-of-freedom estimate and rate
of missing information.
}
\usage{
MIcombine(results, ...)
\method{MIcombine}{default}(results,variances,call=sys.call(),df.complete=Inf,...)
\method{MIcombine}{imputationResultList}(results,call=NULL,df.complete=Inf,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{A list of results from inference on separate imputed datasets}
  \item{variances}{If \code{results} is a list of parameter vectors,
    \code{variances} should be the corresponding variance-covariance
    matrices}
  \item{call}{A function call for labelling the results}
  \item{df.complete}{Complete-data degrees of freedom}
  \item{\dots}{Other arguments, not used}
}
\details{
   The
\code{results} argument in the default method may be either a list of
parameter vectors or a list of objects that have \code{coef} and
\code{vcov} methods.  In the former case a list of variance-covariance
matrices must be supplied as the second argument.

The complete-data degrees of freedom are used when a complete-data
analysis would use a t-distribution rather than a Normal distribution
for confidence intervals, such as some survey applications.
}
\value{
  An object of class \code{MIresult} with \code{summary} and
  \code{print} methods
}
\references{ ~put references to the literature/web site here ~ }
\seealso{ \code{\link{MIextract}}, \code{\link{with.imputationList}}}
\examples{
data(smi)
models<-with(smi, glm(drinkreg~wave*sex,family=binomial()))
summary(MIcombine(models))

betas<-MIextract(models,fun=coef)
vars<-MIextract(models, fun=vcov)
summary(MIcombine(betas,vars))
}
\keyword{htest}% at least one, from doc/KEYWORDS
\keyword{manip}% __ONLY ONE__ keyword per line
