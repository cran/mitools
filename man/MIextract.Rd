\name{MIextract}
\alias{MIextract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract a parameter from a list of results }
\description{
 Used to extract parameter estimates and standard errors from
lists produced by \code{\link{with.imputationList}}.
}
\usage{
MIextract(results, expr, fun)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{A list of objects}
  \item{expr}{an expression}
  \item{fun}{a function of one argument}
}
\value{
  A list
}
\details{
  If \code{expr} is supplied, it is evaluated in each element of
  \code{results}. Otherwise each element of \code{results} is passed as
  an argument to \code{fun}.
  }
\seealso{\code{\link{with.imputationList}}, \code{\link{MIcombine}}}
\examples{
data(smi)
models<-with(smi, glm(drinkreg~wave*sex,family=binomial()))

betas<-MIextract(models,fun=coef)
vars<-MIextract(models, fun=vcov)
summary(MIcombine(betas,vars))
}
\keyword{manip}% at least one, from doc/KEYWORDS

