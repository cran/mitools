\name{with.imputationList}
\alias{with.imputationList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Evaluate an expression in multiple imputed datasets}
\description{
  Performs a computation of each of imputed datasets in \code{data}
  }
\usage{
\method{with}{imputationList}(data, expr, fun, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{An \code{imputationList} object }
  \item{expr}{An expression }
  \item{fun}{A function taking a data frame argument}
  \item{...}{Other arguments, passed to \code{fun} }
}
\value{
 Either a list or an \code{imputationResultList} object
}
\details{
 If \code{expr} is supplied, evaluate it in each dataset in \code{data};
if \code{fun} is supplied, it is evaluated on each dataset. If all the
results inherit from \code{"imputationResult"} the return value is an
\code{imputationResultList} object, otherwise it is an ordinary list.
}
\seealso{\code{\link{imputationList}} }
\examples{
data(smi)
models<-with(smi, glm(drinkreg~wave*sex,family=binomial()))
tables<-with(smi, table(drkfre,sex))
with(smi, fun=summary)
}
\keyword{manip}% at least one, from doc/KEYWORDS

