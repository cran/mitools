\name{imputationList}
\alias{imputationList}
\alias{print.imputationList}
\alias{rbind.imputationList}
\alias{cbind.imputationList}
\alias{dimnames.imputationList}
\alias{dim.imputationList}
\alias{update.imputationList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Constructor for imputationList objects }
\description{
Create and update \code{imputationList} objects to be used as input to other
\code{MI} routines.
}
\usage{
imputationList(datasets)
\method{update}{imputationList}(object,...)
\method{rbind}{imputationList}(...)
\method{cbind}{imputationList}(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datasets}{a list of data frames corresponding to the multiple
    imputations}
  \item{object}{An object of class \code{imputationList}}
  \item{...}{Arguments \code{tag=expr} to \code{update}
    will create new variables \code{tag}  by evaluating \code{expr}
    in each imputed dataset}
}
\value{
An object of class \code{imputationList}
}
\examples{
data.dir<-system.file("dta",package="mitools")
files.men<-list.files(data.dir,pattern="m.\\.dta$",full=TRUE)
men<-imputationList(lapply(files.men, foreign::read.dta))
files.women<-list.files(data.dir,pattern="f.\\.dta$",full=TRUE)
women<-imputationList(lapply(files.women, foreign::read.dta))
men<-update(men, sex=1)
women<-update(women,sex=0)
all<-rbind(men,women)
all<-update(all, drinkreg=as.numeric(drkfre)>2)
all
}
\keyword{manip}% at aleast one, from doc/KEYWORDS

