\name{imputationList}
\alias{imputationList}
\alias{print.imputationList}
\alias{rbind.imputationList}
\alias{cbind.imputationList}
\alias{dimnames.imputationList}
\alias{dim.imputationList}
\alias{update.imputationList}
\alias{imputationList.default}
\alias{imputationList.character}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Constructor for imputationList objects }
\description{
Create and update \code{imputationList} objects to be used as input to other
\code{MI} routines.
}
\usage{
imputationList(datasets,...)
\method{imputationList}{default}(datasets,...)
\method{imputationList}{character}(datasets,dbtype,dbname,...)
\method{update}{imputationList}(object,...)
\method{rbind}{imputationList}(...)
\method{cbind}{imputationList}(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datasets}{a list of data frames corresponding to the multiple
    imputations, or a list of names of database tables or views}
  \item{dbtype}{"ODBC" or a database driver name for
    \code{DBI::dbDriver()}}
  \item{dbname}{Name of the database}
  \item{object}{An object of class \code{imputationList}}
  \item{...}{Arguments \code{tag=expr} to \code{update}
    will create new variables \code{tag}  by evaluating \code{expr}
    in each imputed dataset.  Arguments to \code{imputationList()} are
    passed to the database driver}
}
\value{
An object of class \code{imputationList} or \code{DBimputationList}
}
\details{
When the arguments to \code{imputationList()} are character strings a
database-based imputation list is created.  This can be a database
accessed through ODBC with the \code{RODBC} package or a database with a
DBI-compatible driver. The \code{dbname} and \code{\dots} arguments are
passed to \code{dbConnect()} or \code{odbcConnect()} to create a
database connection.  Data are read from the database as needed.

For a database-backed object the \code{update()} method creates variable
definitions that are evaluated as the data are read, so that read-only
access to the database is sufficient.

  }
\examples{
\dontrun{
## CRAN doesn't like this example
data.dir <- system.file("dta",package="mitools")
files.men <- list.files(data.dir,pattern="m.\\\\.dta$",full=TRUE)
men <- imputationList(lapply(files.men, foreign::read.dta))
files.women <- list.files(data.dir,pattern="f.\\\\.dta$",full=TRUE)
women <- imputationList(lapply(files.women, foreign::read.dta))
men <- update(men, sex=1)
women <- update(women,sex=0)
all <- rbind(men,women)
all <- update(all, drinkreg=as.numeric(drkfre)>2)
all
}
}
\keyword{manip}% at aleast one, from doc/KEYWORDS

