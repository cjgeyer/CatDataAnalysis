\name{exercise_6.28}
\docType{data}
\alias{exercise_6.28}
\title{Data Set for Exercise 6.28}
\description{Data set for Exercise 6.28.}
\usage{data(exercise_6.28)}
\format{A data frame containing 48 observations (rows)
  and the following columns:
  \describe{
  \item{Occupational_aspirations}{}
  \item{Socioeconomic_status}{}
  \item{IQ}{}
  \item{Residence}{}
  \item{Gender}{}
  \item{counts}{}
  }
}
\source{\url{http://www.stat.ufl.edu/~aa/cda/data.html}}
\references{
  Agresti, A. (2013)
  \emph{Categorical Data Analysis}, Third Edition.
  Hoboken, NJ: John Wiley \& Sons.
  ISBN: 978-0-470-46363-5.
}
\examples{
# in the source was a table rather than a data frame
# to convert to a table do
data(exercise_6.28)
exercise_6.28_as_table <- xtabs(counts ~ ., data = exercise_6.28)
\dontrun{exercise_6.28_as_table}
}
\keyword{datasets}
