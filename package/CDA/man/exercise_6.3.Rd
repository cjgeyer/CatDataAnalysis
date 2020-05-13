\name{exercise_6.3}
\docType{data}
\alias{exercise_6.3}
\alias{exercise_9.13}
\title{Data Set for Exercises 6.3 and 9.13}
\description{Data set for Exercises 6.3 and 9.13.}
\usage{data(exercise_6.3)}
\format{A data frame containing 24 observations (rows)
  and the following columns:
  \describe{
  \item{Birth_control}{}
  \item{Religious_Attendence}{}
  \item{Premarital_Sex}{}
  \item{Political_Views}{}
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
data(exercise_6.3)
exercise_6.3_as_table <- xtabs(counts ~ ., data = exercise_6.3)
\dontrun{exercise_6.3_as_table}
}
\keyword{datasets}
