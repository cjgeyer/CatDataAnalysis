\name{exercise_6.7}
\docType{data}
\alias{exercise_6.7}
\title{Data Set for Exercise 6.7}
\description{Data set for Exercise 6.7.}
\usage{data(exercise_6.7)}
\format{A data frame containing 168 observations (rows)
  and the following columns:
  \describe{
  \item{Gender}{}
  \item{Age}{}
  \item{Race}{}
  \item{Satisfaction}{}
  \item{Region}{}
  \item{counts}{}
  }
}
\source{\url{https://users.stat.ufl.edu/~aa/cda/data.html}}
\references{
  Agresti, A. (2013)
  \emph{Categorical Data Analysis}, Third Edition.
  Hoboken, NJ: John Wiley & Sons.
  ISBN: 978-0-470-46363-5.
}
\examples{
# in the source was a table rather than a data frame
# to convert to a table do
data(exercise_6.7)
exercise_6.7_as_table <- xtabs(counts ~ ., data = exercise_6.7)
\dontrun{exercise_6.7_as_table}
}
\keyword{datasets}
