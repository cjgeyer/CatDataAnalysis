\name{exercise_8.18}
\docType{data}
\alias{exercise_8.18}
\title{Data for Exercise 8.18 on Dumping Severity}
\description{Data for Exercise 8.18 on dumping severity.}
\usage{data(exercise_8.18)}
\format{A data frame containing 48 observations (rows)
  and the following columns:
  \describe{
  \item{Dumping_severity}{}
  \item{Hospital}{}
  \item{Operation}{}
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
data(exercise_8.18)
exercise_8.18_as_table <- xtabs(counts ~ ., data = exercise_8.18)
\dontrun{exercise_8.18_as_table}
}
\keyword{datasets}
