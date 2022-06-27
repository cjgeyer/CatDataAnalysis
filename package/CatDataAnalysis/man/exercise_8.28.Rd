\name{exercise_8.28}
\docType{data}
\alias{exercise_8.28}
\title{Data for Exercise 8.28 on Satisfaction with Housing}
\description{Data for Exercise 8.28 on satisfaction with housing.}
\usage{data(exercise_8.28)}
\format{A data frame containing 72 observations (rows)
  and the following columns:
  \describe{
  \item{Satisfaction}{}
  \item{Contact}{}
  \item{Influence}{}
  \item{Housing}{}
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
data(exercise_8.28)
exercise_8.28_as_table <- xtabs(counts ~ ., data = exercise_8.28)
\dontrun{exercise_8.28_as_table}
}
\keyword{datasets}
