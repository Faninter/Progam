\name{Newton Method-package}
\alias{Newton Method-package}
\alias{Newton Method}
\docType{package}
\title{
Newton Method (short line)

}
\description{
Use Newton Method to find the root of the functions through a series of iteration.
Finally plot x axis the iteration number and the y axis the f(x) value.

}
\details{
\tabular{ll}{
Package: \tab Newton Method\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-12-21\cr
License: \tab GPL version 2 or later\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Fan Jia

Maintainer: Fan Jia<jia@rsm.nl>

}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
~~ documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
# 1. f is a single-argument fuction;
# 2. x0 is the starting point of Newton method;
# 3. delta is the threshold of accepting the result of Newton method;
# 4. N is the maximum of iteration;
# 5. d is used for approximizing f'(x);
# 6. p represents the number of decimals

f <- function (x) {x^2*(x-1000)+1}
x0 <- runif(1)
delta <- 0.01
N <- 100
d <- 0.00001
p <- 6

Solution <- Newton(f, x0, delta, N, d, p)
plotting (Solution)


}
