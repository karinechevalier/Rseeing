\name{Quantiles}
\alias{Quantiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantiles
}
\description{
Calcule les pourcentiles d'une acquistion
}
\usage{
Quantiles(seeing = NULL, probas = c(0, 0.05, 0.1, 0.5, 0.95))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seeing}{
un objet de type zoo tel que lu dans le fichier txt genere dans le SSM
}
  \item{probas}{
Les centiles d'interet
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
seeing <- ReadSSMFile(file="monfichier.txt")
Quantiles(seeing)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
