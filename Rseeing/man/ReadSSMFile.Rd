\name{ReadSSMFile}
\alias{ReadSSMFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ReadSSMFile
}
\description{
Lit le fichier produit par le SSM et cree une serie temporelle de type zoo. le format des dates est de type POSIX et prend en compte les fractions de secondes
}
\usage{
ReadSSMFile(file = NULL, start = NULL, end = NULL, tz = Sys.timezone())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
le nom du fichier, avec son chemin , i.e. "C:/un_repetoire/lefichierssm""
}
  \item{start}{
evntuellement on peut faire ne prendre qu'une partie du fichier, et definir un instant de debut 
}
  \item{end}{
evntuellement on peut faire ne prendre qu'une partie du fichier, et definir un instant de debut
}
  \item{tz}{
Une zone temporelle. 
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
seeing <- ReadSSMFile(file="monfichier1.txt",start="2017-08-27 09:08:39.421",end="2017-08-27 09:08:40.515")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
