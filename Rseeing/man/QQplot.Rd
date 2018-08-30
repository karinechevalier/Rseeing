\name{QQplot}
\alias{QQplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
QQplot
}
\description{
Comparaison des quantiles de deux series d'acquisition
}
\usage{
QQplot(seeing1 = NULL, seeing2 = NULL, filename = NULL, directory = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seeing1}{
un objet de type zoo tel que lu dans le fichier txt genere dans le SSM
}
  \item{seeing2}{
un objet de type zoo tel que lu dans le fichier txt genere dans le SSM
}
  \item{filename}{
un nom de fichier si on souhaite une sauvergarde de la figure 
}
  \item{directory}{
le repertoire de sauvegarde
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
seeing1 <- ReadSSMFile(file="monfichier1.txt")
seeing2 <- ReadSSMFile(file="monfichier2.txt")
QQplot(seeing1 = seeing1,seeing2 = seeing2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
