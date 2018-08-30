\name{QuantilesvsTime}
\alias{QuantilesvsTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
QuantilesvsTime
}
\description{
Calcule l'evolution des centile au cours de l'acquisition, par plages de durees.
}
\usage{
QuantilesvsTime(seeing = NULL, probas = c(0, 0.05, 0.1, 0.5, 0.95), duree_plages = 3600, comment = NULL, filename = NULL, directory = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seeing}{
un objet de type zoo tel que lu dans le fichier txt genere dans le SSM
}
  \item{probas}{
Les centiles d'interet
}
  \item{duree_plages}{
duree des plages temporelles en secondes
}

  \item{comment}{
un commentaire a ajouter eventuellement a la figure, sous forme de chaine de caracteres  
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
seeing <- ReadSSMFile(file="monfichier.txt")
QuantilesvsTime(seeing)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
