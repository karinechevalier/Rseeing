\name{Histogram}
\alias{Histogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Histogram
}
\description{
Calcule l'histograme d'une serie d'acquisition
}
\usage{
Histogram(seeing = NULL, breaks, comment = NULL, titre = NULL, log = FALSE, filename = NULL, directory = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seeing}{
un objet de type zoo tel que lu dans le fichier txt genere dans le SSM
}
  \item{breaks}{
Le nombre de categories discretes de seeing pour creer l'histogramme
}

  \item{titre}{
un titre, chaine de caracteres
}
  \item{log}{
doit on utiliser un echelle logarithmique ? TRUE/FALSE
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
Histogram(seeing,breaks=100,comment = "acquisition 2",directory="C:/SAINTVERAN/",filename = "Histogram")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
