\name{DureePasseSousValeursSeeing}
\alias{DureePasseSousValeursSeeing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DureePasseSousValeursSeeing
}
\description{
Calcule la duree passee en dessous d'une selection de valeurs de seeing au cours d'une acquisition
}
\usage{
DureePasseSousValeursSeeing(seeing = NULL, seeing_interet = NULL, comment = NULL, filename = NULL, directory = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seeing}{
un objet de type zoo tel que lu dans le fichier txt genere dans le SSM
}
  \item{seeing_interet}{
une collection de valeurs de seeing ; typiquement c(valeur1,valeur2,...,valeurn)
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
seeing_interet<-(1:20)/10
DureePasseSousValeursSeeing(seeing ,seeing_interet,comment="un petit commentaire",directory="C:/SAINTVERAN/",filename = "DureePasseSousValeursSeeing")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
