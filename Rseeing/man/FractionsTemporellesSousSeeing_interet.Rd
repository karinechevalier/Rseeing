\name{FractionsTemporellesSousSeeing_interet}
\alias{FractionsTemporellesSousSeeing_interet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FractionsTemporellesSousSeeing_interet
}
\description{
Calcule, au cours d'une ou plusieurs acquisitions, la fraction du temps passe en dessous d'une ou plusieurs valeurs de seeing.
typiquement pour comparer deux sites ou deux journées
}
\usage{
FractionsTemporellesSousSeeing_interet(series = NULL, seeing_interet = NULL, comment = NULL, noms, filename = NULL, directory = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{series}{
Une liste d'objet de type zoo
}
  \item{seeing_interet}{
une collection de valeurs de seeing ; typiquement c(valeur1,valeur2,...,valeurn)
}
  \item{comment}{
un commentaire a ajouter eventuellement a la figure, sous forme de chaine de caracteres  
}
  \item{noms}{
les chaines de caracteres associees aux series de seeing , typiquement c("site 1","site 2")
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
seeing_interet<-(1:20)/10
FractionsTemporellesSousSeeing_interet(series=list(seeing1,seeing2),seeing_interet = seeing_interet,noms = c("site 1","site 2"),directory="C:/SAINTVERAN/",filename = "FractionsTemporellesSousSeeing_interet")

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
