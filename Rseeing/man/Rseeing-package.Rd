\name{Rseeing-package}
\alias{Rseeing-package}
\alias{Rseeing}
\docType{package}
\title{
\packageTitle{Rseeing}
}
\description{
\packageDescription{Rseeing}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{Rseeing}
\packageIndices{Rseeing}
Package permettant de post-traiter les acquisitions de seeing faites au moyen du SSM.
De maniere generale,chacune des fonctions permet d'afficher les graphiques calcules soit a l'ecran, soit dans un fichier.
Dans ce dernier cas, le repertoire et le nom du fichier sont a indiquer.
}
\author{
\packageAuthor{Rseeing}

Maintainer: \packageMaintainer{Rseeing}
}
\references{
ajouter le papier de reference du SSM
}

\examples{
#Lire un fichier cree par le SSM
ValeursLues1 <- ReadSSMFile(file=mon_fichier)

#on enleve les valeurs nulles de seeing
seeing1<-CleanValues(ValeursLues1)
}
