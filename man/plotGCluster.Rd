\name{plotGCluster}
\alias{plotGCluster}

\title{
 mapping gene expression on tsne
}

\description{
 mapping gene expression on tsne
}

\usage{
  plotGCluster(dat, coord, gene, col.g=list(c1=c(0,1,1), c2=c(0,1,0), c3=c(1,1,0), alpha=0.6)
               , col.m=rgb(0.6, 0.6, 0.6, 0.4), col.b.p=c(-20, -20, -15, -15), pch=19, cex=0.4, 
               color.spec="rgb", ...)
}

\arguments{
   \item{dat}{gene expression matrix/data.frame, column is for sample}
   \item{coord}{single cell coordinates from tsne}
   \item{gene}{gene(s) should be in column names}
   \item{col.g}{color names for gene expression}
   \item{col.m}{color names for gene expression, the values are zero or NA}
   \item{col.b.p}{color bar position}
   \item{pch}{from plot}
   \item{cex}{from plot}
   \item{color.spec}{from color.scale of plotrix}
   \item{...}{more options of the plot}
}

\author{
 Ying Hu <yhu@mail.nih.gov>
 Chunhua Yan <yanch@mail.nih.gov>
}

\references{
##
}

\examples{
##
}

