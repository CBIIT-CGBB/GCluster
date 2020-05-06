\name{plottjGCluster}
\alias{plottjGCluster}

\title{
 mapping gene expression on trajectory tree
}

\description{
 mapping gene expression on trajectory tree
}

\usage{
  plottjGCluster(dat, gene, method="median", col.g=list(c1=c(0,1,1), c2=c(0,1,0), c3=c(1,1,0), 
                 alpha=0.6), col.b.p, l1, l2, level1, coord, col.bg, l.n, color.spec="hsv", ...);
}

\arguments{
   \item{dat}{gene expression matrix/data.frame, column is for sample}
   \item{gene}{gene(s) should be in row names}
   \item{method}{mean or median}
   \item{col.g}{color names for gene expression}
   \item{col.b.p}{color bar position}
   \item{l1}{level 1 branch layout (matrix/data.frame)}
   \item{l2}{level 2 branch layout (list)}
   \item{level1}{samples in level 1 as list}
   \item{coord}{coordinates of samples (output of tsne)}
   \item{col.bg}{background color}
   \item{l.n}{level 2 samples from smlayout2 (list)}
   \item{color.spec}{color.spec by plotrix}
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

