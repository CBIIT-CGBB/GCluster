\name{vizGCluster}
\alias{vizGCluster}

\title{
 graph visualization
}

\description{
 graph visualization
}

\usage{
  vizGCluster(dat=dat, wt=wt, clu=clu, edge.col="blue", edge.adj=2,
              col=rainbow(length(unique(clu)), alpha=0.8), node.adj=10, main=main);
}

\arguments{
   \item{dat}{data.frame or matrix}
   \item{wt}{igraph weight for cutting}
   \item{clu}{cluster of dat}
   \item{edge.col}{edge colors}
   \item{edge.adj}{edge lwd adjust}
   \item{col}{node colors}
   \item{node.adj}{node size adjust}
   \item{main}{main of plot}
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

