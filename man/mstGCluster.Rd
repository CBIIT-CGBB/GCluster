\name{mstGCluster}
\alias{mstGCluster}

\title{
 graph based cluster
}

\description{
 graph based cluster
}

\usage{
  mstGCluster(dat=dat, k=2, filter=TRUE, filter.n=8, filter.d=0);
}

\arguments{
   \item{dat}{data.frame or matrix}
   \item{k}{number of cluster. if k=0, output mst edges only}
   \item{filter}{if TRUE, it will remove noise.}
   \item{filter.n}{if filter is TRUE, it will be used as cluster size cutoff.}
   \item{filter.d}{if filter is TRUE, it will be used as weight/distance cutoff.
                   if filter.d is zero, the cutoff will be calculated by kmeans.
   }
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

