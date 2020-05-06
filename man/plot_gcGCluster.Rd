\name{plot_hcGCluster}
\alias{plot_hcGCluster}

\title{
 Plot hierarchical clustering by cluster
}

\description{
 Plot hierarchical clustering by cluster
}

\usage{
  plot_hcGCluster(x1, y1, x2, y2, data=data, layout="v", edge.color = NULL,
                  tip.color=NULL, edge.width = 1, lab.d=1, cex=1);
}

\arguments{
   \item{x1, y1}{coordinates of points of cluster plot bottom left}
   \item{x2, y2}{coordinates of points of cluster plot top right}
   \item{data}{cluster oblect, such as hclust output}
   \item{layout}{v: for vertical line; h: horizontal line}
   \item{edge.color}{edge colors}
   \item{tip.color}{tip.colors}
   \item{edge.width}{edge width}
   \item{lab.d}{distance from tip to cluster}
   \item{cex}{tip font size}
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

