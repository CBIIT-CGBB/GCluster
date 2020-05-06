\name{mlGCluster}
\alias{mlGCluster}

\title{
 get the gene features by random forest given cluster
}

\description{
 Gene feature selection for clustered by random forest given cluster
}

\usage{
  mlGCluster(dat=dat, method=c("randomForest", "xgboost", "party", "CORElearn"), 
                       filter=NULL, cluster=NULL, top.number=200, mtry=2, ntree=50,
                       estimator="ReliefFexpRank", ReliefIterations=100,
                       objective="multi:softprob", eval_metric="mlogloss",    
                       nthread=8, max_depth=16, eta=0.3, gam=0, subsample=1,   
                       colsample_bytree=1, min_child_weight=12, nrounds=20)
}

\arguments{
   \item{dat}{data.frame or matrix, row is by gene and column is for single cell}
   \item{method}{any one or more method(s) from packages (eg. randomForest, 
                 xgboost, party  CORElearn). The methods is(are) named as the 
                 package name(s).}
   \item{filter}{numeric, top number of most variance genes.}
   \item{cluster}{vector, cluster number}
   \item{top.number}{numeric, top feature number}
   \item{mtry}{party package: cforest_unbiased(mtry)}
   \item{ntree}{party package: cforest(cforest_unbiased(ntree))}
   \item{estimator}{CORElearn package: attr(estimator)}
   \item{ReliefIterations}{CORElearn package: attr(ReliefIterations)}
   \item{objective}{xgboost package: xgboost(objective)}
   \item{eval_metric}{xgboost package: xgboost(eval_metric)}
   \item{nthread}{xgboost package: xgboost(nthread)}
   \item{max_depth}{xgboost package: xgboost(max_depth)}
   \item{eta}{xgboost package: xgboost(eta)}
   \item{gam}{xgboost package: xgboost(gamma)}
   \item{subsample}{xgboost package: xgboost(subsample)}
   \item{colsample_bytree}{xgboost package: xgboost(colsample_bytree)}
   \item{min_child_weight}{xgboost package: xgboost(min_child_weight)}
   \item{nrounds}{xgboost package: xgboost(nrounds)}
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

