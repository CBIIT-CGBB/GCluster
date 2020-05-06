do_filter <- function(dat, filter){
  var1         <- function(x) var(x,na.rm=T)
  vargenes     <- apply(dat, 1, var1)
  rankvar      <- rank(-vargenes)
  restVariance <- rankvar <= filter
  dat          <- dat[restVariance,];
  return(dat);
}

mlGCluster <- function(dat=dat, method=c("randomForest", "xgboost", "party", "CORElearn"), 
                       filter=NULL, cluster=NULL, top.number=200, mtry=2, ntree=50,
                       estimator="ReliefFexpRank", ReliefIterations=100,
                       objective="multi:softprob", eval_metric="mlogloss",    
                       nthread=8, max_depth=16, eta=0.3, gam=0, subsample=1,   
                       colsample_bytree=1, min_child_weight=12, nrounds=20){
  if (length(cluster) < 1){
    stop("cluster is not default.")
  }
  if (ncol(dat) != length(cluster)){
    stop("cluster length should be the same as the number of single cell.")
  }
  if (class(filter)=="numeric"){
    dat <- do_filter(dat, filter);
  }
  m.num <- length(method);
  out.s <- list();
  dat.f <- data.frame(Y=as.factor(cluster), t(dat));
  for (i in 1:m.num){
    if (method[i]=="randomForest"){
      cat("randomForest is running ...\n")
      fit   <- randomForest::randomForest(factor(Y)~., data=dat.f);
      importanceOrder <- order(-fit$importance);
      rf.genes        <- fit$importance[importanceOrder,];
      out             <- data.frame(gene=names(rf.genes), rf.genes);
      out.s[[i]]      <- out[1:top.number,];
      cat("randomForest is done.\n")
    }
    if (method[i]=="party"){
      cat("party is running ...\n")
      cf_un      <- party::cforest_unbiased(mtry=mtry, ntree=ntree);
      cf1        <- party::cforest(Y~., data=dat.f, control=cf_un);
      VI_F3      <- party::varimp(cf1);
      impOrder   <- order(-VI_F3);
      out3.s     <- VI_F3[impOrder];
      out3       <- data.frame(gene=names(out3.s), imp=out3.s);
      out.s[[i]] <- out3[1:top.number,];
      cat("party is done.\n")
    }
    if (method[i]=="CORElearn"){
      cat("CORElearn is running ...\n")
      estReliefF <- attrEval("Y", dat.f, estimator=estimator, ReliefIterations=ReliefIterations);
      impOrder4  <- order(-estReliefF);
      out4.s     <- estReliefF[impOrder4];
      out4       <- data.frame(gene=names(out4.s), imp=out4.s);
      out.s[[i]] <- out4[1:top.number,];
      cat("CORElearn is done.\n")
    }
    if (method[i]=="xgboost"){
      cat("xgboost is running ...\n");
      y     <- cluster - 1;
      dat.t <- t(dat);
      param <- list("objective" = objective, "num_class" = length(unique(y)), "eval_metric" = eval_metric,    
                    "nthread" = nthread, "max_depth" = max_depth, "eta" = eta, "gamma" = gam, "subsample" = subsample,   
                    "colsample_bytree" = colsample_bytree, "min_child_weight" = min_child_weight);
      invisible(capture.output(bst <- xgboost::xgboost(param=param, data = dat.t, label = y, nrounds = nrounds)));
      importance <- xgboost::xgb.importance(feature_names = colnames(dat.t), model = bst);
      out.s[[i]] <- importance[1:top.number,];
      cat("xgboost is done.\n");
    }
  }
  names(out.s) <- method;
  return(out.s);
}
