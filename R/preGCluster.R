
preGCluster <- function(dat=dat, check.n=4, plot.out=FALSE){
  dat.s   <- as.matrix(dat);
  dat.d   <- dist(dat.s);
  
  g       <- graph_from_adjacency_matrix(as.matrix(dat.d), weighted =T, mode = "upper");
  mst.g   <- mst(graph=g, weights=E(g)$weight);
  mst.w   <- sort(E(mst.g)$weight, decreasing = T);
  g.s     <- g;

  out.s <- NULL;
  out.t <- NULL;
  for (i in 1:check.n){
    wt  <- mst.w[i]
    w.i <- which(E(g)$weight > wt);
    igraph::E(g)$weight[w.i] <- 0;
    out        <- cluster_louvain(g, weights = E(g)$weight); 
    out.s[[i]] <- out;
    clu.n      <- unique(out$membership)
    out.t      <- rbind(out.t, c(i, length(clu.n), wt));
    if (plot.out){
      cols <- rainbow(length(clu.n), alpha=0.5);
      main <- paste0("clu.n ", length(clu.n), " weight:", round(wt, 2));
      plot(dat[,c(1,2)], col=cols[out$membership], pch=19, main=main);
    }
    g <- g.s;
  }
  colnames(out.t) <- c("i.num", "cluster.number", "weight");
  return(list(cluster.out=out.s, cluster.table=out.t));
}






