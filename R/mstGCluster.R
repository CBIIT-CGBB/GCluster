#
#

mstGCluster <- function(dat=dat, k=2, filter=TRUE, filter.n=8, filter.d=0){
  dat.s   <- as.matrix(dat);
  clu.num <- k - 1;
  do_it   <- TRUE;
  dat.d   <- dist(dat.s);
  ## make mst coordinates if k = 0
  if (k == 0) {
    g     <- igraph::graph_from_adjacency_matrix(as.matrix(dat.d), weighted =T, mode = "upper");
    mst.g <- igraph::minimum.spanning.tree(graph=g, weights=E(g)$weight);
    mst.e <- igraph::get.edgelist(mst.g);
    p1 <- c();
    p2 <- c();
    out.s <- NULL;
    for (j in 1:nrow(mst.e)){
      p1.i <- which(row.names(dat) == mst.e[j,1]);
      p2.i <- which(row.names(dat) == mst.e[j,2]);
      out.s <- rbind(out.s, c(row.names(dat)[p1.i], dat[p1.i,1], dat[p1.i,2], 
                              row.names(dat)[p2.i], dat[p2.i,1], dat[p2.i,2]))
    }
    colnames(out.s) <- c("node1", "node1.x", "node1.y", "node2", "node2.x", "node2.y");
    return(out.s);
  }
  
  ## BEGIN filter
  ## if filter is TRUE, the outlier(s) will be removed.
  outlier <- NULL;
  if (filter){
    g       <- graph_from_adjacency_matrix(as.matrix(dat.d), weighted =T, mode = "upper");
    mst.g   <- mst(graph=g, weights=E(g)$weight);
    
    ## filter by the distance or the weight 
    if (filter.d > 0){
      k.i <- which(E(mst.g)$weight > filter.d);
    } else {
      k.clu   <- kmeans(E(mst.g)$weight, 2);
      k.i  <- which(k.clu$cluster==1);
    }
    if (mean(E(mst.g)$weight[k.i]) > mean(E(mst.g)$weight[-k.i])){
      k.cut <- min(E(mst.g)$weight[k.i]); 
    } else {
      k.cut <- min(E(mst.g)$weight[-k.i]); 
    }
    
    e.i    <- which(E(mst.g)$weight > k.cut);
    mst.g1 <- delete.edges(mst.g, e.i)
    c.out  <- components(mst.g1);
    c.tab  <- table(c.out$membership);
    
    ## filter.n: filter
    c.name <- which(c.tab < filter.n);
    if (length(c.name) > 0){
      name.i <- which(c.out$membership %in% c.name);
      dat.s  <- dat[-name.i,];
      outlier <- dat[name.i,];
      dat.d   <- dist(dat.s);  
    } 
  }
  ## END filter
  
  g       <- graph_from_adjacency_matrix(as.matrix(dat.d), weighted =T, mode = "upper");
  mst.g   <- mst(graph=g, weights=E(g)$weight);
  
  rank.w  <- rank(-E(mst.g)$weight);
  edge.i  <- which(rank.w <= clu.num);
  mst.g1  <- delete.edges(mst.g, edge.i)
  c.out   <- components(mst.g1);

  return(list(clu=c.out, dat=dat.s, outlier=outlier));
}






