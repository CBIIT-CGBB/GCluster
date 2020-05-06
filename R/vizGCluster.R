
vizGCluster <- function(dat=dat, wt=wt, clu=clu, edge.col="blue", edge.adj=2,
                        col=rainbow(length(unique(clu)), alpha=0.8), node.adj=10, main=main){
  
  plot(dat, type="n", main=main)
  dat.d   <- dist(dat);
  g       <- graph_from_adjacency_matrix(as.matrix(dat.d), weighted =T, mode = "upper");
  w.i     <- which(E(g)$weight > wt);
  igraph::E(g)$weight[w.i] <- 0;
  
  g.i <- which(E(g)$weight==0);
  g.s <- igraph::delete_edges(g, g.i);
  out           <- as.data.frame(igraph::get.edgelist(g.s));
  names(clu)    <- row.names(dat);
  ## out is named with cluster ID
  out           <- data.frame(n1=clu[as.character(out[,1])], n2=clu[as.character(out[,2])]);

  ## edge numbers between modules
  clu.u <- unique(clu);
  clu.n <- length(clu.u);
  out.s <- c();
  for (i1 in 1:clu.n){
    for (i2 in i1:clu.n){
      if (i1==i2){
        next;
      }
      j1 <- which(out[,1]==i1);
      j2 <- which(out[,2]==i2);
      n  <- intersect(j1, j2);
      out.s <- rbind(out.s, c(i1, i2, length(n)));
    }
  }

  ## coordinates of edges of modules
  for (i in 1:nrow(out.s)){
    if (out.s[i,3]==0){
      next;
    }
    i1 <- as.numeric(out.s[i,1]);
    i2 <- as.numeric(out.s[i,2]);
    j1 <- which(clu==i1);
    x1 <- mean(as.numeric(dat[j1, 1]));
    y1 <- mean(as.numeric(dat[j1, 2]));
    j2 <- which(clu==i2);
    x2 <- mean(as.numeric(dat[j2, 1]));
    y2 <- mean(as.numeric(dat[j2, 2]));
    segments(x1, y1, x2, y2, lwd=out.s[i,3]/edge.adj, col=edge.col);
  }
  
  ## coordinates of modules
  for (i in 1:length(clu.u)){
    clu.i <- clu.u[i];
    j <- which(clu==clu.i);
    x <- mean(as.numeric(dat[j, 1]));
    y <- mean(as.numeric(dat[j, 2]));
    points(x, y, col="white", cex=length(j)/node.adj, pch=19);
    points(x, y, col=col[i], cex=length(j)/node.adj, pch=19);
  }
}