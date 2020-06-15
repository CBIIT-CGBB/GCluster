rm(list=ls());

library(GCluster);

col.l  <- rgb(0.7, 0.7, 0.7, 0.5);
xlab   <- "";
files  <- paste0("cluster_tree/clu", 2:30, ".txt");

pdf("13cluster_tree_plot.pdf", 8, 10);
out.s <- treeGCluster(files=files, k = c(2:30), col.i=4, col.l=col.l, lwd.s=c(1, 20), xlab=xlab);

out   <- out.s$xy;
text(out[,3], out[,4]+0.3, out[,5], cex=0.5);
text(out[,3], out[,4], out[,2], cex=0.4);
clu.n <- unique(out[,1]);
for (i in 2:length(clu.n)){
  j   <- i - 1;
  clu <- clu.n[i];
  out.i <- which(out[,1]==clu);
  cols  <- rainbow(length(out.i), alpha=0.6);
  points(out[out.i, c(3,4)], pch=19, cex=out[out.i,6]*0.6, col=cols);
  #text(out[out.i,3], out[out.i,4]+0.1, cex=0.25);
}

dev.off();

