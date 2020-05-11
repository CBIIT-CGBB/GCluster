rm(list=ls());

library(GCluster);

set.seed(1234);
dat   <- read.table("./example_data/tsne_dat2_coord_30_1000.txt", header=T, row.names=1);

colnames(dat) <- c("tSNE 1", "tSNE 2");
pdf("03do_GCluster02_1.pdf", 8,8);
par(mfrow=c(2,2));
out <- preGCluster(dat=dat, plot.out=T);
dev.off();

pdf("03do_GCluster02_2.pdf", 8,8);
par(mfrow=c(2,2));
for (i in 1:length(out$cluster.out)){
  clu   <- out$cluster.out[[i]];
  clu.i <- out$cluster.out[[i]]$membership;
  clu.n <- length(unique(clu.i));
  cols  <- rainbow(clu.n, alpha=0.5);
  main  <- paste0("clu.n:", out$cluster.table[[i,2]], " weight:", round(out$cluster.table[[i,3]], 2));
  plot(dat, pch=19, col=cols[clu.i], main=main, xlab="tSNE 1", ylab="tSNE 2");
}
dev.off();

