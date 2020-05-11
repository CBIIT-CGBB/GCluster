rm(list=ls());

library(GCluster);

dat   <- read.table("./example_data/Rtsne_1.txt", header=T, row.names=1);
## weight
wt    <- 4;
set.seed(1234);
## k numbers
k.s   <- c(5, 10, 15, 20)

pdf("do_GCluster06.pdf", 8, 8);
par(mfrow=c(2,2))
for (j in 1:length(k.s)){
  k     <- k.s[j];
  out   <- GCluster(dat=dat, wt=wt, k=k);
  clu.i <- out$membership;
  clu.n <- length(unique(clu.i));
  cols  <- rainbow(clu.n, alpha=0.5);
  main  <- paste0("clu.n:", clu.n, " weight:", wt);
  plot(dat, pch=19, col=cols[clu.i], main=main);
}
dev.off();

pdf("04do_GCluster06_kmeans.pdf", 8, 8);
par(mfrow=c(2,2))
for (j in 1:length(k.s)){
  k     <- k.s[j];
  out   <- kmeans(dat, k);
  clu.i <- out$cluster;
  clu.n <- length(unique(clu.i));
  cols  <- rainbow(clu.n, alpha=0.5);
  main  <- paste0("clu.n:", clu.n, " weight:", wt);
  plot(dat, pch=19, col=cols[clu.i], main=main);
}
dev.off();
