rm(list=ls());

library(GCluster);

dat   <- read.table("./example_data/Rtsne_1.txt", header=T, row.names=1);
## weight
wt    <- 4;
set.seed(1234);
## k numbers
k.s   <- c(15)

pdf("06do_GCluster08.pdf", 10, 5);
par(mfrow=c(1,2))
for (j in 1:length(k.s)){
  k     <- k.s[j];
  out   <- GCluster(dat=dat, wt=wt, k=k);
  clu.i <- out$membership;
  clu.n <- length(unique(clu.i));
  cols  <- rainbow(clu.n, alpha=0.5);
  main  <- paste0("clu.n:", clu.n, " weight:", wt);
  plot(dat, pch=19, col=cols[clu.i], main=main);
  for(i in 1:clu.n){
    ii <- which(clu.i==i);
    x  <- mean(dat[ii,1]);
    y  <- mean(dat[ii,2]);
    shadowtext(x, y, i, cex=2);
  }
  l    <- list(c(1,3), c(12, 14, 15), c(5, 4));
  mout <- mgGCluster(clu.i, l, rename=T);
  clu.m <- length(unique(mout$renamed));
  cols  <- rainbow(clu.m, alpha=0.5);
  main  <- paste0("clu.m:", clu.m, " weight:", wt);
  plot(dat, pch=19, col=cols[mout$renamed], main=main);
  for(i in 1:clu.m){
    ii <- which(mout$renamed==i);
    x  <- mean(dat[ii,1]);
    y  <- mean(dat[ii,2]);
    shadowtext(x, y, i, cex=2);
  }
}
dev.off();
