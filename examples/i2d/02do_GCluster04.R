rm(list=ls());

library(GCluster);
set.seed(1234);
## k number
f.c <- c(2, 2, 3, 4, 4, 4, 2, 10, 9, 10, 5, 2);
## simulation data size
f.n <- c(rep(400, 6), 1500, 800, 3500, 2500, 2000, 400);

pdf("02do_GCluster04.pdf", 6,6);
par(mfrow=c(2,2));
for (i in 1:12){
  if (i < 10){
    infile <- paste0("../example_figure/p0", i, ".png"); 
  } else {
    infile <- paste0("../example_figure/p", i, ".png"); 
  }
  cat(infile, "\n");
  dat  <- i2d(infile, p.n=f.n[i]);
  dat  <- as.matrix(dat);
  if (i == 12){
    out  <- mstGCluster(dat, k=f.c[i], filter=TRUE, filter.n=10);
  } else {
    out  <- mstGCluster(dat, k=f.c[i], filter=FALSE);
  }
  
  main <- paste0("k= ", f.c[i]);
  cols <- rainbow(f.c[i], alpha=0.5);
  plot(out$dat, col=cols[out$clu$membership], main=main, pch=19);
  if(i==12){
    points(out$outlier, pch=19, col="gray50");
  }
}
dev.off();

