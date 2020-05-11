rm(list=ls());

library(GCluster);

dat   <- read.table("./example_data/tsne_dat2_coord_30_1000.txt", header=T, row.names=1);
wt    <- 3.8;
set.seed(1234);
out   <- GCluster(dat=dat, wt=wt);

clu.i <- out$membership;
clu.n <- length(unique(clu.i));
cols  <- rainbow(clu.n, alpha=0.5);
main  <- paste0("clu.n:", clu.n, " weight:", wt);

pdf("01do_GCluster03.pdf", 6,6);
plot(dat, pch=19, col=cols[clu.i], main=main, xlab="tSNE 1", ylab="tSNE 2");
dev.off();

