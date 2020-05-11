rm(list=ls());

library(GCluster);

dat   <- read.table("./example_data/tsne_dat2_coord_30_1000.txt", header=T, row.names=1);
wts   <- seq(1, 3, length.out=4);

pdf("02do_GCluster03_1.pdf", 12, 12);
par(mfrow=c(2,2), mar=c(2,2,2,2));
for (wt in wts){
  main <- paste("weight:", round(wt, 2))
  wc_plot(dat=dat, wt=wt, main=main);
}
dev.off();

