rm(list=ls());

library(GCluster);
dat  <- read.table("example_data/Rtsne_1_clu.txt", header=T);
cols <- rainbow(length(unique(dat[,3])), alpha=0.8)

pdf("05do_GCluster07_2.pdf", 6,6);
plot(dat[,c(1, 2)], type="n", main="", xlab="", ylab="");
points(dat, pch=19, col=cols[dat[,3]], cex=1.2);
out <- mst_plot(dat=dat[,c(1,2)], col=cols[9], lwd=2);
dev.off();
