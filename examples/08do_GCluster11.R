rm(list=ls());

options(stringsAsFactors = F);
library(GCluster);

pdf("08do_GCluster11.pdf", 8, 8);
par(mfrow=c(2,2));

dat   <- read.table("./example_data/Rtsne_1.txt", header=T, row.names=1);
clu.d <- read.table("./example_data/Rtsne_topvar_1.txt", header=T, row.names=1);
wt    <- 1.5;
main  <- "";
clu  <- clu.d[,1];
cols <- rainbow(10, alpha=0.8);
col2 <- rainbow(length(unique(clu)), alpha=0.6);
plot(dat, col=col2[clu], pch=19);
vizGCluster(dat=dat, wt=wt, clu=clu, edge.col=cols[7], edge.adj=2,
            col=col2[unique(clu)], node.adj=10, main=main);

dat   <- read.table("./example_data/tsne_dat2_coord_30_1000.txt", header=T, row.names=1);
clu.d <- read.table("./example_data/mat_30_1000_3.5.txt", header=T, row.names=1);
wt    <- 1.5;
main  <- "";
clu  <- clu.d[,1];
cols <- rainbow(10, alpha=0.8);
col2 <- rainbow(length(unique(clu)), alpha=0.6);
plot(dat, col=col2[clu], pch=19);
vizGCluster(dat=dat, wt=wt, clu=clu, edge.col=cols[7], edge.adj=2,
            col=col2[unique(clu)], node.adj=40, main=main);

dev.off();
