rm(list=ls());

library(GCluster);

dat   <- read.table("example_data/Rtsne_1_clu.txt", header=T, row.names=1);
clu.u <- unique(dat[,3]);
cols  <- rainbow(length(clu.u), alpha=0.6);
col0  <- rainbow(length(clu.u), alpha=1);
out   <- hcGCluster(dat[,c(1,2)], dat[,3]);

pdf("11do_hcGCluster.pdf", 8, 8);
par(mfrow=c(2,2));
plot(dat[,c(1:2)], pch=19, col=cols[dat[,3]]);
for (clu in clu.u){
  dat.i <- which(dat[,3]==clu);
  dat.s <- dat[dat.i,];
  x     <- mean(dat[dat.i,1]);
  y     <- mean(dat[dat.i,2]);
  shadowtext(x, y, clu, cex=1.2);
}

par(mar=c(1,1,4,2));
col1 <- rep(col0[7], length(clu.u));
col1[c(1, 9)]    <- col0[2];
col1[c(2, 5, 10)] <- col0[9];

plot(1:10, 1:10, type="n", axes=F, xlab="", ylab="", main="colored by cell types")
plot_hcGCluster(1, 4, 10, 9.5, out$clu, edge.color=col1, 
            tip.color=col1, lab.d=0.4, cex=2);

plot(1:10, 1:10, type="n", axes=F, xlab="", ylab="", main="colored by clusters")
plot_hcGCluster(3, 1, 9, 10, out$clu, layout="h", edge.color=col0[clu.u], 
            tip.color=col0[clu.u], lab.d=0.4, cex=1.2);

plot(1:10, 1:10, type="n", axes=F, xlab="", ylab="", main="tips colored by clusters\nedge colored by cell type")
plot2_hcGCluster(6, 6, 0.5, 3, out$clu, angle.start=1,  angle.end=360, edge.color=col1, 
            tip.color=col0[clu.u], lab.d=0.4, tip.cex=1);
dev.off();