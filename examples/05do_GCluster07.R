rm(list=ls());

library(GCluster);

dat   <- read.table("example_data/Rtsne_1.txt", header=T, row.names=1);

out.s <- mstGCluster(dat=dat, k=0);
out   <- out.s[,c(2,3,5,6)];
out   <- as.matrix(out);

out2  <- GCluster(dat=dat, wt=4, k=10);
clu.i <- out2$membership;

out.c <- data.frame(dat, cluster=clu.i);
write.table(out.c, "example_data/Rtsne_1_clu.txt", quote=F, sep="\t");

clu.n <- length(unique(clu.i));
cols  <- rainbow(clu.n, alpha=0.5);

pdf("05do_GCluster07.pdf", 6, 6);
plot(dat, type="n", main="", xlab="", ylab="");
points(dat, pch=19, col=cols[clu.i], cex=1.2);
segments(as.numeric(out[,1]), as.numeric(out[,2]), 
         as.numeric(out[,3]), as.numeric(out[,4]), col=cols[9], lwd=2);
dev.off();

