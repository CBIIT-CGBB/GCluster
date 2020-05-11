rm(list=ls());

## trajectory of single cell
options(stringsAsFactors = F);
library(GCluster);

tsne  <- read.table("example_data/Rtsne_1_clu.txt", header=T);
m     <- tsne[,c(1:2)];

## select start/end point
m.i   <- which(m[,1] < -10);
m.s   <- m[m.i,];
m.j   <- which.min(m.s[,2]);
m.n   <- row.names(m.s)[m.j];
m.k   <- which(row.names(m)==m.n);

## smooth main branch/trunk
out2  <- tjGCluster2(m, from=m.k, cutoff=26);
l1.i  <- match(out2$node, row.names(m));
l1    <- smlayout(m[l1.i, 1], m[l1.i,2]);
row.names(l1) <- out2$node;

## smooth the branches
out3   <- smlayout2(out2$level2, out2$node, out2$mst, m, l1);
l2     <- out3$l;

m.i <- which(row.names(m) %in% out3$l.n);

cols  <- rainbow(10, alpha=0.8);
col1  <- rainbow(10, alpha=0.6);
col2  <- rainbow(10, alpha=0.2);

pdf("12do_tjGCluster4_1.pdf", 8, 8);
par(mfrow=c(2,2));
plot(m, type="n", xlab="tSNE 1", ylab="tSNE 2", main="Clusters");
points(m, pch=19, col=cols[tsne[,3]], cex=1);
plot(m, type="n", xlab="tSNE 1", ylab="tSNE 2", main="MST and Clusters");
points(m, pch=19, col=cols[tsne[,3]], cex=1);
out <- mst_plot(m, col=cols[9], lwd=2);
plot(m, type="n", xlab="tSNE 1", ylab="tSNE 2", main="Trajectory of the clusters");
points(m, pch=19, col=cols[tsne[,3]], cex=1);
out <- mst_plot(m, col=cols[9], lwd=2);
segments(out2$coord[,1], out2$coord[,2], out2$coord[,3], out2$coord[,4], col="white", lwd=3)
segments(out2$coord[,1], out2$coord[,2], out2$coord[,3], out2$coord[,4], col="red");
plot(m, type="n", xlab="tSNE 1", ylab="tSNE 2", main="Smooth trajectory of the clusters");
points(m, pch=19, col=cols[tsne[,3]], cex=1);
out <- mst_plot(m, col=cols[9], lwd=2);
segments(out2$coord[,1], out2$coord[,2], out2$coord[,3], out2$coord[,4], col="white", lwd=3)
segments(out2$coord[,1], out2$coord[,2], out2$coord[,3], out2$coord[,4], col="red");
points(l1, type="l", col=cols[1], lwd=3)
for (j in 1:length(l2)){
  points(l2[[j]], type="l", lwd=3, col=cols[1]) 
}
dev.off();

pdf("12do_tjGCluster4_2.pdf", 8, 8);
par(mfrow=c(2,2));
wt    <- 1.5;
dat   <- read.table("./example_data/Rtsne_1.txt", header=T, row.names=1);
wc_plot(dat=dat, wt=wt, main="");

plot(m, type="n", xlab="tSNE 1", ylab="tSNE 2", main="");
points(m, pch=19, col=col2[7], cex=1);
points(l1, type="l", col=cols[9], lwd=3)
for (j in 1:length(l2)){
  points(l2[[j]], type="l", lwd=3, col=cols[9]); 
}

cex  <- NULL;
l5.n <- lapply(out2$level1, `[[`, 1);
for(j in 1:length(l5.n)){
  ii  <- grep(l5.n[j], out2$level1);
  num <- length(unlist(out2$level1[[ii[1]]]));
  cex <- c(cex, num);
}

for (i in 1:length(out3$l.n)){
  myl  <- out3$l.n[[i]];
  tmp  <- lapply(myl, `[[`, 1);
  l5.n <- c(l5.n, tmp);
  for (ii in length(myl)){
    tmp <- length(unlist(myl[[ii]]));
    cex <- c(cex, tmp);
  }
}
m.j <- which(row.names(m) %in% l5.n);

plot(m[,1:2], pch=19, col=col2[7]);
segments(out2$coord[,1], out2$coord[,2], out2$coord[,3], out2$coord[,4], col="white", lwd=3)
segments(out2$coord[,1], out2$coord[,2], out2$coord[,3], out2$coord[,4], col="red");
points(m[m.j, c(1,2)], pch=19, col=col1[9], cex=log2(cex+1)/2);

plot(m, type="n", xlab="tSNE 1", ylab="tSNE 2", main="");
## smooth lines
##   trunk of tree/skeleton
points(l1, type="l", col=cols[9], lwd=2);
##   branches
for (j in 1:length(l2)){
  points(l2[[j]], type="l", col=cols[9], lwd=2); 
}
## number of links of the nodes on the lines
##   trunk of tree/skeleton
for(i in 1:nrow(l1)){
  n   <- row.names(l1)[i];
  j   <- grep(n, out2$level1);
  tmp <- length(unlist(out2$level1[[j[1]]]));
  points(l1[i,1], l1[i,2], pch=19, cex=log2(tmp)/2, col=col1[1])
}

##   branches 
for (j in 1:length(l2)){
  ln.l <- l2[[j]];
  ln.n <- out3$l.n[[j]];
  ln.n <- ln.n[-1];
  for (n2 in 1:length(ln.n)){
    tmp <- length(unlist(ln.n[[n2]]));
    points(ln.l[n2,1], ln.l[n2,2], pch=19, cex=log2(tmp)/2, col=col1[1])
  }
}
dev.off();
