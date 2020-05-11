rm(list=ls());

library(GCluster);

dat   <- read.table("./example_data/Rtsne_1.txt", header=T, row.names=1);
## weight
wt    <- 4;
set.seed(1234);
## k numbers
k.s   <- c(5, 10, 15, 20)

pdffile <- paste0("07do_GCluster10_1.pdf");
pdf(pdffile, 12, 12);
par(mfrow=c(4, 4), oma=c(5.5, 5.5, 5.5, 5.5), mar=rep(.1, 4));

for (j in 1:length(k.s)){
  k   <- k.s[j];
  m.i <- 1;
  for (m in c("louvain", "fast_greedy", "infomap", "label_prop")){
    print(m)
    out   <- GCluster(dat=dat, wt=wt, k=k, method=m);
    clu.i <- out$membership;
    clu.n <- length(unique(clu.i));
    cols  <- rainbow(clu.n, alpha=0.5);
    if (j==length(k.s)){
      if (m.i==1){
        plot(dat, pch=19, col=cols[clu.i], main=main, ann=FALSE);
      } else {
        plot(dat, pch=19, col=cols[clu.i], main=main, ann=FALSE, yaxt="n");
      }
    } else if (m.i==1){
      plot(dat, pch=19, col=cols[clu.i], main=main, ann=FALSE, xaxt="n");
    } else {
      plot(dat, pch=19, col=cols[clu.i], main=main, ann=FALSE, xaxt="n", yaxt="n");
    }
    m.i <- m.i + 1;
  }
}
#title("Clustering examples with GCluster package", outer=TRUE)
m.n <- c("louvain", "fast_greedy", "infomap", "label_prop");
m.n <- gsub("_", " ", m.n);
mtext(m.n, 3, 1, cex=2, outer=TRUE, at=seq(0.15, 0.85, length.out=4));
mtext(c("k=20", "k=15", "k=10", "k=5"), 2, 3, cex=2, outer=TRUE, las=0, at=seq(0.15, 0.85, length.out=4));
dev.off();

