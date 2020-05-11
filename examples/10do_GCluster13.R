rm(list=ls());

options(stringsAsFactors = F);
library(GCluster);

dat   <- read.table("./example_data/Rtsne_1.txt", header=T, row.names=1);
clu.d <- read.table("./example_data/Rtsne_topvar_1.txt", header=T, row.names=1);
clu   <- clu.d[,1];
files <- dir("simu", "topvar", full.names=T);
out.s <- simGCluster(dat=dat, clu=clu, files=files);
out   <- out.s$sum;
## bg colors
col1 <- rainbow(length(unique(clu)), alpha=0.2);
## point colors
col2 <- rainbow(length(unique(clu)), alpha=0.6);

cutoff <- 0.75;

pdf("10do_GCluster13.pdf", 10, 5);
par(mfrow=c(1,2));
plot(dat, pch=19, col=col2[clu]);
main <- paste0("cutoff: ", cutoff);
plot(dat, pch=1, col=col1[clu], main=main);
for (i in 1:11){
  n     <- paste0("clu", i);
  clu.i <- which(clu==i);
  ## names of cluster i
  n2    <- row.names(dat)[clu.i];
  ## simulation results
  tmp   <- out[[n]];
  ## selection of the points with high probability
  tmp.i <- which(tmp > cutoff);
  ## names of cluster i in simulation and in probability > the cutoff
  tmp.n <- names(tmp[tmp.i]);
  ## intersection names
  n3    <- intersect(tmp.n, n2);
  dat.i <- which(row.names(dat) %in% n3);
  points(dat[dat.i,], col=col2[i], pch=19);
}
dev.off();
