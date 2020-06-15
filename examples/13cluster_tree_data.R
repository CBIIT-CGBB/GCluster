rm(list=ls());

library(GCluster);

dat <- read.table("../GCluster_test/data_sets/Rtsne_1.txt", header=T);
wt  <- 1.2;
for (k in 2:30){
  cat(k, " ...\n");
  out   <- GCluster(dat=dat[,c(2,3)], wt=wt, k=k);
  df    <- data.frame(dat, clu=out$membership);
  outf  <- paste0("cluster_tree/clu", k, ".txt");
  write.table(df, outf, quote=F, sep="\t", row.names=F);
}

