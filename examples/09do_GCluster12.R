rm(list=ls());

options(stringsAsFactors = F);
library(GCluster);

dat   <- read.table("./example_data/Rtsne_1.txt", header=T, row.names=1);
dat   <- as.matrix(dat);
clu.d <- read.table("./example_data/Rtsne_topvar_1.txt", header=T, row.names=1);

clu  <- clu.d[,1];
clu.n <- length(unique(clu));
col2  <- rainbow(clu.n, alpha=0.1);
col1  <- rainbow(clu.n, alpha=0.3);
col3  <- rainbow(clu.n, alpha=0.6);
alpha <- c(0.75, 0.5);

pdf("09do_GCluster12.pdf", 10, 5);
par(mfrow=c(1,2));
plot(dat, pch=19, col=col3[clu]);
main <- paste0("alpha: ", paste(alpha, collapse=" & "));
out  <- ctGCluster(dat=dat, alpha=alpha, clu=clu, col1=col1[unique(clu)], 
                  col2=col2[unique(clu)], col3=col3[unique(clu)], main=main);
dev.off();
