rm(list=ls());

options(stringsAsFactors = F);
library(GCluster);

set.seed(1234);
f <- function(x){ (x - min(x))/(max(x)-min(x)) * (1.6 - 0) + 0};
dat            <- i2d(image="../example_figure/p01.png",   p.n=2000);
dat2           <- i2d(image="../example_figure/p_c2.png",  p.n=2000);
dat2           <- dat2[order(dat2[,1]),];
r              <- f(dat2[,2]);
x              <- dat[,1];
z              <- dat[,2] + r;
y              <- seq(1.5, 2.5, length.out = 2000);
## 3d data
out.s          <- data.frame(x=x, y=y, z=z);

## for colors
row.names(dat) <- paste0("n", 1:nrow(dat));
out            <- tjGCluster2(dat);
out2           <- tree_list(out$mst, out$node);
cols           <- rainbow(10, alpha=0.6);

l.n   <- length(out2);
col.n <- colorRampPalette(cols)(l.n);
n2col <- NULL;
for (i in 1:l.n){
  tmp   <- unlist(out2[[i]]);
  tmp.s <- data.frame(n=tmp, cols=rep(col.n[i], length(tmp)))
  n2col <- rbind(n2col, tmp.s);
}

row.names(out.s) <- row.names(dat);
n.i              <- match(row.names(out.s), n2col[,1]);
n2col            <- n2col[n.i,]

#library("plot3D");
#scatter3D(out.s$x, out.s$y, out.s$z,  col = n2col[,2], 
#          pch = 19, cex = 0.5, phi=0, theta = 0);

library(scatterplot3d);
pdf("do_3d_U1.pdf", 8, 8);
scatterplot3d(out.s$x, y=out.s$y, z=out.s$z,
              color = n2col[,2], pch=19);
dev.off();

pdf("do_3d_U2.pdf", 4, 4);
par(mar=c(2,2,2,2));
plot(dat, col=n2col[,2], pch=19, axes=F, xlab="", ylab="", main="", cex=3);
dev.off();

library(rgl);
plot3d(out.s$x, out.s$y, out.s$z, col=n2col[,2], size=5, axes=F, xlab="", ylab="", zlab="")
rgl.postscript("do_3d_U3.pdf","pdf");
rgl.postscript("do_3d_U4.pdf","pdf");

dir.create("do_3d_U_d", showWarnings = FALSE)
for (i in 1:90) {
  view3d(userMatrix=rotationMatrix(2*pi * i/90, 1, -1, -1))
  rgl.snapshot(filename=paste("do_3d_U_d/frame-",
                              sprintf("%03d", i), ".png", sep=""))
}

system("convert -delay 5 -loop 0 do_3d_U_d/frame*.png do_3d_U.gif");
