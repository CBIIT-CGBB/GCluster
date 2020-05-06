
plotGCluster <- function(dat, coord, gene, col.g=list(c1=c(0,1,1), c2=c(0,1,0), c3=c(1,1,0), alpha=0.6)
                         , col.m=rgb(0.6, 0.6, 0.6, 0.4), col.b.p=c(-20, -20, -15, -15), pch=19, cex=0.4, 
                         color.spec="rgb", ...){
  col.n <- col.g[c(1:3)]; 
  g.i   <- which(row.names(dat)==gene);
  if (length(g.i) < 1){
    stop("check the gene name: ", gene);
  }
  if (!sum(colnames(dat)==row.names(coord))==ncol(dat)){
    stop("The dat colnames are different from coord row names.")  
  }
  val   <- as.numeric(dat[g.i,]);
  zero.i <- which(val==0);
  if (length(zero.i) > 0){
    coord0  <- coord[zero.i,];
    coord1  <- coord[-zero.i,];
    val1    <- val[-zero.i];
  } else {
    coord1 <- coord;
    val1   <- val;
  }
  
  v.min <- min(val, na.rm = T);
  v.max <- max(val, na.rm = T);
  val1.i <- order(val1);
  col   <- plotrix::color.scale(val1[val1.i], col.g$c1, col.g$c2, col.g$c3, alpha=col.g$alpha);
  if (length(zero.i)>0){
    points(coord0[,1], coord0[,2], pch=pch, cex=0.2, col=col.m, ...);
  }
  points(coord1[val1.i,1], coord1[val1.i,2], pch=pch, cex=cex, col=col, ...);
  color_bar(col.b.p, col.n=col.n, alpha=0.8, color.spec=color.spec, v.min=v.min, v.max=v.max); 
}

