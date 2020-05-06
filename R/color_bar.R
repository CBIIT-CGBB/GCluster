## color bar
## xl=xleft, yb=ybottom, xr=xright, yt=ytop, colors <- c("","",""...)
color_bar <- function(col.b.p, col.n, alpha, color.spec="rgb", v.min, 
                      v.max, col.b.n=100, b.type="h", title="", col.b=NULL) {
  xl <- col.b.p[1];
  yb <- col.b.p[2];
  xr <- col.b.p[3];
  yt <- col.b.p[4];
  
  nticks <- 11; 
  ticks  <- seq(v.min, v.max, len=nticks);
  tmp.v  <- seq(v.min, v.max, length.out=col.b.n);

  lut   <- color.scale(tmp.v, col.n[[1]], col.n[[2]], col.n[[3]], alpha=alpha, color.spec=color.spec);
  if (length(col.b)>0){
    col.b.l <- length(col.b);
    lut[1:col.b.l] <- col.b;
  }
  scale <- (length(lut)-1)/(v.max-v.min);     
  
  if (b.type=="v"){
    ys    <- (yt-yb)/(length(lut)-1);
    
    yb.old <- yb;
    for (i in 1:(length(lut)-1)) {
      rect(xl, yb.old, xr, yb.old+ys, col=lut[i], border=NA)
      yb.old <- yb.old + ys;
    }
    v.med <- (v.max+v.min)/2;
    
    ym  <- (yt+yb)/2;
    
    v.max <- round(v.max,2);
    v.min <- round(v.min,2);
    v.med <- round(v.med,2);
    x.d   <- xr-xl;
    y.d   <- yt-yb;
    text(xl, yt+y.d/5, title, cex=1);
    text(xl-x.d, yt, v.max, cex=0.6);
    text(xl-x.d, yb, v.min, cex=0.6);
    text(xl-x.d, ym, v.med, cex=0.6);
    segments(xl-x.d/5, yb, xl-x.d/5,  yt, col="black", lwd=0.8);
    segments(xl-x.d/5, yb, xl-x.d/2, yb, col="black", lwd=0.8);
    segments(xl-x.d/5, yt, xl-x.d/2, yt, col="black", lwd=0.8);
    segments(xl-x.d/5, ym, xl-x.d/2, ym, col="black", lwd=0.8);
  } else {
    
    xs    <- (xr-xl)/(length(lut)-1);
    
    xl.old <- xl;
    for (i in 1:(length(lut)-1)) {
      rect(xl.old, yb, xl.old+xs, yt, col=lut[i], border=NA)
      xl.old <- xl.old + xs;
    }
    v.med <- (v.max+v.min)/2;
    
    xm  <- (xl+xr)/2;
    
    v.max <- round(v.max,2);
    v.min <- round(v.min,2);
    v.med <- round(v.med,2);
    x.d   <- xr-xl;
    y.d   <- yt-yb;
    text(xl, yt+x.d, title, cex=1);
    text(xr, yb-y.d, v.max, cex=0.6);
    text(xl, yb-y.d, v.min, cex=0.6);
    text(xm, yb-y.d, v.med, cex=0.6);
    segments(xl, yb-y.d/5, xr,  yb-y.d/5, col="black", lwd=0.8);
    segments(xl, yb-y.d/5, xl,  yb-y.d/2, col="black", lwd=0.8);
    segments(xm, yb-y.d/5, xm,  yb-y.d/2, col="black", lwd=0.8);
    segments(xr, yb-y.d/5, xr,  yb-y.d/2, col="black", lwd=0.8);
  }
}