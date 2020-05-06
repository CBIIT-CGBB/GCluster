
plottjGCluster <- function(dat, gene, method="median", col.g=list(c1=c(0,1,1), c2=c(0,1,0), c3=c(1,1,0), alpha=0.6), col.b.p, 
                           l1, l2, level1, coord, col.bg, l.n, color.spec="hsv", ...){
  col.n <- col.g[c(1:3)];
  m   <- coord;
  c1  <- col.g$c1;
  c2  <- col.g$c2;
  c3  <- col.g$c3;
  c.a <- col.g$alpha;
  for (g in gene){
    m.i   <- which(row.names(dat)==g);
    v     <- as.numeric(dat[m.i,]);
    v.min <- min(v);
    v.max <- max(v);
    col.v <- color.scale(v[order(v)], c1, c2, c3, c.a, color.spec=color.spec);
    v.o   <- v[order(v)];
    plot(m, type="n", main=g, ...);
    
    ## smooth lines
    ##   trunk of tree/skeleton
    points(l1, type="l", col=col.bg, lwd=1);
    ##   branches
    for (j in 1:length(l2)){
      points(l2[[j]], type="l", col=col.bg, lwd=1); 
    }
    ## number of links of the nodes on the lines
    ##   trunk of tree/skeleton
    for(i in 1:nrow(l1)){
      n   <- row.names(l1)[i];
      j   <- grep(n, level1);
      j.n <- unlist(level1[[j[1]]]);
      tmp <- length(j.n);
      m.i <- which(colnames(dat) %in% j.n);
      m.v <- v[m.i];
      if (method == "median"){
        m.m <- median(m.v, na.rm = T);
      } else if (method == "mean") {
        m.m <- mean(m.v, na.rm = T);
      } else {
        stop("The method should be mean or median.");
      }

      m.s <- abs(v.o - m.m);
      m.j <- which.min(m.s)[1];
      points(l1[i,1], l1[i,2], pch=19, cex=log2(tmp)/2, col=col.v[m.j])
    }
    
    ##   branches 
    for (j in 1:length(l2)){
      ln.l <- l2[[j]];
      ln.n <- l.n[[j]];
      ln.n <- ln.n[-1];
      for (n2 in 1:length(ln.n)){
        j.n <- unlist(ln.n[[n2]]);
        tmp <- length(j.n);
        m.i <- which(colnames(dat) %in% j.n);
        m.v <- v[m.i];
        m.m <- median(m.v);
        m.s <- abs(v.o - m.m);
        m.j <- which.min(m.s)[1];
        points(ln.l[n2,1], ln.l[n2,2], pch=19, cex=log2(tmp)/2, col=col.v[m.j])
      }
    }
    color_bar(col.b.p, col.n=col.n, alpha=0.8, color.spec=color.spec, v.min=v.min, v.max=v.max); 
  }
}