
# scale between a and b; v: vector
scale.v <- function(v, a, b) {
  v <- v-min(v); 
  v <- v/max(v); 
  v <- v*(b-a);  
  v+a
}

# count row number
count.rows <- function(x){
  order.x <- do.call(order, as.data.frame(x))
  equal.to.previous <-
    rowSums(x[tail(order.x,-1),] != x[head(order.x,-1),])==0
  tf.runs <- rle(equal.to.previous)
  counts <- c(1,
              unlist(mapply( function(x,y) if (y) x+1 else (rep(1,x)),
                             tf.runs$length, tf.runs$value )))
  counts <- counts[ c(diff(counts) <= 0, TRUE ) ]
  unique.rows <- which( c(TRUE, !equal.to.previous ) )
  cbind( counts, x[order.x[ unique.rows ], ,drop=F] )
}
# count row number, simpler? readable?
count.rows2 <- function(x){
  out   <- as.data.frame(table(as.data.frame(x)));
  out.i <- which(out[,3]==0);
  if (length(out.i) > 0){
    out   <- out[-out.i,];
  }
  out.j <- order(out[,1]);
  out   <- out[out.j,];
  out   <- out[,c(3,1,2)];
  out;
}

merge_dat <- function(files, k = 2:30, col.i=3){
  out.s <- NULL;
  for (f in 1:length(files)){
    dat <- read.table(files[f], header=T, row.names=1);
    if (f == 1){
      row.n <- row.names(dat);
    } else {
      ## check row.names
      if (sum(row.n==row.names(dat)) != nrow(dat)){
        stop("The row.names are not consistance in input files.")
      }
    }
    out.s <- cbind(out.s, dat[,col.i]);
  }
  col.n <-  paste0("k", k);
  colnames(out.s)  <- col.n;
  row.names(out.s) <- row.n;
  out.s;
}

treeGCluster <- function(files=files, types=NULL, k=2:30, col.i=3, col.l=col.l, lwd.s=lwd.s, l.adj=1, ...){
  ## merge cluster IDs into one matrix
  dat      <- merge_dat(files=files, k=k, col.i=3);
  
  top.n    <- length(k);
  lwd.from <- lwd.s[1]; 
  lwd.to   <- lwd.s[2];
  col.n    <- ncol(dat);
  row.n    <- nrow(dat);
  med.n    <- median(c(1:top.n));   # x coordinate of root, center of page
  
  x.len <-  seq(-med.n, med.n, length.out=top.n+1)
  plot(x.len, 0:top.n+1, type="n", ylab="", axes=FALSE, ...);
  
  x1 <- 0;
  x2 <- 0;
  y1 <- top.n + 1;
  y2 <- top.n;
  x.old <- c(1:top.n+1);
  x.tmp <- c(1:top.n+1);
  
  x.old[1] <- x2;
  y.old    <- y2;
  y        <- y2;
  x        <- x2;
  
  lwd <- scale.v(c(100, row.n, row.n), lwd.from, lwd.to);
  segments(x1, y1, x2, y2, col= col.l, lwd = lwd[2]);
  dat.0 <- rep(1, row.n);
  
  ## output, out.s
  ## Cluster#, ClusterID, x, y, Obj# and lwd
  out.s <- c(1, 1, x1, y2, row.n, lwd[2]);
  ## for each k
  for (j in 1:top.n){
    clu.n        <- dat[,j];
    names(clu.n) <- row.names(dat);
    y     <- y - 1 * l.adj;
    if (j == 1){
      str <- cbind(dat.0, dat[,j]);
    } else {
      str <- cbind(dat[,j-1], dat[,j]);
    }
    str.out <- count.rows2(str);
    str.u   <- as.numeric(sort(unique(str.out[,3]))); 
    cols    <- rainbow(length(str.u), alpha=0.6)
    for (n in str.u){
      ## length(str.i) should be 1.
      str.i <- which(str.out[,3]==n);
      num <- 0;
      for (i in 1:length(str.i)){
        lwd <- scale.v(c(100, str.out[str.i[i],1], row.n), lwd.from, lwd.to);
        x   <- n - length(str.u)/2 - 0.5;
        segments(x.old[str.out[str.i[i],2]], y.old, x, y, col= col.l, lwd = lwd[2]);
        num <- num + str.out[str.i[i],1];
        x.tmp[str.out[str.i[i],3]] <- x;
      }
      out.s <- rbind(out.s, c(k[j], n, x, y, num, lwd[2]));
    }
    x.old <- x.tmp;
    y.old <- y;
  }
  colnames(out.s) <- c("cluster.number", "cluster.ID", "x", "y", "number", "lwd");
  row.names(out.s) <- c(1:nrow(out.s));
  return(list(xy=out.s, clu.table=dat));
}


