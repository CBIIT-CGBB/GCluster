v2df <- function(v){
  n1 <- v[-length(v)];
  n2 <- v[-1];
  out.s <- data.frame(n1, n2);
  return(out.s);
}

list2df <- function(list.n, start.n){
  out.node <- NULL;
  out.n    <- NULL;
  last.n   <- start.n;
  for (n.i in 1:length(list.n)){
    n <- list.n[[n.i]][1];
    out.node <- rbind(out.node, c(last.n, n))
    last.n   <- n;
    n1.n     <- list.n[[n.i]];
    if (length(list.n[[n.i]]) > 1){
      out.n   <- rbind(out.n, c(n, length(n1.n)));
    } else {
      out.n   <- rbind(out.n, c(n, 1));
    }
  }
  return(list(node.df=out.node, node.n=out.n));
}

list2dat <- function(dat, list.n, m.type="median"){
  out.s <- NULL;
  n     <- lapply(list.n, "[[", 1);
  pb <- txtProgressBar(min = 0, max = length(list.n), style = 3);
  for (i in 1:length(list.n)){
    n2   <- unlist(list.n[[i]]);
    n2.i <- which(row.names(dat) %in% n2);
    if (length(n2.i)<1){
      print(n2);
      stop("The node", n2, " was not in the data.")
    }
    dat.s <- dat[n2.i,];
    if (length(n2.i)==1){
      dat.m <- dat.s;
    } else {
      if (m.type=="median"){
        dat.m <- apply(dat.s, 2, function(x) median(x, na.rm=T));
      } else {
        dat.m <- apply(dat.s, 2, function(x) mean(x, na.rm=T));
      }
    }
    out.s <- rbind(out.s, dat.m);
    setTxtProgressBar(pb, i);
  }
  close(pb);
  row.names(out.s) <- n;
  return(out.s);
}

bezierCurve <- function(x, y, n=10){
  outx <- NULL
  outy <- NULL
  i <- 1
  for (t in seq(0, 1, length.out=n)){
    b <- bez(x, y, t)
    outx[i] <- b$x
    outy[i] <- b$y
    i <- i + 1
  }
  return (list(x=outx, y=outy))
}

bez <- function(x, y, t){
  outx <- 0
  outy <- 0
  n <- length(x)-1
  for (i in 0:n){
    outx <- outx + choose(n, i)*((1-t)^(n-i))*t^i*x[i+1]
    outy <- outy + choose(n, i)*((1-t)^(n-i))*t^i*y[i+1]
  }
  return (list(x=outx, y=outy))
}

smlayout <- function(x, y, l.type="random", f=0.5, v=NULL){
  ## smooth layout
  if (length(x) != length(y)){
    stop("x and y have not the same length")
  }
  x <- as.numeric(x);
  y <- as.numeric(y);
  if (l.type=="random"){
    i <- sort(sample(1:length(x), length(x)*f));
  } else if (l.type=="all"){
    i <- 1:length(x);
  } else {
    if (length(v) < 1){
      stop("v is missing, with no default if l.type is not \"random\"")
    }
    ## points were selected by user, v is index
    i <- v;
  }
  x0  <- x[i]
  y0  <- y[i];
  out <- bezierCurve(x0, y0, length(x));
  l   <- as.matrix(data.frame(out[[1]], out[[2]]));
  return(l);
}

smlayout2 <- function(level2, node, mst, coord, smlayout, l.type="all", f=1){
  l1    <- smlayout;
  l1.n  <- NULL;
  l2    <- NULL;
  l4.n  <- NULL;
  f     <- rep(f, length(level2));
  for(l.i in 1:length(level2)){
    l2.n <- unlist(level2[[l.i]]);
    for (j in 1:length(node)){
      n2 <- node[j];
      n3 <- c(n2, l2.n);
      mst.i  <- which(V(mst)$name %in% n3);
      mst2   <- igraph::induced_subgraph(mst, mst.i);
      mst2.o <- components(mst2);
      mst2.l <- length(unique(mst2.o$membership));
      if (mst2.l == 2){
        # the vecter, n1, was not linked with the branch
        next;
      }
      mst4 <- mst2;
      n4   <- n2;
    }
    out3   <- tjGCluster2(mst4, from=n4);
    l4.n[[l.i]] <- out3$level;
    l1.n   <- c(l1.n, out3$node);
    out3.i <- which(row.names(coord) %in% out3$node);
    m.s    <- coord[out3.i, c(1:2)];
    m.s.i  <- which(row.names(m.s) == n4)
    m.s.j  <- which(row.names(l1) == n4)
    m.s[m.s.i,] <- l1[m.s.j,];
    m.s         <- m.s[out3$node,]
    l2.s        <- smlayout(m.s[,1], m.s[,2], l.type=l.type, f=f[l.i]);
    l2[[l.i]]   <- l2.s
  }
  return(list(l=l2, node=l1.n, l.n=l4.n))
}
