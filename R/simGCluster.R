
## the sample as lst.siml
lst.siml2 <- function(lst){
  z      <- expand.grid(x=1:length(lst),y=1:length(lst));
  lst2siml  <- function(x,y) {
    length(intersect(lst[[x]],lst[[y]]))/length(union(lst[[x]],lst[[y]]))
  }
  result <- mapply(lst2siml, z$x,z$y);
  dim(result) <- c(length(lst), length(lst));
  result;
}

lst.siml <- function(lst){
  sapply(lst, function(x) 
    sapply(lst, function(y,x)length(intersect(x,y))/length(union(x,y)),x))
}

lst2.siml <- function(lst1, lst2){
  sapply(lst1, function(x) 
    sapply(lst2, function(y,x) length(intersect(x,y))/length(union(x,y)),x))
}

clu2list <- function(x, n="clu"){
  if (class(x)=="integer"){
    x <- data.frame(name=names(x), cluster=x);
  }
  clu    <- unique(x[,2]);
  out.s  <- list();
  for (i in clu){
    out.s[[i]] <- as.character(x[x[,2]==i,1]);
  }
  names(out.s) <- paste0(n, clu);
  out.s
}

simGCluster <- function(dat=dat, clu=clu, files=files){

  dat.s   <- data.frame(name=row.names(dat), clu=clu);
  ref.c   <- clu2list(dat.s, n="clu");
  myn     <- length(unique(dat.s[,2])); ## number of cluster
  all.n   <- nrow(dat.s);
  all.t   <- round(table(dat.s[,2])/all.n*100, 2);
  sim.n   <- length(files);
  
  out.n     <- list();
  out.s     <- list();
  out.m     <- c();
  for (f in 1:sim.n){
    infile <- files[f];
    ## column 1 and 2 are cell name and cluster ID respectively.
    dat    <- read.table(infile, header=T);
    ## list genes in each cluster
    out.c  <- clu2list(dat, n="clu");
    ## similarity matrix of clusters between ref.c and out.c 
    out    <- lst2.siml(ref.c, out.c);
    ## index of the most similarity
    out.i  <- apply(out, 2, function(x) which(max(x)==x));
    ## cluster name of the most similarity
    out.m  <- rbind(out.m, row.names(out)[out.i]);
    ## list genes for each cluster for all runs
    out.s[[f]] <- out.c;
    ## list similarities for all genes for all clusters for all runs
    out.n[[f]] <- out;
  }
  
  sim.m <- sim.n - 1;
  names(out.n)     <- c("ref.c", paste0("run", 1:sim.m));
  names(out.s)     <- c("ref.c", paste0("run", 1:sim.m));
  row.names(out.m) <- c("ref.c", paste0("run", 1:sim.m));
  
  out.f <- list();
  ## for each cluster
  for (l in 1:myn){
    cell <- c();
    ## for each run
    for (r.i in 1:sim.n){
      tmp  <- as.character(out.m[r.i, l]);
      cell <- c(cell, out.s[[r.i]][[tmp]]);
    }
    out.f[[l]] <- table(cell)/sim.n;
  }
  names(out.f) <- paste0("clu", 1:myn);
  return(list(sum=out.f, out=out.n));
}