wc_plot <- function(dat=dat, wt=wt, vertex.size=2, vertex.label=NA, ...){
  dat.d <- dist(dat);
  g     <- igraph::graph_from_adjacency_matrix(as.matrix(dat.d), weighted =T, mode = "upper");
  g.i   <- which(E(g)$weight > wt);
  g.s   <- igraph::delete_edges(g, g.i);
  plot(g.s, vertex.size=vertex.size, vertex.label=vertex.label, layout=as.matrix(dat), ...);
  return(g.s)
}


draw_contour <- function( a, line.col=line.col,
                          alpha=0.95, 
                          plot.dens=FALSE, 
                          line.width=2, 
                          line.type=1, 
                          limits=NULL, 
                          density.res=300,
                          spline.smooth=-1){
  
  ##a is a list or matrix of x and y coordinates (e.g., a=list("x"=rnorm(100),"y"=rnorm(100)))
  ## if a is a list or dataframe, the components must be labeled "x" and "y"
  ## if a is a matrix, the first column is assumed to be x, the second y
  ##alpha is the contour level desired
  ##if plot.dens==TRUE, then the joint density of x and y are plotted,
  ##   otherwise the contour is added to the current plot.
  ##density.res controls the resolution of the density plot
  
  ##A key assumption of this function is that very little probability mass lies outside the limits of
  ## the x and y values in "a". This is likely reasonable if the number of observations in a is large.
  
  ##require(MASS)
  ##require(ks)
  
  if(length(line.width)!=length(alpha)){
    line.width <- rep(line.width[1],length(alpha))
  }
  
  if(length(line.type)!=length(alpha)){
    line.type <- rep(line.type[1],length(alpha))
  }
  
  if(is.matrix(a)){
    a=list("x"=a[,1],"y"=a[,2])
  }
  
  ##generate approximate density values
  if(is.null(limits)){
    limits=c(range(a$x),range(a$y))
  }
  
  ##Two-Dimensional Kernel Density Estimation
  f1 <- MASS::kde2d(a$x,a$y,n=density.res,lims=limits)
  
  ##plot empirical density
  if(plot.dens) image(f1)
  
  if(is.null(dev.list())){
    ##ensure that there is a window in which to draw the contour
    plot(a,type="n",xlab="X",ylab="Y")
  }
  
  ##estimate critical contour value
  ## assume that density outside of plot is very small
  out.l <- list();
  l.i   <- 0;
  zdens <- rev(sort(f1$z))
  Czdens <- cumsum(zdens)
  Czdens <- (Czdens/Czdens[length(zdens)])
  for(cont.level in 1:length(alpha)){
    ##This loop allows for multiple contour levels
    crit.val <- zdens[max(which(Czdens<=alpha[cont.level]))]
    
    ##determine coordinates of critical contour
    b.full <- contourLines(f1,levels=crit.val)
    for(c in 1:length(b.full)){
      ##This loop is used in case the density is multimodal or if the desired contour
      ##  extends outside the plotting region
      b <- list("x"=as.vector(unlist(b.full[[c]][2])),"y"=as.vector(unlist(b.full[[c]][3])))
      
      ##plot desired contour
      line.dat           <- xspline(b,shape=spline.smooth,open=TRUE,draw=FALSE, col=line.col)
      lines(line.dat,lty= line.type[cont.level],lwd=line.width[cont.level], col=line.col)
      l.i <- l.i + 1;
      out.l[[l.i]] <- line.dat;
    }
  }
  out.l
}

ctGCluster <- function(dat=dat, alpha=alpha, clu=clu, col1=col1, col2=col2, col3=col3, main=main){
  plot(dat, pch=1, col=col2[clu], main=main);
  clu.u <- unique(clu);
  clu.n <- length(clu.u);
  out.s <- c();
  for (i in 1:length(clu.u)){
    clu.i <- which(clu==clu.u[i]);
    xy    <- dat[clu.i,];
    out   <- draw_contour(xy, line.col=col1[i], alpha=alpha);
    for (l in 1:length(out)){
      x <- out[[l]]$x;
      y <- out[[l]]$y;
      out.m <- data.frame(x=x, y=y);
      out.m <- as.matrix(out.m)
      out.i <- mgcv::in.out(out.m, xy);
      if(sum(out.i) < 2){
        next;
      }
      points(xy[out.i,], col=col3[i], pch=19);
      tmp    <- xy[out.i,];
      tmp.df <- data.frame(clu=rep(i, nrow(tmp)), alpha=rep(alpha[l], nrow(tmp)), x=tmp[,1], y=tmp[,2]) 
      out.s  <- rbind(out.s, tmp.df);
    }
  }
  return(out.s);
}

mst_plot <- function(dat=dat, ...){
  dat.s  <- dat[,c(1:2)];
  dat.d  <- dist(dat.s);
  g      <- igraph::graph_from_adjacency_matrix(as.matrix(dat.d), weighted =T, mode = "upper");
  mst.g  <- igraph::minimum.spanning.tree(graph=g, weights=E(g)$weight);
  mst.e  <- igraph::get.edgelist(mst.g);
  out    <- NULL;
  for (j in 1:nrow(mst.e)){
    p1.i <- which(row.names(dat) == mst.e[j,1]);
    p2.i <- which(row.names(dat) == mst.e[j,2]);
    out  <- rbind(out, c(dat[p1.i,1], dat[p1.i,2], 
                         dat[p2.i,1], dat[p2.i,2]));
  }
  segments(as.numeric(out[,1]), as.numeric(out[,2]), 
           as.numeric(out[,3]), as.numeric(out[,4]), ...);
  return(list(mst.g=mst.g, mst.e=mst.e));
}
