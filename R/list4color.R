

list4color <- function(list.in, col, n.order=NULL){
  l.n   <- length(list.in);
  cols  <- col;
  col.n <- colorRampPalette(cols)(l.n);
  n2col <- NULL;
  for (i in 1:l.n){
    element   <- unlist(list.in[[i]]);
    element.s <- data.frame(n=element, cols=rep(col.n[i], length(element)))
    n2col     <- rbind(n2col, element.s);
  }
  n.i                  <- match(n.order, n2col[,1]);
  n2col                <- n2col[n.i,]
  return(n2col);
}