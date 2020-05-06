
tsneGCluster <- function(dat, perplexity=30, max_iter = 1000, ...){
  tsne       <- Rtsne::Rtsne(dat, perplexity=perplexity, max_iter = max_iter, ...);
  out        <- tsne$Y;
  row.names(out) <- row.names(dat);
  return(out);
}
