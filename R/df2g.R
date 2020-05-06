
df2g <- function(df, w){
  g                   <- igraph::graph_from_data_frame(df[,c(1,2)], directed = FALSE);
  igraph::E(g)$weight <- as.numeric(w);
  return(g);
}