
simGCluster2 <- function(ref.d=ref.d, ref.col=ref.col, file.n=file.n, clu.n=1:18, clu.col=clu.col){
  
  ref.name <- unique(ref.d[,ref.col]);
  file.num <- length(file.n);
  
  out.s <- NULL;
  ## get max propertion ref.d type matched the sumulation dat for each cluster
  ## for each simulation file
  cat("Mapping simulation clusters to reference cell types or clusters ...\n");
  pb <- txtProgressBar(min = 0, max = file.num, style = 3);
  i  <- 0;
  for (f.i in 1:file.num){
    i <- i + 1;
    ## read simulation data
    sim.d <- read.table(file.n[f.i], header=T);
    ## for each cluster of simulation data
    for (clu.i in clu.n){
      ## indices of the cluster in simulation data
      sim.i      <- which(sim.d[,clu.col]==clu.i);
      ## sub-data of the cluster
      sim.s      <- sim.d[sim.i,];
      
      ## indices of the reference data matched the sub-data cells
      ref.i  <- which(row.names(ref.d) %in% row.names(sim.s));
      ref.s  <- ref.d[ref.i,];
      ## get table of the cell types of the sub reference data matched the sub-data cells
      ref.t  <- table(ref.s[, ref.col]);
      ## get index of the cell type with the maximum number matched in the sub-data cells
      ref.j  <- which.max(ref.t);
      ## get the cell type name with the maximum matched number
      cell.n <- names(ref.t)[ref.j];
      ## indices of the sub-reference data with the cell type
      ref.k  <- which(ref.s[, ref.col]==cell.n);
      ref.s2 <- ref.s[ref.k,];
      ## 
      tmp.n  <- row.names(ref.s2);
      row.names(ref.s2) <- NULL;
      tmp.df <- data.frame(tmp.n, ref.s2);
      out.s  <- rbind(out.s, tmp.df);
    }
    setTxtProgressBar(pb, i);
  }
  close(pb);
  
  cat("Generating output table ...\n")
  name.u <- unique(out.s[,1]);
  out.s2 <- NULL;
  ## for each cell
  pb <- txtProgressBar(min = 0, max = length(name.u), style = 3);
  i  <- 0;
  for (u.i in 1:length(name.u)){
    i <- i + 1;
    ## index of the cell
    out.i <- which(out.s[,1] == name.u[u.i]);
    ## the sub out.s of the cell 
    out     <- out.s[out.i,];
    tmp.s   <- NULL;
    for (ref.n in ref.name){
      j <- which(out[,ref.col+1]==ref.n);
      if (length(j)>0){
        tmp.s <- c(tmp.s, length(j));
      } else {
        tmp.s <- c(tmp.s, length(j));
      }
    }
    out.s2 <- rbind(out.s2, c(name.u[u.i], tmp.s));
    setTxtProgressBar(pb, i);
  }
  close(pb);
  colnames(out.s2) <- c("cell", ref.name);
  return(out.s2);
}