#
#
#

i2d <- function(image=image, p.n=500, scale=T){
  if (class(image)[1]=="Image"){
    img1 <- image;
  } else {
    ## read image
    img0  <- EBImage::readImage(image);
    ## convert to mirror image
    img0  <- EBImage::flip(img0);
    ## convert to gray image 
    img1  <- EBImage::channel(img0, "gray");
  }

  ## convert to negtive image
  img2   <- 1 - img1;
  wh     <- 100;
  wh.n   <- min(dim(img0)[c(1:2)])/2;
  if (wh > wh.n){
    wh <- wh.n;
  }
  my.off <- 0.1;
  img3  <- EBImage::thresh(img2, wh, wh, my.off);
  dat.m <- as.matrix(img3@.Data);
  colnames(dat.m)  <- c(1:ncol(dat.m));
  row.names(dat.m) <- c(1:nrow(dat.m));
  dat   <- as.data.frame(as.table(dat.m));
  dat.i <- which(dat[,3]==0);
  dat   <- dat[-dat.i,];
  if (p.n > nrow(dat)){
    dat.f <- dat[sample(1:nrow(dat), p.n, replace=TRUE),];
  } else {
    dat.f <- dat[sample(1:nrow(dat), p.n),];
  }
  x   <- as.numeric(dat.f[,1]);
  y   <- as.numeric(dat.f[,2]);
  if (scale){
    x   <- scale(x);
    y   <- scale(y);
  }
  out <- data.frame(x=x, y=y);
  return(out);
}
