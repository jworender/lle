# CASE 3 PLOTS
library(nmaps)
library(stringr)
library(dplyr)

SETUP  <- T
MPLOT1 <- T
VERSNS <- T
MPLOT2 <- T
BPLOT  <- T

if (SETUP) {
  message("Detecting groups...")
  dstruct     <- organize(dset_train)
  dset_groups <- dstruct$groups
  message("  Done.")

  message("Fitting initial model...")
  model_1 <- dfit(data = dset_train, dtype = "SQ", groups = dset_groups,
               params = list(sdfilter = NULL))
  message("  Done.")
}

if (MPLOT1) {
  message("Plotting training set examples...")
  plot(model_1, title = "Case #3 Training Set (no extra versions calculated)", h=.5)
  message("  Done.")
}

if (VERSNS) {
  message("Calculating additional feature versions that *might* exist...")
  dset_train_SQ <- squarify(dset_train, groups = dset_groups, sdfilter = NULL)
  dset_train_SQO <- OR_encode(dset_train_SQ, stride = 600,
                              feats = unlist(dset_groups))
  # the encoded data set is inverted, reverting for this application
  dset_train_SQO$data$INDC <- !dset_train_SQO$data$INDC
  for (f in unlist(dset_train_SQO$groups)) 
    dset_train_SQO$data[,f] <- -dset_train_SQO$data[,f]
  dset_test_SQ <- squarify(dset_test, limits = dset_train_SQ$limits,
                           groups = dset_train_SQ$groups, sdfilter = NULL)
  dset_test_SQO <- OR_encode(dset_test_SQ, limits = dset_train_SQO$limits,
                             stride = 600, feats = unlist(dset_groups))
  # the encoded data set is inverted, reverting for this application
  dset_test_SQO$data$INDC <- !dset_test_SQO$data$INDC
  for (f in unlist(dset_train_SQO$groups)) 
    dset_test_SQO$data[,f] <- -dset_test_SQO$data[,f]
  message("Fitting new models...")
  model_1a <- dfit(data = dset_train_SQO$data, dtype = "LS")
  model_2 <- dfit(data = dset_train, dtype = "LS")
  message("  Done.")
}


if (MPLOT2) {
  message("Plotting examples...")
  plot(model_1a, title = "Case #3 Training Set (with extra versions)", h=.5)
  plot(model_1a, title = "Case #3 Test Set", data = dset_test_SQO$data, h=.5)
  plot(model_2, title = "Case #3 Training Set (no transformation)", h=.5)
  plot(model_2, title = "Case #3 Test Set (no transformation)",
       data = dset_test, h=.5)
  message("  Done.")
  
  
}

if (BPLOT) {
  TRANS   <- 0.2
  BARHT   <- 15
  beta1   <- model_1a$model$beta[,]
  markers <- rep(0,length(beta1))
  cols    <- rep(rgb(0,0,0,0), length(beta1))

  cols[grep("^V1TM5_", names(beta1))]     <- rgb(1.0, 0.0, 0.0,TRANS) #RED
  markers[grep("^V1TM5_", names(beta1))]  <- BARHT
  cols[grep("^V2TM10_", names(beta1))]     <- rgb(0.5, 0.0, 0.5,TRANS) #PURPLE
  markers[grep("^V2TM10_", names(beta1))]  <- BARHT
  cols[grep("^V4TM3_", names(beta1))]      <- rgb(0.0, 1.0, 0.0,TRANS) #GREEN
  markers[grep("^V4TM3_", names(beta1))]   <- BARHT
  cols[grep("^V5TM8_", names(beta1))]     <- rgb(1.0, 0.5, 0.0,TRANS) #ORANGE
  markers[grep("^V5TM8_", names(beta1))]  <- BARHT

  par(new=FALSE, mar = c(6,3,3,2))
  barplot(beta1, border = "blue", col = "blue", ylim = c(-0.5, BARHT), las = 2,
          xaxt = "n")

  par(new=TRUE)
  barplot(markers, border = cols, col = cols, ylim = c(-0.5, BARHT),
          axes = FALSE)

  
  
  BARHT   <- 2
  beta2   <- model_2$model$beta[,]
  markers <- rep(0,length(beta2))
  cols    <- rep(rgb(0,0,0,0), length(beta2))
  
  cols[grep("V1TM5", names(beta2))]     <- rgb(1.0, 0.0, 0.0,TRANS) #RED
  markers[grep("V1TM5", names(beta2))]  <- BARHT
  cols[grep("V2TM10", names(beta2))]     <- rgb(0.5, 0.0, 0.5,TRANS) #PURPLE
  markers[grep("V2TM10", names(beta2))]  <- BARHT
  cols[grep("V4TM3", names(beta2))]      <- rgb(0.0, 1.0, 0.0,TRANS) #GREEN
  markers[grep("V4TM3", names(beta2))]   <- BARHT
  cols[grep("V5TM8", names(beta2))]     <- rgb(1.0, 0.5, 0.0,TRANS) #ORANGE
  markers[grep("V5TM8", names(beta2))]  <- BARHT
  
  par(new=FALSE, mar = c(6,3,3,2))
  barplot(beta2, border = "blue", col = "blue", ylim = c(-BARHT, BARHT), las = 2,
          xaxt = "n")

  par(new=TRUE)
  barplot(markers, border = cols, col = cols, ylim = c(-BARHT, BARHT),
          axes = FALSE)
}

message("Pulling out the best critical ranges and fitting a new model...")
# taking the version with the strongest response for each feature
pats <- dstruct$patterns
# using the original transformed data set as a basis for the new one
dset_train_SQnew <- dset_train_SQ
# all features
feats <- unlist(dset_groups)
for (p in pats) {
  gps   <- names(dset_train_SQ$groups)
  gp    <- gps[grep(p,gps)]
  fvect <- feats[grep(p, feats)]
  for (f in fvect) {
    fpat  <- paste0("^", f, "_")
    fvers <- unlist(dset_train_SQO$groups)
    fvers <- fvers[grep(fpat, fvers)]
    dset_train_SQnew$data[,f] <-
      dset_train_SQO$data[,names(beta1[fvers])[which.max(abs(beta1[fvers]))]]
    dset_train_SQnew$limits[[gp]][[f]] <-
      dset_train_SQO$limits[[f]][[names(beta1[fvers])[which.max(abs(beta1[fvers]))]]]
  }
}

dset_test_SQnew <- squarify(dset_test, limits = dset_train_SQnew$limits,
                            groups = dset_groups, sdfilter = NULL,
                            checkdata = FALSE)
model_1b <- dfit(data = dset_train_SQnew$data, dtype = "LS")

BARHT   <- 15
beta1b  <- model_1b$model$beta[,]
markers <- rep(0,length(beta1b))
cols    <- rep(rgb(0,0,0,0), length(beta1b))

cols[grep("V1TM5", names(beta1b))]     <- rgb(1.0, 0.0, 0.0,TRANS) #RED
markers[grep("V1TM5", names(beta1b))]  <- BARHT
cols[grep("V2TM10", names(beta1b))]     <- rgb(0.5, 0.0, 0.5,TRANS) #PURPLE
markers[grep("V2TM10", names(beta1b))]  <- BARHT
cols[grep("V4TM3", names(beta1b))]      <- rgb(0.0, 1.0, 0.0,TRANS) #GREEN
markers[grep("V4TM3", names(beta1b))]   <- BARHT
cols[grep("V5TM8", names(beta1b))]     <- rgb(1.0, 0.5, 0.0,TRANS) #ORANGE
markers[grep("V5TM8", names(beta1b))]  <- BARHT

par(new=FALSE, mar = c(6,3,3,2))
barplot(beta1b, border = "blue", col = "blue", ylim = c(-BARHT, BARHT), las = 2,
        xaxt = "n")

par(new=TRUE)
barplot(markers, border = cols, col = cols, ylim = c(-BARHT, BARHT),
        axes = FALSE)






























