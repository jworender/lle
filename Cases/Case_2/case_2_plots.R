# CASE 2 PLOTS
library(nmaps)
library(stringr)
library(dplyr)

SETUP <- T
MPLOT <- T
BPLOT <- T

if (SETUP) {
  message("Detecting groups...")
  dstruct     <- organize(dset_train)
  dset_groups <- dstruct$groups
  message("  Done.")

  message("Fitting Models...")
  model_1 <- dfit(data = dset_train, dtype = "SQ", groups = dset_groups,
               params = list(sdfilter = NULL))
  model_2 <- dfit(data = dset_train, dtype = "LS", groups = dset_groups)
  message("  Done.")
}

if (MPLOT) {
  message("Plotting examples...")
  plot(model_1, title = "Case #2 Training Set", h=.5)
  plot(model_1, title = "Case #2 Test Set", data = dset_test, h=.5)
  plot(model_2, title = "Case #2 Training Set (no transformation)", h=.5)
  plot(model_2, title = "Case #2 Test Set (no transformation)",
       data = dset_test, h=.5)
  message("  Done.")
  
  
}

if (BPLOT) {
  TRANS   <- 0.2
  BARHT   <- 7.5
  beta1   <- model_1$model$beta[,]
  markers <- rep(0,length(beta1))
  cols    <- rep(rgb(0,0,0,0), length(beta1))

  # cols[grep("V5TM10", names(beta1))]     <- rgb(1.0, 0.0, 0.0,TRANS) #RED
  # markers[grep("V5TM10", names(beta1))]  <- BARHT
  cols[grep("V8TM10", names(beta1))]     <- rgb(0.5, 0.0, 0.5,TRANS) #PURPLE
  markers[grep("V8TM10", names(beta1))]  <- BARHT
  cols[grep("V9TM1", names(beta1))]      <- rgb(0.0, 1.0, 0.0,TRANS) #GREEN
  markers[grep("V9TM1", names(beta1))]   <- BARHT
  # cols[grep("V10TM0", names(beta1))]     <- rgb(1.0, 0.5, 0.0,TRANS) #ORANGE
  # markers[grep("V10TM0", names(beta1))]  <- BARHT
  cols[grep("V13TM2", names(beta1))]     <- rgb(1.0, 0.0, 1.0,TRANS) #MAGENTA
  markers[grep("V13TM2", names(beta1))]  <- BARHT
  cols[grep("V25TM3", names(beta1))]     <- rgb(0.1, 0.5, 0.5,TRANS) #TEAL
  markers[grep("V25TM3", names(beta1))]  <- BARHT
  cols[grep("V30TM7", names(beta1))]     <- rgb(0.1, 0.1, 0.0,TRANS) #DARK BROWN
  markers[grep("V30TM7", names(beta1))]  <- BARHT

  par(new=FALSE, mar = c(6,3,3,2))
  barplot(beta1, border = "blue", col = "blue", ylim = c(-0.5, BARHT), las = 2,
          xaxt = "n")

  par(new=TRUE)
  barplot(markers, border = cols, col = cols, ylim = c(-0.5, BARHT),
          axes = FALSE)

  
  
  BARHT   <- 30
  beta2   <- model_2$model$beta[,]
  markers <- rep(0,length(beta2))
  cols    <- rep(rgb(0,0,0,0), length(beta2))
  
  # cols[grep("V5TM10", names(beta2))]     <- rgb(1.0, 0.0, 0.0,TRANS) #RED
  # markers[grep("V5TM10", names(beta2))]  <- BARHT
  cols[grep("V8TM10", names(beta2))]     <- rgb(0.5, 0.0, 0.5,TRANS) #PURPLE
  markers[grep("V8TM10", names(beta2))]  <- BARHT
  cols[grep("V9TM1", names(beta2))]      <- rgb(0.0, 1.0, 0.0,TRANS) #GREEN
  markers[grep("V9TM1", names(beta2))]   <- BARHT
  # cols[grep("V10TM0", names(beta2))]     <- rgb(1.0, 0.5, 0.0,TRANS) #ORANGE
  # markers[grep("V10TM0", names(beta2))]  <- BARHT
  cols[grep("V13TM2", names(beta2))]     <- rgb(1.0, 0.0, 1.0,TRANS) #MAGENTA
  markers[grep("V13TM2", names(beta2))]  <- BARHT
  cols[grep("V25TM3", names(beta2))]     <- rgb(0.1, 0.5, 0.5,TRANS) #TEAL
  markers[grep("V25TM3", names(beta2))]  <- BARHT
  cols[grep("V30TM7", names(beta2))]     <- rgb(0.1, 0.1, 0.0,TRANS) #DARK BROWN
  markers[grep("V30TM7", names(beta2))]  <- BARHT
  
  par(new=FALSE, mar = c(6,3,3,2))
  barplot(beta2, border = "blue", col = "blue", ylim = c(-30, BARHT), las = 2,
          xaxt = "n")

  par(new=TRUE)
  barplot(markers, border = cols, col = cols, ylim = c(-30, BARHT),
          axes = FALSE)
}
