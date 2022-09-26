#' Collapse limit boundaries that result in false positives.
#' 
#' This function uses the false positive information from the final model run to
#' identify portions of the rang.  This is useful when there are multiple
#' features that can independently cause the same outcome.
#' 
#' @name collapse_limits
#' @param obj The object that contains the model that has limits that need to be
#'     contracted.  That is, a model in which different features can
#'     independently result in the same outcome.
#' @param devset Use a development data set (data that was set aside at the very
#'     beginning of the process for the purpose of refining the model at the
#'     end) instead of the training set to refine the model.
#' @param thresh The threshold at which to evaluate the outcome at.
#' @param nzones The maximum number of zones to assume.  Each zone is an
#'     additional multiplier for the number of columns.  For one zone, the
#'     number of collumns will double, for two zones, it will triple, and so on.
#'     Four should probably be the minimum since two of the zones may simply be
#'     due to extreme outliers on the top or the bottom.
#' @param exclude The columns present in the data set to exclude from the
#'     process.
#' @return A new object which has collapsed boundaries.
#' @export
collapse_limits <- function(obj, devset=NULL, thresh = 0.5, nzones = 4,
                            exclude = EXCLUDE) {
  if (is.null(devset))  data <- obj$data[,-which(colnames(obj$data) %in%
                                                   exclude)]
  else                  data <- devset[,-which(colnames(devset) %in%
                                                 exclude)]
  
  data_pred <- modfunc(obj$model, data = data, rescale = c(0,1))
  resp <- as.logical(data[,obj$resp])
  fps  <- !resp & (data_pred$pred > thresh)
  
  if (sum(fps) == 0) {
    message("Nothing to do.")
    return(obj)
  }
  else if (sum(fps) <= nzones)
    nzones <- sum(fps) - 1
  
  lims <- unlist(unname(obj$model$limits), recursive = FALSE)
  dict <- unlist(lapply(1:length(obj$model$limits), FUN = function(i) {
    x <- names(obj$model$limits)
    n <- names(obj$model$limits[[i]])
    r <- rep(x[i],length(n))
    names(r) <- n
    return(r)
  }))
  
  # ensuring there is an index column
  data$X     <- 1:dim(data)[1]
  # extract all features specified in the groups
  feats      <- unlist(unname(obj$model$groups))
  # calculating the mins and max's of the true positives and false positives
  pos_mins   <- unlist(lapply(feats, FUN = function(f) {
    min(data[(resp | fps),f])
  }))
  names(pos_mins) <- feats
  pos_maxs   <- unlist(lapply(feats, FUN = function(f) {
    max(data[(resp | fps),f])
  }))
  names(pos_maxs) <- feats
  # setting up the difference data frame
  fps_df    <- rbind(data.frame(matrix(c(0,pos_mins), dimnames = list("0",
                     c("X",feats)), nrow = 1)), data[fps,c("X",feats)],
                     data.frame(matrix(c(max(data$X)+1,pos_maxs),
                     dimnames = list(as.character(max(data$X)+1),
                                     c("X",feats)), nrow = 1)))
  diff_df   <- fps_df
  # setting up the data frame to store the top index since this information is
  # lost in the index assignment
  top_df    <- fps_df
  # The largest gaps in false positives should identify the indices that define
  # the ranges of the true positive zones.  When each feature is bounded by
  # these zones by assuming that they represent the new critical range, the ones
  # that are truly correct will be identified by the LASSO
  for (f in feats) {
    onums <- diff_df[,f]
    names(onums) <- diff_df$X
    onums <- sort(onums)
    rowns <- as.numeric(names(onums)[1:(length(onums)-1)])
    diffs <- onums[2:length(onums)] - onums[1:(length(onums)-1)]
    diff_df[(diff_df$X %in% rowns),f] <- diffs[rowns %in% diff_df$X]
    top_df[(top_df$X %in% rowns),f]   <- names(onums)[2:length(onums)]
  }
  # removing the last row since this is the maximum and will not have produced
  # a difference beyond the maximum
  diff_df <- diff_df[-dim(diff_df),]
  top_df  <- top_df[-dim(top_df),]
  
  # creating a data frame with just the positive example feature values
  tps_df  <- data[resp,]
  # initializing the zone data
  ndata   <- data
  ngroups <- obj$model$groups
  nlimits <- obj$model$limits
  for (n in 1:nzones) {
    # calculating new limits for each zone
    limits <- obj$model$limits
    for (i in 1:length(lims)) {
      # the name of the feature
      nm      <- names(lims)[i]
      # the bottom index of the zone
      idx_bot <- names(sort(diff_df[,nm] %n% diff_df$X, decreasing = TRUE))[n]
      idx_top <- top_df[top_df$X == idx_bot,nm]
      lfps <- c(fps_df[fps_df$X == idx_bot,nm], fps_df[fps_df$X == idx_top,nm])
      # now figure out the value of the first positive example within the zone
      tps <- sort(tps_df[,nm] %n% tps_df$X)
      ltps <- c(tps[tps > lfps[1]][1],
                tps[tps < lfps[2]][sum(tps < lfps[2])])
      # take the middle ground so that the false positive will not be included
      # in the zone, but the true positive definitely will be
      lnew <- unname((lfps + ltps) / 2)
      limits[[dict[nm]]][[nm]] <- lnew    
    }
    adata  <- data[,c("X", feats)]
    colnames(adata) <- c("X", paste0(feats, ".", n+1))
    # rename the groups and limits appropriately to be consistent with the
    # new column names
    newgr  <- obj$model$groups
    for (i in 1:length(newgr)) {
      newgr[[i]] <- paste0(newgr[[i]],".",n+1)
      names(limits[[i]]) <- paste0(names(limits[[i]]),".",n+1)
    }
    names(newgr)  <- paste0(names(newgr),".",n+1)
    names(limits) <- paste0(names(limits),".",n+1)
    
    ngroups <- c(ngroups, newgr)
    nlimits <- c(nlimits, limits)
    ndata <- left_join(ndata,adata[,c("X",paste0(feats, ".", n+1))], by="X")
  }

  # fit the new model with these limits creating additional features
  nmodel <- modelfit(data = ndata, resp = obj$resp, dtype = "SQ",
                     groups = ngroups, params = list(sdfilter = NULL,
                     limits = nlimits))
  
  # now make a new model with just the most dominant limits with the original
  # features
  
  # create a list of the features with the least significant features first so
  # that as critical ranges are being replaced the most signfificant features
  # are replaced last
  betas <- nmodel$model$beta[,]
  betas <- betas[betas != 0]
  betas <- betas[names(sort(abs(betas)))]
  
  new_limits <- obj$model$limits
  for (i in 1:length(betas)) {
    if (grepl("[.][0-9]+",substr(names(betas)[i],nchar(names(betas)[i])-1,nchar(names(betas)[i])))) {
      nm <- substr(names(betas)[i],1,nchar(names(betas)[i])-2)
      sf <- substr(names(betas)[i],nchar(names(betas)[i])-1, nchar(names(betas)[i]))
      new_limits[[dict[nm]]][nm] <-
        nlimits[[paste0(dict[nm],sf)]][paste0(nm,sf)]
    }
    else {
      nm <- names(betas)[i]
      new_limits[[dict[nm]]][nm] <-
        obj$model$limits[[dict[nm]]][nm]
    }
  }
  
  if (!is.null(devset))
    data <- rbind(obj$data[,c("X",feats,obj$resp)], devset[,c("X",feats,obj$resp)])
  else
    data <- obj$data
  data   <- data[,-which(colnames(data) %in% exclude)]
  data$X <- 1:dim(data)[1]
  nmodel <- modelfit(data = data, resp = obj$resp, dtype = "SQ", groups = obj$model$groups,
                     params=list(limits=new_limits))
  
  return(nmodel)
}




