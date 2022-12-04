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
#' @param nzones The maximum number of zones to assume.  One zone is equivalent
#'     to the original range, greater than one zone shrinks the critical range
#'     to the next higher density.  Choosing zero uses a simplified algorithm
#'     which assumes that the region of false positives is sharply defined.
#' @param zone The zone of nzones to select. Whether the choice is correct 
#'     should be evident by the behavior of the training set.
#' @param exclude The columns present in the data set to exclude from the
#'     process.
#' @return A new object which has collapsed boundaries.
#' @export
collapse_limits <- function(obj, devset=NULL, thresh = 0.5, nzones = 2,
                            zone = 1, exclude = "X", snap = 0.001) {
  
  if (is.null(devset)) {
    if (length(which(colnames(obj$data) %in% exclude)) > 0)
      data <- obj$data[,-which(colnames(obj$data) %in% exclude)]
    else
      data <- obj$data
  }
  else {
    if (length(which(colnames(devset) %in% exclude)) > 0)
      data <- devset[,-which(colnames(devset) %in% exclude)]
    else
      data <- devset
  }
  # ensuring input data is in data frame form
  data  <- data.frame(data)
  odata <- data
  
  data_pred <- modfunc(obj$model, data = data, rescale = c(0,1))
  resp <- as.logical(data[,obj$response])
  fps  <- (!resp & (data_pred$pred > thresh)) | (resp & (data_pred$pred < thresh))
  
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
  
  groups <- obj$model$groups
  if ((obj$model$diffs) | (obj$model$ratios) | (obj$model$addrev)){
    dstruct <- rectify(data, resp = obj$response, exclude = obj$model$exclude,
                       limits = obj$model$limits, groups = obj$model$groups,
                       dfilter = NULL, sdfilter = NULL, diffs = obj$model$diffs,
                       ratios = obj$model$ratios, addrev = obj$model$addrev,
                       cdata = obj$model$cdata, verbose = FALSE,
                       ilcats = obj$model$lcats, reti = TRUE)
    data <- dstruct$data
    groups <- dstruct$groups
  }
  
  # ensuring there is an index column
  data$X     <- 1:dim(data)[1]
  # extract all features specified in the groups
  feats      <- unlist(unname(groups))
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
  ngroups <- groups
  nlimits <- obj$model$limits
  
  allcdata <- unname(unlist(obj$model$ngroups[paste0(obj$model$cdata,".cats")],
                            recursive = FALSE))
  if (nzones == 0) {
    # for nzones == 0, assuming that the false positive region is one contiguous
    # region that extends from somewhere in the middle of the critical range to
    # one of its edges - this is a simple case that might effectively clip the
    # critical ranges and it will be obvious by looking at the training set
    # whether this strategy worked
    limits <- obj$model$limits
    for (i in 1:length(lims)) {
      l  <- lims[[i]]
      nm <- names(lims)[i]
      if (!(nm %in% allcdata)) {
        in_min <- min(data[fps,nm])
        in_max <- max(data[fps,nm])
        nt     <- ceiling(snap*dim(data)[1])
        nmax   <- sum((data[,nm] > in_max) & (data[,nm] < l[2]))
        nmin   <- sum((data[,nm] < in_min) & (data[,nm] > l[1]))
        
        # if both the min and max are unbounded for this feature, there is
        # nothing left to do
        if      (is.na(l[1]) & is.na(l[2])) rg <- c(NA, NA)
        # if one or the other are unbounded, then the other member of the pair
        # must be the boundary of the false positive region
        else if (is.na(l[1]))               rg <- c(NA, in_min)
        else if (is.na(l[2]))               rg <- c(in_max, NA)
        # if there are very few examples between the critical range limits
        # and the false positive range limits, they can be assumed to represent
        # the same number
        else if (nmin <= nt)                rg <- c(l[1], in_max)
        else if (nmax <= nt)                rg <- c(in_min, l[2])
        # the remaining possibility is that the true positive region must lie
        # somewhere in-between the bounds of the critical range, so finding the
        # region containing the most true positives and least false positives
        else {
          rmin <- l[1]
          rmax <- l[2]
          # setting up the number of divisuions such that there are an average
          # of 10 examples per bin
          NDIV <- ceiling(sum((data[,nm] >= l[1]) & (data[,nm] <= l[2]))/10)
          # demarcate the divisions
          divs <- seq(rmin, rmax, (rmax - rmin)/NDIV)
          # sum the false positives in each range
          sfps <- unlist(lapply(1:(length(divs)-1), FUN = function(j) {
            sum(!resp[(data[,nm] >= divs[j]) & (data[,nm] < divs[j+1])])
          }))
          # normalize
          sfps <- sfps/sum(!resp)
          # sum the true positives in each range
          tfps <- unlist(lapply(1:(length(divs)-1), FUN = function(j) {
            sum(resp[(data[,nm] >= divs[j]) & (data[,nm] < divs[j+1])])
          }))
          # normalize
          tfps  <- tfps/sum(resp)
          # take the difference between the accumulated false positives and
          # true positives
          diffs <- unlist(lapply(1:NDIV, FUN = function(i)
            sum((tfps-sfps)[1:i])))
          # find the extreme points on that curve and use those as the new
          # critical range min and max
          f1a  <- min(data[which(diffs == min(diffs)),nm])
          f1b  <- max(data[which(diffs == min(diffs)),nm])
          f2a  <- min(data[which(diffs == max(diffs)),nm])
          f2b  <- max(data[which(diffs == max(diffs)),nm])
          # if there are multiple points with the same value, choose the points
          # such that the resultant range is the smallest
          if (f1a < f2a) rg <- c(f1b,f2a)
          else           rg <- c(f2b,f1a)
          # if there are very few points between the range extremes and the edge
          # of the data, make the range unbounded in that direction
          if (sum(data[,nm] < rg[1]) <= nt)  rg <- c(NA, in_min)
          if (sum(data[,nm] > rg[2]) <= nt)  rg <- c(in_max, NA)
        }

        lnew <- rg
        # checking to make sure that the new range actually contains positive
        # results
        if ((is.na(lnew[1])) & (is.na(lnew[2])))
          ncontained <- dim(data)[1]
        else if (is.na(lnew[1]))
          ncontained <- sum(data[,nm] < lnew[2])
        else if (is.na(lnew[2]))
          ncontained <- sum(data[,nm] > lnew[1])
        else
          ncontained <- sum((data[,nm] > lnew[1]) &
                            (data[,nm] < lnew[2]))
        # if it doesn't, just revert back to the old limits
        if (ncontained == 0) lnew <- l
        
        limits[[dict[nm]]][[nm]] <- lnew    
      }
    }
    adata  <- data[,(colnames(data) %in% names(dict))]
    colnames(adata) <- 
      paste0(colnames(adata)[!(colnames(adata) %in% obj$model$cdata)],".2")
    # fit the new model with these limits creating additional features
    ndata <- cbind(data,adata)
    ngroups <- groups
    names(ngroups) <- paste0(names(groups),".2")
    alimits <- limits
    names(alimits)  <- paste0(names(obj$model$limits),".2")
    for (i in 1:length(ngroups)) {
      ngroups[[i]] <- paste0(ngroups[[i]][!(ngroups[[i]] %in% obj$model$cdata)], ".2")
      names(alimits[[i]]) <- paste0(names(alimits[[i]]), ".2")
    }
    ngroups <- c(groups, ngroups)
    nlimits <- c(obj$model$limits, alimits)
  }
  else {
    # translating the logical vector into a set of indices
    fpis <- which(fps)
    for (n in 1:nzones) {
      # for many cases, there might be multiple zones where the critical range
      # might be active
      limits <- obj$model$limits
      for (i in 1:length(lims)) {
        # the name of the feature
        nm      <- names(lims)[i]
        # this won't work for categorical data, so preventing running on those
        # features
        if (!(nm %in% allcdata)) {
          # # sorting the gaps in-between false examples by how many true positive
          # # examples there are in-between
          # #
          # # tabulating the false examples and sorting by feature value
          # vals <- sort(data[!resp,nm] %n% which(!resp))
          # # creating a small delta so that all examples will be inside bookends,
          # # to account for the cases in which the true positives go all the way
          # # to the edge and are not bounded by false examples
          # small_delta <- 1E-6*mean(data[,nm])
          # vals <- c((min(data[,nm]) - small_delta) %n% "0",
          #           vals,
          #           (max(data[,nm]) + small_delta) %n% (dim(data)[1]+1))
          # # sorting the gaps by how many true positives there are in-between
          # # each false example
          # gaps <- unlist(lapply(2:length(vals), FUN = function(i) {
          #   fmin <- vals[i-1]
          #   fmax <- vals[i]
          #   sum(resp[(data[,nm] > fmin) & (data[,nm] < fmax)])
          # }))
          # gnames <- names(gaps[2:length(gaps)])
          # gaps   <- gaps[1:(length(gaps)-1)] - gaps[2:length(gaps)]
          # names(gaps) <- gnames
          # gaps   <- gaps[-length(gaps)]
          # gaps   <- sort(gaps, decreasing = TRUE)
          idx_zn1 <- names(sort(diff_df[,nm] %n% diff_df$X, decreasing = TRUE))[1]
          idx_bot <- names(sort(diff_df[,nm] %n% diff_df$X, decreasing = TRUE))[n]
          idx_top <- top_df[top_df$X == idx_bot,nm]
          lfps <- c(fps_df[fps_df$X == idx_zn1,nm], fps_df[fps_df$X == idx_top,nm])
          # now figure out the value of the first positive example within the zone
          tps <- sort(tps_df[,nm] %n% tps_df$X)
          if ((sum(tps > lfps[1]) > 0) & (sum(tps < lfps[2]) > 0))
            ltps <- c(sort(tps[tps > lfps[1]])[1],
                      sort(tps[tps < lfps[2]], decreasing = TRUE)[1])
          else if (sum(tps > lfps[1]) > 0)
            ltps <- c(sort(tps[tps > lfps[1]])[1], lfps[2])
          else if (sum(tps < lfps[2]) > 0)
            ltps <- c(lfps[1], sort(tps[tps < lfps[2]], decreasing = TRUE)[1])
          # take the middle ground so that the false positive will not be included
          # in the zone, but the true positive definitely will be
          lnew <- unname((lfps + ltps) / 2)
          ntop <- sum(data[,nm] > lnew[2])
          nbot <- sum(data[,nm] < lnew[1])
          nmin <- dim(data)[1]*snap
          if (ntop <= nmin) lnew[2] <- NA
          if (nbot <= nmin) lnew[1] <- NA
          limits[[dict[nm]]][[nm]] <- lnew    
        }
      }
      adata  <- data[,c("X", feats)]
      colnames(adata) <- c("X", paste0(feats, ".", n+1))
      # rename the groups and limits appropriately to be consistent with the
      # new column names
      newgr  <- groups
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
  }
  # fit the new model with these limits creating additional features
  nmodel <- modelfit(data = ndata, response = obj$response, fit_type = "SQ",
                     groups = ngroups, params = list(sdfilter = NULL,
                                                     limits = nlimits))
  
  # now make a new model with just the most dominant limits with the original
  # features
  
  # create a list of the features with the least significant features first so
  # that as critical ranges are being replaced the most significant features
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
    data <- rbind(data.frame(obj$data[,c(feats,obj$resp)]),
                  data.frame(devset[,c(feats,obj$resp)]))
  else
    data <- data.frame(obj$data)
  
  if (length(which(colnames(data) %in% exclude)) > 0)
    data   <- data[,-which(colnames(data) %in% exclude)]
  data$X <- 1:dim(data)[1]
  nmodel <- modelfit(data = odata, resp = obj$response, fit_type = "SQ",
                     groups = obj$model$groups,
                     params=list(limits=new_limits))
  
  return(nmodel)
}



