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
#' @param thresh The threshold at which to evaluate the outcome at.
#' @return A new object which has collapsed boundaries.
#' @export
collapse_limits <- function(obj, thresh = 0.5) {
  data_pred <- modfunc(obj$model, data = obj$data, rescale = c(0,1))
  
  resp <- as.logical(obj$data[,obj$resp])
  fps  <- !resp & (data_pred$pred > thresh)
  lims <- unlist(unname(obj$model$limits), recursive = FALSE)
  dict <- unlist(lapply(1:length(obj$model$limits), FUN = function(i) {
    x <- names(obj$model$limits)
    n <- names(obj$model$limits[[i]])
    r <- rep(x[i],length(n))
    names(r) <- n
    return(r)
  }))
  
  # calculating new limits
  limits <- obj$model$limits
  for (i in 1:length(lims)) {
    l  <- lims[[i]]
    nm <- names(lims)[i]
    
    in_min <- min(obj$data[fps,nm])
    in_max <- max(obj$data[fps,nm])
    ncontained <- sum((obj$data[fps,nm] > l[1]) & (obj$data[fps,nm] < l[2]))
    
    # region #1 is the portion of the critical range that is below the false
    # range
    r1 <- c(l[1],0)
    if (l[1] < in_min) r1[2] <- in_min
    else if ((l[1] >= in_min) & (l[1] <= in_max)) r1[2] <- in_max
    else if (l[1] > in_max) r1[2] <- l[2]
    if (r1[2] > l[2]) r1[2] <- l[2]
    
    # region #2 is the portion of the critical range that is above the false
    # range
    r2 <- c(0,l[2])
    if (l[2] > in_max) r2[1] <- in_max
    else if ((l[2] >= in_min) & (l[2] <= in_max)) r2[1] <- in_min
    else if (l[2] < in_min) r2[1] <- l[1]
    if (r2[1] < in_min) r2[1] <- l[1]
    
    # figure out which region is bigger and choose that region for the new
    # limits - one of the regions is sometimes a sliver that does not actually
    # contain any points
    if ((r1[2] - r1[1]) > (r2[2] - r2[1]))
      lnew <- r1
    else
      lnew <- r2
    
    limits[[dict[nm]]][[nm]] <- lnew    
  }
  
  alt_data <- rectify(obj$data, limits = limits, groups = obj$model$groups,
                      resp = obj$resp)
  
  adata  <- alt_data$data[,(colnames(alt_data$data) %in% names(dict))]
  colnames(adata) <- paste0(colnames(adata),".2")
  odata <- rectify(obj$data, groups = obj$model$groups, resp = obj$resp)
  
  # fit the new model with these limits creating additional features
  ndata <- cbind(odata$data,adata)
  ngroups <- obj$model$groups
  names(ngroups) <- paste0(names(obj$model$groups),".2")
  for (i in 1:length(ngroups))
    ngroups[[i]] <- paste0(ngroups[[i]], ".2")
  ngroups <- c(obj$model$groups, ngroups)
  
  nmodel <- modelfit(data = ndata, resp = obj$resp, dtype = "LS", groups = ngroups)
  
  # now make a new model with just the most dominant limits with the original
  # features
  
  # use the collapsed limits as a starting point
  new_limits <- limits
  
  # create a new dictionary so that the proper group is identified for each
  # feature
  dict2 <- c(dict, dict %n% paste0(names(dict),".2"))
  
  # create a list of the features with the least significant features first
  betas <- nmodel$model$beta[,]
  betas <- betas[betas != 0]
  betas <- betas[names(sort(abs(betas)))]
  
  for (i in 1:length(betas)) {
    if (substr(names(betas)[i],nchar(names(betas)[i])-1,nchar(names(betas)[i])) != ".2") {
      new_limits[[dict2[names(betas)[i]]]][names(betas)[i]] <-
        obj$model$limits[[dict2[names(betas)[i]]]][names(betas)[i]]
    }
    else {
      nm <- substr(names(betas)[i],1,nchar(names(betas)[i])-2)
      new_limits[[dict2[nm]]][nm] <-
        limits[[dict2[nm]]][nm]
    }
  }
  
  nmodel <- modelfit(data = obj$data, resp = obj$resp, dtype = "SQ", groups = obj$model$groups,
                     params=list(limits=new_limits))
  
  return(nmodel)
}




