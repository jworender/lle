#' Transform a continuous wave into a square rectified wave given a response
#' column.
#'
#' This function takes as input a data set with a set of continuous
#' longitudinal features and transforms them into square rectified waves that
#' are TRUE if the feature feature shifted by time ventures into a range in
#' which there exists a positive response, and FALSE otherwise.

#'
#' @name rectify
#' @param data The data set to convert to columns of logical values.
#' @param resp The name of the response column
#' @param exclude The columns to exclude from the procedure.
#' @param limits The limits associated with each group.  If limits and
#'     groups are provided, these will be used instead of calculating
#'     based off of the supplied data set.  This would be done if using
#'     a prediction data set, for example.
#' @param groups The groups associated with the limits supplied. If this is
#'     already known and passed as a parameter, it will speed up the
#'     rectification process and increase the chances of success.  If this
#'     parameter is set to NULL, the function will attempt to find out what
#'     the groups are by collating them according to their naming
#'     convention.
#' @param dfilter A density filter to screen outliers from noisy data and
#'     attempt to establish more accurate ranges for variables with complex
#'     interactions.  The value controls the size of the window used for the
#'     density calculation, with 1.0 being the "normal" size.  If NULL, no
#'     density filter is used and the ranges are set by the absolute max and
#'     min for the positive examples.
#' @param sdfilter A filter to screen outliers from noisy data.  The value
#'     is the number of standard deviations outside of which the data will
#'     be disregarded for the purposes of calculating the limits that
#'     generate the rectified wave. If the data is known to be noisy this will
#'     improve performance, but if the data is very precise, this may make
#'     performance worse (since no filter applied is essentially an implicit
#'     assumption that there is no error in the readings).  If set to NULL,
#'     no filter will be used. The default is three, which guarantees that at
#'     least 99% of normally distributed data will be used.
#' @param diffs Add in the differences for each unique pair of features to the
#'     data set.  This will allow modeling based on relative differences between
#'     the features.
#' @param ratios Add in the ratios for each unique pair of features to the data
#'     set.  This will also allow modeling based on relative differences, but
#'     may capture nonlinear behavior.
#' @param addrev Add a set of reversed data values, so when a -1 appears in the
#'     regular data set a 1 will appear and vice-versa.  If there is just an
#'     'AND' relationship between the features, this will be superfluous since
#'     the reverse is just accounted for by a negative coefficient. However,
#'     if an 'OR' relationship is present, there may well be a situation where
#'     both the original feature and its reverse are needed to get a good fit.
#' @param snap Snap to the unlimited min/max if the number of points outside the
#'     range is less than this fraction (0.1% by default) of the total.  Set to
#'     NULL to disable. This assumes that the small amount of data outside the
#'     limits is not enough to disqualify a greater limit.
#' @param cdata If fitting categorical data (i.e. integers without a continuous
#'     relationship), set this character vector to the feature names of the
#'     categorical columns.
#' @param verbose A logical which determines whether informational messages
#'     are displayed to the console.
#' @param reti If TRUE, return the intermediate data frame that exists after an
#'     operation is performed on it (like 'diffs'), but before it is rectified.
#'     This is used in the collapse_limits routine.
#' @export
rectify <- function(data, resp = "INDC", exclude = "X", limits = NULL,
                    groups = NULL, dfilter = NULL, sdfilter = NULL,
                    diffs = FALSE, ratios = FALSE, addrev = FALSE, snap = 0.001,
                    cdata = NULL, verbose = FALSE, ilcats = NULL, reti = FALSE) {

    # ensuring the input data is just a generic data frame
    data <- data.frame(data)
    
    # excluding named columns
    odata   <- data
    ogroups <- groups
    cnames  <- colnames(odata)
    cnames  <- cnames[!(cnames %in% exclude)]
    # extract the features minus the indicator variable
    if (!is.null(resp)) feats <- cnames[cnames != resp]
    else                feats <- cnames

    if (is.null(groups)) {
        # reorganizing the data by naming convention
        datastruct <- organize(data, exclude = c(resp, exclude),
                               group_by = "names")
        data       <- datastruct$data
        # the features within the group are ordered by decreasing correlation,
        # making the assumption that this defines the arrow of time this
        # places the features in the correct chronological order - if this is
        # not the case, the groups need to be supplied by the calling function.
        groups     <- datastruct$groups
    }
    
    # adding combinations that must be derived before transformation
    
    # diffs
    if (diffs) {
      feats <- sort(feats)
      for (i in 1:(length(feats)-1)) {
        fbegin <- colnames(data)
        for (j in (i+1):length(feats)) {
          nfname <- paste0(feats[i],"_D_",feats[j])
          data[,nfname] <- data[,feats[i]] - data[,feats[j]]
          if ((feats[i] %in% cdata) & (feats[j] %in% cdata)) {
            # if both features are categorical, then the difference is also
            # categorical
            cdata <- c(cdata, nfname)
          }
        }
        # add to the groups
        groups[[feats[i]]] <- colnames(data)[!(colnames(data) %in% fbegin)]
      }
    }
    
    # ratios
    if (ratios) {
      feats <- sort(feats)
      for (i in 1:(length(feats)-1)) {
        fbegin <- colnames(data)
        for (j in (i+1):length(feats)) {
          nfname <- paste0(feats[i],"_R_",feats[j])
          data[,nfname] <- data[,feats[i]] / data[,feats[j]]
        }
        # add to the groups
        groups[[feats[i]]] <- colnames(data)[!(colnames(data) %in% fbegin)]
      }
      keep <- rep(TRUE,dim(data)[1])
      for (i in 1:dim(data)[1])
        if (any(is.na(data[i,]))) keep[i] <- FALSE
      data <- data[keep,]
    }
    
    if (reti) return(list(data = data, groups = groups))
    
    # determining the critical ranges

    # the lim_return list will contain the limits associated with each feature
    # to be returned with the final data structure
    lim_return <- list()
    # the features of the data set will be reorganized according to the order
    # specified by the "groups" variable (either calculated by the degree of
    # correlation or supplied by the calling function)
    feats      <- unlist(groups)
    # only keep those features that actually exist in the data set - if the
    # calling function supplied the groups and has more features than actually
    # exist in the data, this prevents an error
    feats      <- feats[feats %in% colnames(data)]
    # creating a copy of the data set that will ultimately be revised and
    # returned as the new, transformed data set
    dset_new <- data[,c(feats, resp)]
    # initialize the list of categorical columns
    lcats <- list()
    # iterate through each group separately - the groups will expand if there
    # is categorical data, so using a frozen version of the groups to iterate
    igroups <- groups
    for (j in 1:length(igroups)) {
        # lim is the list of limits for each feature within a specific group
        lim   <- list()
        # iterate through the individual features within each group
        for (i in 0:(length(igroups[[j]])-1)) {
            # identifying the group by name so that if the order of the limits
            # or groups changes for any reason, the correct limits applies
            # to the group selected
            gp <- names(igroups)[j]
            # similarly for the feature (may be a time step)
            ts <- igroups[[gp]][length(igroups[[gp]])-i]
            # if the limits are not supplied, it is understood that the
            # function will find them for each feature - this would be for
            # model training
            if (ts %in% cdata) {
               # processing categorical data
               if (!is.null(ilcats))
                 cats <- ilcats[[paste0(ts,".cats")]][paste0(ts,".",
                                 1:length(ilcats[[paste0(ts,".cats")]]))]
               else 
                 cats <- unique(data[,ts])
               lcats[[paste0(ts,".cats")]] <- cats
               names(lcats[[paste0(ts,".cats")]]) <- paste0(ts,".",1:length(cats))
               cat_lims <- list()
               for (k in 1:length(cats)) {
                 nfname <- paste0(ts,".",k)
                 dset_new[,nfname] <- data[,ts] %in% cats[k]
                 dset_new[,nfname][!dset_new[,nfname]] <- -1
                 cat_lims[[nfname]] <- c(0.999,1.001)
               }
               lim[[paste0(ts,".cats")]] <- cat_lims
               groups[[paste0(ts,".cats")]] <- paste0(ts,".",1:length(cats))
               # removing the original feature since it is replaced by the one-
               # hot encoded features
               groups[[gp]] <- groups[[gp]][-which(groups[[gp]] == ts)]
               dset_new     <- dset_new[,-which(colnames(dset_new) == ts)]
             }
            else if (is.null(limits)) {
                # find out what the limits are and calculate a new response
                # as if this feature were the only relevant one
                p <- presp(measurement = as.numeric(data.frame(data)[,ts]),
                           sdfilter = sdfilter, dfilter = dfilter, snap = snap,
                           response = as.logical(data.frame(data)[,resp]))
                # record the new limits
                lim[[ts]] <- p$limits
                # record the transformed column of the new data set
                dset_new[,ts] <- p$vector
            }
            # if the limits are supplied, it is understood that the function
            # will apply those limits to the data set - this would be for model
            # application to a data set after it has been trained
            else if (any(colnames(data) == ts)) {
                # use the limits provided
                lim[[ts]] <- limits[[gp]][[ts]]
                # calculate the new response
                p <- presp(measurement = data[,ts], sdfilter = sdfilter,
                           dfilter = dfilter, rmin = lim[[ts]][1],
                           rmax = lim[[ts]][2])
                # record the transformed column of the new data set
                if (!any(is.na(p)) & (length(p) > 0))
                    dset_new[,ts] <- p$vector
                else
                    dset_new[,ts] <- rep(-1, dim(dset_new)[1])
            }
        }
        if (any(igroups[[gp]] %in% cdata)) { 
          lim_working <- list()
          con <- unlist(lapply(lim, FUN = function(x) !is.list(x)))
          lim_working[[gp]] <- lim[con]
          lim_working <- c(lim_working, lim[!con])
          lim_return <- c(lim_return, lim_working)
        }
        else
          lim_return[[gp]] <- lim
    }
    # removing any zero-length groups (may occur if a group was entirely
    # composed of categorical variables)
    keep <- rep(TRUE,length(groups))
    for (i in 1:length(groups))
      if (length(groups[[i]]) == 0) keep[i] <- FALSE
    groups <- groups[keep]
    # adding combinations that must be derived after transformation
    
    # excluding named columns
    cnames <- colnames(odata)
    cnames <- cnames[!(cnames %in% exclude)]
    # extract the features minus the indicator variable
    if (!is.null(resp)) feats <- cnames[cnames != resp]
    else                feats <- cnames
    
    #addrev
    if (addrev) {
      nset <- -dset_new[,unlist(groups)]
      colnames(nset) <- paste0("n",unlist(groups))
      dset_new <- cbind(dset_new,nset)
    }

    # return the data structure
    #names(lim_return)  <- names(groups)
    return(list(data = dset_new, original_data = odata, exclude = c(exclude,resp),
                groups = ogroups, ngroups = groups, resp = resp, limits = lim_return,
                dfilter = dfilter, sdfilter = sdfilter, cdata = cdata,
                lcats = lcats, feats = feats, diffs = diffs, ratios = ratios,
                addrev = addrev))
}

#' Project what the response would be if only this variable were relevant.
#'
#' This function extrapolates what the response would be if this variable
#' were the only relevant one.  It determines what range the current responses
#' cluster in and predicts that all times in which this variable occupies
#' that range there would be a positive response.  This is a non-exported
#' utility function used to simplify the squarify function code.  Since there
#' is inevitably some error in real data, using the 10th and 90th percentiles
#' to define the range instead of the absolute min and max, that way
#' significant outliers are excluded.
#'
#' @name presp
#' @param measurement The input data set to transform.
#' @param response The responses of the indicator variable from "measurement"
#'     and any other relevant variables.  This is a logical variable. If NULL,
#'     rmax and rmin must be provided, and vice versa.
#' @param rmax The top of the range that defines a TRUE response.  This is
#'     set to NULL in order to calculate it from the given data.
#' @param rmin The bottom of the range that defines a TRUE response.  This is
#'     set to NULL in order to calculate it from the given data.
#' @return A rectified version of the input data set and associated feature
#'     ranges.
presp <- function(measurement, response = NULL, rmax = NULL, rmin = NULL,
                  sdfilter = 3, dfilter = NULL, snap = NULL) {

    # ensuring that "measurement" and "response" are vectors
    if (!is.null(response))
        response <- as.logical(data.frame(response)[,1])
    measurement  <- as.numeric(data.frame(measurement)[,1])

    # uses the actual density vs. the density that would occur if distribution
    # were random to pick out the relevant range
    dens2rg <- function(v, dfilt) {
        # this is what the density would be if the points were randomly
        # distributed
        rand_dens <- 1/abs(max(v) - min(v))
        # find the actual density curve
        dens <- density(v, bw = "SJ", kernel = "rectangular", cut = 0,
                        na.rm = TRUE, adjust = 0.7*as.numeric(dfilt))
        # find the points closest to the intersection between the random density
        # line and the actual density line
        diff <- abs(dens$y - rand_dens)
        names(diff) <- 1:length(diff)
        diff <- sort(diff)
        idx  <- as.numeric(names(diff))
        dmin <- min(dens$x[idx[1:5]])
        dmax <- max(dens$x[idx[1:5]])
        return(c(dmin,dmax))
    }

    # if rmin and rmax are calculated, vsens will be calculated as well,
    # otherwise not
    vsens <- NULL
    # there is nothing to do if niether a response nor a max and min are
    # supplied, so returning "NA"
    if (all(is.null(c(response, rmax, rmin)))) return(NA)
    # if both rmax and rmin are missing, but the response is not, it is
    # understood that the intent is to calculate them
    if (is.null(c(rmax,rmin))) {
        rdata <- measurement[response]
        # an adjustable standard deviation filter to screen out outliers
        # if desired
        if (!is.null(sdfilter)) {
          rdata <- rdata[(rdata > (mean(rdata) - sdfilter*sd(rdata))) &
                         (rdata < (mean(rdata) + sdfilter*sd(rdata)))]
        }
          
        if (is.null(dfilter)) {
          if (length(rdata) == 0) {
            rmin <- min(measurement)
            rmax <- max(measurement)
          }
          else {
            rmin  <- min(rdata)
            if (sum(measurement < rmin) < floor(snap*length(measurement)))
              rmin <- NA
            rmax  <- max(rdata)
            if (sum(measurement > rmax) < floor(snap*length(measurement)))
              rmax <- NA
          }
        }
        else {
          rg   <- dens2rg(rdata, dfilter)
          rmin <- rg[1]
          rmax <- rg[2]
        }
    }
    # calculating what the response would be if this measurement were the only
    # relevant one
    retrn <- rep(-1, length(measurement))
    if (!any(is.na(c(rmin,rmax)))) {
      if (rmin != rmax)
        retrn[(measurement >= rmin) & (measurement <= rmax)] <- 1
    }
    else {
      # an NA as rmin or rmax just means unbounded in that direction
      if ((is.na(rmin)) & (!is.na(rmax))) retrn[measurement <= rmax] <- 1
      else                                retrn[measurement >= rmin] <- 1
      
      
    }

    return(list(vector = retrn, limits = c(rmin, rmax)))

}
