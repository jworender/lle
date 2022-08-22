#' Expand a square rectified data set to include logical OR candidates.
#'
#' This function takes a previously square rectified data set (processed using
#' the "rectify" function), and expands the columns to include logical OR
#' candidates with the critical ranges progressivly shrunk from the maximum
#' size to the minimum size.  This process executed on the previously square
#' rectified data.
#'
#' @name OR_encode
#' @param sqr_data A data structure that has previously been square rectified
#'     using the "rectify" function.
#' @param feats The feature set to profile.  If this is set to NULL, the
#'     function will attempt an initial fit to identify features of interest.
#' @param limits Supply the limits rather than calculate them.
#' @param stride How many points to skip over.  The function tabulates the
#'     positions of all points that are within the critical range, but are not
#'     true positive examples, and then takes 'stride' # of examples from either
#'     extreme end off and recalculates the new number that meet this criteria.
#'     If this is set to one, the function will attempt to create a version for
#'     *every* possible mutation.  If this is a large data set, the number of
#'     possible mutations may be *very* large, so a few runs at a higher nstride
#'     may be worth doing to narrow down the range a bit before investigating
#'     every possible mutation.
#' @return A new expanded data set that includes new columns based on
#'     progressively shrunken critical ranges.
#' @export
OR_encode <- function(sqr_data, feats = NULL, limits = NULL, stride = 10) {
    groups      <- sqr_data$groups
    data        <- sqr_data$data
    odata       <- sqr_data$original_data
    resp        <- sqr_data$resp
    olimits     <- sqr_data$limits
    resp        <- sqr_data$resp
    sdfilter    <- sqr_data$sdfilter
    sensitivity <- sqr_data$sensitivity

    # figuring out which features will ultimately be important by doing a LASSO
    # fit - while this won't tell us which features will have a logical OR
    # relationship, it will give a comprehensive list of important features
    # since a logical OR relationship looks exactly the same as a logical AND
    # relationship from the perspective of the *apparent* critical ranges

    if (is.null(feats)) {
        LS_fit <- modelfit(data = data, resp = resp, dtype = "LS",
                           groups = groups)
        feats  <- LS_fit$model$feats
    }

    # finding out what the possible mutations of each important feature are when
    # the critical range is narrowed in order to progressively eliminate
    # examples that appear outside the true positive set

    if (is.null(limits)) {
        # creating a flattened list of rectified dataset limits
        limlist <- list()
        for (i in 1:length(olimits)) {limlist <- c(limlist, olimits[[i]])}
        # narrowing scope to just the limits that are needed
        limlist <- limlist[feats]
        # creating a list of "versions" defined by narrowing limits
        verlist <- limlist
        # the number of unique elements in verlist
        nv <- 0
        # the response vector
        rvect <- as.logical(data.frame(data)[,resp])
        for (f in feats) {
            # the original limits for all positive examples
            lim <- limlist[[f]]
            # the extra-true-positive examples (examples within the apparent
            # critical range but not true-positives)
            etp <- as.numeric(data.frame(data)[,f])
            etp[etp == -1] <- FALSE
            etp[etp ==  1] <- TRUE
            etp <- as.logical(etp)
            etp <- xor(etp, rvect)
            # the values for the extra-true-positive examples
            val <- unique(sort(c(lim[1], odata[etp, f], lim[2])))

            # if stride is one, attempt to find every possible mutation
            # of the critical range (for large data sets, this could take
            # quite a long time, so it may be a good idea to reduce the
            # sizes of the ranges using a larger stride value first)
            verlist[[f]] <- list(c(min(val), max(val)))
            aval         <- sort(val, decreasing = TRUE)
            bval         <- sort(val)
            aval         <- aval[aval > bval[2]]
            while (length(bval) > stride) {
                if (length(aval) > stride) {
                    aval <- aval[-(1:stride)]
                    verlist[[f]] <- c(verlist[[f]],list(c(bval[1], aval[1])))
                }
                else {
                    # replenish the aval vector and take one off the bval
                    # vector
                    aval <- sort(val, decreasing = TRUE)
                    bval <- bval[-(1:stride)]
                    aval <- aval[aval > bval[2]]
                }
            }
            nv %+=% length(verlist[[f]])
        }
    }
    else {
        nv <- 0
        verlist <- limits
        for (i in 1:length(verlist))
            nv %+=% length(verlist[[i]])
    }
    # creating the skeleton of the new data set
    ds_new <- data.frame(genMat(nrow = dim(data)[1], ncol = (nv+1),
                                dimnames = c("INDC", paste0("V",1:nv))))
    # the new response column is the complement of the old response column
    ds_new$INDC <- !as.logical(data.frame(data)[,resp])
    new_groups  <- as.list(feats)
    names(new_groups) <- feats

    # encoding the *complement* of the variable versions with the new limits
    k <- 1
    for (f in feats) {
        v <- verlist[[f]]
        for (i in 1:length(v)) {
          old_col <- as.numeric(data.frame(odata)[,f])
          new_col <- old_col
          # encoding the complement
          new_col[(old_col >= v[[i]][1]) & (old_col <= v[[i]][2])] <- -1
          new_col[(old_col <  v[[i]][1]) | (old_col >  v[[i]][2])] <- 1
          ds_new[,(k+i)] <- new_col
        }
        cnames <- colnames(ds_new)
        new_groups[[f]] <- sprintf("%s_CV%02.0f", f, 1:length(v))
        names(verlist[[f]]) <- new_groups[[f]]
        cnames[(k+1):(k+length(v))] <- new_groups[[f]]
        colnames(ds_new) <- cnames
        k %+=% length(v)
    }


    return(list(data = ds_new, original_data = odata, groups = new_groups,
                resp = "INDC", limits = verlist, stride = stride,
                sdfilter = sdfilter, sensitivity = sensitivity))

}
