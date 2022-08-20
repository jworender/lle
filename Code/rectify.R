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
#' @param checkdata Whether to check for abnormalities in the data set (like
#'     missing, NA, or NULL data).  The default is TRUE.
#' @param verbose A logical which determines whether informational messages
#'     are displayed to the console.
#' @export
rectify <- function(data, resp = "INDC", exclude = "X", limits = NULL,
                    groups = NULL, dfilter = NULL, sdfilter = NULL,
                    verbose = FALSE) {

    # excluding named columns
    cnames <- colnames(data)
    cnames <- cnames[!(cnames %in% exclude)]
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

    # the lim_return list will contain the limits associated with each feature
    # to be returned with the final data structure
    lim_return <- list()
    # sensitivity is a list of sensitivity vectors for each group - this is only
    # populated if new rectified parameters are being generated
    sensitivity <- list()
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
    # iterate through each group separately
    for (j in 1:length(groups)) {
        # lim is the list of limits for each feature within a specific group
        lim   <- list()
        # the sensitivity for each time step
        vsens <- NULL
        # iterate throught the individual features within each group
        for (i in 0:(length(groups[[j]])-1)) {
            # identifying the group by name so that if the order of the limits
            # or groups changes for any reason, the correct limits applies
            # to the group selected
            gp <- names(groups)[j]
            # similarly for the time step
            ts <- groups[[gp]][length(groups[[gp]])-i]
            # if the limits are not supplied, it is understood that the
            # function will find them for each feature - this would be for
            # model training
            if (is.null(limits)) {
                # find out what the limits are and calculate a new response
                # as if this feature were the only relevant one
                p <- presp(measurement = as.numeric(data.frame(data)[,ts]),
                           sdfilter = sdfilter, dfilter = dfilter,
                           response = as.logical(data.frame(data)[,resp]))
                # record the new limits
                lim[[ts]] <- p$limits
                # record the sensitivity
                vsens <- c(vsens, p$vsens)
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
                # record filler values for sensitivity
                vsens <- c(vsens, 0)
                # record the transformed column of the new data set
                if (!any(is.na(p)) & (length(p) > 0))
                    dset_new[,ts] <- p$vector
                else
                    dset_new[,ts] <- rep(-1, dim(dset_new)[1])
            }
        }
        # record this list of limits associated with each element to the master
        # list containing the limits for all groups
        #names(lim)   <- groups[[j]]
        names(vsens) <- groups[[j]]
        lim_return[[j]]  <- lim
        sensitivity[[j]] <- vsens
    }

    # return the data structure
    names(lim_return)  <- names(groups)
    names(sensitivity) <- names(groups)
    return(list(data = dset_new, original_data = data, groups = groups,
                resp = resp, limits = lim_return, dfilter = dfilter,
                sdfilter = sdfilter, sensitivity = sensitivity))
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
                  sdfilter = 3, dfilter = NULL) {

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
                rmax  <- max(rdata)
            }
        }
        else {
            rg   <- dens2rg(rdata, dfilter)
            rmin <- rg[1]
            rmax <- rg[2]
        }

        # Calculating a sensitivity to this variable based on the complement of
        # the fraction of the range occupied by the critical range.  The
        # argument is that if most examples stay within the critical range of a
        # variable almost all of the time and only occasionally drift out, it
        # is a far less important variable to watch than one in which the
        # critical range is a very small fraction of the total range for the
        # variable.  If the variable starts to draw close to this critical range
        # when fraction of the total range is low, it should cause a greater
        # level of interest.
        mdata <- measurement
        tmin  <- min(mdata)
        tmax  <- max(mdata)
        vsens <- 1 - (rmax - rmin)/(tmax - tmin)
    }
    # calculating what the response would be if this measurement were the only
    # relevant one
    retrn <- rep(-1, length(measurement))
    if (rmin != rmax)
        retrn[(measurement >= rmin) & (measurement <= rmax)] <- 1

    return(list(vector = retrn, limits = c(rmin, rmax), vsens = vsens))

}
