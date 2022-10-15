#' Combine several data sets and consolidate any indicator columns.
#'
#' This function takes as inputs several data sets, then combines them and
#' consolidates any response variable columns if they are named differently
#' between the data sets.
#'
#' @name combine_datasets
#' @param ... The data sets to combine.
#' @param resps A vector containing the response variable names.  If this is
#'     not null, there must be as many elements in this vector as there are
#'     unique response variable names in the data sets submitted for
#'     combination.  If there is a desire to combine data sets that have a
#'     response column with those that do not, this must be done in two steps:
#'     1) combine all of the data sets that have a response variable, and
#'     2) combine the resultant data set from #1 with all of the data sets that
#'     do not have a  response variable.  This will result in a data set that
#'     has NULL entries in the response column.  If there are no differences
#'     between the names of the response variables, this parameter can be NULL,
#'     but this also means that the returned structure will not include the
#'     name of a response variable. The value for this parameter can be a
#'     single character string instead of a vector, and this will imply that
#'     all response variables are named identically.  The resultant response
#'     variable will be consolidated under the banner of the *first* response
#'     variable listed in this parameter.
#' @param join If this parameter is NULL, the data sets are concatenated and
#'     any differences in columns will result in blank elements for the data
#'     sets that do not contain a column.  If this parameter contains a
#'     vector of strings, the join will occur on those fields; note that all
#'     data sets must have all join fields or an error will result.  This
#'     is an *inner* join, meaning that the only rows retained are the ones
#'     that satisfy the join criteria for all data sets submitted for
#'     combination.  If there were duplicate column names among the data
#'     sets that are not part of the joining criteria, only the columns with
#'     duplicate names from the first data set will be retained (for instance,
#'     if two data sets both have a "Date" column, the "Date" column from the
#'     first data set will be the only one retained).
#' @param rm.na Removes any rows with NA or NULL values in them.  Note that if
#'     combining data sets that have inconsistent column names this will remove
#'     any rows that do not have all of the columns.  Most fitting algorithms
#'     have trouble with NA or NULL values, so this is by default TRUE.
#' @return A structure (list) that includes the combined data set (the "data"
#'     element) as well as the name of the response variable that the
#'     consolidated data set uses (the "resp" element).
#' @export
combine_datasets <- function(..., resps = NULL, join = NULL, rm.na = TRUE) {
    dsets <- list(...)

    if (length(dsets) == 0)
    { message("ERROR! No data sets provided."); return(NULL) }

    pkg <- c("stringr", "dplyr")
    for (p in pkg) {
        if (!require(p, character.only = TRUE, quietly = TRUE)) {
            message(paste0("The '",p,"' package must be installed to use ",
                           "this function."))
            return(NULL)
        }
    }

    if (length(dsets) == 1)  { # nothing to do if only one data set provided
        # returning the only data set provided
        return(list(data = dsets[[1]], resp = resp[1])) }

    # no join data, so concatenating
    if (is.null(join)) {
        # Concatenating the data sets and keeping the unique rows. Any columns
        # that exist in one data set but do not exist in the other will be
        # added as columns with empty elements for the rows that belonged to
        # data sets in which the column was absent.  If the response variable
        # is called by different names in each data set, they will be
        # consolidated in a later stage if the response variable names are
        # specified in the 'resps' parameter.
        comb_data <- dsets[[1]]
        if (length(dsets) > 1) {
            for (k in 1:(length(dsets)-1)) {
                ds <- dsets[-1][[k]]
                # the column names of the sets combined so far
                colnams_comb <- colnames(comb_data)
                # the column names in the new set
                colnams_ds   <- colnames(ds)
                # the column name differences
                noncols_comb <- colnams_ds[!(colnams_ds %in% colnams_comb)]
                noncols_ds   <- colnams_comb[!(colnams_comb %in% colnams_ds)]
                # creating empty columns if they exist in the other data set
                if (length(noncols_comb) > 0) comb_data[,noncols_comb] <- NA
                if (length(noncols_ds) > 0)   ds[,noncols_ds]          <- NA
                # binding the new data set together
                comb_data    <- rbind(comb_data, ds)
            }
        }
    }
    else {
        if (isTRUE(join)) {
            # Trying to figure out what the join criteria should be by
            # inference if the "join" parameter is simply set to TRUE.

            # Determining which columns are the same in all data sets, since
            # that is the most basic requirement.
            samecols <- colnames(dsets[[1]])[colnames(dsets[[1]]) %in%
                                             colnames(dsets[[2]])]
            if (length(dsets) > 2)
                for (i in 3:length(dsets))
                    samecols <- samecols[samecols %in% colnames(dsets[[i]])]

            if (length(samecols) == 0) {
                # if there are no common columns, joining cannot occur
                message("ERROR! There are no identically named columns in all")
                message("       data sets. The data sets cannot be joined.")
                return(NULL)
            }
            else if (length(samecols) == 1) {
                # if there is only one common column, then this is the only
                # join criteria possible
                join <- samecols
            }
            else {
                # Trying to determine whether any of the repeated column names
                # are an index column, since these will not provide a good
                # joining criteria.  Assuming an index column will mostly be
                # numbered in sequence, though there might be gaps if rows were
                # removed.  There might be more than one, so prioritizing by
                # a likelihood score.  The response columns will also be
                # screened out by this algorithm.

                lik <- NULL
                for (i in 1:length(samecols)) {
                    idxvotes <- unlist(lapply(1:length(dsets), FUN =
                    function(j) {
                        suppressWarnings(
                        values <- sort(as.numeric(dsets[[j]][,samecols[i]])))
                        # if more than half of the elements cannot be coerced
                        # to a numeric, assuming this is not an index column
                        if ((sum(is.na(values)) >= 0.5*length(values)) |
                            (length(values) == 0))
                            return(FALSE)
                        # calculating increments
                        incs <- values[2:length(values)] -
                                values[1:(length(values)-1)]
                        inct <- table(incs)
                        # if more than half of the values have the same
                        # increment (most likely 1), assume it is an index
                        # column
                        if (inct[which.max(inct)] > 0.5*length(values))
                            return(TRUE)
                        else
                            return(FALSE)
                    }))
                    # tabulating the votes
                    lik <- c(lik, sum(idxvotes)/length(idxvotes))
                }
                # removing the high likelihood columns
                samecols <- samecols[lik < 0.5]
                # the columns that are left are assumed to be the joining columns
                join <- samecols
            }
        }

        # joining the data sets on the fields specified by the "join" parameter
        comb_data <- dsets[[1]]
        if (length(dsets) > 1) {
            for (k in 1:(length(dsets)-1)) {
                ds <- dsets[-1][[k]]
                comb_data <- inner_join(comb_data, ds, by = join)
                # eliminating duplicate named columns
                cnames <- colnames(comb_data)
                # get the duplicate column names, these will have been denoted
                # by the ".x" suffix after the join
                dups  <- sort(cnames[grepl(".x$", cnames)])
                # if there are no duplicates do nothing
                if (length(dups) > 0) {
                    # get the root names (the part before the ".x" implicating
                    # it as a duplicate) - because of the way that the string
                    # is split there will always be a null string in the
                    # vector, and sorting will make it always the first
                    # element, which can then be removed
                    roots <- sort(unique(unlist(str_split(dups,".x"))))[-1]
                    # create a regex pattern that describes all of the columns
                    # that have an identically named counterpart
                    pat <- paste0("^(", paste(roots, collapse = "|"), ")[.]")
                    # remove all columns that have an identically named
                    # counterpart
                    newds <- comb_data[,!grepl(pat, cnames)]
                    # checking to see if there are copies of the columns
                    # identified as roots
                    if (!all(roots %in% colnames(newds))) {
                        # add back in just one version of the duplicated
                        # columns
                        cnames <- colnames(newds)
                        newds <- cbind(comb_data[,paste0(roots, ".x")], newds)
                        colnames(newds) <- c(paste0(roots, ".x"), cnames)
                    }
                    else
                        newds <- newds[,c(roots, colnames(newds)[!(colnames(newds)
                                                                   %in% roots)])]
                    # getting a new copy of the column names
                    cnames <- colnames(newds)
                    # changing the duplicated column names to the root names
                    # only (minus the ".x")
                    cnames <- c(roots, cnames[!(cnames %in% c(roots,dups))])
                    # reassigning the new column names
                    colnames(newds) <- cnames
                    # overwriting the old data set with the newly revised data
                    # set without the additional columns that were identically
                    # named before the join
                    comb_data <- newds
                }
            }
        }
    }

    # ensuring that if there are any identical response variable names, only
    # the unique names are retained
    resps <- unique(resps)
    # Consolidating all columns marked as response columns into one under the
    # the first column name in resps if there is more than one.
    if (length(resps) > 1) {
        combr <- unlist(lapply(1:dim(comb_data)[1], FUN = function(i) {
            # if the primary response column has an empty spot, check the
            # other columns
            if (is.na(comb_data[i,resps[1]])) {
                remains <- comb_data[i,resps[-1]]
                nonnull <- which(!is.na(remains))
                if (length(nonnull) > 0)
                    return(remains[min(nonnull)])
                else
                    return(NA)
            }
            else
                return(comb_data[i,resps[1]])
        }))
        # replacing the first named response column with the consolidated
        # responses
        comb_data[,resps[1]] <- combr
        # removing all but the first named response column
        comb_data <- comb_data[,-which(colnames(comb_data) %in% resps[-1])]
    }

    # recording the name of the response column used if there are any
    if (length(resps) > 0) {
        resp <- resps[1]
        comb_data[, resp] <- as.logical(comb_data[, resp])
        comb_data[is.na(comb_data[,resp]), resp] <- NA
    }
    else
        resp <- NULL

    # Setting all index values to zero to prevent this from being a
    # difference that the following algorithm keys in on.  The index will be
    # restored after duplicate elimination.
    comb_data$X <- 0

    # Identifying the duplicates by creating a hash (or fingerprint) of each
    # row's data. The slightest difference will cause the row to be retained,
    # so this needs to be done after the response columns are consolidated.
    fprint <- unlist(lapply(1:dim(comb_data)[1], FUN = function(i) {
        hashfunc(paste(comb_data[i,], collapse = ""), trnc = 50)[1]
    }))
    # picking the lowest index of those that are identical to retain
    keep1 <- unique(unlist(lapply(1:length(fprint), FUN = function(i) {
        min(which(fprint == fprint[i]))
    })))
    # eliminating duplicates
    comb_data <- comb_data[keep1,]
    # removing rows with NA and NULL values if desired (this is the default)
    if (rm.na) comb_data <- rmna_rows(comb_data)
    # if there are no rows after eliminating those with NA and NULL values,
    # these data sets cannot be concatenated.
    if (dim(comb_data)[1] == 0) {
        message("ERROR! These data sets appear to be incompatible for")
        message("       concatenation.  Try creating models based on a")
        message("       joined data set or setting rm.na to FALSE.")
        return(NULL)
    }

    # renumbering the index variable
    comb_data$X <- 1:dim(comb_data)[1]

    return(list(data = comb_data, resp = resp))
}
