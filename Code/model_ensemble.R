#' Take several models (possibly of different types) and create an ensemble.
#'
#' This function takes several already fitted models and determines what
#' combination of these models best describes the actual response. The output
#' of this function is a new model structure that contains an additional
#' ensemble layer which employs all of the other models in an optimal way.
#'
#' @name model_ensemble
#' @param ... Specify each model to evaluate.
#' @param devset This is the data set used to generate the response for the
#'     model. If a separate development set is not available, use the training
#'     set for this.  If not supplied, an amalgamation of all of the training
#'     sets for each of the models is attempted.
#' @param resp This parameter identifies the response variable.  The default
#'     is "INDC" (for "indicator").  This will be the name of the appropriate
#'     column in the data set.  This can be set to NULL.  If so, this function
#'     only builds the response matrix and returns it without trying to fit
#'     an ensemble model.
#' @param dtype The discriminator type to use for the 2nd layer ensemble. The
#'     default is to use a decision tree model (Random Forest), though to get
#'     a sense of which model is contributing the most, a simple logistic
#'     regression is most useful (dtype = 0, or dtype = "LR").
#' @param rm.na Remove models that produce all NAs for the examples given.
#' @param fill.na The value to fill in with in case there are NA values in the
#'     data set.  This is useful if a consolidated data set is created from
#'     the individual training sets since each data set may not have exactly the
#'     same columns.
#' @return A single model structure that contains all of the relevant input
#'     models along with directions regarding how to combine them in order
#'     to achieve the best result.
#' @export
model_ensemble <- function(..., devset = NULL, resp = "INDC", dtype = "RF",
                           rm.na = TRUE, fill.na = 0) {

    allmodels <- list(...)

    # attempting to create a consolidated data set based on each of the
    # training sets for the submitted models
    if (is.null(devset)) {
        message("Creating consolidated data set...")
        devset <- data.frame(allmodels[[1]]$data)
        devset$key <- rownames(devset)
        if (length(allmodels) > 1) {
            for (i in 2:length(allmodels)) {
                cat(".")
                addto  <- allmodels[[i]]$data
                addto$key <- rownames(addto)
                if (any(addto$key %in% devset$key))
                    devset <- combine_datasets(devset, addto, rm.na = FALSE,
                                               resps = c(resp,resp),
                                               join = "key")$data
                if (any(!addto$key %in% devset$key))
                    devset <- combine_datasets(devset, rm.na = FALSE,
                                               addto[!(addto$key %in% devset$key),],
                                               resps = c(resp,resp),
                                               join = NULL)$data
            }
            message()
        }
        if (!is.null(fill.na))
            devset[is.na(devset)] <- fill.na
        message("  Done.")
    }

    resmat    <- genMat(0, ncol = (length(allmodels)+2), nrow = dim(devset)[1],
                        dimnames = c("X", paste0("C", 1:length(allmodels)),
                                     "INDC"))
    resmat        <- data.frame(resmat)
    resmat$X      <- as.character(1:dim(resmat)[1])
    if (!is.null(resp)) resmat$INDC <- devset[,resp]
    for (i in 1:length(allmodels))
        resmat[,(i+1)] <- modfunc(allmodels[[i]], data = devset,
                                  result_column = NULL, rescale = NULL)
    k <- i

    # if any of the models result in all NAs, remove that result and remove
    # the model from the list of models to be included in the final ensemble
    allNAs <- unlist(lapply(2:(k+1), FUN = function(i) {
        all(is.na(resmat[,i]))
    }))
    if (sum(allNAs) > 0) {
        warning(sprintf("The following model types produced all NAs: %s",
                        paste(unlist(lapply(which(allNAs), FUN = function(i) {
                            allmodels[[i]]$dtype
                        })), collapse = ", "))) }

    if ((sum(allNAs) > 0) & (rm.na)) {
        resmat    <- resmat[,-(which(allNAs)+1)]
        allmodels <- allmodels[-which(allNAs)]
        k %-=% sum(allNAs) }

    if (k == 0) {
        message("ERROR! No models produced valid results!")
        return(NULL) }

    colnames(resmat) <- c("X", paste0("C", 1:k), "INDC")

    # only build the response matrix and return it if desired (resp == NULL)
    if (is.null(resp)) return(resmat[,-which(colnames(resmat) == "INDC")])
    groups <- colnames(resmat)
    groups <- groups[!(groups %in% c("X", "INDC"))]

    if (dtype == "MEAN") {
        # clearing any data sets from subordinate models and tabulating features used
        feats <- NULL
        for (i in 1:length(allmodels)) {
            allmodels[[i]]$data <- NULL
            if (!is.null(allmodels[[i]]$fmap))  feats <- c(feats, allmodels[[i]]$fmap)
            if (!is.null(allmodels[[i]]$feats)) feats <- c(feats, allmodels[[i]]$feats)
        }
        feats <- unique(feats)
        # creating the ensemble
        ensemble <- list(model = list(models = allmodels, dtype = "ENSEMBLE",
                                      dtype2 = "MEAN", feats = groups,
                                      feats2 = feats),
                         resp = resp, data = devset, feats = feats)
        class(ensemble) <- "modobj"

        return(ensemble)
    }
    else
        ensemble <- modelfit(resmat, resp = resp, dtype = dtype,
                             groups = (list(groups) %n% "resmat"))

    # figuring out what the most representative features are
    types <- unlist(lapply(1:length(allmodels), FUN = function(k)
    { if (any(class(allmodels[[k]]) == "modobj")) return(allmodels[[k]]$model$dtype)
      else                                        return(allmodels[[k]]$dtype)}))
    
    feats <- NULL
    for (i in 1:length(allmodels))
        feats <- c(feats, allmodels[[i]]$feats)
    feats <- unique(feats)

    ensemble$model$models <- allmodels
    ensemble$model$dtype2 <- ensemble$model$dtype
    ensemble$model$dtype  <- "ENSEMBLE"
    ensemble$model$feats2 <- feats
    ensemble$data         <- devset
    ensemble$feats        <- feats
    class(ensemble)       <- "modobj"

    return(ensemble)
}
