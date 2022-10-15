#' Creates a simple confusion matrix given a data set and an index of a feature
#'     set
#'
#' This function calculates a simple confusion matrix for a model calculated
#' from a feature set and an input data set of the format produced by the
#' "get_ds" function. Note that in order to calculate a confusion matrix,
#' there needs to be an indicator column, so the "uncategorized" data sets,
#' for instance, won't work with this function.
#'
#' This function will also do a Pearson chi-square test and store the result
#' in an "nmaps.chisq" object if desired.  The object will also include the
#' points for the ROC curve and the AUC (area under the ROC curve).  The p-value
#' from the chi-square test is the best indication of how well the positive and
#' negative examples are separated at the threshold selected (if the selected
#' threshold is NULL, the optimal value will be selected), while the ROC and AUC
#' give the best indication of how sharply the positive examples are
#' differentiated from the negative examples.
#'
#' @name mod_chisq
#' @param model The model to use.  This can also be an nmaps object. If NULL,
#'     a prediction vector is required.
#' @param data A data set to use.  If this is not supplied, the data will be
#'     extracted from the nmaps object if available.
#' @param pred A prediction vector if one has already been calculated.  If
#'     NULL, a model must be provided to calculate the prediction vector.
#' @param resp The name of the response variable to be used for this data set.
#'     This should be a character string.  The default is "INDC" (for
#'     "Indicator").
#' @param thresh The threshold to use on the model result to distinguish
#'     "positive" examples from "negative" examples.  Everything greater
#'     than the threshold will be a "positive" example, while everything
#'     less than or equal to the threshold will be a "negative" example.
#'     If NULL, a calculated "best" value will be used.
#' @param chisq If TRUE, returns the results of the Pearson chi-square test
#'     instead of just the confusion matrix.  If the argument is the result
#'     of a chi-square test previously performed by this function, it will
#'     re-perform the test using the new threshold specified in the "thresh"
#'     parameter. The data in the chisq object will override any parameters
#'     supplied other than the "thresh" parameter.
#' @param display Draws the confusion matrix on the console.
#' @param rescale Rescale the results to be within the scale provided. The
#'     default is zero to one.
#' @param verbose Display warnings and informational messages.
#' @return If the "return_table" parameter is true, this function returns
#'     a matrix of values that represent the confusion matrix entries.
#' @export
mod_chisq <- function(model = NULL, data = NULL, pred = NULL, resp = "INDC",
                        thresh = NULL, chisq = TRUE, display = FALSE,
                        rescale = c(0,1), verbose = TRUE) {

    # using the data supplied in the chisq object (overrides the data parameter)
    if (is.list(chisq)) if (!is.null(chisq$resp)) {
        data <- chisq$resp
        cnames <- colnames(data)
        cnames[1] <- resp
        colnames(data) <- cnames }
    if (is.list(chisq)) if (!is.null(chisq$pred))
        { pred <- chisq$pred }
    if (is.list(chisq)) chisq <- TRUE

    # If data is not passed, attempting to extract the training data from the
    # model object.  Using the last object in the model list since the first
    # one might be a layer 2 model.
    if (is.null(data) & (any(class(model) == "nmaps")))  data <- model$data
    else if (is.null(data)) {
        message("ERROR! No data set provided.")
        return(NULL) }

    # if an nmaps object was supplied, scale it down to a simple model
    if (any(class(model) == "nmaps")) mod <- model$model
    else                              mod <- model

    if (is.null(pred)) {
        if (verbose) message("Calculating predictions...")
        res <- modfunc(mod, data = data, rescale = rescale, result_column = NULL)
        # counting NA results as FALSE by definition - if another treatment is
        # needed, get the predictions by calling dfunc separately and submitting
        # the modified prediction vector through the "pred" parameter
        res[is.na(res)] <- 0 # numeric FALSE
        if (verbose) message("Done.")
    }
    else {
        if (verbose) message("Using supplied prediction vector.")
        res <- pred
    }

    correct <- as.logical(data[,resp])
    tresp   <- unlist(lapply((1:100)/100, FUN = function(x) {
        pr  <- res > x
        TPR <- sum(pr & correct)/sum(correct)          # True Positive Rate
        FPR <- sum(xor(pr,correct) & pr)/sum(!correct) # False Positive Rate
        d   <- sqrt((1-TPR)^2 + FPR^2)
        return(d)
    }))
    tresp %idx% 1
    tresp <- sort(tresp)
    tvec  <- as.numeric(names(tresp))/100
    if (sum(tresp == tresp[1]) != 0) {
        # pick the index of the closest threshold to 0.5 that has a distance
        # to the optimal point equal to the minimum
        idx <- as.numeric(names(tresp[tresp == tresp[1]]))
        thr_all <- idx/100 - 0.5
        names(thr_all) <- idx
        thr   <- as.numeric(names(sort(thr_all))[1])/100
        #thr <- sum(tvec[tresp == tresp[1]])/sum(tresp == tresp[1])
    }
    else
        thr <- 0.5

    if (is.null(thresh)) thresh <- thr
    tconf <- NULL
    pred  <- res >= thresh
    TP = sum(pred & correct)            # True/Positives
    TN = sum(!pred & !correct)          # True/Negatives
    FP = sum(xor(pred,correct) & pred)  # False/Positives
    FN = sum(xor(pred,correct) & !pred) # False/Negatives

    conf  <- matrix(c(TP,FN,FP,TN), nrow = 2,
                    dimnames = list(c("Pred TRUE", "Pred FALSE"),
                                    c("Actual TRUE", "Actual FALSE")))

    if ((TP+TN+FP+FN) == 0) {
        if (chisq) {
            ret <- list()
            ret$observed <- conf
            ret$thr      <- 0.5
            ret$p.value  <- 1.0
            return(ret)
        }
        else {
            return(conf)
        }
    }

    tconf <- matrix(c(sprintf("%d / %.1f%%", c(TP, FN),
                              100*c(TP, FN)/(TP+FN)),
                      sprintf("%d / %.1f%%", c(FP, TN),
                              100*c(FP, TN)/(FP+TN))),
                    nrow = 2,
                    dimnames = list(c("Pred TRUE", "Pred FALSE"),
                                    c("Actual TRUE", "Actual FALSE")))

    # return the chi-square test if desired
    if (chisq) {
        suppressWarnings(
            conf      <- chisq.test(conf))
        conf$thr  <- thr
        conf$pred <- res
        conf$resp <- data.frame(data[,resp])

        ROC   <- data.frame(genMat(nrow = 101, ncol = 3,
                                   dimnames = c("THR", "FPR", "TPR")))

        stats <- data.frame(genMat(nrow = 101, ncol = 11,
                                   dimnames = c("THR", "TP", "FP", "FN", "TN",
                                                "d", "SENS", "SPEC", "PPV",
                                                "Fscore", "Youdens")))
        for (i in 1:101) {
            ROC$THR[i]       <- (i-1)/100
            stats$THR[i]     <- (i-1)/100
            pr               <- res >= (ROC$THR[i] - 1E-6)
            # Raw numbers
            stats$TP[i]      <- sum(pr & correct)
            stats$FP[i]      <- sum(xor(pr,correct) & pr)
            stats$FN[i]      <- sum(correct) - sum(pr & correct)
            stats$TN[i]      <- sum(!correct) - sum(xor(pr,correct) & pr)
            # True Positive Rate
            ROC$TPR[i]       <- sum(pr & correct)/sum(correct)
            stats$SENS[i]    <- sum(pr & correct)/sum(correct)
            # False Positive Rate
            ROC$FPR[i]       <- sum(xor(pr,correct) & pr)/sum(!correct)
            stats$d[i]       <- sqrt((1-ROC$TPR[i])^2 + ROC$FPR[i]^2)
            stats$SPEC[i]    <- sum(!pr & !correct)/sum(!correct)
            stats$PPV[i]     <- sum(pr & correct)/(sum(pr)+1E-6)
            stats$Fscore[i]  <- 2*(ROC$TPR[i] * stats$PPV[i]) /
                                (ROC$TPR[i] + stats$PPV[i] + 1E-6)
            stats$Youdens[i] <- stats$SENS[i] + stats$SPEC[i] - 1
        }
        # While the p-value gives the best indication of separation between
        # positive and negative examples at the threshold selected, the ROC and
        # AUC give the best indication of how sharply defined the model results
        # are.
        ROC         <- data.frame(arrange(ROC, FPR, TPR, desc(THR)))
        conf$ROC    <- ROC
        conf$stats  <- stats
        conf$tconf  <- tconf
        conf$display <- function(x=tconf) {
            message(text_table(x, forms = c("%s","%s")))
            message(sprintf("P-value: %.4e",conf$p.value)) }
        conf$AUC    <- sum((ROC$FPR[2:101]-ROC$FPR[1:100]) *
                           (ROC$TPR[1:100]+ROC$TPR[2:101])/2)
        conf$FAREA  <- sum(stats$Fscore)/101
        if (is.na(conf$p.value)) conf$p.value = 1.0
        class(conf) <- "nmaps.chisq"
    }

    if (display) {
        message(text_table(tconf, forms = c("%s","%s")))
        message(sprintf("P-value: %.4e",conf$p.value)) }

    return(conf)
}
