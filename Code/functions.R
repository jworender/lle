# A set of generalized functions used in the markdown documents - these may
# also be used by others as long as the following comment block is located
# within the code and proper credit is given in the product produced.

# (c) Jason Orender, all rights reserved
# ** This code was originally published in support of the "LASSO Logic Engine"
# ** paper.  Authors:  Jason Orender, Mohammad Zubair, Jiangwen Sun

#' Fit a discriminator function.
#'
#' This function uses a set of data and a set of features to fit a model to
#' a selection of data.  This function works with both
#'
#' @name modelfit
#' @author Jason Orender
#' @param data The training data for the model.  It should have columns
#'     that contain the response variable and additional columns corresponding
#'     to any features used in the formula.  If NULL, the data must be
#'     provided in the "params" parameter in the specific way that the
#'     discriminator function requires.  Any data conditioning is done
#'     automatically for the built-in discriminator types.  If provided, this
#'     parameter overrides the data provided in the "params" parameter (i.e. if
#'     both are present only the data provided in this parameter is used).
#' @param fvec A vector of feature names to fit the model to.  If NULL, every
#'     column except for the response column (supplied usig the "resp"
#'     parameter) and the excluded columns (supplied by the "exclude"
#'     parameter) will be assumed to be a feature.
#' @param exclude The names of columnns to exclude if they exist in the data
#'     set.  An error will NOT be generated if a column exists in the exclude
#'     parameter but does not exist within the data set.
#' @param resp This parameter identifies the response variable.  The default
#'     is "INDC" (for "indicator").  This will be the name of the appropriate
#'     column in the data set.
#' @param dtype The discriminator type to use.  "LR" = logistic regression,
#'     "LRGD" = logistic regression with gradient descent, "NN" = neural net,
#'     "LS" = LASSO regression, "RF" = Random Forest, "RR" = Ridge regression,
#'     "SQ" = Square rectified LASSO regression (the default). A function can
#'     also be passed through this parameter to use an arbitrary model fitting
#'     function with the "modelfit" function used essentially as a wrapper that
#'     organizes the results into a properly formatted modobj object. The model
#'     fitting function must produce something that will work with the "predict"
#'     standard R function, since that is what the "modfunc" function uses to
#'     calculate new results.
#' @param groups The groupings of the features in correct chronological order.
#'     If this is not provided, the function will attempt discern this by
#'     using the "organize" function.  Currently, this data is only used to fit
#'     when using the square rectified LASSO option, however this data is used
#'     during other operations to better organize the outputs.
#' @param params A list of parameters to be used for the specific fit type. The
#'     parameters correspond to the specific package fit function.  If NULL,
#'     the defaults will be used.  Any parameters provided here (with the
#'     exception of data and response parameters) will override any defaults and
#'     be recorded in the resulting NMAPS object.  To determine which options
#'     are available, look up the corresponding package fit function.  For "LR",
#'     see the \link[stats]{glm} function.  For "LRGD", "LS", "RR", or "SQ"see
#'     the \link[glmnet]{glmnet} function; additionally, for "SQ" see the
#'     rectify function.  For "NN", see the \link[neuralnet]{neuralnet}
#'     function. For "RF", see the \link[randomForest]{randomForest} function.
#'     Each of these have highly specialized inputs for controlling the fitting
#'     process.
#' @param verbose If TRUE, informational messages are sent to the console as
#'     the fit progresses.
#' @return An nmaps object that contains a model and the data used to fit it..
modelfit <- function(data = NULL, fvec = NULL, exclude = "X", resp = "INDC",
                     dtype = "SQ", groups = NULL, params = NULL,
                     verbose = FALSE) {
  
  # This function is intended to be a wrapper for as many different
  # discriminator function types as desired during later development, so that
  # a consistent set of inputs can be used to generate a consistent set of
  # outputs regardless of the pecularities of the model generation function;
  # it also selects optimal default inputs.  This function, in effect,
  # modularizes fitting a model using any number of methods.
  
  # checking to make sure that all required packages are installed and loaded
  # for the predefined discriminator function types
  pkg <- c("stringr", "rlang", "dplyr")
  if (!is.function(dtype)) {
    if (dtype == "LR")
      pkg <- c(pkg, "stats")
    else if ((dtype == "LRGD") | (dtype == "LS") | (dtype == "RR") |
             (dtype == "SQ") | (dtype == "DSQ"))
      pkg <- c(pkg, "glmnet")
    else if (dtype == "NN")
      pkg <- c(pkg, "neuralnet")
    else if (dtype == "RF")
      pkg <- c(pkg, "randomForest")
  }
  for (p in pkg) {
    if (!require(p, character.only = TRUE, quietly = TRUE)) {
      message(paste0("The '",p,"' package must be installed to use ",
                     "this function."))
      return(NULL)
    }
  }
  
  # removing columns intended to be excluded from analysis - keeping only
  # analysis features and the response column as the training data set from
  # this point forward
  if (!is.null(data)) {
    feats <- colnames(data)
    feats <- sort(feats[!(feats %in% c(resp, exclude))])
    tdata <- data[,c(feats, resp)]
  }
  else {
    feats <- NULL
    tdata <- NULL
  }
  
  # attempting to ensure that the feature groups are properly set up
  # if there is an error, try to proceed anyway
  if (is.null(groups) & !is.null(tdata)) {
    groups <- tryCatch({
      organize(data = tdata[,feats], group_by = "names")$groups },
      error = function(cond) {
        NULL
      })
  }
  
  # pre-process the data if necessary
  if (!is.function(dtype)) if ((dtype == "SQ") |(dtype == "DSQ")) {
    # get the default parameters
    rectify_params <- as.list(fn_fmls(rectify))
    # set specific default parameters
    rectify_params$data      <- tdata
    rectify_params$exclude   <- exclude
    rectify_params$resp      <- resp
    rectify_params$groups    <- groups
    # figure out which of the default parameters has been overridden by
    # the submitted parameters
    common_params   <- names(rectify_params)[names(rectify_params)
                                              %in% names(params)]
    # overwrite any default parameters that have been supplied
    if (length(common_params) > 0)
      rectify_params[common_params] <- params[common_params]
    
    # transform the data via square rectification
    dstruct <- do.call(rectify, args = rectify_params)
    # if the data needs to be dwell time encoded, do that
    if (dtype == "DSQ") dstruct <- dwell_encode(dstruct)
    tdata   <- dstruct$data
    groups  <- dstruct$groups
    fvec    <- unique(unlist(lapply(fvec, FUN = function(x) {
      colnames(tdata)[grep(paste0(x,"$"),colnames(tdata))] })))
  }
  
  model_info <- NULL
  # loading the modeling function
  if (is.function(dtype)) {
    # if a function was supplied via the dtype parameter, use it for the
    # modeler
    modeler    <- dtype
    model_info <- find_func(modeler)
    if (is.null(model_info)) {
      message(paste0("Need to actually load the package into the",
                     "environment before it can be used as a",
                     "discriminator type."))
      return(NULL)
    }
    dtype   <- paste0(model_info$pkg,"::",model_info$func)
    mparams <- list()
    zparams <- list()
  }
  else if (dtype == "LR") {
    # use all valid columns as the default feature list if none is provided
    if (is.null(fvec)) fvec <- feats
    # setting the modeler
    modeler <- stats::glm
    # set any explicitly provided parameters
    mparams <- list(formula = paste0(resp," ~ ",
                                     paste(fvec, collapse = " + ")),
                    data = tdata)
    # these defaults can be overridden by the "params" parameter
    zparams <- list(family = "binomial")
  }
  else if ((dtype == "LRGD") | (dtype == "LS") | (dtype == "RR") |
           (dtype == "SQ") | (dtype == "DSQ")) {
    # use all valid columns as the default feature list if none is provided
    if (is.null(fvec)) fvec <- feats
    # setting the modeler
    modeler <- glmnet::glmnet
    # set any explicitly provided parameters
    mparams <- list(x = tdata[,fvec], y = as.logical(data.frame(tdata)[,resp]))
    if ((dtype == "SQ") | (dtype == "DSQ"))
      mparams <- c(mparams, list(standardize = FALSE))
    # these defaults can be overridden by the "params" parameter
    zparams <- list(family = "binomial")
    
    if (dtype == "LRGD")
      mparams$alpha <- 0
    else if ((dtype == "LS") | (dtype == "SQ"))
      mparams$alpha <- 1
    else if (dtype == "RR")
      mparams$alpha <- 2
  }
  else if (dtype == "NN") {
    # use all valid columns as the default feature list if none is provided
    if (is.null(fvec)) fvec <- feats
    # setting the modeler
    modeler <- neuralnet::neuralnet
    # set up any explicitly provided parameters or dfit specific defaults
    mparams <- list(formula = paste0(resp," ~ ", paste(fvec, collapse = " + ")),
                    data = tdata)
    # these defaults can be overridden by the "params" parameter
    zparams <- list(hidden = 3, act.fct = "logistic", linear.output = FALSE)
  }
  else if (dtype == "RF") {
    # use all valid columns as the default feature list if none is provided
    if (is.null(fvec)) fvec <- feats
    # if there are NAs, call the randomForest imputation module
    if (any(is.na(tdata[,fvec]))) {
      message("Imputing NA values in data...")
      suppressWarnings(
        idata <- rfImpute(tdata[,fvec],
                          y = as.logical(data.frame(tdata)[, resp])))
      tdata[,fvec] <- idata[,fvec]
      message("Done.")
    }
    # setting up the modeler
    modeler <- randomForest::randomForest
    # set up any explicitly provided parameters or dfit specific defaults
    mparams <- list(x = tdata[,fvec],
                    y = as.logical(data.frame(tdata)[, resp]),
                    importance = TRUE)
    # these defaults can be overridden by the "params" parameter
    zparams <- list(mtry = ceiling(sqrt(length(fvec))))
  }
  else {
    message("ERROR! Discriminator type unknown!")
    return(NULL)
  }
  
  # get the default parameters
  modeler_params <- fn_fmls(modeler)
  # tabulating which parameters actually have data in them by default
  has.data <- unlist(lapply(modeler_params, FUN = function(x) {
    is.atomic(x) | is.matrix(x) | is.data.frame(x)
  }))
  
  # override the function defaults with the the dfit provided defaults -
  # these can be overridden by the "params" parameter
  common_params   <- names(modeler_params)[names(modeler_params)
                                           %in% names(zparams)]
  # overwrite any default parameters that have been supplied
  if (length(common_params) > 0) {
    modeler_params[common_params] <- zparams[common_params]
    has.data[common_params]       <- TRUE }
  
  # figure out which of the default parameters has been overridden by
  # the submitted parameters (in the "params" parameter)
  common_params   <- names(modeler_params)[names(modeler_params)
                                           %in% names(params)]
  # overwrite any default parameters that have been supplied
  if (length(common_params) > 0) {
    modeler_params[common_params] <- params[common_params]
    has.data[common_params]       <- TRUE }
  
  # override the function defaults with the explicitly provided parameters -
  # these parameter override all of the others, and is generally only the
  # data and and response parameters
  common_params   <- names(modeler_params)[names(modeler_params)
                                           %in% names(mparams)]
  # overwrite any default parameters that have been supplied
  if (length(common_params) > 0) {
    modeler_params[common_params] <- mparams[common_params]
    has.data[common_params]       <- TRUE }
  
  # if there are unspecified arguments, setting the has.data element for that
  # element to TRUE so that it can perform its function and not be removed
  has.data[names(has.data) == "..."] <- TRUE
  # removing any default parameters that have embedded logic (and not default
  # data) to ensure that the logic properly executes - any elements that that
  # were updated by supplying something other than the default are
  # automatically retained
  modeler_params <- modeler_params[has.data]
  # if there are unspecified arguments (...) in the modeler function, include
  # the rest of the parameters as well and remove the ellipses from the
  # parameter list - even unused parameters should not generate an error in
  # this case
  if (any(names(modeler_params) == "...")) {
    modeler_params <- modeler_params[names(modeler_params) != "..."]
    param_names    <- names(modeler_params)
    modeler_params <- c(modeler_params, zparams[!(names(zparams) %in%
                                                    unique(c(param_names, names(mparams),
                                                             names(params))))])
    modeler_params <- c(modeler_params, params[!(names(params) %in%
                                                   unique(c(param_names, names(mparams))))])
    modeler_params <- c(modeler_params, mparams[!(names(mparams) %in%
                                                    param_names)])
  }
  
  # fitting the model with the updated parameters
  model <- NULL
  tries <- 0
  while ((is.null(model)) & (tries <= 10)) {
    model <- tryCatch({ suppressWarnings(
      do.call(modeler, args = modeler_params))
    }, error = function(cond) {
      if (verbose) message("Runtime error, retrying...")
      return(NULL)
    })
    tries <- tries + 1
  }
  if (is.null(model)) return(NULL)
  model$dtype <- dtype
  
  
  # perform post-processing for known model types
  if ((dtype == "LRGD") | (dtype == "LS") | (dtype == "RR") |
      (dtype == "SQ") | (dtype == "DSQ")) {
    # eliminating the iterations that had a lower lambda for a smaller,
    # more concise model object
    nbest           <- which(model$lambda == min(model$lambda))
    model$a0        <- model$a0[nbest]
    model$beta      <- matrix(model$beta[,nbest], nrow = dim(model$beta)[1],
                              dimnames = list(rownames(model$beta),
                                              colnames(model$beta)[nbest]))
    model$df        <- model$df[nbest]
    model$dim[2]    <- 1
    model$lambda    <- model$lambda[nbest]
    model$dev.ratio <- model$dev.ratio[nbest]
    
    fvec <- as.numeric(model$beta[,1])
    names(fvec) <- rownames(model$beta)
    fvec <- sort(fvec, decreasing = TRUE)
    fvec <- fvec[fvec > 0]
    fvec <- names(fvec)
  }
  
  if ((dtype == "SQ") | (dtype == "DSQ")) {
    climits <- lapply(1:length(dstruct$limits), FUN = function(i) {
      (dstruct$limits[[i]][names(dstruct$limits[[i]]) %in% fvec]) })
    climits <- climits %n% names(dstruct$limits)
    
    model$groups      <- dstruct$groups
    model$limits      <- dstruct$limits
    model$climits     <- climits
    model$sdfilter    <- modeler_params$sdfilter
    model$sensitivity <- dstruct$sensitivity
    # setting tdata back to the original data set so that the original
    # data set us stored in the "tdata" parameter of the nmaps object
    # instead of the altered square-rectified data.
    tdata <- data
  }

  if (dtype == "RF") {
    fvec <- names(sort(model$importance[,'%IncMSE'], decreasing = TRUE))
  }
  
  # if the data was not passed through the main arguments, attempt to extract
  # it from the "params" parameter - in some cases, the data will not have the
  # response column if extracted this way
  if (is.null(tdata)) {
    isdata <- unlist(lapply(modeler_params, FUN = function(x)
      (is.matrix(x) | is.data.frame(x))))
    tdata <- modeler_params[[which(isdata)[1]]]
    fvec  <- colnames(tdata)
  }
  
  # find name of modeler package and function and record in model structure - this
  # has already been done if a modeling function was supplied directly through the
  # "dtype" parameter
  if (is.null(model_info)) model_info <- find_func(modeler)
  model$pkg    <- model_info$pkg
  model$func   <- model_info$func
  if (model_info$pkg == "unpackaged") model$modeler <- modeler
  model$params <- modeler_params
  model$feats  <- fvec
  
  modobj <- list(model = model, data = data, resp = resp, feats = fvec)
  class(modobj) <- "modobj"
  
  return(modobj)
}

#' Use a data source and a model to generate a result.
#'
#' This function takes a model (this model must have a "dtype" element unless
#' the dtype is provided via the parameter) and a data set, and then uses
#' them to produce an additional column in the supplied data set named
#' "pred" (by default).
#'
#' @name modfunc
#' @author Jason Orender
#' @param model The model used to produce an output based on the input data
#'     set.  If the model was generated by the "modelfit" function, it will have
#'     an element named "dtype" which is used to identify what type of model
#'     it is.  This will be used if the "dtype" parameter is NULL (the default).
#' @param data The data set used in conjunction with the model to produce
#'     the output.
#' @param dtype The discriminator type.  This is the type of model used to
#'     fit the data.  If NULL, the function will attempt to extract this
#'     information from the model itself; however, this will only work if the
#'     model was generated using the "modelfit" function since that function
#'     records the specifics of what type of model and how it was fitted within
#'     the data structure of the model.  If neither of those inputs are
#'     available, a NULL result is returned.  This input is a two letter
#'     identifier such as "LR" for logistic regression. See the documentation
#'     for the "modelfit" function for a full list of supported model types.
#' @param result_column The name of the result column to be appended to the
#'     input data set.  If NULL, a named vector is returned that has the
#'     same number of elements as the input data set rows and in the same
#'     order.
#' @param rescale Takes a two-element vector and scales the result between
#'     the first element and the second element.  The default vector is
#'     c(0,1) to scale between zero and one.  If this paramter is NULL, no
#'     rescaling is performed.
#' @return A copy of the input data set with a set of results appended in
#'     an additional column or a vector of the results.
modfunc <- function(model, data, dtype = NULL, result_column = "pred",
                    rescale = NULL) {
  
  if (is.null(model)) return(NULL)
  # an nmaps object was submitted instead of just a model
  if (any(class(model) == "modobj")) model <- model$model
  
  if (isTRUE(model$dtype == "ENSEMBLE")) {
    if (length(model$models) == 1) {
      model <- model$models[[1]]
      dtype <- model$dtype
      warning("'Ensemble' only contains one model.  Passing model through.")
    }
    else {
      resmat <- do.call(densemble, args = c(model$models,
                                            list(devset = data, resp = NULL,
                                                 rm.na = FALSE)))
      model$dtype <- model$dtype2
      pred   <- modfunc(model, resmat, result_column = NULL)
      dtype  <- -1
    }
  }
  else if (isTRUE(model$dtype == "BOOSTED")) {
    pred <- NULL
    for (i in 1:length(model$models)) {
      result <- modfunc(model$models[[i]], data = data,
                      result_column = NULL)
      if (i == 1) pred <- result
      else        pred <- pred+result
    }
    # decision tree models can sometimes give a slightly negative
    # number or a number slightly greater than one
    pred[pred < 0] <- 0
    pred[pred > 1] <- 1
    dtype          <- -1
  }
  
  if (is.null(dtype)) dtype <- model$dtype
  if (is.null(dtype)) {
    warning(paste0("No discriminator type embedded in the model or ",
                   "otherwise supplied.\n  Assuming this is a legacy ",
                   "logistic regression (LR) model."))
    dtype <- "LR" }
  
  # checking to make sure that all required packages are installed and loaded
  # for the predefined discriminator function types
  pkg <- c("stringr", "rlang", "dplyr")
  if (!is.function(dtype)) {
    if (dtype == "LR")
      pkg <- c(pkg, "stats")
    else if ((dtype == "LRGD") | (dtype == "LS") | (dtype == "RR") |
             (dtype == "SQ"))
      pkg <- c(pkg, "glmnet")
    else if (dtype == "NN")
      pkg <- c(pkg, "neuralnet")
    else if (dtype == "RF")
      pkg <- c(pkg, "randomForest")
  }
  for (p in pkg) {
    if (!require(p, character.only = TRUE, quietly = TRUE)) {
      message(paste0("The '",p,"' package must be installed to use ",
                     "this function."))
      return(NULL)
    }
  }
  
  if      (dtype == "LR")
    mfeats <- names(model$coefficients)[-1]
  else if (dtype == "NN")
    mfeats <- model$model.list$variables
  else if ((dtype == "LS") | (dtype == "LRGD") | (dtype == "SQ") |
           (dtype == "RR"))
    mfeats <- rownames(model$beta)[model$beta[,1] != 0]
  else if (dtype == "RF")
    mfeats <- rownames(model$importance)
  else if ((dtype == "MEAN") | (dtype == "XGB"))
    mfeats <- model$feats
  else
    mfeats <- model$feats
  
  if (any(grepl("::", dtype))) {
    pkg    <- strsplit(dtype, "::", fixed = TRUE)[[1]][1]
    func   <- strsplit(dtype, "::", fixed = TRUE)[[1]][2]
    # looking to see if there is a set of example prediction parameters
    # embedded within the model
    if (!is.null(model$predict_params)) params <- model$predict_params
    else                                params <- list(genMat(nrow = 2, ncol = 2))
    isdata <- unlist(lapply(params, FUN = function(x)
      (is.matrix(x) | is.data.frame(x))))
    if (is.null(mfeats)) params[[which(isdata)[1]]] <- data
    else                 params[[which(isdata)[1]]] <- data[,mfeats]
    # if there is not a specific set of example prediction parameters,
    # assuming that just using the model as the first parameter and the
    # data as the second will work - it should work in most cases
    pred <- as.numeric(do.call(predict, args = c(list(model), params)))
  }
  else if (dtype == "LR") {
    if (!require("stats")) {
      message("The 'stats' package must be installed to use this function.")
      return(NULL)
    }
    # if this is a list of models, assume that the mean is meant to be
    # taken of the results for all listed models several legacy models
    # are constructed this way
    if (is.list(model) & is.null(model$coefficients)) {
      pred <- NULL
      n    <- 0
      for (i in 1:length(model)) {
        if (is.null(model[[i]]$supermodel)) {
          # legacy supermodels are not supported - ensembles are a
          # much better way to accomplish this task, and the
          # subordinate legacy models can be easily transformed
          # into an ensemble model object
          if (is.null(pred))
            pred <- predict(model[[i]], newdata = data,
                            type = "response")
          else
            pred <- pred + predict(model[[i]], newdata = data,
                                   type = "response")
          n <- n + 1
        }
      }
      pred <- pred / n
    }
    else
      pred <- predict(model, newdata = data, type = "response")
    names(pred) <- as.character(1:length(pred))
  }
  else if (dtype == "NN") {
    if (!require("neuralnet")) {
      message("The 'neuralnet' package must be installed to use this function.")
      return(NULL)
    }
    pred <- tryCatch({
      compute(model, data)
    },
    error = function(cond) {
      return(NA)
    })
    if (any(is.na(pred))) return(NA)
    pred <- as.numeric(pred$net.result)
    names(pred) <- as.character(1:length(pred))
  }
  else if ((dtype == "LS")  | (dtype == "LRGD") | (dtype == "SQ") |
           (dtype == "RR")) {
    if (!require("glmnet")) {
      message("The 'glmnet' package must be installed to use this function.")
      return(NULL)
    }
    
    # can only pass numeric columns to the LASSO prediction routine
    numcols <- unlist(lapply(1:dim(data)[2],
                             FUN = function(col) { is.numeric(data[,col]) }))
    
    best_lambda <- min(model$lambda)
    featnames   <- names(model$beta[,which(model$lambda == best_lambda)])
    
    if (dtype == "SQ") {
      dstruct <- rectify(data = data, resp = NULL, limits = model$limits,
                         groups = model$groups, sdfilter = model$sdfilter)
      othercols <- colnames(data)[!(colnames(data) %in% colnames(dstruct$data))]
      if (length(othercols) > 0) {
        data <- cbind(data[,othercols], dstruct$data)
        colnames(data) <- c(othercols, colnames(dstruct$data))
      }
      else data <- dstruct$data
    }
    
    if (all(featnames %in% colnames(data)) & (length(featnames) > 0)) {
      pred <- as.numeric(predict(model, newx = as.matrix(data[,featnames]),
                                 type = "response", s = best_lambda))
      names(pred) <- as.character(1:length(pred))
    }
    else return(NA)
  }
  else if (dtype == "RF") {
    
    if (!require("randomForest")) {
      message("The 'randomForest' package must be installed to use this function.")
      return(NULL) }
    fvec <- names(model$forest$ncat)
    # if there are NAs, call the randomForest imputation module
    if (any(is.na(data[,fvec]))) {
      message("Imputing NA values in data...")
      # screen out any columns that are all NAs
      allnas <- unlist(lapply(fvec, FUN = function(x) {
        all(is.na(data[,x])) }))
      data[,fvec[allnas]] <- 0
      # checking again
      if (any(is.na(data[,fvec]))) {
        yTRUE  <- rep(TRUE, dim(data)[1])
        yFALSE <- rep(FALSE, dim(data)[1])
        suppressWarnings(
          idata1 <- rfImpute(data[,fvec], yTRUE))
        suppressWarnings(
          idata2 <- rfImpute(data[,fvec], yFALSE))
        data[,fvec] <- (idata1[,fvec] + idata2[,fvec])/2
      }
      message("Done.")
    }
    
    pred <- as.numeric(predict(model, data, type = "response"))
    names(pred) <- as.character(1:length(pred))
  }
  else if (dtype == "RM") {
    # extracting the parameters
    nbins     <- model$params$nbins
    rg        <- model$range
    binsize   <- (rg[2] - rg[1])/(nbins - 1)
    conv      <- model$conv
    off       <- model$off
    conv_data <- as.numeric(data[,model$rule_variables[2]])
    conv_bins <- floor((conv_data - rg[1])/binsize)+1
    conv_bins[conv_data <= rg[1]] <- 0
    conv_bins[conv_data >= rg[2]] <- nbins-1
    off_data  <- as.numeric(data[,model$rule_variables[1]])
    off_bins  <- floor((off_data - rg[1])/binsize)+1
    off_bins[off_data <= rg[1]] <- 0
    off_bins[off_data >= rg[2]] <- nbins-1
    # using the latest time step as the basis for the prediction vector
    pred <- conv_data
    # calculating the prediction vector
    for (i in 1:nbins) {
      off_i   <- as.numeric(off[i,])
      idx_vec <- which((conv_bins+1)==i)
      pred[idx_vec] <-
        conv[i] + off_i[off_bins[idx_vec]+1]
    }
    pred[pred < rg[1]] <- rg[1]
    pred[pred > rg[2]] <- rg[2]
    names(pred) <- as.character(1:length(pred))
  }
  else if (isTRUE(model$dtype == "XGB")) {
    
    if (!require("xgboost")) {
      message("The 'xgboost' package must be installed to use this function.")
      return(NULL)
    }
    
    pred <- predict(model, as.matrix(data[,model$feature_names]))
    pred <- pred/max(pred)
    pred %idx% 1
  }
  else if ((dtype == 5) | (dtype == "MEAN")) {
    if (length(mfeats) > 1) pred <- (rowMeans(data[,mfeats], na.rm = TRUE))
    else                    pred <- data[,mfeats]
    pred %idx% 1
  }
  else if (dtype != -1)
  { message("ERROR! Model type not supported"); return(NA) }
  
  if (!is.null(rescale)) {
    pred <- (pred - min(pred[!is.na(pred)])) *
      (rescale[2] - rescale[1]) /
      (max(pred[!is.na(pred)]) - min(pred[!is.na(pred)]) + 1E-10) +
      rescale[1]
  }
  
  if (is.null(result_column)) return(pred)
  else {
    data <- cbind(data, pred)
    if (result_column != "pred") {
      cnames <- colnames(data)
      cnames[cnames == "pred"] <- result_column
      colnames(data) <- cnames
    }
    return(data)
  }
}

#' Creates a "frozen" model object
#'
#' This function calculates the results of running a data set through a model
#' and then saves the results in an object that can be plotted or queried for
#' statistics concerning the results.
#'
#'
#' @name modfrz
#' @author Jason Orender
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
#' @param display Draws the confusion matrix on the console.
#' @param rescale Rescale the results to be within the scale provided. The
#'     default is zero to one.
#' @param verbose Display warnings and informational messages.
#' @return If the "return_table" parameter is true, this function returns
#'     a matrix of values that represent the confusion matrix entries.
modfrz <- function(model = NULL, data = NULL, pred = NULL, resp = NULL,
                        thresh = NULL, display = FALSE, rescale = c(0,1),
                        verbose = TRUE) {
  
  # If data is not passed, attempting to extract the training data from the
  # model object.  Using the last object in the model list since the first
  # one might be a layer 2 model.
  if (is.null(data) & (class(model) == "modobj"))  data <- model$data
  else if (is.null(data)) {
    message("ERROR! No data set provided.")
    return(NULL) }
  
  # if an modobj object was supplied, scale it down to a simple model
  if (class(model) == "modobj") mod <- model$model
  else                          mod <- model
  # if a response vector name was not supplied, use the one embedded
  if (is.null(resp)) resp <- model$resp
  
  if (is.null(pred)) {
    if (verbose) message("Calculating predictions...")
    res <- modfunc(mod, data = data, rescale = rescale, result_column = NULL)
    # counting NA results as FALSE by definition - if another treatment is
    # needed, get the predictions by calling modfunc separately and submitting
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
    ret <- list()
    ret$observed <- conf
    ret$thr      <- 0.5
    ret$p.value  <- 1.0
    return(ret)
  }
  
  tconf <- matrix(c(sprintf("%d / %.1f%%", c(TP, FN),
                            100*c(TP, FN)/(TP+FN)),
                    sprintf("%d / %.1f%%", c(FP, TN),
                            100*c(FP, TN)/(FP+TN))),
                  nrow = 2,
                  dimnames = list(c("Pred TRUE", "Pred FALSE"),
                                  c("Actual TRUE", "Actual FALSE")))
  
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
  class(conf) <- "modobj.frz"

  if (display) {
    message(text_table(tconf, forms = c("%s","%s")))
    message(sprintf("P-value: %.4e",conf$p.value)) }
  
  return(conf)
}

#' Plot an modobj.frz object, which contains all of the results data from a
#' prediction as well as evaluation data.
#'
#' This function takes in an modobj.frz object, which contains results data
#' from application of a model to a full data set, as well as the evaluation
#' data using the Pearson's Chi-Square test.
#'
#' This function is embeddable within a modobj or modobj.frz object.
#'
#' @name plot_frz
#' @param frz The modobj.frz object that contains the results data to be
#'     plotted.
#' @param h The threshold to apply to the results data for calculation of the
#'     confusion matrix. If set to NULL, the optimal threshold stored within
#'     the modobj.frz object will be used; if there is not one, it will simply
#'     be set to 0.5.  To eliminate the confusion matrix and the horizontal
#'     line, set the "h" parameter to zero.
#' @param v The vertical line position.  If set to NULL, the vertical line is
#'     drawn where the threshold ("h" parameter) crosses the monotonically
#'     increasing sorted results.  To eliminate the vertical line, set the "v"
#'     parameter to zero.
#' @param title The title of the plot.
#' @param xlab The x-axis label on the plot.
#' @param ylab The y-axis label on the plot.
#' @param cx The x-location of the confusion matrix, with zero being the left
#'     hand side, and one being the right hand side.
#' @param cy The y-location of the confusion matrix, with zero being the
#'     bottom, and one being the top.
#' @param cols The colors of the data points.
#' @param plot_confusion Include the confusion matrix on the plot. This is
#'     included by default.
#' @export
plot_frz <- function(frz, h = NULL, v = 0, title = NULL,
                     xlab = "Sorted Examples", ylab = "Model Response",
                     cx = 0.4, cy = 0.3, cols = NULL,
                     plot_confusion = TRUE) {
  
  # making sure all of the necessary packages are loaded - this allows
  # embedding the code into the object instead of assuming that the
  # package dependencies will automatically be loaded when the package
  # is loaded
  if (!require(gridExtra, quietly = TRUE)) {
    warning(paste0("The 'gridExtra' package must be installed to plot ",
                   "the confusion matrix. Skipping confusion matrix",
                   "plotting."))
    plot_confusion <- FALSE }
  
  if (!require(grid, quietly = TRUE)) {
    warning(paste0("The 'grid' package must be installed to plot ",
                   "the confusion matrix. Skipping confusion matrix",
                   "plotting."))
    plot_confusion <- FALSE }
  
  if (!require(dplyr, quietly = TRUE)) {
    warning(paste0("The 'dplyr' package must be installed to auto-title ",
                   "the plot. Using a generic title."))
    plot_confusion <- FALSE }
  
  if (!require(stringr, quietly = TRUE)) {
    warning(paste0("The 'stringr' package must be installed to auto-title ",
                   "the plot. Using a generic title."))
    plot_confusion <- FALSE }
  
  if (!(class(frz) == "modobj.frz")) {
    message("ERROR! This function requires an 'modobj.frz' object.")
    return(NULL) }
  
  if (is.null(title)) {
    # use the original name of the "modobj.frz" parameter as the title
    # if one is not provided, and strip out special characters
    tryCatch({
      title <- as.list(match.call())$frz %>% as.character
      if (title[1] == "$") {
        title <- str_replace_all(title[2],"`","") %>%
          str_replace_all("_", " ") }
      else {
        title <- str_replace_all(title[1],"`","") %>%
          str_replace_all("_", " ") }
    }, error = function(cond) {
      # if there is an error, send a warning to the console and default
      # to a generic title
      message(sprintf("Warning: %s",cond))
      title <<- "Distribution of Results"
    })
  }
  
  pred <- frz$pred
  pred %idx% 1
  pred <- sort(pred)
  
  if (!is.null(frz$resp))
    resp <- as.logical(frz$resp[,1])[as.numeric(names(pred))]
  else
    resp <- rep(FALSE, length(pred))
  
  if (is.null(cols))  cols <- resp%?%L(rgb(1,0,0), rgb(0,0,1,0.2))
  
  par(mar = c(5, 5, 3, 3))
  plot(1:length(pred), pred, xlab = xlab, ylab = ylab, pch = 20,
       main = title, col = cols, ylim = c(0,1))
  
  # plotting the horizontal (threshold) line
  if (!is.null(frz$thr) & is.null(h)) h <- frz$thr
  else if (is.null(h))                h <- 0.5
  if (h != 0) {
    abline(h = h, lw = 2, lt = 2, col = "red")
    legend(1, h + 0.065, sprintf("%.2f",h),
           box.col = "white", bg = "white", text.col = "red")
  }
  
  # plotting the vertical line
  if ((is.null(v)) & (h != 0)) {
    iclosest <- which.min(abs(pred - h))
    dclosest <- pred[iclosest] - h
    v = iclosest - dclosest/(pred[iclosest] -
                             pred[iclosest - sign(dclosest)])
  }
  if (v != 0) {
    offset <- length(pred) * 0.05
    abline(v = v, lw = 2, lt = 2, col = "purple")
    legend(v - offset, 0.2, sprintf("%.0f",v), box.col = "white",
           bg = "white", adj = 0.2, text.col = "purple")
  }
  
  # plotting the confusion matrix
  pval <- NULL
  if ((sum(resp) > 0) & plot_confusion) {
    predct  <- pred > h
    correct <- as.logical(resp)
    TP = sum(predct & correct)              # True/Positives
    TN = sum(!predct & !correct)            # True/Negatives
    FP = sum(xor(predct,correct) & predct)  # False/Positives
    FN = sum(xor(predct,correct) & !predct) # False/Negatives
    
    tconf <- matrix(c(sprintf("%d / %.1f%%", c(TP, FN),
                              100*c(TP, FN)/(TP+FN)),
                      sprintf("%d / %.1f%%", c(FP, TN),
                              100*c(FP, TN)/(FP+TN))),
                    nrow = 2,
                    dimnames = list(c("Pred TRUE", "Pred FALSE"),
                                    c("Actual TRUE", "Actual FALSE")))
    conf  <- matrix(c(TP,FN,FP,TN), nrow = 2,
                    dimnames = list(c("Pred TRUE", "Pred FALSE"),
                                    c("Actual TRUE", "Actual FALSE")))
    
    suppressWarnings(new_chisq <- chisq.test(conf))
    pval <- ifelse(is.na(new_chisq$p.value), 1.0, new_chisq$p.value)
    
    pushViewport(viewport(x = cx, y = cy))
    grid.table(tconf)
  }
  
  # annotating the p-value with the updated threshold
  if (!is.null(pval)) {
    pcolor <- ifelse(pval > 0.05, "red", "black")
    text(length(pred),0.05, sprintf("P-value: %.3e", pval), pos = 2, col = pcolor)
  }
}


#' Generate a matrix based on input value(s) and a transform function.
#'
#' This function generates a matrix based on an input value or a vector of
#' input values coupled with a transform function.  The default transform
#' function is simply to repeat the input values in every cell.  Using all
#' default values for the parameters will produce a 10 x 10 matrix filled
#' with zeros.
#'
#' @name genMat
#' @param x A value or vector of values to use as input to the transform
#'     function.  The default is single zero.  If the 'x' vector length exceeds
#'     the number of elements in the matrix (nrow x ncol), the vector will be
#'     truncated.  If the number of elements is not an even multiple of the 'x'
#'     vector length, an error will result and a NULL will be returned.
#' @param nrow The number of rows in the matrix.
#' @param ncol The number of columns in the matrix.
#' @param dimnames A list of two vectors or a single vector.  The first vector
#'     in the list contains the names of the rows and the second vector contains
#'     the names of the columns, while if a vector this is interpreted as just
#'     names of the columns (the rows will be simply numbered from one to nrow).
#'     If the matrix is intended to be converted to a data frame at some point,
#'     ensure that the column names are data frame compliant; this primarily
#'     means that the column names cannot start with a number or a dot (".").
#' @param XFUN The transform function.  This function is applied to the
#'     value(s) supplied in the 'x' parameter to fill the matrix.  The inputs
#'     must always be i (the row), j (the column), and x (the value or vector
#'     of values supplied as input).  The return value of XFUN must be a single
#'     scalar value.  If a more complex set of inputs is desired, use the 'x'
#'     parameter as an index to a list of those complex inputs and refer to
#'     the list as an out of scope variable.
#' @param axis The direction that the 'x' parameter will fill the matrix. The
#'     default (2, or 'col') will fill the matrix down each column before
#'     proceeding to the next.  The alternate value (1 or 'row') will fill the
#'     matrix across each row before proceeding to the next.  If the 'x'
#'     parameter is a single scalar value, this parameter has no impact.
#' @return A matrix containing the value(s) in the 'x' parameter when applied
#'     the 'XFUN' function.
#' @export
genMat <- function(x = 0, nrow = 10, ncol = 10, dimnames = NULL,
                   XFUN = function(i, j, x) { x }, axis = 2) {
  
  # checking to ensure that the supplied function meets requirements if using
  # transform function
  args <- names(formals(XFUN))
  if (!(all(c("i", "j", "x") %in% args) & all(args %in% c("i", "j", "x")))) {
    message("ERROR! The parameters for the supplied function do not meet")
    message("       the specified requirements.  The supplied XFUN function")
    message("       must have exactly three parameters: 'i', 'j', and 'x'")
    message("       only.  No more and no less.  Note that those parameter")
    message("       names are case-sensitive.")
    return(NULL)
  }
  
  # if the x vector is too long, truncate
  if (length(x) >= (nrow*ncol)) x <- x[1:(nrow*ncol)]
  # check to make sure that the length of 'x' divides evenly into the number of
  # cells desired
  if (((ncol*nrow)%%length(x)) != 0) {
    message("ERROR! 'x' is not an integer multiple of the number of cells")
    message("        in the desired matrix.")
    return(NULL)
  }
  
  # the default dimension names
  if (is.null(dimnames)) dimnames <- list(1:nrow, paste0("X", 1:ncol))
  # replicate x as many times as necessary to fill the rows and columns
  xvec <- rep(x, ncol*nrow%/%length(x))
  
  if      ((axis == 2) | (axis == 'col')) {
    ivec <- rep(1:nrow, ncol)
    jvec <- unlist(lapply(1:ncol, FUN =
                            function(j) { rep(j, nrow) }))
  }
  else if ((axis == 1) | (axis == 'row')) {
    ivec <- unlist(lapply(1:nrow, FUN =
                            function(j) { rep(j, ncol) }))
    jvec <- rep(1:ncol, nrow)
  }
  xfrm <- unlist(lapply(1:(nrow*ncol), FUN =
                          function(k) { XFUN(ivec[k], jvec[k], xvec[k])}))
  
  # checking dimnames to determine whether a list or vector was provided
  # and making adjustments if only a vector of column names was provided
  if (!is.list(dimnames)) dimnames <- list(1:nrow, dimnames)
  
  # the final matrix
  retmat <- matrix(xfrm, nrow = nrow, dimnames = dimnames,
                   byrow = ((axis == 1) | (axis == 'row')))
  
  return(retmat)
}


#' Create a text formatted table view of a data frame
#'
#' This function takes in a slice of a data frame and returns one text
#' string that renders into a formatted table.
#'
#' @name text_table
#' @author Jason Orender
#' @param df The data frame to render.
#' @param forms The formats of each of the columns.  This is a valid
#'     character vector sprintf formats (e.g. "%5.3f" is a floating point
#'     number that is 5 characters wide and 3 decimal places).
#' @param sep The characters used to separate the columns.
#' @param use_rownames Whether to use the rownames when drawing the table.
#' @return A text string with the formatted table.
text_table <- function(df, forms, sep = "   ", use_rownames = TRUE) {
  ncols <- dim(df)[2]
  nrows <- dim(df)[1]
  
  # takes a vector strings and tells how many characters the largest one is
  maxchar <- function(x)
  { rev(sort(unlist(lapply(x, FUN = function(l) { nchar(l) }))))[1] }
  
  # pad a string to n spaces
  pad <- function(x, n)
    if (n>nchar(x)) paste(c(x, rep(" ",n-nchar(x))), collapse = "") else x
  
  cnames <- colnames(df)
  rnames <- rownames(df)
  if (use_rownames) rnsize <- maxchar(rnames)
  else              rnsize <- 0
  
  # convert the columns to text
  text_cols <- list()
  csizes    <- NULL
  for (j in 1:ncols) {
    text_cols[[j]] <- c(cnames[j], sprintf(forms[j], df[,j]))
    csizes <- c(csizes, maxchar(text_cols[[j]]))
  }
  # adjust column names according to max column characters
  cnames <- unlist(lapply(1:ncols, FUN = function(i) {
    center(cnames[i], csizes[i]) }))
  
  # making the header
  if (use_rownames)
    header <- paste(c(paste(rep(" ", rnsize), collapse = ""), cnames),
                    collapse = sep)
  else
    header <- paste(cnames, collapse = sep)
  
  # making the horizontal dividers
  double_divider <- paste(rep("=", nchar(header)), collapse = "")
  single_divider <- paste(rep("-", nchar(header)), collapse = "")
  
  # constructing each data line of the table
  lines <- list()
  for (i in 1:nrows) {
    line <- unlist(lapply(1:ncols, FUN = function(j) {
      center(text_cols[[j]][i+1], csizes[j])
    }))
    if (use_rownames) line <- c(pad(rnames[i],rnsize), line)
    line <- paste(line, collapse = sep)
    lines[[i]] <- line
  }
  
  # creating the table string
  tt <- paste(c(double_divider, header, double_divider, unlist(lines),
                single_divider), collapse = "\n")
  
  return(tt)
}


#' Extract the optimal number of unique regex patterns that describe a
#' character vector.
#'
#' This function takes a character vector as input and then creates a
#' set of regular expressions that describe the optimal grouping of that
#' character vector.  This can be used to group longitudinal feature sets
#' together in which the only differentating character is a number or code
#' indicating the time point of the reading.  Note that the function gives
#' preference to suffixes that begin with a dot ("."), though this is not
#' a requirement, as is having a suffix at all (like ending all of a
#' certain type of feature in ".Z" for instance).
#'
#' @name extract_patterns
#' @author Jason Orender
#' @param cvec The character vector to analyze.
#' @param elim_numerics Eliminates any suffixes with numeric values from
#'    consideration.  This can improve the reliability of the pattern
#'    extraction algorithm.
#' @return The set of regular expressions that optimally describe the
#'    input character vector.
extract_patterns <- function(cvec, elim_numerics = TRUE) {
  # determining the common suffixes first
  epat <- data.frame(PATTERN = "X", COUNT = 0)
  for (s in cvec) {
    n       <- nchar(s)
    newline <- NULL
    for (i in n:1) {
      cstring <- str_replace(substr(s, i, n), "[.]", "\\\\.")
      fvec    <- grep(paste0(cstring,"$"), cvec)
      newline <- data.frame(PATTERN = cstring, COUNT = length(fvec))
      if (newline$COUNT > 1) epat <- rbind(epat, newline)
    }
  }
  
  # determining unique candidate suffixes in order of frequency
  epat <- unique(epat[-1,])
  csuf <- epat$COUNT
  names(csuf) <- as.character(epat$PATTERN)
  csuf <- names(sort(csuf, decreasing = TRUE))
  # add a null suffix just to ensure that the trivial solution is checked
  csuf <- c("", csuf)
  if (elim_numerics) csuf <- csuf[-grep("[0-9]", csuf)]
  
  # removing the items described by the patterns one-by-one until all strings are accounted for
  regex_vec <- NULL
  while (length(cvec) > 0) {
    
    # determining the optimal prefixes for each candidate suffix
    pats <- data.frame(PREFIX = "X", SUFFIX = "X", COUNT = 0, stringsAsFactors = FALSE)
    for (p in csuf) {
      for (s in cvec) {
        n       <- nchar(s)
        newline <- NULL
        for (i in 1:n) {
          oldline <- newline
          cstring <- str_replace(substr(s, 1, i), "[.]", "\\\\.")
          fvec <- grep(paste0("^",cstring,".*",p,"$"), cvec)
          newline <- data.frame(PREFIX = cstring, SUFFIX = p, COUNT = length(fvec),
                                stringsAsFactors = FALSE)
          if ((newline$COUNT == 1) & (!is.null(oldline))) {
            if (oldline$COUNT > 1) pats <- rbind(pats, oldline)
            break
          }
        }
      }
    }
    
    # calculating a score for each set of patterns based on minimizing the number of
    # non-pattern characters in the middle of the prefix and suffix and maximizing
    # the discriminatory capability of the pattern
    pats <- pats[-1,]
    pats <- unique(pats)
    pats$SCORE <- 0
    for (i in 1:dim(pats)[1]) {
      fvec <- grep(paste0("^", pats$PREFIX[i], ".*", pats$SUFFIX[i], "$"), cvec)
      svec <- cvec[fvec]
      svec <- str_replace(svec, "[.]", "\\\\.")
      plen <- nchar(pats$PREFIX[i]) + nchar(pats$SUFFIX[i])
      # since suffixes frequently begin with the "." character, giving a bonus
      # for finding this character at the beginning of the suffix
      if (substr(pats$SUFFIX[i],1,2) == "\\.") bonus <- 1
      else                                     bonus <- 0
      pats$SCORE[i] <- pats$COUNT[i] - mean(nchar(svec) - plen) + bonus
    }
    pats <- arrange(pats, -SCORE)
    
    fvec <- grep(paste0("^", pats$PREFIX[1], ".*", pats$SUFFIX[1], "$"), cvec)
    if (length(fvec) > 0) {
      regex_vec <- c(regex_vec, paste0("^", pats$PREFIX[1], ".*", pats$SUFFIX[1], "$"))
      cvec <- cvec[-fvec]
    }
  }
  
  return(sort(regex_vec))
}

#' Organize a data set by correlation or by the patterns of the column names.
#'
#' This function takes in a data set and places the most correlated columns
#' next to each other.
#'
#' @name organize
#' @author Jason Orender
#' @param data The data set to organize.
#' @param group_by The type of grouping to do.  This can be either "correlation"
#'    or "names" (the default).
#' @param exclude The columns to exclude from this algorithm.  In the final
#'    data set returned, these will be the first columns and their order
#'    will remain in the same relative position that they were at the
#'    beginning.  This parameter can include column names that this data set
#'    does not have without creating an error.  The response column should be
#'    included in this vector.
#' @return A list, the first element of which is the organized data set and the
#'    second is a list of the groups that the algorithm found.
organize <- function(data, exclude = c("X", "INDC"), group_by = "names") {
  
  feats <- colnames(data)
  feats <- feats[!(feats %in% exclude)]
  
  data_sort <- data[,feats]
  
  follow_cluster <- function(col, corr, hist = NULL) {
    corr[, hist] <- 0
    corr[hist, ] <- 0
    hist  <- c(hist, col)
    cands <- corr[, col]
    cands <- names(rev(sort(cands[cands > 0.5])))
    if (length(cands) > 0) {
      ret  <- follow_cluster(cands[1], corr, hist) # <- recursive call
      hist <- ret[[1]]
      corr <- ret[[2]]
    }
    return(list(hist, corr))
  }
  
  pats <- NULL
  if (group_by == "correlation") {
    # calculating the correlation matrix
    corr_matrix <- cor(data_sort)
    # setting the diagonals to zero
    corr_matrix[corr_matrix == 1.0] <- 0
    # sorting the features by correlation group
    groups <- list()
    k      <- 0
    while (sum(corr_matrix) != 0) {
      nextcols <- colnames(corr_matrix)
      if (length(nextcols) > 0) {
        ret <- follow_cluster(nextcols[1], corr_matrix)
        k           <- k + 1
        groups[[k]] <- ret[[1]]
        cnames      <- colnames(ret[[2]])
        colsleft    <- cnames[!(cnames %in% ret[[1]])]
        if (length(colsleft) == 0) break
        corr_matrix <- ret[[2]][colsleft, colsleft]
      }
    }
    
    # consolidating any groups with two or less into a single group
    remove <- NULL
    replac <- NULL
    for (i in 1:length(groups)) {
      if (length(groups[[i]]) <= 2) {
        remove %&=% i
        replac %&=% groups[[i]]
      }
    }
    if (!is.null(remove)) {
      groups <- groups[-remove]
      groups %&=% replac
    }
    
    names(groups) <- paste0("GROUP", 1:length(groups))
  }
  else if (group_by == "names") {
    pats   <- extract_patterns(feats)
    groups <- list()
    for (p in pats)
      groups <- c(groups, list(feats[grepl(p, feats)]))
    
    # creating names based off of the extracted patterns that are compatible
    # with easy list referencing
    pnames <- str_replace(
      str_replace(
        str_replace(
          str_replace(
            str_replace(pats, "[.]", ""),
            "[*]", ""),
          "\\^", ""),
        "\\\\", ""),
      "\\$", "")
    
    # renaming the group elements
    names(groups) <- pnames
    
    # Some patterns can return multiple groupings if the only thing
    # differentiating is a suffix, as in "DOPT2" vs. "DOPT2.D". Checking
    # for this and ensuring that all groups are completely distinct.
    contains_another <- lapply(1:length(groups), FUN =
                                 function(i) unique(c(i,groups%search%groups[[i]])))
    # a list of vectors is produced - the first element of the vector
    # is the group being interrogated and the remaining elements are the
    # other groups that have all of their elements contained within this
    # one
    for (i in contains_another) {
      if (length(i) > 1) # if this group is replicated within another
        for (j in i[2:length(i)]) # iterate through the other groups
          # and remove any duplicated elements in the other groups
          groups[[j]] <- groups[[j]][!(groups[[j]] %in%
                                         groups[[i[1]]])]
    }
    
  }
  else return(NULL)
  
  feats     <- unlist(groups)
  data_sort <- data_sort[,feats]
  data_return <- cbind(data[,(colnames(data) %in% exclude)], data_sort)
  colnames(data_return) <- c(colnames(data)[(colnames(data) %in% exclude)],
                             colnames(data_sort))
  
  return(list(data = data_return, groups = groups, patterns = pats))
}

#' Find out the package and name for a function.
#'
#' Given a reference to a function, find out it's name and the package to
#' which it belongs.  This function creates a hash table of the all functions
#' currently loaded and then compares the hash of the provided function with
#' the hash of those loaded to find a match.  This method works even if the
#' function was assigned to another variable.  Note that it will only look
#' in the global environment for a match to find unpackaged functions.
#'
#' @name find_func
#' @author Jason Orender
#' @param func A function to find the name of.
#' @return A character vector with the named elements having the names of the
#'     package of the function (if any) and the name of the function.
find_func <- function(func) {
  
  if (!require(openssl)) {
    message("Package dependency 'openssl' not installed.")
    return(NULL) }
  
  if (!require(rlang)) {
    message("Package dependency 'rlang' not installed.")
    return(NULL) }
  
  # small utility function to hash the function source
  fhash <- function(func) {
    if (is.character(func)) {
      if (any(grepl("::", func))) {
        p <- strsplit(func, "::", fixed = TRUE)[[1]][1]
        f <- strsplit(func, "::", fixed = TRUE)[[1]][2]
        func <- eval(substitute(a::b, list(a = sym(p), b = sym(f))))
      }
      else func <- eval(sym(func))
      func <- tryCatch(func,
                       error = function(cond) NULL)
    }
    fsource <- paste(deparse(func), collapse = "")
    return(sha256(fsource))
  }
  
  pkgs <- (.packages())
  # get functions
  pkg_funcs <- lapply(pkgs, FUN = function(x)
    ls(paste0("package:",x)))
  names(pkg_funcs) <- pkgs
  pkg_funcs$unpackaged = as.character(lsf.str(envir = .GlobalEnv))
  
  # find hashes
  hashes     <- NULL
  pkg_hashes <- list()
  for (p in names(pkg_funcs)) {
    for (f in pkg_funcs[[p]]) {
      if (p == "unpackaged") h = fhash(f)
      else  {
        h = tryCatch(fhash(paste0(p,"::",f)),
                     error = function(cond) return(NULL))
        if (is.null(h))
          h = tryCatch(fhash(paste0(p,"::`",f,"`")),
                       error = function(cond) return(NULL))
      }
      hashes <- c(hashes, h)
      pkg_hashes <- c(pkg_hashes, list(list(pkg = p, func = f)))
    }
  }
  names(pkg_hashes) <- hashes
  
  # finding a match
  func_hash <- fhash(func)
  
  if (any(func_hash == hashes))
    func_return <- pkg_hashes[[which(hashes == func_hash)[1]]]
  else
    func_return <- NULL
  
  return(func_return)
}


#' Search a list or vector for an entry that contains the entries of another
#' list or vector and return the index of all elements that satisfy the
#' condition.  All of the entries from y must be contained within an element
#' of x in order for an index to be returned.
#' @author Jason Orender
setGeneric("%search%", function(x,y) {
  contains <- NULL
  for (i in 1:length(x))
    if (all(y %in% x[[i]])) contains%&=%i
  return(contains)
})

#' Increment operator
#' @author Jason Orender
setGeneric("%+=%", function(x, y) {
  if (is.character(x) & is.character(y)) {
    eval.parent(substitute(x<-paste0(x,y)))
    ans <- paste0(x,y) }
  else if (is.numeric(x) & is.numeric(y)) {
    eval.parent(substitute(x<-x+y))
    ans <- x+y }
  else {
    warning(paste0("May only use %+=% on character and numeric atomics. ",
                   "Use %&=% for vectors and lists. Returning LHS."))
    ans <- x
  }
  return(ans) })

#' CONCATENATION operator concatenates two vectors or lists
#' @author Jason Orender
setGeneric("%&=%", function(x, y) {
  if (is.list(x) & !is.list(y)) {
    eval.parent(substitute(x<-c(x,list(y))))
    ans <- c(x,list(y)) }
  else {
    eval.parent(substitute(x<-c(x,y)))
    ans <- c(x,y) }
  return(ans) })

# Return a vector with new names specified by the second argument - the new
# names will be repeated or truncated if the names vector is not the correct
# length.
#' @author Jason Orender
setGeneric("%n%", function(x, y) {
  if (is.numeric(y) & (length(y) == 1))
    names(x) <- as.character(y:(length(x)+y-1))
  else if (length(x) > 0)
    names(x) <- rep(y, length(x)%/%length(y)+1)[1:length(x)]
  return(x) })

# TRINARY OPERATOR - The base R "ifelse" statement is limited in that it can
# only return a single scalar value.  By defining a custom trinary operator the
# return value can be set as any object, and the input condition can actually
# be a vector of logicals).  The first argument is a logical condition
# (e.g. "n == 1"), and the second argument is a list where the first element
# is returned if the logical condition is true, and the second element is
# returned if the condition is false.  If the custom list creation shortcut
# is also used, the trinary operator would look something like this in
# practice:  "r <- (n > 5)%?%L(0, 1:100)".  In this case n is evaluated for
# being greater than the value of 5; if TRUE, a scalar of zero is returned,
# while if FALSE, a vector of integers from one to one-hundred is returned
# and stored in the variable "r". This would not be possible with the built-
# in R "ifelse" statement (it would just return a "1" if the expression was
# evaluated as FALSE, rather than the entire vector).

#' @author Jason Orender
setGeneric("%?%", function(condition, result_list) {
  unlist(lapply(1:length(condition), FUN = function(i) {
    if (condition[i]) return(result_list[[1]])
    else   return(result_list[[2]]) }))
})

# CUSTOM LIST ALIAS FUNCTION is a substitution for the R standard "list"
# function that transforms a set of arguments to a list - This is done so often
# that having a single letter substituion like this can be more convenient and
# make code appear more streamlined.

#' @author Jason Orender
L <- function(...) { list(...) }

#' Center a string to a specified width by adding padding.
#'
#' This function takes in a string as input and adds padding (spaces generally)
#' on the beginning and end in order to center it in a specifed field width.
#' This function is abbreviated with the "%c%" infix operator.
#'
#' @name center
#' @author Jason Orender
#' @param instring The string to center.
#' @param width The width of the field to center the string in.
#' @param pad The padding character to use.
#' @return A string padded to "width" with the supplied string centered in the
#'     middle.
#' @export
center <- function(instring, width, pad = " ") {
  swidth  <- nchar(instring)
  if (swidth >= width) return(instring) # nothing to do
  nbefore <- (width - swidth)%/%2
  nafter  <- width - (nbefore + swidth)
  return(paste(c(rep(" ", nbefore), instring, rep(" ", nafter)), collapse = ""))
}

# the summary function
#' @author Jason Orender
summary.modobj <- function(modobj) {
  
  print_confusion <- function(conf, tx) {
    cat(sprintf("\nModel Confusion Matrix (%s):\n", tx))
    cat("    +================================================+\n")
    cat("    |           |               Actual               |\n")
    cat("    +-----------+------------------+-----------------+\n")
    cat("    | Predicted |       True       |      False      |\n")
    cat("    +-----------+------------------+-----------------+\n")
    cat(sprintf("    |  True     |  %14s  | %14s  |\n",
                conf[1,1]%c%14, conf[1,2]%c%14))
    cat("    +-----------+------------------+-----------------+\n")
    cat(sprintf("    |  False    |  %14s  | %14s  |\n",
                conf[2,1]%c%14, conf[2,2]%c%14))
    cat("    +================================================+\n")
    cat("    ")
  }
  
  if (!is.null(modobj$tconf)) print_confusion(modobj$tconf,
                                                 "Main Set")
  if (!is.null(modobj$h)) {
    cat(sprintf("Threshold = %.2f\n    ", modobj$h))
  }
  if (!is.null(modobj$dconf)) print_confusion(modobj$dconf,
                                                 "Additional Set")
  if (!is.null(modobj$auc)) {
    cat(sprintf("AUC       = %.2f\n", modobj$auc))
  }
  
  cat("\n")
  
  cat("Data set summary statistics:\n")
  cat(sprintf("#Examples = %6.0-f     ", dim(modobj$data)[1]))
  if (!is.null(modobj$resp)) {
    pos_ex <- sum(modobj$data[,modobj$resp])
    cat(sprintf("#Positive = %6.0-f", pos_ex))
    cat(sprintf(" (%.2f%%)\n", 100*pos_ex/dim(modobj$data)[1]))
  }
  else
    cat("\n")
  
  # cat("\nThe most reliable features driving this model are:\n")
  # cat(paste(nmaps_obj$feats, collapse = ", "))
  # cat("\n")
}

#' @author Jason Orender
summary.modobj.frz <- function(modobj.frz) {
  message(text_table(modobj.frz$tconf, forms = c("%s","%s")))
}

# INDEX A VECTOR OR LIST return a vector with new names that are numerical
# indices prepended by a character string if provided or starting at a given
# numerical index (the data type of the second argument will determine the
# behavior).  The second argument may also be  null or an empty string.  The
# original LHS variable is updated in in addition to returning the vector with
# the updated names. For instance:
# a %idx% 1
#
# will rename a with indices from 1 to length(a) and store the result in a,
# while:
# b <- a %idx% 1
#
# will rename a, store the result in a, and then also store the result in b
#
#' @author Jason Orender
setGeneric("%idx%", function(x, y) {
  
  ans <- x
  if (is.numeric(y)) {
    eval.parent(substitute(names(x) <- as.character(y:(length(x)+y-1))))
    names(ans) <- as.character(y:(length(x)+y-1))
  }
  else if (is.character(y)) {
    eval.parent(substitute(names(x) <- paste0(y, 1:length(x))))
    names(ans) <- paste0(y, 1:length(x))
  }
  else if (is.null(y)) {
    eval.parent(substitute(names(x) <- as.character(1:length(x))))
    names(ans) <- as.character(1:length(x))
  }
  
  return(ans)
})


# the print function
#' @author Jason Orender
print.modobj <- function(modobj) { summary(modobj) }

#' @author Jason Orender
print.modobj.frz <- function(modobj.frz) { summary(modobj.frz) }

# Setting up a custom operator to make the following functions more concise.
# This custom operator takes a variable on the lhs and a list object on the
# rhs: if there is a correspondingly named element in the list and the variable
# has a value of null, the operator will substutute that value for NULL in the
# original *parent* environment and then return a TRUE.  Otherwise it will
# return the original value passed, even if that value is NULL.
#' @export
setGeneric("%O?%", function(x, y) {
  args <- as.list(match.call())
  elem <- paste0(args$y, "$", args$x)
  
  # evaluating the identically named element of the object in "y"
  obj_value <- eval.parent(parse(text = elem))
  
  # if the override value is NULL, substitute with the object's value for the
  # named element if it exists
  if (is.null(x) & !is.null(obj_value)) {
    eval.parent(substitute(x <- obj_value))
    return(TRUE)
  }
  
  return(x)
})

# the generic plot function for modobj.frz objects
#' @author Jason Orender
plot.modobj.frz <- function(frz_obj, h = NULL, v = 0,
                            title = "Distribution of Results",
                            xlab = "Sorted Examples", ylab = "Model Response",
                            cx = 0.4, cy = 0.3, cols = NULL,
                            rescale = c(0,1)) {
  
  plot_frz(frz = frz_obj, h = h, v = v, title = title,
           xlab = xlab, ylab = ylab, cx = cx, cy = cy,
           cols = cols)
  
}



# the generic plot function for modobj objects
#' @author Jason Orender
plot.modobj <- function(modobj, data = NULL, h = 0.5,  v = 0,
                        title = "Distribution of Results",
                        xlab = "Sorted Examples", ylab = "Model Result",
                        cx = 0.3, cy = 0.4, resp = NULL, rescale = c(0,1),
                        cols = NULL) {
  
  if (is.null(data)) data <- modobj$data
  frz_obj <- modfrz(modobj, data = data, rescale = rescale, resp = resp,
                    verbose = FALSE)
  plot_frz(frz = frz_obj, h = h, v = v, title = title,
           xlab = xlab, ylab = ylab, cx = cx, cy = cy,
           cols = cols)
  
}

# the generic prediciton function
#' @author Jason Orender
predict.modobj <- function(modobj, data = NULL) {
  # use a new data set if it is supplied, otherwise use embedded data
  if (is.null(data %O?% modobj))  return(NULL)
  return(modfunc(model = modobj$model, data = data, result_column = NULL,
               checkdata = FALSE))
}
