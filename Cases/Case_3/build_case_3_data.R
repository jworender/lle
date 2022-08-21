library(dplyr)

# Executing this script will result in three data sets of interest:
# 1. dset       = the consolidated data set
# 2. dset_train = the training split of the consolidated data set
# 3. dset_test  = the test split of the consolidated data set
#
# NOTE: if it is desired to inspect the intermediate variables, comment out the
# last block, which removes all of the variables except the three listed above
# from the environment.


N          <- 100  # the number of RANDOM variables to generate
S          <- 5   # the number of superposed variables to generate
              # relevant variables vector (length must be smaller than "S" - not 5)
R          <- c(1, 2, 4, 5)  
              # whether the high & above or low & below or in-between thresholds
#               area is used
AB         <- c('a', 'a', 'b', 'b')
              # each group will be elements of an "or" clause, whereas each
#               element within the group will be considered an "and" clause
#               (the groups must be sequentially numbered from one)
gp         <- c(1, 1, 2, 2)
n          <- 40   # setting the number of cycles to generate
H          <- 10   # number of historical time steps to use for prediction
rseed      <- 1234 # the random seed for repeatability
train_fr   <- 0.7  # the fraction of the dataset used for the training portion of the data
              # the time displacements for the variables
disp       <- c(5, 10, 0, 3, 8)
thresh_a   <- .7   # threshold above which the result contributes to a TRUE
#                    example for 'a' curves
thresh_b   <- .5   # threshold below which the result contributes to a TRUE
#                    example for 'b' curves
                   # (as a fraction of the max)
                   # NOTE:  For the narrow range 'n', results that lie within
#                           the boundaries defined by 'a' and 'b' will
#                           contribute to a TRUE example.

# setting the seed
set.seed(rseed)

# generating a set of sinusoidal signals that vary randomly by frequency with ~n cycles
# to mimic real-world phenomena driven by unseen inputs that tend to generate values
# as a continuous function
SIG_SIN_RND <- lapply(1:N, FUN = function(x) {
  unlist(lapply(1:n, FUN = function(x) { sin(seq(90, 449, sample(5, 1))/(180/pi)) })) })
# clipping a random number of elements to ensure that the phase is random as well
nmin <- n*360+1
SIG_SIN_RND <- lapply(SIG_SIN_RND, function(x) { 
  r    <-  x[sample(72, 1):length(x)]
  nmin <<- ifelse(length(r) < nmin, length(r), nmin) 
  return(r)
  })
# clipping the lengths so that all data sets are equal and adding a random vertical shift
SIG_SIN_RND <- lapply(SIG_SIN_RND, function(x) { return(x[1:nmin] + sample(200,1)/100) })

# Generating a set of superposed signals that use the random sinusoidal signals as a base
# determining how many random variables to sample.  As with the real world, the variables
# that you choose to measure may have overlapping causal effects that also affect some
# of the other variables that you chose, so sampling the curves with replacement.
nfun <- N%/%S + 1
SIG_SUPPOS  <- list()
for (j in 1:S) {
  SIG_SUPPOS[[j]] <- SIG_SIN_RND[[sample(N,1)]]
  for (i in 1:nfun)
    SIG_SUPPOS[[j]] <- SIG_SUPPOS[[j]] + SIG_SIN_RND[[sample(N,1)]]
  # varying the magnitude of the result
  SIG_SUPPOS[[j]] <- (rnorm(1, sd = .05) + .02) * 100 * SIG_SUPPOS[[j]] 
  }

# setting time displacements in which each "event" occurs after the threshold is breached
# in the real world an even may not occur immediately after a condition is met, there may
# be a reaction time between meeting a condition and event commencement.  Possible Examples:
# 1) Price response for a shortage of production for a specific product. There is a certain
#    amount of inventory space for the product so vendors likely will not raise their prices
#    before they need to re-order.
# 2) Price response for shortage in production of a specific product precursor, that is, an
#    element of a product that that is required for product assembly, but which may have
#    substitutes available.  This means that multiple thresholds will need to be breached
#    in order to trigger a corresponding event.  And there may be a time displacement
#    associated with each threshold if a shortage in the preferred item causes cascading
#    shortages in the substitute items.
# 3) A safety incident that depends on several dangerous conditions occurring simultaneously
#    or in sequence.  A risk precursor that is measurable (say aggregate training levels)
#    may beget other conditions that take some time to materialize, thus creating a danger
#    signal if a certain sequence of measurable precursors occurs.
#

# constructing a version of the curves that is shifted forward according to the
# displacements vector so that the original time sequence can remain untouched
SS <- SIG_SUPPOS
for (r in R) 
  if (disp[r] > 0) SS[[r]] <- c(rep(0, disp[r]), SS[[r]][1:(length(SS[[r]])-disp[r])])

# superposing the relevant variables
posg = list() # the gth element of the "or" clause
# keeping track of the limits for comparison purposes later
limits <- list()
for (i in 1:length(R)) {
  g <- gp[i]
  if (length(posg) < g) {
    # initializing the vector elements if the posg list element does not yet
    # have data in it 
    posg[[g]] <- rep(TRUE, length(SS[[1]]))
    }
  r  <- R[i]
  if      (AB[i] == 'a') {
    posg[[g]] <- posg[[g]] &
      (SS[[r]] > (thresh_a*(max(SS[[r]]) - min(SS[[r]])) + min(SS[[r]])))
    limits[[i]] <-
      c((thresh_a*(max(SS[[r]]) - min(SS[[r]])) + min(SS[[r]])), max(SS[[r]]))
  }
  else if (AB[i] == 'b') {
    posg[[g]] <- posg[[g]] &
      (SS[[r]] < (thresh_b*(max(SS[[r]]) - min(SS[[r]])) + min(SS[[r]])))
    limits[[i]] <-
      c(min(SS[[r]]), (thresh_b*(max(SS[[r]]) - min(SS[[r]])) + min(SS[[r]])))
  }
  else if (AB[i] == 'n') {
    posg[[g]] <- posg[[g]] &
      (SS[[r]] < (thresh_b*(max(SS[[r]]) - min(SS[[r]])) + min(SS[[r]]))) &
      (SS[[r]] > (thresh_a*(max(SS[[r]]) - min(SS[[r]])) + min(SS[[r]])))
    limits[[i]] <- c((thresh_a*(max(SS[[r]]) - min(SS[[r]])) + min(SS[[r]])),
                     (thresh_b*(max(SS[[r]]) - min(SS[[r]])) + min(SS[[r]])))
  }
}
lnames <- paste0(paste0("V",R,"TM"),disp[R])
names(limits) <- lnames

# combine all of the 'and' clauses together with the 'or' clause
pos <- rep(FALSE, length(SS[[1]]))
# keeping track of which groups are contributing to which outcome - each group
# uses a bit in a 32 bit integer, and all of those integers are retained in a
# vector - used for testing strategies to deal with entangled data sets (those
# that have positive outcomes from multiple sources - each source is entangled
# with the others)
attrib <- rep(0, length(SS[[1]]))
for (g in 1:length(posg)) {
  pos    <- pos | posg[[g]]
  attrib <- attrib + 2 ^ ((g-1)*pos[[g]])
}

time_steps <- which(pos)

# plotting the curves
for (i in 1:S) {
  srelev <- ifelse(i %in% R, "RELEVANT", "non-relevant")
  xvals  <- 0:(length(SIG_SUPPOS[[i]]) - 1)
  plot(xvals, SIG_SUPPOS[[i]], xlab = "Time Step", ylab = "Variable Value",
       main = sprintf("Curve #%d (%s)", i, srelev), type = "l", col = "black",
       ylim = c(mean(SIG_SUPPOS[[i]]) - 0.6*(max(abs(SIG_SUPPOS[[i]])) - min(abs(SIG_SUPPOS[[i]]))), 
                mean(SIG_SUPPOS[[i]]) + 0.6*(max(abs(SIG_SUPPOS[[i]])) - min(abs(SIG_SUPPOS[[i]])))))

  points(xvals[time_steps], SIG_SUPPOS[[i]][time_steps], col = "red", pch = 20)
}


# assembling the positive examples for each variable
# getting the indices of the positive examples
pos_indices  <- (1:nmin)[time_steps]
pos_indices  <- pos_indices[pos_indices > H]
# getting H number of historical time steps prior to the positive event
for (i in 1:S) {
  pos_hist     <- t(sapply(pos_indices, FUN = function(x)
    { return(SIG_SUPPOS[[i]][(x-H):x]) }))
  colnames(pos_hist) <- paste0("V",as.character(i),"TM", as.character(H:0))
  if (i == 1) pos_examples <- pos_hist
  if (i >  1) pos_examples <- cbind(pos_examples, pos_hist)
}
pos_examples <- data.frame(pos_examples)
pos_examples$INDC <- TRUE
pos_examples$X    <- pos_indices

# assembling the negative examples for each variable
# getting the indices of the negative examples
neg_indices  <- (1:nmin)[-time_steps]
# eliminating any that have H or less historical time steps preceding them
neg_indices  <- neg_indices[neg_indices > H]
# getting H number of historical time steps prior to the positive event
for (i in 1:S) {
  neg_hist     <- t(sapply(neg_indices, FUN = function(x)
    { return(SIG_SUPPOS[[i]][(x-H):x]) }))
  colnames(neg_hist) <- paste0("V",as.character(i),"TM", as.character(H:0))
  if (i == 1) neg_examples <- neg_hist
  if (i >  1) neg_examples <- cbind(neg_examples, neg_hist)
}

neg_examples <- data.frame(neg_examples)
neg_examples$INDC <- FALSE
neg_examples$X    <- neg_indices

dset   <- rbind(pos_examples, neg_examples)
dset   <- arrange(dset, X)

curve_data <- list(relev = R, thresh_a = thresh_a, thresh_b = thresh_b, disp = disp,
                   SIG_SIN_RND = SIG_SIN_RND, SIG_SUPPOS = SIG_SUPPOS,
                   time_steps = time_steps, dset = dset)

dset_train <- dset[sample(dim(dset)[1], train_fr*dim(dset)[1]),]
dset_test  <- dset[!(dset$X %in% dset_train$X),]

limlist    <- list()
for (i in 0:H) limlist[[i+1]] <- c(0,0)
all_limits <- list()
for (i in 1:S) {
  all_limits[[i]] <- limlist
  names(all_limits[[i]]) <- paste0(paste0("V",i,"TM"),0:H)

  prefix <- paste0("V",i,"TM")
  for (j in 0:H)
    all_limits[[i]][[j+1]] <- c(min(dset_train[,paste0(prefix,j)][dset_train$INDC]),
                                max(dset_train[,paste0(prefix,j)][dset_train$INDC]))
  
  in_this_list <- names(limits)[names(limits) %in% names(all_limits[[i]])]
  if (length(in_this_list) > 0) {
    all_limits[[i]][in_this_list] <- limits[in_this_list]
  }
}
names(all_limits) <- paste0("V",c(1:S),"TM")

# CLEAN-UP
# (if you wish to inspect the intermediate variables, comment out this block)

rm(list=c("all_limits", "curve_data", "limits", "limlist", "neg_examples",
          "neg_hist", "pos_examples", "pos_hist", "posg", "SIG_SIN_RND",
          "SIG_SUPPOS", "SS", "AB", "attrib", "disp", "g", "gp", "H", "i",
          "in_this_list", "j", "lnames", "n", "N", "neg_indices", "nfun",
          "nmin", "pos", "pos_indices", "prefix", "r", "R", "rseed", "S",
          "srelev", "thresh_a", "thresh_b", "time_steps", "train_fr", "xvals"))