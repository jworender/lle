library(nmaps)
library(readr)

wd <- getwd()

message("Reading data...")
setwd("C:/Users/publi/OneDrive/Graduate School/ODU/Dissertation/Comparison Study")

ionosphere_data <- data.frame(read_csv("./Data/Ionosphere/ionosphere.data.csv",
                                       col_names = FALSE))
cnames <- c(unlist(lapply(1:17, FUN = function(i)
    { c(paste0("Pulse_", i, "_r"), paste0("Pulse_", i, "_i")) })),
    "class")
colnames(ionosphere_data) <- cnames
ionosphere_data$class <- ionosphere_data$class == "g"
ionosphere_data$X     <- 1:dim(ionosphere_data)[1]

resp    <- "class"
exclude <- c("X")
groups  <- list()
groups[['Pulse_r']] <- paste0("Pulse_", 1:17, "_r")
groups[['Pulse_i']] <- paste0("Pulse_", 1:17, "_i")
feats               <- unlist(groups)
message("  Done.")

message("Generating data sets...")
rseed       <- 2345 # the random seed for repeatability
train_split <- 0.67
pos_balance <- 1.0
neg_balance <- 1.0

ion       <- ionosphere_data
ion_pos   <- ionosphere_data[ionosphere_data$class,]
ion_neg   <- ionosphere_data[!ionosphere_data$class,]

# setting the seed
set.seed(rseed)
ion_train <- rbind(ion_pos[sample(dim(ion_pos)[1], train_split*pos_balance*dim(ion_pos)[1]),],
                   ion_neg[sample(dim(ion_neg)[1],train_split*neg_balance*dim(ion_neg)[1]),])
ion_test  <- rbind(ion_pos[!(ion_pos$X %in% ion_train$X),],
                   ion_neg[!(ion_neg$X %in% ion_train$X),])

ion_sq_train <- squarify(data = ion_train, resp = resp, exclude = exclude,
                         groups = groups, dfilter = NULL, checkdata = TRUE,
                         sdfilter = NULL)
# ion_sqo_train <- OR_encode(ion_sq_train, feats = feats, stride = 5)
# ion_sqo_train$data$INDC <- !ion_sqo_train$data$INDC
# for (f in unlist(ion_sqo_train$groups)) 
#   ion_sqo_train$data[,f] <- -ion_sqo_train$data[,f]

ion_sq_test <- squarify(data = ion_test, resp = resp, exclude = exclude,
                        groups = groups, limits = ion_sq_train$limits,
                        sdfilter = NULL)
# ion_sqo_test <- OR_encode(ion_sq_test, feats = feats, limits = ion_sqo_train$limits,
#                           stride = 5)
# ion_sqo_test$data$INDC <- !ion_sqo_test$data$INDC
# for (f in unlist(ion_sqo_test$groups)) 
#   ion_sqo_test$data[,f] <- -ion_sqo_test$data[,f]
message("  Done.")

message("Fitting model...")
ion_sq_fit <- dfit(data = ion_sq_train$data, exclude = exclude, resp = resp,
                    dtype = "LS")
# ion_sqo_fit <- dfit(data = ion_sqo_train$data, exclude = exclude, resp = "INDC",
#                     dtype = "LS")
message("  Done.")

message("Plotting modeled data...")
plot(ion_sq_fit, title = "Goose Bay Training Set (transformed data)",
     h = .5)
plot(ion_sq_fit, data = ion_sq_test$data, title = "Goose Bay Testing Set (transformed data)", h = .5)
barplot(ion_sq_fit$model$beta[,1], las=2)

# plot(ion_sqo_fit, title = sprintf("SQR Training Set (%.0f%%)", train_split*100),
#      h = .5)
# plot(ion_sqo_fit, data = ion_sqo_test$data, title = "SQR Testing Set", h = .5)
# barplot(ion_sqo_fit$model$beta[,1], las=2)

feats_order <- ion_sq_fit$model$beta[,1]
names(feats_order) <- rownames(ion_sq_fit$model$beta)
feats_order <- feats_order[names(sort(abs(feats_order), decreasing = TRUE))]
feats_order <- feats_order[feats_order != 0]
feats <- names(feats_order)[names(feats_order) %in% feats]

# a fit using all features
ion_ls_all <- dfit(data = ion_train, exclude = exclude, resp = resp, dtype = "LS")
allfeats <- ion_ls_all$model$beta[,1]
names(allfeats) <- rownames(ion_ls_all$model$beta)
allfeats <- allfeats[names(sort(abs(allfeats)))]
allfeats <- allfeats[names(sort(abs(allfeats), decreasing = TRUE))]

plot(ion_ls_all, title = "Goose Bay Training Set (un-transformed data)",
                                 h = .5)
plot(ion_ls_all, data = ion_test, title = "Goose Bay Testing Set (un-transformed data)", h = .5)
barplot(ion_ls_all$model$beta[,1], las=2)
message("  Done.")
# fits using the features curated by the rectified LASSO
# ion_ls_fits <- list()
# features_used <- genMat(ncol = 3, nrow = length(feats),
#                         dimnames = c("NUM_FEATS", "SQR", "LAS"))
# for (i in 1:length(feats)){
#     message(sprintf("\n\nFitted with SQR %.0f prioritized features:",i))
#     ion_ls_fits[[i]] <- dfit(data = ion_train[,c(feats[1:i], resp)],
#                              exclude = exclude, resp = resp, dtype = "LS")
#
#     features_used[i,1] <- i
#     if (length(ion_ls_fits) >= i) if (!is.null(ion_ls_fits[[i]])) {
#         ion_ls_fits[[i]]$stats <- conf_matrix(ion_ls_fits[[i]], resp = resp,
#                                              data = ion_test,
#                                              chisq = TRUE, display = TRUE,
#                                              rescale = NULL, checkdata = FALSE)
#         idx <-  which(ion_ls_fits[[i]]$stats$stats$THR ==
#                       ion_ls_fits[[i]]$stats$thr)
#         features_used[i,2] <- (ion_ls_fits[[i]]$stats$stats$TP[idx] +
#                                ion_ls_fits[[i]]$stats$stats$TN[idx]) /
#                                dim(ion_ls_fits[[i]]$stats$resp)[1]
#     }
#     message(sprintf("Acc: %.3f", features_used[i,2]))
#
#     message(sprintf("\nFitted with LASSO %.0f prioritized features:",i))
#     af_fits <- dfit(data = ion_train[,c(names(allfeats)[1:i], resp)],
#                     exclude = exclude, resp = resp, dtype = "LS")
#
#     if (!is.null(af_fits)) {
#         af_fits$stats <- conf_matrix(af_fits, resp = resp, data = ion_test,
#                                      chisq = TRUE, display = TRUE,
#                                      rescale = NULL, checkdata = FALSE)
#         features_used[i,3] <- (af_fits$stats$stats$TP[idx] +
#                                af_fits$stats$stats$TN[idx]) /
#                                dim(af_fits$stats$resp)[1]
#     }
#     message(sprintf("Youdens: %.3f", features_used[i,3]))
#
#
# }
# features_used <- data.frame(features_used)
#
# plot(features_used$NUM_FEATS, features_used$SQR, type = "l", col = "red",
#      main = "Features used vs. Performance", xlab = "Number of Features Used",
#      ylab = "Youden's Index", ylim = c(0,1))
#
# points(features_used$NUM_FEATS, features_used$LAS, type = "l", col = "blue")
# setwd(wd)
