#' Score out-of-bag samples
#'
#' @param model A ranger random forest model.
#' @param X Training data feature matrix.
#' @param y Training data response vector.
#'
#' @return A tibble summarizing OOB performance.
#' @export
#'
#' @examples
#' ## survival example
#'
#' lung_clean <- na.omit(survival::lung)
#' rsf <- ranger::ranger(survival::Surv(time, status) ~ .,
#'                       data = lung_clean, num.trees = 100, keep.inbag=TRUE)
#' score_oob(rsf, lung_clean[, -c(2, 3)], survival::Surv(lung_clean$time, lung_clean$status))
score_oob <- function(model, X, y) {
  if(!inherits(model, c("ranger"))) {
    stop("Model is not from the ranger package.")
  }

  if(!("inbag.counts" %in% attributes(model)$names)) {
    stop("Model must be run with `keep.inbag=TRUE`")
  }

  if(model$treetype != "Survival") {
    stop(paste0("Unsupported treetype: ", model$treetype))
  }

  # get inbag counts
  inbag_counts <- model$inbag.counts
  names(inbag_counts) <- 1:model$num.trees
  inbag_counts <- dplyr::bind_cols(inbag_counts)

  # process data n observations at a time
  chf_list <- c()

  n <- 100
  max_row <- nrow(X)
  beg_idx <- seq(1, max_row, n)

  for (i in beg_idx) {
    message(paste0("Progress: ", round((i / max_row)*100), "%"))
    j <- min(i + n - 1, max_row)
    chf <- calculate_chf(model, inbag_counts[i:j, ], X[i:j, ], y[i:j, ])
    chf_list <- append(chf_list, list(chf))
  }

  chf <- dplyr::bind_rows(chf_list)

  num_trees <- NULL
  chf |>
    dplyr::summarize(dplyr::across(dplyr::everything(), function(x) calculate_cindex(-1*x, y))) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "num_trees", values_to = "c_index") |>
    dplyr::mutate(num_trees = as.numeric(num_trees))
}


calculate_chf <- function(model, inbag_counts, X, y) {
  # make predictions
  p <- stats::predict(
    model,
    data = X,
    predict.all = TRUE
  )

  # reformat to sample x tree x time
  chf <- aperm(p$chf, c(1, 3, 2))
  # remove trees that had the data in-bag
  chf[inbag_counts != 0] <- NA
  # cast back to sample x time x tree
  chf <- aperm(chf, c(1, 3, 2))

  sum_mean_chf <- list()

  last_chf_sum <- rowSums(chf[, , 1, drop=FALSE], dims = 2, na.rm = TRUE)
  last_oob_count <- rowSums(!is.na(chf[, , 1, drop=FALSE]), dims = 2, na.rm=TRUE)

  sum_mean_chf[[1]] <- rowSums(last_chf_sum / last_oob_count)

  for (i in 2:model$num.trees) {
    last_chf_sum <-
      abind::abind(last_chf_sum, chf[, , i, drop = TRUE], along = 3) |>
      rowSums(dims = 2, na.rm = TRUE)

    last_oob_count <-
      abind::abind(last_oob_count, !is.na(chf[, , i, drop = TRUE]), along = 3) |>
      rowSums(dims = 2, na.rm = TRUE)

    sum_mean_chf[[i]] <- rowSums(last_chf_sum / last_oob_count)
  }

  names(sum_mean_chf) <- 1:model$num.trees
  sum_mean_chf <- dplyr::bind_cols(sum_mean_chf)
}


calculate_cindex <- function(preds, y) {
  c_index <- yardstick::concordance_survival_vec(y, preds)
}
