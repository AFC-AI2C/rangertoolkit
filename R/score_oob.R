score_oob <- function(model, X, y) {
  num_trees <- seq(2, model$num.trees, 50)

  # get inbag counts
  inbag_counts <-
    model$inbag.counts |>
    bind_cols()

  # process data n observations at a time
  chf_list <- c()

  n <- 100
  max_row <- nrow(X)
  beg_idx <- seq(1, max_row, n)

  for (i in beg_idx) {
    print(i / max_row)
    j <- min(i + n - 1, max_row)
    chf <- calculate_chf(model, inbag_counts[i:j, ], X[i:j, ], y[i:j, ], num_trees)
    chf_list <- append(chf_list, list(chf))
  }

  chf <- bind_rows(chf_list)

  chf |>
    summarize(across(everything(), function(x) calculate_cindex(x, y))) |>
    pivot_longer(cols=everything(), names_to="num_trees", values_to="c_index") |>
    mutate(num_trees = as.numeric(num_trees))
}


calculate_chf <- function(model, inbag_counts, X, y, n_tree_seq) {
  # make predictions
  p <- predict(
    model,
    data=X,
    predict.all=TRUE
  )

  # reformat to sample x tree x time
  chf <- aperm(p$chf, c(1, 3, 2))
  # remove trees that had the data in-bag
  chf[inbag_counts != 0] <- NA
  # cast back to sample x time x tree
  chf <- aperm(chf, c(1, 3, 2))

  sum_chf <- list()

  for (i in seq_along(n_tree_seq)) {
    num_trees <- n_tree_seq[i]
    mean_chf <- rowMeans(chf[,,1:num_trees], dims=2, na.rm=TRUE)
    sum_chf[[i]] <- rowSums(mean_chf)
  }

  sum_chf <- bind_cols(sum_chf)
  names(sum_chf) <- as.character(n_tree_seq)
  return(sum_chf)
}


calculate_cindex <- function(preds, y) {
  result <- tibble(
    preds = preds,
    surv_obs = y
  )

  c_index <- concordance_survival(
    data = result,
    truth = surv_obs,
    estimate = preds
  ) |>
    select(.estimate) |>
    pull()

  c_index <- 1 - c_index
}
