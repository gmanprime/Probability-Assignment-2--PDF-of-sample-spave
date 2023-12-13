expansion <- function(outcomes, experiments = 4) {
  # Get all permutations
  sample_space <- expand.grid(rep(list(outcomes), experiments))

  # Print the permutations
  # print ( sample_space_2 )

  # return the permutations to source
  return(sample_space)
}

probability_dist <- function(outcomes, value = "H", experiments = 4) {
  sample_space <- expansion(outcomes, experiments)

  temp_sum <- cbind(
    sample_space,
    count = apply(sample_space, 1, function(x) sum(grepl(pattern = value, x)))
  )

  st <- t(temp_sum)

  counts <- list(tail(st, 1))
  vector_count <- as.numeric(unlist(counts[1][1]))

  indexes <- c(0)
  values <- c(0) # list of values and counts
  val_counts <- c(0)

  for (i in 1: max(vector_count)){
    i_shift <- i + 1
    val_counts[i_shift] <- 0
    indexes[i_shift] <- i
    for (j in seq_along(vector_count)){
      if (vector_count[j] == i) {
        val_counts[i_shift] <- val_counts[i_shift] + 1
      }
    }
    values[i_shift] <- val_counts[i_shift] / length(vector_count)
  }

  val_counts[1] <- 1
  values[1] <- 1 / length(vector_count)

  output <- t(data.frame(indexes, val_counts, values))
  rownames(output) <- c(
    sprintf("%s", value),
    sprintf("'%s' Count", value),
    sprintf("P( X = '%s' )", value)
  )

  return(output)
}

pd_h <- probability_dist(c("H", "T"), "H", 4)
pd_t <- probability_dist(c("H", "T"), "T", 4)

# another test case
# pmf_m <- coin_toss_pdf(c("H", "T", "M"), "M", 4)