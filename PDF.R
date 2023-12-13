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

  indexes <- c()
  values <- c() # list of values and counts

  for (i in 1: max(vector_count)){
    values[i] <- 0
    indexes[i] <- i
    for (j in seq_along(vector_count)){
      if (vector_count[j] == i) {
        values[i] <- values[i] + 1
      }
    }
  }

  output <- t(data.frame(indexes, values))
  rownames(output) <- c(
    sprintf("%s count", value),
    sprintf("P( X = '%s' )", value)
  )

  return(output)
}

pd_h <- probability_dist(c("H", "T"), "H", 4)
pd_t <- probability_dist(c("H", "T"), "T", 4)

# another test case
# pmf_m <- coin_toss_pdf(c("H", "T", "M"), "M", 4)