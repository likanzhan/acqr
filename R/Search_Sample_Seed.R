#' Define a seed seach function
#' This function is used to find the appropriate seed to find the sample that does not contain
#' the elements with the same positions.
#' @examples
#' set.seed(Search_Sample_Seed(1:5))
#' sample(1:5)
#' @export

Create_Sample <- function(vector) {
  if (length(vector) == 1) stop("The data should be longer than 2 !")
  V1 <- 1:length(vector)
  V2 <- V1
  seed <- 1
  while (sum(V1 == V2) > 0) {
    seed <- seed + 1
    set.seed(seed)
    V2 <- sample(V1)
  }
  seed
}

#' @export
Search_Sample_Seed <- Create_Sample
