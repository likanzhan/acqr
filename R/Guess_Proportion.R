#' @export
Create_Permutations <- function(x) {
    if (length(x) == 1) {
        return(x)
    }
    else {
        res <- matrix(nrow = 0, ncol = length(x))
        for (i in seq_along(x)) {
            res <- rbind(res, cbind(x[i], Recall(x[-i])))
        }
        return(res)
    }
}

#' @export
Guess_Proportion <- function(
  n = 4
){
Perms <- Create_Permutations(1:n)
Each_Focus_Position <- function(Focus_Position){
    Sub_Set <- Perms[Perms[, Focus_Position] == Focus_Position, , drop = FALSE]
    Cell_Left <- as.vector(Sub_Set[, 0:(Focus_Position - 1)])
    Cell_Left_Length <- length(Cell_Left)
    Correct_Cell_Left_Length <- length(Cell_Left[Cell_Left < Focus_Position])
    Focus_Correct <-  Correct_Cell_Left_Length / Cell_Left_Length
    Focus_Correct[is.na(Focus_Correct)] <- 0
    Result <- data.frame(
      Focus_Rank = Focus_Position,
      Focus_Correct_Length = Correct_Cell_Left_Length,
      Trial_Total_Length = Cell_Left_Length,
      Focus_Correct_Proportion = Focus_Correct
      )
    return(Result )
}

Guess_Proportion_Total <- lapply(1:n, 
    function(pp) Each_Focus_Position(pp)
    )
res <- do.call(rbind, Guess_Proportion_Total)
return(res)
}