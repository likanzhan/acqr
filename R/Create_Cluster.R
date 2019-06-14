#' @export

# Source: https://stackoverflow.com/questions/18508363/split-a-numeric-vector-into-continuous-chunks-in-r

Create_Cluster <- function(vector){
    Group <- 1 + cumsum(seq_along(vector) %in% (which(diff(vector) > 1) + 1))	
    ResList  <- split(vector, Group)	
    return(Group)
}
