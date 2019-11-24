#' @export

# Source: https://stackoverflow.com/questions/18508363/split-a-numeric-vector-into-continuous-chunks-in-r
# Source: https://stackoverflow.com/questions/15464793/restoring-original-order-of-a-vector-matrix-in-r

Create_Cluster <- function(vector){
    Original_Order <- order(vector)
    Sorted_Vector <- vector[Original_Order]
    Sorted_Group <- 1 + cumsum(seq_along(Sorted_Vector) %in% (which(diff(Sorted_Vector) > 1) + 1))
    Original_Group <- Sorted_Group[order(Original_Order)]	
    ResList  <- split(vector, Original_Group)	
    return(Original_Group)
	# return(ResList)
}
