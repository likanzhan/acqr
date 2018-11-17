#' Plot the sound wave graph
#'
#' Draw the sound wave graphs in a directory
#'
#' @param Root_Directory The directory that contains the sound files
#' @examples
#' Draw_Soundwave_Graph()
#' @export
#' @importFrom tuneR readWave
#' @importFrom seewave oscillo

Plot_Soundwave_Graph <- function(
Root_Directory = "~/Desktop/Conditional_New/Important_Information/Experiment_Structure"
) {
Draw_Graph <- function(Sound_File_Name) {
    Sound_File <- tuneR::readWave(Sound_File_Name)
    Sound_Length <- ((length(Sound_File @ left) / Sound_File @ samp.rate))
    png(filename = file.path(dirname(Sound_File_Name), gsub(".wav", ".png", basename(Sound_File_Name))),
        height = 200, width = Sound_Length * 200, pointsize = 300)
    par(mar = c(0, 0, 0, 0))
    seewave::oscillo(Sound_File, colline = "white", colaxis = "white", xaxt = "n", labels = FALSE)
    dev.off()	
}
file_list <- list.files(Root_Directory, ".wav", full.names = TRUE)
invisible(sapply(file_list, Draw_Graph))	
}

