List_Audio_Files <- function(Audio_File_Directory){
	library(tuneR)
    Audio_File_List <- list.files(Audio_File_Directory)
    File_Length <- length(Audio_File_List)
    Audio_File_Info <- data.frame(
        File_Name   = rep(0, File_Length), 
        File_Length = rep(0, File_Length))
    for (i in 1:File_Length) {
        File_Path <- file.path(Audio_File_Directory, Audio_File_List[i])
        Audio_File <- tuneR::readWave(File_Path)
        Audio_File_Length <- length(Audio_File @ left) / Audio_File @ samp.rate
        Audio_File_Info[i, "File_Name"]   <- Audio_File_List[i]
        Audio_File_Info[i, "File_Length"] <- Audio_File_Length
    }
    return(Audio_File_Info)	
}
