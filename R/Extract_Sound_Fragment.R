#' Extract Sound Fragment
#'
#' @import tuneR seewave
#' @export

Extract_Sound_Fragment <- function(
  Original_File = "NaMe",
  Start_Offset = 0,
  Stop_Offset  = 0,
  Sound_File_Path = "Objects_Audios",
  Play_New_File = TRUE
) {
  Start_Offset <- Start_Offset / 1000
  Stop_Offset <- Stop_Offset / 1000
  Original_Sound_Name <- file.path(Sound_File_Path, paste0(Original_File, ".wav"))
  New_Sound_Name      <- file.path(Sound_File_Path, paste0(Original_File, "_NEW", ".wav"))
  Original_Sound_File <- tuneR::readWave(Original_Sound_Name)
  Original_Sound_Length <- ((length(Original_Sound_File @ left) / Original_Sound_File @ samp.rate))
  Stop_Position  <- Original_Sound_Length - Start_Offset - Stop_Offset
  if (Start_Offset >= 0) {
    New_Sound_File_0 <- tuneR::extractWave(Original_Sound_File, from = Start_Offset, to = Original_Sound_Length, xunit = "time")	
  } else {
  	New_Sound_File_0 <- tuneR::bind(tuneR::silence(- Start_Offset, xunit = "time", pcm = TRUE, bit = 16), Original_Sound_File)
  }
  if (Stop_Offset >= 0) {
    New_Sound_File <- tuneR::extractWave(New_Sound_File_0, from = 0, to = Stop_Position, xunit = "time")	
  } else {
  	New_Sound_File <- tuneR::bind(New_Sound_File_0, tuneR::silence(- Stop_Offset, xunit = "time", pcm = TRUE, bit = 16))
  }  
  New_Sound_Length <- ((length(New_Sound_File @ left) / New_Sound_File @ samp.rate))
  par(mfrow = c(2, 1))
  seewave::oscillo(Original_Sound_File, alab = "Amplitude (Original File)")
  abline(v = Start_Offset, col = "red")
  abline(v = Stop_Position + Start_Offset, col = "red")
  seewave::oscillo(New_Sound_File,  alab = "Amplitude (New File)")
  abline(v = - Start_Offset, col = "red")
  abline(v = Stop_Position + Stop_Offset, col = "red")
  if (Play_New_File) {
    tuneR::play(New_Sound_File, player = '/usr/bin/afplay')  	
  }
  Sound_Files_Info <- paste0("Original_Sound_Length: ", Original_Sound_Length * 1000, "\n", "New_Sound_Length: ", New_Sound_Length * 1000)
  cat(Sound_Files_Info)
  tuneR::writeWave(New_Sound_File, New_Sound_Name)
}