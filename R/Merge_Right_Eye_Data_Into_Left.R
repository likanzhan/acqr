#' Merge data obtained from Right eye into relevant columns of left columns
#' Merge eye tracing data with eyelink
#' @param Eyelink_Data_File  Data outputed from Data Viewer samping report
#' @import data.table
#' @export

Merge_Right_Eye_Data_Into_Left <- function(Eyelink_Data_File) {
  Right_Eye_Row_Numbers <- Eyelink_Data_File["Right", on = "EYE_TRACKED", which = TRUE] # Use which = TRUE to Extract the row number of the data table
  Left_Eye_Columns <- colnames(Eyelink_Data_File)[like(colnames(Eyelink_Data_File), "LEFT")]
  Right_Eye_Columns <- gsub("LEFT", "RIGHT", Left_Eye_Columns)
  Column_Length <- length(Left_Eye_Columns)
  for (Column_to_Change in 1:Column_Length) {
    Old_Left_Eye_Column <- Left_Eye_Columns [Column_to_Change]
    Old_Right_Eye_Column <- Right_Eye_Columns[Column_to_Change]
    New_Left_Eye_Column <- Eyelink_Data_File[Right_Eye_Row_Numbers, Old_Right_Eye_Column, with = FALSE] # Use with = F to indicate the name of the column is a string
    set(Eyelink_Data_File, i = Right_Eye_Row_Numbers, j = Old_Left_Eye_Column, value = New_Left_Eye_Column) # Change the data of left eyes to data obtained in right eye in corresponding columns
  }
}
