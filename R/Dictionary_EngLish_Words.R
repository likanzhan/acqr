Generate_Word_List <- function(List = "~/Documents/ADMIN/Academic_Words/Words_Original.csv") {
 Word_List <- read.csv(List, stringsAsFactors = FALSE)
 Word_Name <- substr(Word_List$Words, 1, regexpr("\\[", Word_List$Words) - 1)
 Word_Name <- gsub(" $", "", Word_Name)
 Word_Sound <- substr(Word_List$Words, regexpr("\\[", Word_List$Words) + 1, regexpr("]:", Word_List$Words) -1)
 Word_Meaning <- substr(Word_List$Words, regexpr("]:", Word_List$Words) + 2, nchar(Word_List$Words))
 Word_Meaning <- gsub("^ ", "", Word_Meaning)
 Word_Context <- NA
 Source_Page <- NA
 Source_DOI <- NA
 Source_Author <- NA
 Source_Title <- NA
 Source_Year <- NA
 Source_Journal <- NA
 Source_Volume <- NA
 Source_Issue <- NA
 Source_Pages <- NA

 Word_Database <- data.frame(
    Word, Sound, Meaning, Context, 
    Source_Page, Source_DOI, Source_Author, Source_Title, Source_Year, Source_Journal, Source_Volume, Source_Issue, Source_Pages, 
    stringsAsFactors = FALSE)	
OutPut <- paste0(dirname(List), .Platform$file.se, "Word_Database.csv")
write.csv(Word_Database, OutPut, row.names = FALSE)
}

#' Add words to my dictionary
#' @export
#' @import svDialogs
Word <- function(
  Stored_Data_File = "~/Documents/ADMIN/Academic_Words/Word_Database.csv"	
  ){
    Word_Name  = dlgInput(message = "Word: ", default = "")$res
    Word_Sound = dlgInput(message = "Sound: ", default = "")$res
    Word_Meaning = dlgInput(message = "Mean: ", default = "")$res
    Word_Context = dlgInput(message = "Context: ", default = "")$res
    Source_Page = dlgInput(message = "Page: ", default = "NA")$res
    Source_DOI = dlgInput(message = "DOI: ", default = "NA")$res
	Source_Author = dlgInput(message = "Author: ", default = "NA")$res
	Source_Title = dlgInput(message = "Title: ", default = "NA")$res
	Source_Year = dlgInput(message = "Year: ", default = "NA")$res
	Source_Journal = dlgInput(message = "Journal: ", default = "NA")$res 
	Source_Volume = dlgInput(message = "Vol: ", default = "NA")$res
	Source_Issue = dlgInput(message = "Issue: ", default = "NA")$res
	Source_Pages = dlgInput(message = "Pages: ", default = "NA")$res
	Word_Item <- data.frame(Word_Name, Word_Sound, Word_Meaning, Word_Context, Source_Page, Source_DOI, 
    Source_Author, Source_Title, Source_Year, Source_Journal, Source_Volume, Source_Issue, Source_Pages, stringsAsFactors = FALSE, row.names = FALSE)
    Word_Database_Old <- read.csv(Stored_Data_File)
    Word_Database_New <- rbind(Word_Database_Old, Word_Item)
write.csv(Word_Database_New, file = Stored_Data_File, row.names = FALSE)
}

#' Add words to my dictionary
#' @export

Word2 <- function(Word_Name, Word_Sound, Word_Meaning, Word_Context, Source_Page, Source_DOI, 
	Source_Author = NA, Source_Title = NA, Source_Year = NA, Source_Journal = NA, Source_Volume = NA, Source_Issue = NA, Source_Pages = NA,
	Stored_Data_File = "~/Documents/ADMIN/Academic_Words/Word_Database.csv"
	) {
	Word_Item <- data.frame(Word_Name, Word_Sound, Word_Meaning, Word_Context, Source_Page, Source_DOI, 
    Source_Author, Source_Title, Source_Year, Source_Journal, Source_Volume, Source_Issue, Source_Pages, stringsAsFactors = FALSE, row.names = FALSE)
    Word_Database_Old <- read.csv(Stored_Data_File)
    Word_Database_New <- rbind(Word_Database_Old, Word_Item)
write.csv(Word_Database_New, file = Stored_Data_File, row.names = FALSE)
}

#' Add words to my dictionary
#' @export
Word3 <- function(
  Stored_Data_File = "~/Documents/ADMIN/Academic_Words/Word_Database.csv"	
  ){
    Word_Name  = readline(prompt = "Word: ")
    Word_Sound = readline(prompt = "Sound: ")
    Word_Meaning = readline(prompt = "Mean: ")
    Word_Context = readline(prompt = "Context: ")
    Source_Page = readline(prompt = "Page: ")
    Source_DOI = readline(prompt = "DOI: ") 
	Source_Author = readline(prompt = "Author: ") 
	Source_Title = readline(prompt = "Title: ") 
	Source_Year = readline(prompt = "Year: ") 
	Source_Journal = readline(prompt = "Journal: ") 
	Source_Volume = readline(prompt = "Vol: ") 
	Source_Issue = readline(prompt = "Issue: ") 
	Source_Pages = readline(prompt = "Pages: ")
	Word_Item <- data.frame(Word_Name, Word_Sound, Word_Meaning, Word_Context, Source_Page, Source_DOI, 
    Source_Author, Source_Title, Source_Year, Source_Journal, Source_Volume, Source_Issue, Source_Pages, stringsAsFactors = FALSE, row.names = FALSE)
    Word_Database_Old <- read.csv(Stored_Data_File)
    Word_Database_New <- rbind(Word_Database_Old, Word_Item)
write.csv(Word_Database_New, file = Stored_Data_File, row.names = FALSE)
}



#####
#install.packages('rdrop2')
#library(rdrop2)
#drop_auth()
#library(httr)
#set_config(use_proxy(url = "127.0.0.1", port = 1087))
#drop_acc()
#new_iris <- drop_read_csv("iris.csv")
#drop_create("WORDS")
#drop_copy("new_folder/mtcars.csv", "new_folder2/mtcars.csv")
#drop_download("test_folder/mtcars.csv")
#drop_upload("WORDS/mtcars.csv")

