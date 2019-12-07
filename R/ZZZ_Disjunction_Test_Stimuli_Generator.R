#' import magick 
#' import tuneR

#' Generate Test Stimuli List for the Disjunctions and the Modality Experiments
#' @export
Disjunction_Generate_Stimuli_List <- function(...){
	Generate_Stimuli_List(...)	
}

#' Generate Test Images for the Disjunctions and the Modality Experiments
#' @export
Disjunction_Generate_Test_Image <- function(row, ...) {
	invisible(Generate_Test_Image(row, ...))
}

#' Generate Test Audios for the Disjunctions and the Modality Experiments
#' @export
Disjunction_Generate_Test_Audio <- function(row, ...){
	invisible(Generate_Test_Audio(row, ...))
}

################################################################################
################################################################################
#### 1. Generate Stimuli List ################################################## 
#################################################################################
#################################################################################
Generate_Stimuli_List <- function(
	Root_Directory      = "~/Desktop/Modality_NEW",
	Stimuli_List_Name   = "Modailty_Test_Stimuli_List.csv",
	Information_File    = "Audio_Info.txt",
	Animal_Audio_Folder = "Animal_Audios",
	Object_Audio_Folder = "Object_Audios",
	Animal_Image_Folder = "Animal_Images",
	Object_Image_Folder = "Object_Images"
){	
#sapply(list.files("Boxes/", pattern = ".pdf$", full.names = TRUE), function(i)magick::image_info(magick::image_read(i))$height/magick::image_info(magick::image_read(i))$width)

	#### 1. Define Relevant Dirs #################################################################################
	Information_File_Dir   <- file.path(Root_Directory, Animal_Audio_Folder, Information_File)
	Animal_Audio_Directory <- file.path(Root_Directory, Animal_Audio_Folder)
	Object_Audio_Directory <- file.path(Root_Directory, Object_Audio_Folder)
	Animal_Image_Directory <- file.path(Root_Directory, Animal_Image_Folder)
	Object_Image_Directory <- file.path(Root_Directory, Object_Image_Folder)
	Output_File_Directory  <- file.path(Root_Directory, Stimuli_List_Name)

	#### 2. Establish Box Layouts #################################################################################
	Box_Category      <- c("First_Mentioned", "Second_Mentioned", "Small_Closed", "Big_Open")
	Box_Layout        <- acqr::Create_Latin_Square_Matrix(length(Box_Category))
	Box_Layout[]      <- Box_Category[Box_Layout]
	Box_Layout        <- as.data.frame(Box_Layout, stringsAsFactors = FALSE)
	names(Box_Layout) <- c("Top_Left", "Top_Right", "Bottom_Left", "Bottom_Right")
	Box_Layout$Spatial_Order <- paste0(
		substr(gsub("_.*$", "", Box_Layout$Top_Left    ), 1, 1), substr(gsub(".*_", "", Box_Layout$Top_Left    ), 1, 1), "_",
		substr(gsub("_.*$", "", Box_Layout$Top_Right   ), 1, 1), substr(gsub(".*_", "", Box_Layout$Top_Right   ), 1, 1), "_",
		substr(gsub("_.*$", "", Box_Layout$Bottom_Left ), 1, 1), substr(gsub(".*_", "", Box_Layout$Bottom_Left ), 1, 1), "_",
		substr(gsub("_.*$", "", Box_Layout$Bottom_Right), 1, 1), substr(gsub(".*_", "", Box_Layout$Bottom_Right), 1, 1)
		)

	##### 3. Retract Information about Audio files ##################################################################
	Audio_Names            <- read.table(Information_File_Dir, header = TRUE, stringsAsFactors = FALSE)
	Audio_Names$Audio_File <- as.character(Audio_Names$Audio_File)
	Animal_Audio_Files     <- list.files(Animal_Audio_Directory, pattern = ".wav$", full.names = FALSE)
	Object_Audio_Files     <- list.files(Object_Audio_Directory, pattern = ".wav$", full.names = FALSE)
	Audio_Files            <- c(Animal_Audio_Files, Object_Audio_Files)
	Audio_Files            <- gsub("(\\.wav$)|(.*(//))", "", Audio_Files)
	Audio_Files            <- data.frame(Audio_File = Audio_Files, stringsAsFactors = FALSE)
	Audio_Files            <- merge(Audio_Files, Audio_Names, all.x = TRUE)
	Audio_Files            <- Audio_Files[complete.cases(Audio_Files), ]

	##### 4. Image files ######################################################################
	Animal_Image_Files <- list.files(Animal_Image_Directory, pattern = ".png$", full.names = FALSE)
	Object_Image_Files <- list.files(Object_Image_Directory, pattern = ".png$", full.names = FALSE)
	Image_Files        <- c(Animal_Image_Files, Object_Image_Files)
	Image_Files        <- gsub("(\\.png$)|(.*(//))", "", Image_Files)
	Image_Files        <- data.frame(Audio_Content = Image_Files, stringsAsFactors = FALSE)

	##### 5. Combine All Files   ######################################################################
	All_Files   <- merge(Audio_Files, Image_Files, all.x = TRUE)
	Split_Files <- split(All_Files, All_Files$Category)

	##### 6. Create First mentioned and Second Mentioned Animals ########################################
	##### 6.1. Animal Category
	Animal_First_Mentioned              <- Split_Files$Animal
	SEED                                <- acqr::Search_Sample_Seed(1:nrow(Animal_First_Mentioned))
	set.seed(SEED)
	Animal_Second_Mentioned             <- Animal_First_Mentioned[sample(nrow(Animal_First_Mentioned)), ]
	names(Animal_First_Mentioned)[1:3]  <- paste0("First_Mentioned_",  names(Animal_First_Mentioned)[1:3])
	names(Animal_Second_Mentioned)[1:3] <- paste0("Second_Mentioned_", names(Animal_Second_Mentioned)[1:3])
	Animals                             <- cbind(Animal_First_Mentioned, Animal_Second_Mentioned)
	Animals                             <- Animals[, unique(names(Animals))]
	nrow(Animals[Animals$First_Mentioned_Audio_Content == Animals$Second_Mentioned_Audio_Content, ])

	##### 6.2. Object Category
	Object_First_Mentioned              <- Split_Files$Object[1:85, ]
	SEED                                <- acqr::Search_Sample_Seed(1:nrow(Object_First_Mentioned))
	set.seed(SEED)
	Object_Second_Mentioned             <- Object_First_Mentioned[sample(nrow(Object_First_Mentioned)), ]
	names(Object_First_Mentioned)[1:3]  <- paste0("First_Mentioned_",  names(Object_First_Mentioned)[1:3])
	names(Object_Second_Mentioned)[1:3] <- paste0("Second_Mentioned_", names(Object_Second_Mentioned)[1:3])
	Objects                             <- cbind(Object_First_Mentioned, Object_Second_Mentioned)
	Objects                             <- Objects[, unique(names(Objects))]
	nrow(Objects[Objects$First_Mentioned_Audio_Content == Objects$Second_Mentioned_Audio_Content, ])

	##### 6.3. Combine the two Categories
	Combined_Files        <- rbind(Animals, Objects)
	Combined_Files$Number <- sprintf("T%03d", 1:nrow(Combined_Files))
	Combined_Files        <- Combined_Files[, c(8, 1:3, 5:7, 4)]

	##### 7. Combine File Information and Box_Layouts ########################################
	Test_Image_List            <- cbind(Combined_Files, Box_Layout)
	Test_Image_List$Test_Image <- paste0(Test_Image_List$Number, ".png")
	names(Test_Image_List)     <- gsub("_Audio_Content", "", names(Test_Image_List))
	names(Test_Image_List)     <- gsub("_File", "", names(Test_Image_List))

	##### 8.  ########################################
	Sentential_Connectives       <- data.frame(Sentential_Connective = c("And", "Or", "But", "Not"), stringsAsFactors = FALSE)
	Test_Stimuli_List            <- merge(Test_Image_List, Sentential_Connectives)
	Test_Stimuli_List            <- Test_Stimuli_List[order(Test_Stimuli_List$Number, Test_Stimuli_List$Sentential_Connective), ]
	Test_Stimuli_List$Group      <- LETTERS[acqr::Create_Latin_Square_Matrix(nrow(Sentential_Connectives))]
	Test_Stimuli_List$Test_Audio <- paste0(Test_Stimuli_List$Number, "_", Test_Stimuli_List$Sentential_Connective, ".wav")

	##### 9.  ########################################
	write.csv(Test_Stimuli_List, file = Output_File_Directory, row.names = FALSE)	
}

################################################################################
################################################################################
#### 2. Generate Test Images  ################################################## 
#################################################################################
#################################################################################
Generate_Test_Image <- function(
	row,
	Add_Grid_Lines    = FALSE,
	Root_Directory    = "~/Desktop/Modality_NEW",
	Test_Image_Folder = "Test_Images",
	Stimuli_List      = "Modailty_Test_Stimuli_List.csv"
){
	########## 1. Define the varibles ###################################################################################
    suppressMessages(library(magick))
	
	### 1.0. Select the data
	Data_File              <- read.csv(file.path(Root_Directory, Stimuli_List), stringsAsFactors = FALSE)
	Data_File              <- Data_File[!duplicated(Data_File$Number), ]
	Current_Trial          <- Data_File[row, ] #Cols: Top_Left, Top_Right,Bottom_Left, Bottom_Right, Spatial_Order, First_Mentioned, Second_Mentioned 
	Input_Box_Folder       <- "Boxes"
	Input_Box_Directory    <- file.path(Root_Directory, Input_Box_Folder)

	Input_Object_Folder    <- paste0(Current_Trial[["Category"]], "_Images")
	Input_Object_Directory <- file.path(Root_Directory, Input_Object_Folder)

	Output_Image_Directory <- file.path(Root_Directory, Test_Image_Folder)
	dir.create(Output_Image_Directory, showWarnings = FALSE)
			
	### 1.1. Define Originals    
    Grid <- 40 / 3
	HL   <- c(0, 45, 6 * Grid,  4 * Grid, 8 * Grid, 198, 6 * Grid,  4 * Grid, 8 * Grid, 45)
	VL   <- c(0, 88, 4 * Grid, 16 * Grid, 4 * Grid, 208, 4 * Grid, 16 * Grid, 4 * Grid, 88)
	HP   <- cumsum(HL)
	VP   <- cumsum(VL)
	HL_Pos <- (HP[-1] + HP[-length(HP)]) / 2
	VL_Pos <- (VP[-1] + VP[-length(VP)]) / 2

	### 1.2. Define Box Offsets	
	Box_Offset_Top_Left     <- paste0("+", VP[2], "+", HP[2]) # "+  88 +   45"
	Box_Offset_Top_Right    <- paste0("+", VP[6], "+", HP[2]) # "+ 616 +   45"
	Box_Offset_Bottom_Right <- paste0("+", VP[6], "+", HP[6]) # "+ 616 +  483"
	Box_Offset_Bottom_Left  <- paste0("+", VP[2], "+", HP[6]) # "+  88 +  483"

	### 1.3. Define Annotation Offsets
	Annotate_Location_A <- paste0("+", - 208/2 - 12 * Grid, "+", - 198/2 -  4 * Grid)
	Annotate_Location_B <- paste0("+", + 208/2 + 12 * Grid, "+", - 198/2 -  4 * Grid)
	Annotate_Location_C <- paste0("+", - 208/2 - 12 * Grid, "+", + 198/2 + 14 * Grid)
	Annotate_Location_D <- paste0("+", + 208/2 + 12 * Grid, "+", + 198/2 + 14 * Grid)

	### 1.4. Define Animal Offsets		
	Spatial_Order       <- rep(c("FM_SM_BO_SC", "SM_SC_FM_BO", "SC_BO_SM_FM", "BO_FM_SC_SM"), each = 4)
	X_Scale             <- list(3:4, 3:4, 3:4, 7:8, 7:8, 7:8, 3:4, 3:4, 7:8, 7:8, 7:8, 3:4, 3:4, 3:4, 7:8, 7:8)
	Y_Scale             <- list(  6,   6,   2,   2,   6,   6,   6,   2,   2,   2,   6,   6,   2,   2,   2,   6)
	Current_Animal      <- rep(c("1st_Mentioned_1", "2nd_Mentioned_1", "1st_Mentioned_2", "2nd_Mentioned_2"), 4)
	All_Animal_Offsets  <- data.frame(
							Spatial_Order = Spatial_Order, Current_Animal = Current_Animal,
							X_Scale = I(X_Scale), Y_Scale = I(Y_Scale), stringsAsFactors = FALSE)
	Current_Trial_Offset <- All_Animal_Offsets[All_Animal_Offsets[, "Spatial_Order"] == Current_Trial[["Spatial_Order"]], ]
		
	Animal_1st_Mentioned_1_X_Offset <- mean(VP[subset(Current_Trial_Offset, Current_Animal=="1st_Mentioned_1")[["X_Scale"]][[1]]]) - 5 - 100
	Animal_1st_Mentioned_2_X_Offset <- mean(VP[subset(Current_Trial_Offset, Current_Animal=="1st_Mentioned_2")[["X_Scale"]][[1]]]) - 100/2
	Animal_2nd_Mentioned_1_X_Offset <- mean(VP[subset(Current_Trial_Offset, Current_Animal=="2nd_Mentioned_1")[["X_Scale"]][[1]]]) + 5
	Animal_2nd_Mentioned_2_X_Offset <- mean(VP[subset(Current_Trial_Offset, Current_Animal=="2nd_Mentioned_2")[["X_Scale"]][[1]]]) - 100/2
	Animal_1st_Mentioned_1_Y_Offset <- 		HP[subset(Current_Trial_Offset, Current_Animal=="1st_Mentioned_1")[["Y_Scale"]][[1]]]  + 3 * Grid
	Animal_1st_Mentioned_2_Y_Offset <- 		HP[subset(Current_Trial_Offset, Current_Animal=="1st_Mentioned_2")[["Y_Scale"]][[1]]]  + 3 * Grid
	Animal_2nd_Mentioned_1_Y_Offset <- 		HP[subset(Current_Trial_Offset, Current_Animal=="2nd_Mentioned_1")[["Y_Scale"]][[1]]]  + 3 * Grid
	Animal_2nd_Mentioned_2_Y_Offset <- 		HP[subset(Current_Trial_Offset, Current_Animal=="2nd_Mentioned_2")[["Y_Scale"]][[1]]]  + 3 * Grid

	Animal_Offset_FM1 <- paste0("+", Animal_1st_Mentioned_1_X_Offset, "+", Animal_1st_Mentioned_1_Y_Offset)  
	Animal_Offset_FM2 <- paste0("+", Animal_1st_Mentioned_2_X_Offset, "+", Animal_1st_Mentioned_2_Y_Offset)
	Animal_Offset_SM1 <- paste0("+", Animal_2nd_Mentioned_1_X_Offset, "+", Animal_2nd_Mentioned_1_Y_Offset)
	Animal_Offset_SM2 <- paste0("+", Animal_2nd_Mentioned_2_X_Offset, "+", Animal_2nd_Mentioned_2_Y_Offset)
	    
	########## 2. Define Functions ###################################################################   
    Read_Box <- function(Box_Name) {
    	Box_Name <- paste0(Input_Box_Directory, "/", Box_Name, ".pdf")
    	Load_Box <- magick::image_read(Box_Name, density = 72)
    	Load_Box <- magick::image_transparent(Load_Box, color = "white") # Remove background
    	Load_Box <- magick::image_resize(Load_Box, geometry = "320x240")
    	return(Load_Box)
    	}
    	
    Load_Box <- function(Box_Name) {
    	Box_Name <- ifelse(Box_Name %in% c("First_Mentioned", "Second_Mentioned"), "Small_Open", Box_Name)
		Read_Box(Box_Name)
    	}
    	
    Load_Front <- function(Box_Name) {
    	Box_Name <- ifelse(Box_Name %in% c("Big_Open", "Big_Closed"), "Big_Front", "Small_Front")
		Read_Box(Box_Name)	
    	} 	

    Load_Animal <- function(Animal_Name) {
    	Animal_File <- paste0(Input_Object_Directory, "/", Animal_Name, ".png")    	
    	Animal_Image <- magick::image_read(Animal_File, density = 72)
    	Animal_Image <- magick::image_resize(Animal_Image, geometry = "100x100")
	    #Animal_Image <- magick::image_background(Animal_Image, color = "blue")    	
		return(Animal_Image)	
    }
    
    ########## 2. Load Objects ######################################################################   
    Box_Top_Left     <- Load_Box(Current_Trial[["Top_Left"]])
    Box_Top_Right    <- Load_Box(Current_Trial[["Top_Right"]])
    Box_Bottom_Left  <- Load_Box(Current_Trial[["Bottom_Left"]])
    Box_Bottom_Right <- Load_Box(Current_Trial[["Bottom_Right"]])
    
    Animal_First_Mentioned  <- Load_Animal(Current_Trial[["First_Mentioned"]])
    Animal_Second_Mentioned <- Load_Animal(Current_Trial[["Second_Mentioned"]])
    
    Front_Top_Left     <- Load_Front(Current_Trial[["Top_Left"]])
    Front_Top_Right    <- Load_Front(Current_Trial[["Top_Right"]])
    Front_Bottom_Left  <- Load_Front(Current_Trial[["Bottom_Left"]])
    Front_Bottom_Right <- Load_Front(Current_Trial[["Bottom_Right"]])
	
	########## 3. Do the Plotting ###################################################################  
	`%>%` <- magrittr::`%>%`       	
	IMAGE <- magick::image_blank(width = 1024, height = 768, color = "whitesmoke")    %>%
		magick::image_composite(Box_Top_Left,     offset = Box_Offset_Top_Left)       %>%
		magick::image_composite(Box_Top_Right,    offset = Box_Offset_Top_Right)      %>%
		magick::image_composite(Box_Bottom_Left,  offset = Box_Offset_Bottom_Left)    %>%
		magick::image_composite(Box_Bottom_Right, offset = Box_Offset_Bottom_Right)   %>%

		magick::image_composite(Animal_First_Mentioned,   offset = Animal_Offset_FM1) %>%
		magick::image_composite(Animal_Second_Mentioned,  offset = Animal_Offset_SM1) %>%
		magick::image_composite(Animal_First_Mentioned,   offset = Animal_Offset_FM2) %>%
		magick::image_composite(Animal_Second_Mentioned,  offset = Animal_Offset_SM2) %>%

		magick::image_composite(Front_Top_Left,     offset = Box_Offset_Top_Left)     %>%
		magick::image_composite(Front_Top_Right,    offset = Box_Offset_Top_Right)    %>%
		magick::image_composite(Front_Bottom_Left,  offset = Box_Offset_Bottom_Left)  %>%
		magick::image_composite(Front_Bottom_Right, offset = Box_Offset_Bottom_Right) %>%

		magick::image_annotate("A", font = "Times", size = 30, gravity = "Center", location = Annotate_Location_A) %>%
		magick::image_annotate("B", font = "Times", size = 30, gravity = "Center", location = Annotate_Location_B) %>%
		magick::image_annotate("C", font = "Times", size = 30, gravity = "Center", location = Annotate_Location_C) %>%
		magick::image_annotate("D", font = "Times", size = 30, gravity = "Center", location = Annotate_Location_D)
	
	if (Add_Grid_Lines) {
	Rgh_Size <- 60
	Btm_Size <- Rgh_Size / 2
	IMAGE_BG <- magick::image_blank(width = 1024 + Rgh_Size, height = 768 + Btm_Size, color = "white")
	IMAGE    <- magick::image_composite(IMAGE_BG, IMAGE)
	IMAGE    <- image_draw(IMAGE)
	invisible(sapply(HP, function(LL) abline(h = LL, col = 'blue', lwd = 1, lty = "dashed")))
	invisible(sapply(VP, function(LL) abline(v = LL, col = 'blue', lwd = 1, lty = "dashed")))
	f1 <- function(N)text(1024 + Rgh_Size / 2, HL_Pos[N]         , paste0(round(HL[N+1]), "px"), cex = 1)
	f2 <- function(N)text(VL_Pos[N]          , 768 + Btm_Size / 2, paste0(round(VL[N+1]), "px"), cex = 1)
	sapply(1:length(HL_Pos), f1)
	sapply(1:length(VL_Pos), f2)
	rect(VP[2], HP[2], VP[5], HP[5], border = "red", lwd = 2)
	rect(VP[6], HP[2], VP[9], HP[5], border = "red", lwd = 2)
	rect(VP[2], HP[6], VP[5], HP[9], border = "red", lwd = 2)
	rect(VP[6], HP[6], VP[9], HP[9], border = "red", lwd = 2)
	invisible(dev.off())
	image_browse(IMAGE)	
	}
	
	print(Current_Trial[["Number"]])
	IMAGE_NAME <- ifelse(Add_Grid_Lines, paste0("Exp_", Current_Trial[["Test_Image"]]), Current_Trial[["Test_Image"]])
	image_write(IMAGE, paste0(Output_Image_Directory, "/", IMAGE_NAME), density = 300)	
}

################################################################################
################################################################################
#### 3. Generate Test Audios  ################################################## 
################################################################################
################################################################################
Generate_Test_Audio <- function(
	row,
	Root_Directory      = "~/Desktop/Modality_NEW",
	Stimuli_List        = "Modailty_Test_Stimuli_List.csv",
	Test_Audio_Folder   = "Test_Audios",
	Animal_Audio_Folder = "Animal_Audios",
	Object_Audio_Folder = "Object_Audios",
	Shared_Audio_Folder = "Shared_Audios",
	Mentioned_Expected_Length = 1400
){
#### 0. Read Information ##################################################################################### 
	library(tuneR)
	Data_File  <- read.csv(file.path(Root_Directory, Stimuli_List), stringsAsFactors = FALSE)

#### 1. Retract Current Trial ################################################################################## 
	#### 1.1. Create Output Dir ############################################################################### 	
	Test_Audios_Output_Directory <- file.path(Root_Directory, Test_Audio_Folder)
    dir.create(Test_Audios_Output_Directory, showWarnings = FALSE)	

	#### 1.1. Retract Information ############################################################################### 	
	Current_Trial           <- Data_File[row, ]
	Current_Trial_Folder    <- ifelse(Current_Trial[["Category"]] == "Animal", Animal_Audio_Folder, Object_Audio_Folder)
	Current_Sentence_Type   <- Current_Trial[["Sentential_Connective"]]
	Current_Test_Audio_Name <- Current_Trial[["Test_Audio"]]

#### 2. Define Relevant Functions ############################################################################## 	
	#### 2.1. Load Mentioned Object ############################################################################# 	
	Mentioned <- function(Which) {
		Mentioned_Name          <- paste0(Current_Trial[[paste0(Which, "_Mentioned_Audio")]], ".wav")
		Mentioned_File          <- file.path(Root_Directory, Current_Trial_Folder, Mentioned_Name)
		Mentioned               <- tuneR::readWave(Mentioned_File)
		Mentioned_Actual_Length <- ((length(Mentioned @ left) / Mentioned @ samp.rate))
    	Silence_Length          <- Mentioned_Expected_Length / 1000 - Mentioned_Actual_Length
    	if (Silence_Length > 0) {
          Slience_Added <- tuneR::silence(Silence_Length, xunit = "time", pcm = TRUE, bit = 16)
          Mentioned     <- tuneR::bind(Mentioned, Slience_Added)	
    	} else {
          Mentioned     <- Mentioned
    	}		
		#tuneR::play(Mentioned, player = '/usr/bin/afplay')
		return(Mentioned)
	}
	#### 2.2. Load Shared Object ################################################################################# 	
	Shared <- function(Name){
		Shared_Name <- paste0(Name, ".wav")
		Shared_File <- file.path(Root_Directory, Shared_Audio_Folder, Shared_Name)
		Shared      <- tuneR::readWave(Shared_File)
		#tuneR::play(Shared, player = '/usr/bin/afplay')
		return(Shared)
	}

	#### 2.2. Create Current Test Audio############################################################################ 	
	Current_Test_Audio <- function(type = c("And", "Or", "But", "Not")) {
  		switch(match.arg(type), 
  		And = tuneR::bind(XiaoMingDe, XiangZiLi, You   , YiZhi, First_Mentioned, He , YiZhi , Second_Mentioned), 
  		Or  = tuneR::bind(XiaoMingDe, XiangZiLi, You   , YiZhi, First_Mentioned, Huo, YiZhi , Second_Mentioned), 
  		But = tuneR::bind(XiaoMingDe, XiangZiLi, You   , YiZhi, First_Mentioned, Dan, MeiYou, Second_Mentioned), 
  		Not = tuneR::bind(XiaoMingDe, XiangZiLi, MeiYou, YiZhi, First_Mentioned, Dan, YouZhi, Second_Mentioned)
  		)
	}

#### 3. Load Objects ############################################################################################### 	
	First_Mentioned  <- Mentioned("First")
	Second_Mentioned <- Mentioned("Second")
	XiaoMingDe       <- Shared("XiaoMingDe")
	XiangZiLi        <- Shared("XiangZiLi")
	You              <- Shared("You")
	MeiYou           <- Shared("MeiYou")
	YiZhi            <- Shared("YiZhi")
	Dan              <- Shared("Dan")
	He               <- Shared("He")
	Huo              <- Shared("Huo")
	YouZhi           <- Shared("YouZhi")

#### 4. Generate Sentence ####################################################################################### 	
	Current_Test_Sentence_Audio <- Current_Test_Audio(Current_Sentence_Type)

#### 5. Write Wave ########################################################################################### 	
    Test_Audios_Output_Name  <- file.path(Test_Audios_Output_Directory, Current_Test_Audio_Name)
    print(Current_Test_Audio_Name)
    tuneR::writeWave(Current_Test_Sentence_Audio, Test_Audios_Output_Name, extensible = FALSE)	
}