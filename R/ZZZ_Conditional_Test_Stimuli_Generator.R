####################################################################################################################################################################
########  Conditional_Information_Generator
####################################################################################################################################################################
Conditional_Information_Generator <- function(Root_Directory, Input_Audio_Component_Folder, Sentential_Components_File){
#Conditional_Information_Generator <- function(){
#################################### 1. Explore the length of the Audio Files ###########################
### a. Read the Audio Files ###########################
library(tuneR)
Audio_File_Directory <- file.path(Root_Directory, Input_Audio_Component_Folder)
Audio_File_Info <- acqr::List_Audio_Files(Audio_File_Directory)

### b. Retract the length of Objects ###########################
Object_Audio_File <- Audio_File_Info[grepl("^[[:digit:]]", Audio_File_Info$File_Name), ]
Object_Audio_File <- Object_Audio_File[order(Object_Audio_File$File_Length), ]
Object_Audio_File $ File_Length <- round(Object_Audio_File$File_Length * 1000)
Object_Audio_File_Max_Length <- ceiling(max(Object_Audio_File[, "File_Length"]) / 100) * 100

### c. Retract the length of Other Elements ###########################
Other_Elements_Audio <- Audio_File_Info[grepl("^[[:alpha:]]", Audio_File_Info$File_Name), ]
Other_Elements_Audio $ File_Name <- gsub("\\.wav$", "", Other_Elements_Audio[, "File_Name"])
Other_Elements_Name <- expand.grid(
    Condition = c("Because", "IF"), Position  = paste0("P", c(1:3, 5:8)),
    stringsAsFactors = FALSE)
Other_Elements_Name $ File_Name <- c(
    "YinWei", "RuGuo", 
    "XiangZiLi", "XiangZiLi", 
    "Shi", "Shi", 
    "SuoYi", "NaMe", 
    "XiaoMing", "XiaoMing",
    "Hen", "Jiu", 
    "GaoXing", "ShangXin")
Other_Elements_Information <- merge(Other_Elements_Audio, Other_Elements_Name, by = "File_Name", all.y = TRUE)
Other_Elements_Information <- Other_Elements_Information[
    order(Other_Elements_Information$Position, Other_Elements_Information$Condition, decreasing = c(FALSE, TRUE)), ]
Other_Elements_Information$File_Length <- round(Other_Elements_Information$File_Length * 1000)

### d. Combine the Information Together ###########################
Audio_File_Information <- list(
    Object_Audio_File = Object_Audio_File,
    Object_Audio_File_Max_Length = Object_Audio_File_Max_Length,
    Other_Elements_Information = Other_Elements_Information)

#################################### 2. Generate the Test Images Information ############################
`%>%` <- magrittr::`%>%`
suppressMessages(library(dplyr))
Sentential_Components_File_Directory <- file.path(Root_Directory, paste0(Sentential_Components_File, ".csv"))
Sentential_Components <- read.csv(Sentential_Components_File_Directory, header = TRUE, stringsAsFactors = FALSE)
Test_Images_Information <- Sentential_Components %>% 
    filter(Component_Category != "Z_Shared") %>%
    filter(!Component_Image %in% c("FengChe", "XiaoDao", "JianDao", "ZhiLou")) %>%
    rename(Object_Same_Audio = Component_Audio, Object_Same_Image = Component_Image) %>%
    group_by(Component_Category) %>%
    ## Search_Sample_Seed to find a random seed to make sure that no element is in the same  position
    mutate(Object_Different_Image = {set.seed(acqr::Search_Sample_Seed(Object_Same_Image)); sample(Object_Same_Image)}) %>% ungroup() %>% 
    mutate(Object_Different_Audio = pull(.[match(Object_Different_Image, Object_Same_Image), "Object_Same_Audio"])) %>%
    mutate(Spatial_Order = rep(c("CS_DD_SD_SS", "SD_SS_CS_DD", "DD_CS_SS_SD", "SS_SD_DD_CS"), 84 / 4)) %>%
    mutate(Temporal_Order = rep(c("Different", "Same"), 84 / 2)) %>%
    mutate(Test_Image = paste0(sprintf("%02d", 1:nrow(.)), ".png")) %>%
    arrange(Component_Category)
Test_Images_Information <- as.data.frame(Test_Images_Information)   
    
#################################### 3. Generate the Test Audios Information  ############################
Experimental_Manipulation <- expand.grid(
    Sentential_Connective = c("Because", "IF"), 
    Mentioned_Object      = c("Different", "Same"),
    Agent_Mood            = c("Happy", "Sad"),
    stringsAsFactors = FALSE)      
Experimental_Manipulation <- Experimental_Manipulation[-5, ] # Remove the illicite Condition, i.e., Because-Different-Sad
Test_Stimuli_Full <- Experimental_Manipulation %>%
    merge(Test_Images_Information) %>%
    mutate(Split = rep(as.vector(t(acqr::Create_Latin_Square_Matrix(7))), 6)) %>%
    mutate(Familize_1_Object_Image = sapply(1:nrow(.), 
        function(i) paste0(.[i, grepl(paste0(.[i, "Temporal_Order"], "_Image"), colnames(.))], ".png")
        )) %>%
    mutate(Familize_1_Object_Audio = sapply(1:nrow(.), 
        function(i) paste0(.[i, grepl(paste0(.[i, "Temporal_Order"], "_Audio"), colnames(.))], ".wav")
        )) %>%
    mutate(Familize_2_Object_Image = sapply(1:nrow(.), 
        function(i) paste0(.[i, grepl(paste0("^(?=Object)(?!.*", .[i, "Temporal_Order"], ")(?=.*Image)"), colnames(.), perl = TRUE)], ".png")
        )) %>%
    mutate(Familize_2_Object_Audio = sapply(1:nrow(.), 
        function(i) paste0(.[i, grepl(paste0("^(?=Object)(?!.*", .[i, "Temporal_Order"], ")(?=.*Audio)"), colnames(.), perl = TRUE)], ".wav")
        )) %>%
    mutate(Test_Audio = paste0(abbreviate(paste(substr(Test_Image, 1, 2), Sentential_Connective, Mentioned_Object, Agent_Mood), 6), ".wav")) %>%
    mutate(Top_Left     = substr(Spatial_Order, 1, 2),
           Top_Right    = substr(Spatial_Order, 4, 5),
           Bottom_Left  = substr(Spatial_Order, 7, 8),
           Bottom_Right = substr(Spatial_Order, 10, 11))
Test_Stimuli <- Test_Stimuli_Full %>%
    select(Split, Sentential_Connective, Mentioned_Object, Agent_Mood,
           Spatial_Order, Temporal_Order, 
           Top_Left, Top_Right, Bottom_Left, Bottom_Right,
           Familize_1_Object_Image, Familize_1_Object_Audio, Familize_2_Object_Image, Familize_2_Object_Audio,
           Test_Image, Test_Audio)
#################################### 3. Generate the Test Audios and Test_Stimuli File  ############################
Full_Information <- list(Audio_File_Information = Audio_File_Information, 
           Test_Images_Information = Test_Images_Information, 
           Test_Stimuli_Full = Test_Stimuli_Full, 
           Test_Stimuli = Test_Stimuli)
return(Full_Information)              
}

####################################################################################################################################################################
########  Conditional_Test_Image_Generator
####################################################################################################################################################################
## Notes: 
## This function requires 
## 1. A Root_Directory,
## 2. A Test_Images_Info File in the root directory with the following required columns: Object_Same_Image, Object_Different_Image, Spatial_Order, Test_Image
## 3. Three foulders in the Root_Directory:
##    a. Input_Objects_Folder: Objects used to concatenate the Test images
##    b. Input_Boxes_Foler: Boxes and the covers used to concatenate the test images.
##    c. Output_Test_Image_Folder: The folder used to store the created test images.

############ 0. Define the Function ################################################################################################
Conditional_Test_Image_Generator <- function(Test_Image_Row, Root_Directory, Input_Objects_Folder, Input_Boxes_Folder, Output_Test_Image_Folder, Test_Image_File){
suppressMessages(library(magick))
########## 1. Read Objects into R ####################################################################################################
Object_Same_File <- file.path(Root_Directory, Input_Objects_Folder, paste0(Test_Image_File[Test_Image_Row, "Object_Same_Image"], ".png"))
Object_Same <- magick::image_read(Object_Same_File)
Object_Same <- magick::image_resize(Object_Same, geometry = geometry_size_percent(width = 200/600 * 100))
Brand_Same  <- magick::image_resize(Object_Same, geometry = geometry_size_percent(width = 70/200 * 100))

Object_Different_File <- file.path(Root_Directory, Input_Objects_Folder, paste0(Test_Image_File[Test_Image_Row, "Object_Different_Image"], ".png"))
Object_Different <- magick::image_read(Object_Different_File)
Object_Different <- magick::image_resize(Object_Different, geometry = geometry_size_percent(width = 200/600 * 100))
Brand_Different  <- magick::image_resize(Object_Different, geometry = geometry_size_percent(width = 70/200 * 100))

########## 2. Read Boxes into R and Define offsets ####################################################################################################
Box_Open_File <- file.path(Root_Directory, Input_Boxes_Folder, "Box_Open.pdf")
Box_Open    <- magick::image_read_pdf(Box_Open_File,   density = 323)
Box_Open <- magick::image_transparent(Box_Open, color = "white") # Remove background
Box_Closed_File <- file.path(Root_Directory, Input_Boxes_Folder, "Box_Closed.pdf")
Box_Closed  <- magick::image_read_pdf(Box_Closed_File, density = 323)
Box_Closed <- magick::image_transparent(Box_Closed, color = "white")
Box_Cover_File <- file.path(Root_Directory, Input_Boxes_Folder, "Box_Cover.pdf")
Box_Cover   <- magick::image_read_pdf(Box_Cover_File,  density = 323)

Box_Open_Width   <- magick::image_info(Box_Open)$width   # Box_Open_Width  <- 320
Box_Open_Height   <- magick::image_info(Box_Open)$height # Box_Open_Height <- 240
Box_Closed_Height <- magick::image_info(Box_Closed)$height
Box_Cover_Height  <- magick::image_info(Box_Cover)$height
Brand_Same_Height <- magick::image_info(Brand_Same)$height

Box_Open_Left_Offset_X   <- 88 # Border_Space_X
Box_Open_Right_Offset_X  <- 1024 - Box_Open_Left_Offset_X  - Box_Open_Width
Box_Open_Top_Offset_Y    <- 45 # Border_Space_Y
Box_Open_Bottom_Offset_Y <- 768 - Box_Open_Top_Offset_Y - Box_Open_Height

Offset_Top_Left_Box_Open     <- paste0("+", Box_Open_Left_Offset_X,  "+", Box_Open_Top_Offset_Y)
Offset_Top_Right_Box_Open    <- paste0("+", Box_Open_Right_Offset_X, "+", Box_Open_Top_Offset_Y)
Offset_Bottom_Left_Box_Open  <- paste0("+", Box_Open_Left_Offset_X,  "+", Box_Open_Bottom_Offset_Y)
Offset_Bottom_Right_Box_Open <- paste0("+", Box_Open_Right_Offset_X, "+", Box_Open_Bottom_Offset_Y)

Height_Difference_Open_Closed <- Box_Open_Height - Box_Closed_Height
Offset_Top_Left_Box_Closed     <- paste0("+", Box_Open_Left_Offset_X,  "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Closed)
Offset_Top_Right_Box_Closed    <- paste0("+", Box_Open_Right_Offset_X, "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Closed)
Offset_Bottom_Left_Box_Closed  <- paste0("+", Box_Open_Left_Offset_X,  "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Closed)
Offset_Bottom_Right_Box_Closed <- paste0("+", Box_Open_Right_Offset_X, "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Closed)

########## 3. Define objects offsets ####################################################################################################
Object_x_Adjust <- 60
Object_y_Adjust <- 5
Offset_Object_Top_Left     <- paste0("+",  Box_Open_Left_Offset_X  + Object_x_Adjust, "+", Box_Open_Top_Offset_Y    + Object_y_Adjust)
Offset_Object_Top_Right    <- paste0("+",  Box_Open_Right_Offset_X + Object_x_Adjust, "+", Box_Open_Top_Offset_Y    + Object_y_Adjust)
Offset_Object_Bottom_Left  <- paste0("+",  Box_Open_Left_Offset_X  + Object_x_Adjust, "+", Box_Open_Bottom_Offset_Y + Object_y_Adjust)
Offset_Object_Bottom_Right <- paste0("+",  Box_Open_Right_Offset_X + Object_x_Adjust, "+", Box_Open_Bottom_Offset_Y + Object_y_Adjust)

Height_Difference_Open_Cover <- Box_Open_Height - Box_Cover_Height
Cover_Offset_Top_Left     <- paste0("+", Box_Open_Left_Offset_X,  "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Cover)
Cover_Offset_Top_Right    <- paste0("+", Box_Open_Right_Offset_X, "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Cover)
Cover_Offset_Bottom_Left  <- paste0("+", Box_Open_Left_Offset_X,  "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Cover)
Cover_Offset_Bottom_Right <- paste0("+", Box_Open_Right_Offset_X, "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Cover)

Height_Difference_Open_Brand <- Box_Cover_Height - Brand_Same_Height
Brand_x_Adjust <- 30
Brand_Offset_Top_Left     <- paste0("+", Box_Open_Left_Offset_X  + Brand_x_Adjust, "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Cover + Height_Difference_Open_Brand / 2)
Brand_Offset_Top_Right    <- paste0("+", Box_Open_Right_Offset_X + Brand_x_Adjust, "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Cover + Height_Difference_Open_Brand / 2)
Brand_Offset_Bottom_Left  <- paste0("+", Box_Open_Left_Offset_X  + Brand_x_Adjust, "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Cover + Height_Difference_Open_Brand / 2)
Brand_Offset_Bottom_Right <- paste0("+", Box_Open_Right_Offset_X  + Brand_x_Adjust, "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Cover + Height_Difference_Open_Brand / 2)

########## 4. Define annotation locations ####################################################################################################
Annotation_x_Adjust <- 180
Annotation_y_Adjust <- 15
Annotation_Offset_Top_Left     <- paste0("+", Box_Open_Left_Offset_X  + Annotation_x_Adjust, "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Cover + Height_Difference_Open_Brand / 2 + Annotation_y_Adjust)
Annotation_Offset_Top_Right    <- paste0("+", Box_Open_Right_Offset_X + Annotation_x_Adjust, "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Cover + Height_Difference_Open_Brand / 2 + Annotation_y_Adjust)
Annotation_Offset_Bottom_Left  <- paste0("+", Box_Open_Left_Offset_X  + Annotation_x_Adjust, "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Cover + Height_Difference_Open_Brand / 2 + Annotation_y_Adjust)
Annotation_Offset_Bottom_Right <- paste0("+", Box_Open_Right_Offset_X  + Annotation_x_Adjust, "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Cover + Height_Difference_Open_Brand / 2 + Annotation_y_Adjust)

########## 5. Combine All information into a Data frame ####################################################################################################
Image_Information <- data.frame(
Spatial_Order        = c("CS_DD_SD_SS", "SD_SS_CS_DD", "DD_CS_SS_SD", "SS_SD_DD_CS"),
Box_Top_Left         = c( "Box_Closed",    "Box_Open",    "Box_Open",    "Box_Open"),
Box_Top_Right        = c(   "Box_Open",    "Box_Open",  "Box_Closed",    "Box_Open"),
Box_Bottom_Left      = c(   "Box_Open",  "Box_Closed",    "Box_Open",    "Box_Open"),
Box_Bottom_Right     = c(   "Box_Open",    "Box_Open",    "Box_Open",  "Box_Closed"),
Box_Offset_Top_Left      = c(Offset_Top_Left_Box_Closed, Offset_Top_Left_Box_Open, Offset_Top_Left_Box_Open, Offset_Top_Left_Box_Open),
Box_Offset_Top_Right     = c(Offset_Top_Right_Box_Open, Offset_Top_Right_Box_Open, Offset_Top_Right_Box_Closed, Offset_Top_Right_Box_Open),
Box_Offset_Bottom_Left   = c(Offset_Bottom_Left_Box_Open, Offset_Bottom_Left_Box_Closed, Offset_Bottom_Left_Box_Open, Offset_Bottom_Left_Box_Open),
Box_Offset_Bottom_Right  = c(Offset_Bottom_Right_Box_Open, Offset_Bottom_Right_Box_Open, Offset_Bottom_Right_Box_Open, Offset_Bottom_Right_Box_Closed),
Object_Offset_DD = c(Offset_Object_Top_Right, Offset_Object_Bottom_Right, Offset_Object_Top_Left, Offset_Object_Bottom_Left),
Object_Offset_SD = c(Offset_Object_Bottom_Left, Offset_Object_Top_Left, Offset_Object_Bottom_Right, Offset_Object_Top_Right),
Object_Offset_SS = c(Offset_Object_Bottom_Right, Offset_Object_Top_Right, Offset_Object_Bottom_Left, Offset_Object_Top_Left), 
Brand_Top_Left     = c("Brand_Same",      "Brand_Different", "Brand_Different", "Brand_Same"),
Brand_Top_Right    = c("Brand_Different", "Brand_Same",      "Brand_Same",      "Brand_Different"),
Brand_Bottom_Left  = c("Brand_Different", "Brand_Same",      "Brand_Same",      "Brand_Different"),
Brand_Bottom_Right = c("Brand_Same",      "Brand_Different", "Brand_Different", "Brand_Same"),
stringsAsFactors = FALSE
)
rownames(Image_Information) <- Image_Information$Spatial_Order

########## 6. Combine different elements together ####################################################################################################
Condition <- Test_Image_File[Test_Image_Row, "Spatial_Order"]
`%>%` <- magrittr::`%>%`
Image_Add_Boxes <- magick::image_blank(width = 1024, height = 768, color = "lightgray") %>%
    magick::image_composite(eval(parse(text = Image_Information[Condition, "Box_Top_Left"])),     offset = Image_Information[Condition, "Box_Offset_Top_Left"]) %>%
    magick::image_composite(eval(parse(text = Image_Information[Condition, "Box_Top_Right"])),    offset = Image_Information[Condition, "Box_Offset_Top_Right"]) %>%
    magick::image_composite(eval(parse(text = Image_Information[Condition, "Box_Bottom_Left"])),  offset = Image_Information[Condition, "Box_Offset_Bottom_Left"]) %>%
    magick::image_composite(eval(parse(text = Image_Information[Condition, "Box_Bottom_Right"])), offset = Image_Information[Condition, "Box_Offset_Bottom_Right"])
Image_Add_Objects <- Image_Add_Boxes %>%
    magick::image_composite(Object_Different, offset = Image_Information[Condition, "Object_Offset_DD"]) %>%
    magick::image_composite(Object_Same,      offset = Image_Information[Condition, "Object_Offset_SD"]) %>%
    magick::image_composite(Object_Same,      offset = Image_Information[Condition, "Object_Offset_SS"])    
Image_Add_Covers <- Image_Add_Objects %>%
    magick::image_composite(Box_Cover, offset = Cover_Offset_Top_Left) %>%
    magick::image_composite(Box_Cover, offset = Cover_Offset_Top_Right) %>%
    magick::image_composite(Box_Cover, offset = Cover_Offset_Bottom_Left) %>%
    magick::image_composite(Box_Cover, offset = Cover_Offset_Bottom_Right)
Image_Add_Brands <- Image_Add_Covers %>%
    magick::image_composite(eval(parse(text = Image_Information[Condition, "Brand_Top_Left"])),     offset = Brand_Offset_Top_Left) %>%
    magick::image_composite(eval(parse(text = Image_Information[Condition, "Brand_Top_Right"])),    offset = Brand_Offset_Top_Right) %>%
    magick::image_composite(eval(parse(text = Image_Information[Condition, "Brand_Bottom_Left"])),  offset = Brand_Offset_Bottom_Left) %>%
    magick::image_composite(eval(parse(text = Image_Information[Condition, "Brand_Bottom_Right"])), offset = Brand_Offset_Bottom_Right)
Image_Add_Numbers <- Image_Add_Brands %>%
    magick::image_annotate("A", font = "Times", location = Annotation_Offset_Top_Left, size = 45) %>%
    magick::image_annotate("B", font = "Times", location = Annotation_Offset_Top_Right, size = 45) %>%
    magick::image_annotate("C", font = "Times", location = Annotation_Offset_Bottom_Left, size = 45) %>%
    magick::image_annotate("D", font = "Times", location = Annotation_Offset_Bottom_Right, size = 45)
# magick::image_browse(Image_Add_Numbers)
File_Path <- file.path(Root_Directory, Output_Test_Image_Folder)
dir.create(File_Path, showWarnings = FALSE)
print(Test_Image_File[Test_Image_Row, "Test_Image"])
magick::image_write(Image_Add_Numbers, file.path(File_Path, Test_Image_File[Test_Image_Row, "Test_Image"]))		
}

####################################################################################################################################################################
########  Conditional_Test_Audio_Generator
####################################################################################################################################################################
#### Notes: 
## 1. The root directory: Root_Directory
## 3. Two Files Stored in the Root Directory and their Required Columns:
##    a. Sentential_Compoents: Component_Image, Component_Audio
##    b. Test_Stimuli: Sentential_Connective, Mentioned_Object, Agent_Mood, Object_Same_Image, Object_Different_Image, Test_Audio, Test_Image
## 2. Two folders in the root directory: 
##    a. Input_Audio_Component_Folder: The Folder storing the audio elements that are going to be concatenated to form the test audios. 
##       The names of the components should be the same as the column of "Component_Audio" in the "Sentential_Components" file.
##    b. Output_Test_Audio_Folder (Created one if it does not exist): 

Conditional_Test_Audio_Generator <- function(Test_Stimuli_Row, Root_Directory, Input_Audio_Component_Folder,  Output_Test_Audio_Folder,  Sentential_Components_File, Test_Stimuli_File, Mentioned_Object_Expected_Length){
#Conditional_Test_Audio_Generator <- function(){
Sentential_Components <- read.csv(file.path(Root_Directory, paste0(Sentential_Components_File, ".csv")), header = TRUE, stringsAsFactors = FALSE)
Audio_Component_Input_Directory <- file.path(Root_Directory, Input_Audio_Component_Folder)
Sentential_Connectives <- data.frame(Because = c("Because", "Therefore", "Very"), IF = c("IF", "Then", "Will"))
Sentential_Connective_1 <- Sentential_Connectives[1, Test_Stimuli_File[Test_Stimuli_Row, "Sentential_Connective"]]
Sentential_Connective_1_Audio_Name <- Sentential_Components[Sentential_Components$Component_Image == Sentential_Connective_1, "Component_Audio"]
Sentential_Connective_1_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, paste0(Sentential_Connective_1_Audio_Name, ".wav")))
Box_In_Audio_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, "XiangZiLi.wav"))
IS_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, "Shi.wav"))
Mentioned_Object <- Test_Stimuli_File[Test_Stimuli_Row, grepl(paste0(Test_Stimuli_File[Test_Stimuli_Row, "Mentioned_Object"], "_Image"), colnames(Test_Stimuli_File))]
Mentioned_Object_Audio_Name <- Sentential_Components[Sentential_Components$Component_Image == Mentioned_Object, "Component_Audio"]
Mentioned_Object_Audio_File_0 <- tuneR::readWave(file.path(Audio_Component_Input_Directory, paste0(Mentioned_Object_Audio_Name, ".wav")))
Mentioned_Object_Audio_File_0_Length <- ((length(Mentioned_Object_Audio_File_0 @ left) / Mentioned_Object_Audio_File_0 @ samp.rate))
Mentioned_Object_Audio_Silence_Length <- Mentioned_Object_Expected_Length / 1000 - Mentioned_Object_Audio_File_0_Length
if (Mentioned_Object_Audio_Silence_Length > 0) {
      Mentioned_Object_Audio_Silence <- tuneR::silence(Mentioned_Object_Audio_Silence_Length, xunit = "time", pcm = TRUE, bit = 16)
      Mentioned_Object_Audio_File <- tuneR::bind(Mentioned_Object_Audio_File_0, Mentioned_Object_Audio_Silence)	
  } else {
      Mentioned_Object_Audio_File <- Mentioned_Object_Audio_File_0
  }
Sentential_Connective_2 <- Sentential_Connectives[2, Test_Stimuli_File[Test_Stimuli_Row, "Sentential_Connective"]]
Sentential_Connective_2_Audio_Name <- Sentential_Components[Sentential_Components$Component_Image == Sentential_Connective_2, "Component_Audio"]
Sentential_Connective_2_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, paste0(Sentential_Connective_2_Audio_Name, ".wav")))
Agent_Name_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, "XiaoMing.wav"))
Sentential_Connective_3 <- Sentential_Connectives[3, Test_Stimuli_File[Test_Stimuli_Row, "Sentential_Connective"]]
Sentential_Connective_3_Audio_Name <- Sentential_Components[Sentential_Components$Component_Image == Sentential_Connective_3, "Component_Audio"]
Sentential_Connective_3_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, paste0(Sentential_Connective_3_Audio_Name, ".wav")))
Agent_Mood <- Test_Stimuli_File[Test_Stimuli_Row, "Agent_Mood"]
Agent_Mood_Audio_Name <- Sentential_Components[Sentential_Components$Component_Image == Agent_Mood, "Component_Audio"]
Agent_Mood_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, paste0(Agent_Mood_Audio_Name, ".wav")))
Test_Sentence_Audio <- tuneR::bind(
    Sentential_Connective_1_Audio_File, Box_In_Audio_Audio_File, IS_Audio_File,  Mentioned_Object_Audio_File,
    Sentential_Connective_2_Audio_File, Agent_Name_Audio_File, Sentential_Connective_3_Audio_File, Agent_Mood_Audio_File)
Test_Audios_Output_Directory <- file.path(Root_Directory, Output_Test_Audio_Folder)
dir.create(Test_Audios_Output_Directory, showWarnings = FALSE)
Test_Audios_Output_Name <- file.path(Test_Audios_Output_Directory, Test_Stimuli_File[Test_Stimuli_Row, "Test_Audio"])
print(Test_Stimuli_File[Test_Stimuli_Row, "Test_Audio"])
tuneR::writeWave(Test_Sentence_Audio, Test_Audios_Output_Name)
#tuneR::play(Test_Sentence_Audio, player = '/usr/bin/afplay')
}

####################################################################################################################################################################
########  Conditional_Information_Generator
###################################################################################################################################################################
Conditional_Full <- function(
Root_Directory = "~/Desktop/Conditional_New",
Sentential_Components_File = "Important_Originals/Sentential_Components",
Input_Audio_Component_Folder = "Important_Originals/Objects_Audios",
Input_Objects_Folder = "Important_Originals/Objects_Images",
Input_Boxes_Folder = "Important_Originals/Boxes",
Output_Test_Image_Folder = "Test_Images",
Output_Test_Audio_Folder = "Test_Audios",
Mentioned_Object_Expected_Length = 1400
){
Conditional_Test_Stimuli_Data <- Conditional_Information_Generator(Root_Directory, Input_Audio_Component_Folder, Sentential_Components_File)
Audio_File_Information <- Conditional_Test_Stimuli_Data $Audio_File_Information
print("========== Audio Info ==========")
print(Audio_File_Information)
Test_Image_File <- Conditional_Test_Stimuli_Data $ Test_Images_Information
Test_Stimuli_File <- Conditional_Test_Stimuli_Data $ Test_Stimuli_Full
Test_Stimuli <- Conditional_Test_Stimuli_Data $ Test_Stimuli
write.csv(Test_Stimuli, file.path(Root_Directory, "Test_Stimuli.csv"), row.names = FALSE)
write.table(Test_Stimuli, file.path(Root_Directory, "Test_Stimuli.txt"), sep = "\t", row.names = FALSE)

Conditional_Test_Image_Generator_Function <- function(i) Conditional_Test_Image_Generator(Test_Image_Row = i, 
    Root_Directory, Input_Objects_Folder, Input_Boxes_Folder, Output_Test_Image_Folder, Test_Image_File)
print("========== Test Images ==========")
invisible(sapply(1:nrow(Test_Image_File), Conditional_Test_Image_Generator_Function))

Conditional_Test_Audio_Generator_Function <- function(i) Conditional_Test_Audio_Generator(Test_Stimuli_Row = i, 
    Root_Directory, Input_Audio_Component_Folder,  Output_Test_Audio_Folder, Sentential_Components_File, Test_Stimuli_File, Mentioned_Object_Expected_Length)
print("========== Test Audios ==========")
invisible(sapply(1:nrow(Test_Stimuli_File), Conditional_Test_Audio_Generator_Function))	
}


