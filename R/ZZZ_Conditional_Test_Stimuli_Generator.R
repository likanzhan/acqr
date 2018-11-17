#' @export
################################################################################
########  Conditional_Information_Generator
################################################################################

Conditional_Information_Generator <- function(
    Root_Directory, Input_Audio_Component_Folder, Sentential_Components_File
){

############## 1. Explore the length of the Audio Files #########################
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
    Other_Elements_Information <- merge(
        Other_Elements_Audio, Other_Elements_Name, by = "File_Name", all.y = TRUE)
    Other_Elements_Information <- Other_Elements_Information[
        order(Other_Elements_Information$Position, 
              Other_Elements_Information$Condition, decreasing = c(FALSE, TRUE)), ]
    Other_Elements_Information$File_Length <- 
        round(Other_Elements_Information$File_Length * 1000)

### d. Combine the Information Together ###########################
    Audio_File_Information <- list(
        Object_Audio_File = Object_Audio_File,
        Object_Audio_File_Max_Length = Object_Audio_File_Max_Length,
        Other_Elements_Information = Other_Elements_Information)

############### 2. Generate the Test Images Information ########################
################################################################################

    `%>%` <- magrittr::`%>%`
    suppressMessages(library(dplyr))
    Sentential_Components_File_Directory <- 
        file.path(Root_Directory, paste0(Sentential_Components_File, ".csv"))
    Sentential_Components <- 
        read.csv(Sentential_Components_File_Directory, header = TRUE, stringsAsFactors = FALSE)
    Audio_File_Info$Component_Audio  <-  gsub(".wav", "", Audio_File_Info$File_Name)
    Audio_File_Info <- Audio_File_Info[, names(Audio_File_Info) != "File_Name"]
    Audio_File_Info $ File_Length <- round(Audio_File_Info$File_Length * 1000)
    Test_Images_Information <- Sentential_Components %>% 
        left_join(Audio_File_Info, by = "Component_Audio") %>%
        filter(Component_Category != "Z_Shared") %>%
        filter(!Component_Image %in% c("FengChe", "XiaoDao", "JianDao", "ZhiLou")) %>%
        rename(Object_Same_Audio = Component_Audio, 
               Object_Same_Audio_Length = File_Length, 
               Object_Same_Image = Component_Image) %>%
        group_by(Component_Category) %>%
    ## Search_Sample_Seed to find a random seed to make sure that no element is in the same  position
        mutate(Object_Different_Image = {set.seed(acqr::Search_Sample_Seed(Object_Same_Image)); 
        	                             sample(Object_Same_Image)}) %>% 
        ungroup() %>% 
        mutate(Object_Different_Audio = pull(.[match(Object_Different_Image, Object_Same_Image), 
                                               "Object_Same_Audio"])) %>%
        mutate(Object_Different_Audio_Length = pull(.[match(Object_Different_Image, Object_Same_Image), 
                                               "Object_Same_Audio_Length"])) %>%                                               
        mutate(Spatial_Order = 
            rep(c("CS_DD_SD_SS", "SD_SS_CS_DD", "DD_CS_SS_SD", "SS_SD_DD_CS"), 84 / 4)) %>%
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
    # Remove the illicite Condition, i.e., Because-Different-Sad   
    Experimental_Manipulation <- Experimental_Manipulation[-5, ] 
    Test_Stimuli_Full <- Experimental_Manipulation %>%
        merge(Test_Images_Information) %>%
        mutate(Split = rep(as.vector(t(acqr::Create_Latin_Square_Matrix(7))), 6)) %>%
        mutate(Familiarize_1_Object_Image = sapply(1:nrow(.), 
            function(i) paste0(.[i, grepl(paste0(.[i, "Temporal_Order"], "_Image"), colnames(.))], ".png")
            )) %>%
        mutate(Familiarize_1_Object_Audio = sapply(1:nrow(.), 
            function(i) paste0(.[i, grepl(paste0(.[i, "Temporal_Order"], "_Audio$"), colnames(.))], ".wav")
            )) %>%
        mutate(Familiarize_1_Object_Audio_Length = sapply(1:nrow(.), 
            function(i) .[i, grepl(paste0(.[i, "Temporal_Order"], "_Audio_Length$"), colnames(.))]
            )) %>%            
        mutate(Familiarize_2_Object_Image = sapply(1:nrow(.), 
            function(i) paste0(.[i, grepl(paste0("^(?=Object)(?!.*", .[i, "Temporal_Order"], ")(?=.*Image)"), 
                                          colnames(.), perl = TRUE)], ".png")
            )) %>%
        mutate(Familiarize_2_Object_Audio = sapply(1:nrow(.), 
            function(i) paste0(.[i, grepl(paste0("^(?=Object)(?!.*", .[i, "Temporal_Order"], ")(?=.*Audio$)"), 
                                          colnames(.), perl = TRUE)], ".wav")
            )) %>%
        mutate(Familiarize_2_Object_Audio_Length = sapply(1:nrow(.), 
            function(i) .[i, grepl(paste0("^(?=Object)(?!.*", .[i, "Temporal_Order"], ")(?=.*Audio_Length$)"), 
                                          colnames(.), perl = TRUE)]
            )) %>%            
        mutate(Test_Audio = paste0(abbreviate(paste(substr(Test_Image, 1, 2), 
                                   Sentential_Connective, Mentioned_Object, Agent_Mood), 6), ".wav")) %>%
        mutate(Top_Left     = substr(Spatial_Order, 1, 2),
               Top_Right    = substr(Spatial_Order, 4, 5),
               Bottom_Left  = substr(Spatial_Order, 7, 8),
               Bottom_Right = substr(Spatial_Order, 10, 11))
        Test_Stimuli <- Test_Stimuli_Full %>%
            select(Split, Sentential_Connective, Mentioned_Object, Agent_Mood,
                   Spatial_Order, Temporal_Order, 
                   Top_Left, Top_Right, Bottom_Left, Bottom_Right,
                   Familiarize_1_Object_Image, Familiarize_1_Object_Audio, Familiarize_1_Object_Audio_Length, 
                   Familiarize_2_Object_Image, Familiarize_2_Object_Audio, Familiarize_2_Object_Audio_Length,
                   Test_Image, Test_Audio)
#################################### 3. Generate the Test Audios and Test_Stimuli File  ############################
    Full_Information <- list(Audio_File_Information = Audio_File_Information, 
               Test_Images_Information = Test_Images_Information, 
               Test_Stimuli_Full = Test_Stimuli_Full, 
               Test_Stimuli = Test_Stimuli)
    return(Full_Information)              
}

################################################################################
########  Conditional_Test_Image_Generator
################################################################################
## Notes: 
## This function requires 
## 1. A Root_Directory,
## 2. A Test_Images_Info File in the root directory with the following required columns: 
##    Object_Same_Image, Object_Different_Image, Spatial_Order, Test_Image
## 3. Three foulders in the Root_Directory:
##    a. Input_Objects_Folder: Objects used to concatenate the Test images
##    b. Input_Boxes_Foler: Boxes and the covers used to concatenate the test images.
##    c. Output_Test_Image_Folder: The folder used to store the created test images.

Conditional_Test_Image_Generator <- function(
    Test_Image_Row, Test_Image_File, Root_Directory, Input_Objects_Folder, Input_Boxes_Folder, Output_Test_Image_Folder
){
    Object_Same_Image_Name      <- Test_Image_File[Test_Image_Row, "Object_Same_Image"]
    Object_Different_Image_Name <- Test_Image_File[Test_Image_Row, "Object_Different_Image"]
    Spatial_Order               <- Test_Image_File[Test_Image_Row, "Spatial_Order"] # "CS_DD_SD_SS", "SD_SS_CS_DD", "DD_CS_SS_SD", "SS_SD_DD_CS"
    Test_Image_Name             <- Test_Image_File[Test_Image_Row, "Test_Image"]
    Cnd_Tst_Img_Gnr(Root_Directory, Input_Objects_Folder, Input_Boxes_Folder, Output_Test_Image_Folder,
         Object_Same_Image_Name, Object_Different_Image_Name, Spatial_Order, Test_Image_Name)
}
########## 0. Define the varibles ###################################################################################

Cnd_Tst_Img_Gnr <- function(
Root_Directory, Input_Objects_Folder, Input_Boxes_Folder, Output_Test_Image_Folder,
Object_Same_Image_Name, Object_Different_Image_Name, Spatial_Order, Test_Image_Name
){    
########## 0. Define the varibles ###################################################################################
    suppressMessages(library(magick))
    
    Image_Template_Size <- c(1024, 768)
        
    Box_Open_Left_Offset_X   <- 88 # Border_Space_X
    Box_Open_Top_Offset_Y    <- 45 # Border_Space_Y
    
    Object_Adjust_X     <- 60
    Object_Adjust_Y     <- 5 
    Annotation_Adjust_X <- 180
    Annotation_Adjust_Y <- 15
    Brand_Adjust_X      <- 30
      
########## 1. Read Objects into R ###################################################################################  
    Object_Same_File <- file.path(Root_Directory, Input_Objects_Folder, paste0(Object_Same_Image_Name, ".png"))
    Object_Same      <- magick::image_read(Object_Same_File)
    Object_Same      <- magick::image_resize(Object_Same, 
                                             geometry = geometry_size_percent(width = 200 / 600 * Image_Template_Size[1] / 10.24))
    Brand_Same       <- magick::image_resize(Object_Same, 
                                             geometry = geometry_size_percent(width = 70 / 200 * Image_Template_Size[1] / 10.24))

    Object_Different_File <- file.path(Root_Directory, Input_Objects_Folder, paste0(Object_Different_Image_Name, ".png"))                                     
    Object_Different <- magick::image_read(Object_Different_File)
    Object_Different <- magick::image_resize(Object_Different, 
                                             geometry = geometry_size_percent(width = 200 / 600 * Image_Template_Size[1] / 10.24))
    Brand_Different  <- magick::image_resize(Object_Different, 
                                             geometry = geometry_size_percent(width = 70 / 200 * Image_Template_Size[1] / 10.24))

########## 2. Read Boxes into R and Define offsets ###################################################################  
    Box_Open_File   <- file.path(Root_Directory, Input_Boxes_Folder, "Box_Open.pdf")
    Box_Closed_File <- file.path(Root_Directory, Input_Boxes_Folder, "Box_Closed.pdf")
    Box_Cover_File  <- file.path(Root_Directory, Input_Boxes_Folder, "Box_Cover.pdf")
  
    Box_Open        <- magick::image_read_pdf(Box_Open_File,   density = 323)
    Box_Open        <- magick::image_transparent(Box_Open, color = "white") # Remove background
    Box_Closed      <- magick::image_read_pdf(Box_Closed_File, density = 323)
    Box_Closed      <- magick::image_transparent(Box_Closed, color = "white")
    Box_Cover       <- magick::image_read_pdf(Box_Cover_File,  density = 323)

    Box_Open_Width   <- magick::image_info(Box_Open)$width   # Box_Open_Width  <- 320
    Box_Open_Height   <- magick::image_info(Box_Open)$height # Box_Open_Height <- 240
    Box_Closed_Height <- magick::image_info(Box_Closed)$height
    Box_Cover_Height  <- magick::image_info(Box_Cover)$height
    Brand_Same_Height <- magick::image_info(Brand_Same)$height
    
    Box_Open_Right_Offset_X  <- Image_Template_Size[1] - Box_Open_Left_Offset_X  - Box_Open_Width
    Box_Open_Bottom_Offset_Y <- Image_Template_Size[2] - Box_Open_Top_Offset_Y - Box_Open_Height

    Offset_Top_Left_Box_Open     <- paste0("+", Box_Open_Left_Offset_X,  "+", Box_Open_Top_Offset_Y)
    Offset_Top_Right_Box_Open    <- paste0("+", Box_Open_Right_Offset_X, "+", Box_Open_Top_Offset_Y)
    Offset_Bottom_Left_Box_Open  <- paste0("+", Box_Open_Left_Offset_X,  "+", Box_Open_Bottom_Offset_Y)
    Offset_Bottom_Right_Box_Open <- paste0("+", Box_Open_Right_Offset_X, "+", Box_Open_Bottom_Offset_Y)

    Height_Difference_Open_Closed <- Box_Open_Height - Box_Closed_Height
    Offset_Top_Left_Box_Closed     <- paste0("+", Box_Open_Left_Offset_X,  
                                             "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Closed)
    Offset_Top_Right_Box_Closed    <- paste0("+", Box_Open_Right_Offset_X, 
                                             "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Closed)
    Offset_Bottom_Left_Box_Closed  <- paste0("+", Box_Open_Left_Offset_X,  
                                             "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Closed)
    Offset_Bottom_Right_Box_Closed <- paste0("+", Box_Open_Right_Offset_X, 
                                             "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Closed)

########## 3. Define objects offsets #########################################################################
    Offset_Object_Top_Left     <- paste0("+",  Box_Open_Left_Offset_X   + Object_Adjust_X, 
                                         "+",  Box_Open_Top_Offset_Y    + Object_Adjust_Y)
    Offset_Object_Top_Right    <- paste0("+",  Box_Open_Right_Offset_X  + Object_Adjust_X, 
                                         "+",  Box_Open_Top_Offset_Y    + Object_Adjust_Y)
    Offset_Object_Bottom_Left  <- paste0("+",  Box_Open_Left_Offset_X   + Object_Adjust_X, 
                                         "+",  Box_Open_Bottom_Offset_Y + Object_Adjust_Y)
    Offset_Object_Bottom_Right <- paste0("+",  Box_Open_Right_Offset_X  + Object_Adjust_X, 
                                         "+",  Box_Open_Bottom_Offset_Y + Object_Adjust_Y)

    Height_Difference_Open_Cover <- Box_Open_Height - Box_Cover_Height
    Cover_Offset_Top_Left     <- paste0("+", Box_Open_Left_Offset_X,  
                                        "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Cover)
    Cover_Offset_Top_Right    <- paste0("+", Box_Open_Right_Offset_X, 
                                        "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Cover)
    Cover_Offset_Bottom_Left  <- paste0("+", Box_Open_Left_Offset_X,  
                                        "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Cover)
    Cover_Offset_Bottom_Right <- paste0("+", Box_Open_Right_Offset_X, 
                                        "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Cover)

    Height_Difference_Open_Brand <- Box_Cover_Height - Brand_Same_Height
    Brand_Offset_Top_Left     <- paste0("+", Box_Open_Left_Offset_X   + Brand_Adjust_X,
                                        "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Cover + 
                                             Height_Difference_Open_Brand / 2)
    Brand_Offset_Top_Right    <- paste0("+", Box_Open_Right_Offset_X  + Brand_Adjust_X, 
                                        "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Cover + 
                                             Height_Difference_Open_Brand / 2)
    Brand_Offset_Bottom_Left  <- paste0("+", Box_Open_Left_Offset_X   + Brand_Adjust_X, 
                                        "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Cover + 
                                             Height_Difference_Open_Brand / 2)
    Brand_Offset_Bottom_Right <- paste0("+", Box_Open_Right_Offset_X  + Brand_Adjust_X, 
                                        "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Cover + 
                                             Height_Difference_Open_Brand / 2)

########## 4. Define annotation locations ####################################################################
    Annotation_Offset_Top_Left     <- paste0("+", Box_Open_Left_Offset_X   + Annotation_Adjust_X, 
                                             "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Cover + 
                                                  Height_Difference_Open_Brand / 2 + Annotation_Adjust_Y)
    Annotation_Offset_Top_Right    <- paste0("+", Box_Open_Right_Offset_X  + Annotation_Adjust_X, 
                                             "+", Box_Open_Top_Offset_Y    + Height_Difference_Open_Cover + 
                                                  Height_Difference_Open_Brand / 2 + Annotation_Adjust_Y)
    Annotation_Offset_Bottom_Left  <- paste0("+", Box_Open_Left_Offset_X   + Annotation_Adjust_X, 
                                             "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Cover + 
                                                  Height_Difference_Open_Brand / 2 + Annotation_Adjust_Y)
    Annotation_Offset_Bottom_Right <- paste0("+", Box_Open_Right_Offset_X  + Annotation_Adjust_X, 
                                             "+", Box_Open_Bottom_Offset_Y + Height_Difference_Open_Cover + 
                                                  Height_Difference_Open_Brand / 2 + Annotation_Adjust_Y)

########## 5. Combine All information into a Data frame #####################################################
    Image_Information <- data.frame(
        Spatial_Order        = c("CS_DD_SD_SS", "SD_SS_CS_DD", "DD_CS_SS_SD", "SS_SD_DD_CS"),
        Box_Top_Left         = c( "Box_Closed",    "Box_Open",    "Box_Open",    "Box_Open"),
        Box_Top_Right        = c(   "Box_Open",    "Box_Open",  "Box_Closed",    "Box_Open"),
        Box_Bottom_Left      = c(   "Box_Open",  "Box_Closed",    "Box_Open",    "Box_Open"),
        Box_Bottom_Right     = c(   "Box_Open",    "Box_Open",    "Box_Open",  "Box_Closed"),
        Box_Offset_Top_Left      = c(Offset_Top_Left_Box_Closed, Offset_Top_Left_Box_Open, 
                                     Offset_Top_Left_Box_Open, Offset_Top_Left_Box_Open),
        Box_Offset_Top_Right     = c(Offset_Top_Right_Box_Open, Offset_Top_Right_Box_Open, 
                                     Offset_Top_Right_Box_Closed, Offset_Top_Right_Box_Open),
        Box_Offset_Bottom_Left   = c(Offset_Bottom_Left_Box_Open, Offset_Bottom_Left_Box_Closed, 
                                     Offset_Bottom_Left_Box_Open, Offset_Bottom_Left_Box_Open),
        Box_Offset_Bottom_Right  = c(Offset_Bottom_Right_Box_Open, Offset_Bottom_Right_Box_Open, 
                                     Offset_Bottom_Right_Box_Open, Offset_Bottom_Right_Box_Closed),
        Object_Offset_DD = c(Offset_Object_Top_Right, Offset_Object_Bottom_Right, 
                             Offset_Object_Top_Left, Offset_Object_Bottom_Left),
        Object_Offset_SD = c(Offset_Object_Bottom_Left, Offset_Object_Top_Left,
                             Offset_Object_Bottom_Right, Offset_Object_Top_Right),
        Object_Offset_SS = c(Offset_Object_Bottom_Right, Offset_Object_Top_Right, 
                             Offset_Object_Bottom_Left, Offset_Object_Top_Left), 
        Brand_Top_Left     = c("Brand_Same",      "Brand_Different", "Brand_Different", "Brand_Same"),
        Brand_Top_Right    = c("Brand_Different", "Brand_Same",      "Brand_Same",      "Brand_Different"),
        Brand_Bottom_Left  = c("Brand_Different", "Brand_Same",      "Brand_Same",      "Brand_Different"),
        Brand_Bottom_Right = c("Brand_Same",      "Brand_Different", "Brand_Different", "Brand_Same"),
        stringsAsFactors = FALSE)
    rownames(Image_Information) <- Image_Information$Spatial_Order

########## 6. Combine different elements together ################################################################
    `%>%` <- magrittr::`%>%`
    Image_Add_Boxes <- magick::image_blank(
            width = Image_Template_Size[1], height = Image_Template_Size[2], color = "WhiteSmoke") %>%
        magick::image_composite(eval(parse(text = Image_Information[Spatial_Order, "Box_Top_Left"])),     
            offset = Image_Information[Spatial_Order, "Box_Offset_Top_Left"]) %>%
        magick::image_composite(eval(parse(text = Image_Information[Spatial_Order, "Box_Top_Right"])),    
            offset = Image_Information[Spatial_Order, "Box_Offset_Top_Right"]) %>%
        magick::image_composite(eval(parse(text = Image_Information[Spatial_Order, "Box_Bottom_Left"])),  
            offset = Image_Information[Spatial_Order, "Box_Offset_Bottom_Left"]) %>%
        magick::image_composite(eval(parse(text = Image_Information[Spatial_Order, "Box_Bottom_Right"])), 
            offset = Image_Information[Spatial_Order, "Box_Offset_Bottom_Right"])
    Image_Add_Objects <- Image_Add_Boxes %>%
        magick::image_composite(Object_Different, offset = Image_Information[Spatial_Order, "Object_Offset_DD"]) %>%
        magick::image_composite(Object_Same,      offset = Image_Information[Spatial_Order, "Object_Offset_SD"]) %>%
        magick::image_composite(Object_Same,      offset = Image_Information[Spatial_Order, "Object_Offset_SS"])    
    Image_Add_Covers <- Image_Add_Objects %>%
        magick::image_composite(Box_Cover, offset = Cover_Offset_Top_Left) %>%
        magick::image_composite(Box_Cover, offset = Cover_Offset_Top_Right) %>%
        magick::image_composite(Box_Cover, offset = Cover_Offset_Bottom_Left) %>%
        magick::image_composite(Box_Cover, offset = Cover_Offset_Bottom_Right)
    Image_Add_Brands <- Image_Add_Covers %>%
        magick::image_composite(eval(parse(text = Image_Information[Spatial_Order, "Brand_Top_Left"])),     
                                offset = Brand_Offset_Top_Left) %>%
        magick::image_composite(eval(parse(text = Image_Information[Spatial_Order, "Brand_Top_Right"])),    
                                offset = Brand_Offset_Top_Right) %>%
        magick::image_composite(eval(parse(text = Image_Information[Spatial_Order, "Brand_Bottom_Left"])),  
                                offset = Brand_Offset_Bottom_Left) %>%
        magick::image_composite(eval(parse(text = Image_Information[Spatial_Order, "Brand_Bottom_Right"])), 
                                offset = Brand_Offset_Bottom_Right)
    Image_Add_Numbers <- Image_Add_Brands %>%
        magick::image_annotate("A", font = "Times", location = Annotation_Offset_Top_Left, size = 45) %>%
        magick::image_annotate("B", font = "Times", location = Annotation_Offset_Top_Right, size = 45) %>%
        magick::image_annotate("C", font = "Times", location = Annotation_Offset_Bottom_Left, size = 45) %>%
        magick::image_annotate("D", font = "Times", location = Annotation_Offset_Bottom_Right, size = 45)
    # magick::image_browse(Image_Add_Numbers)
    File_Path <- file.path(Root_Directory, Output_Test_Image_Folder)
    dir.create(File_Path, showWarnings = FALSE)
    print(Test_Image_Name)
    magick::image_write(Image_Add_Numbers, file.path(File_Path, Test_Image_Name), depth = 16, format = "jpeg", quality = 100)		
}

################################################################################
########  Conditional_Test_Audio_Generator
################################################################################
#### Notes: 
## 1. The root directory: Root_Directory
## 2. Two Files Stored in the Root Directory and their Required Columns:
##    a. Sentential_Compoents: Component_Image, Component_Audio
##    b. Test_Stimuli: Sentential_Connective, Mentioned_Object, Agent_Mood, Object_Same_Image, 
##      Object_Different_Image, Test_Audio, Test_Image
## 3. Two folders in the root directory: 
##    a. Input_Audio_Component_Folder: The Folder storing the audio elements 
##       that are going to be concatenated to form the test audios. 
##       The names of the components should be the same as the column of 
##       "Component_Audio" in the "Sentential_Components" file.
##    b. Output_Test_Audio_Folder (Created one if it does not exist): 

Conditional_Test_Audio_Generator <- function(
    Test_Stimuli_Row, Test_Stimuli_File, 
    Root_Directory, Input_Audio_Component_Folder,  Output_Test_Audio_Folder,  
    Sentential_Components_File, Mentioned_Object_Expected_Length
){  
    Mentioned_Object_Collumn <- grepl(
        paste0(Test_Stimuli_File[Test_Stimuli_Row, "Mentioned_Object"], "_Image"), colnames(Test_Stimuli_File))
    Selected_Sentential_Connective <- Test_Stimuli_File[Test_Stimuli_Row, "Sentential_Connective"] # IF, Because
    Mentioned_Object               <- Test_Stimuli_File[Test_Stimuli_Row,  Mentioned_Object_Collumn]        
    Agent_Mood                     <- Test_Stimuli_File[Test_Stimuli_Row, "Agent_Mood"]
    Test_Audio_Name                <- Test_Stimuli_File[Test_Stimuli_Row, "Test_Audio"]
    Cnd_Tst_Aud_Gnr(
    Selected_Sentential_Connective, Mentioned_Object, Agent_Mood, Test_Audio_Name,
    Root_Directory, Input_Audio_Component_Folder,  Output_Test_Audio_Folder,  
    Sentential_Components_File, Mentioned_Object_Expected_Length)
}   
Cnd_Tst_Aud_Gnr <- function(
    Selected_Sentential_Connective, Mentioned_Object, Agent_Mood, Test_Audio_Name,
    Root_Directory, Input_Audio_Component_Folder,  Output_Test_Audio_Folder,  
    Sentential_Components_File, Mentioned_Object_Expected_Length
){ 
    ####
	Sentential_Components_File_Path <- file.path(Root_Directory, paste0(Sentential_Components_File, ".csv"))
    Sentential_Components <- read.csv(Sentential_Components_File_Path, header = TRUE, stringsAsFactors = FALSE)
    Audio_Component_Input_Directory <- file.path(Root_Directory, Input_Audio_Component_Folder) 
    Sentential_Connectives <- data.frame(
        Because = c("Because", "Therefore", "Very"), 
        IF      = c("IF",           "Then", "Will"))       
                                  
    #### Element 1 #### 
    Sentential_Connective_1 <- Sentential_Connectives[1, Selected_Sentential_Connective]
    Sentential_Connective_1_Audio_Name <- 
        Sentential_Components[Sentential_Components$Component_Image == Sentential_Connective_1, "Component_Audio"]
    Sentential_Connective_1_file_path <- file.path(
        Audio_Component_Input_Directory, paste0(Sentential_Connective_1_Audio_Name, ".wav"))         
    Sentential_Connective_1_Audio_File <- tuneR::readWave(Sentential_Connective_1_file_path)
    
    #### Element 2 ####
    Box_In_Audio_Audio_File_Path <- file.path(Audio_Component_Input_Directory, "XiangZiLi.wav")
    Box_In_Audio_Audio_File <- tuneR::readWave(Box_In_Audio_Audio_File_Path)
    
    #### Element 3 ####
    IS_Audio_File_Path <- file.path(Audio_Component_Input_Directory, "Shi.wav")
    IS_Audio_File <- tuneR::readWave(IS_Audio_File_Path)
    
    #### Element 4 ####
    Mentioned_Object_Audio_Name <- 
        Sentential_Components[Sentential_Components$Component_Image == Mentioned_Object, "Component_Audio"]
    Mentioned_Object_File_Path <- file.path(Audio_Component_Input_Directory, paste0(Mentioned_Object_Audio_Name, ".wav"))
    Mentioned_Object_Audio_File_0 <- tuneR::readWave(Mentioned_Object_File_Path)
    Mentioned_Object_Audio_File_0_Length <- 
        ((length(Mentioned_Object_Audio_File_0 @ left) / Mentioned_Object_Audio_File_0 @ samp.rate))
    Mentioned_Object_Audio_Silence_Length <- 
        Mentioned_Object_Expected_Length / 1000 - Mentioned_Object_Audio_File_0_Length
    if (Mentioned_Object_Audio_Silence_Length > 0) {
          Mentioned_Object_Audio_Silence <- tuneR::silence(
              Mentioned_Object_Audio_Silence_Length, xunit = "time", pcm = TRUE, bit = 16)
          Mentioned_Object_Audio_File <- tuneR::bind(Mentioned_Object_Audio_File_0, Mentioned_Object_Audio_Silence)	
    } else {
          Mentioned_Object_Audio_File <- Mentioned_Object_Audio_File_0
    }
    
    #### Element 5 ####
    Sentential_Connective_2 <- Sentential_Connectives[2, Selected_Sentential_Connective]
    Sentential_Connective_2_Audio_Name <- 
        Sentential_Components[Sentential_Components$Component_Image == Sentential_Connective_2, "Component_Audio"]

    Sentential_Connective_2_File_Path <- file.path(Audio_Component_Input_Directory, paste0(Sentential_Connective_2_Audio_Name, ".wav"))        
    Sentential_Connective_2_Audio_File <- tuneR::readWave(Sentential_Connective_2_File_Path)
    
    #### Element 6 ####
    Agent_Name_File_Path <- file.path(Audio_Component_Input_Directory, "XiaoMing.wav")
    Agent_Name_Audio_File <- tuneR::readWave(Agent_Name_File_Path)
    
    #### Element 7 ####
    Sentential_Connective_3 <- Sentential_Connectives[3, Selected_Sentential_Connective]
    Sentential_Connective_3_Audio_Name <- 
        Sentential_Components[Sentential_Components$Component_Image == Sentential_Connective_3, "Component_Audio"]
    Sentential_Connective_3_File_Path <- file.path(Audio_Component_Input_Directory, paste0(Sentential_Connective_3_Audio_Name, ".wav"))        
    Sentential_Connective_3_Audio_File <- tuneR::readWave(Sentential_Connective_3_File_Path)
    
    #### Element 8 ####
    Agent_Mood_Audio_Name <- Sentential_Components[Sentential_Components$Component_Image == Agent_Mood, "Component_Audio"]
    Agent_Mood_File_Path <- file.path(Audio_Component_Input_Directory, paste0(Agent_Mood_Audio_Name, ".wav"))
    Agent_Mood_Audio_File <- tuneR::readWave(Agent_Mood_File_Path) 
    
    #### Combine all elements together ####
    Test_Sentence_Audio <- tuneR::bind(
        Sentential_Connective_1_Audio_File, 
        Box_In_Audio_Audio_File, 
        IS_Audio_File,  
        Mentioned_Object_Audio_File,
        Sentential_Connective_2_Audio_File, 
        Agent_Name_Audio_File, 
        Sentential_Connective_3_Audio_File, 
        Agent_Mood_Audio_File)
    Test_Audios_Output_Directory <- file.path(Root_Directory, Output_Test_Audio_Folder)
    dir.create(Test_Audios_Output_Directory, showWarnings = FALSE)
    Test_Audios_Output_Name <- file.path(Test_Audios_Output_Directory, Test_Audio_Name)
    print(Test_Audio_Name)
    tuneR::writeWave(Test_Sentence_Audio, Test_Audios_Output_Name, extensible = FALSE)
    #tuneR::play(Test_Sentence_Audio, player = '/usr/bin/afplay')
}

################################################################################
########  Add IAs on a sample test image
################################################################################
Draw_IA_on_Sample_Image <- function(
    Sample_Image_Name = "00_Example_Image.png",
    Root_Directory = "~/Desktop/Conditional_New",
    Input_Sample_Image_Folder = "Test_Images",
    Output_Sample_Image_Folder = "Important_Information/Experiment_Interest_Areas",
    Output_Sample_Image_Name = "Test_Image_Example.png"
){
	Input_Test_Image_File  = file.path(Root_Directory, Input_Sample_Image_Folder, Sample_Image_Name)
	Output_Directory <- file.path(Root_Directory, Output_Sample_Image_Folder, Output_Sample_Image_Name)
    suppressMessages(library(magick))    
    ###### Define length and positions
    Screen_Resolution <- c(1024, 768)   # Width, height
    Area_Size         <- c(320,  240)   # width, height
    Margin_Space      <- c(88,    45)   # left/right, top/bottom
    Middle_Space     <- Screen_Resolution - Area_Size * 2 - Margin_Space * 2
    vlength  <- c(0, Margin_Space[1], Area_Size[1], Middle_Space[1], Area_Size[1], Margin_Space[1])
    hlength  <- c(0, Margin_Space[2], Area_Size[2], Middle_Space[2], Area_Size[2], Margin_Space[2])
    vlines  <- cumsum(vlength)
    hlines  <- cumsum(hlength)

    #### Add the ploting area
    png(Output_Directory, width = 1024 * 1.05, height  = 768 * 1.03)
    par(mar = c(0, 0, 0, 0))
    plot(x = c(-5, 1024 * 1.05), y = c(-5, 768 * 1.03), 
        type = "n", xaxt = 'n', yaxt = "n", xlab = "", ylab = "", bty = "n", 
        xaxs = "i", yaxs = "i", # remove spaces between ploting area and axes
        axes = FALSE)
    
    ##### Add Test Image
    Test_Image <- image_read(Input_Test_Image_File)
    rasterImage(Test_Image,  min(vlines), min(hlines), max(vlines), max(hlines))

    #### Add A big box
    rect(0, 0, 1024, 768, col = scales::alpha("gray", 0.2), border = "black")

    #### Add rule lines
    abline(h = hlines, lty = 2, lwd = 0.5)
    abline(v = vlines, lty = 2, lwd = 0.5)
    vtextx <- (vlines[-length(vlines)] + vlines[-1]) / 2
    text(vtextx, 768, label = paste(vlength[-1], "px"), cex = 1, pos = 3)
    htexty <- (hlines[-length(hlines)] + hlines[-1]) / 2
    text(1024, htexty, label = paste(hlength[-1], "px"), cex = 1, pos = 4)

    #### Add areas of Interest
    adj <- 5
    IA <- function(x1, x2, y1, y2, ...) {
	    rect(vlines[x1] - adj, hlines[y1] - adj, vlines[x2] + adj, hlines[y2] + adj, lwd = 2, ...)
    }
    IA(2, 3, 2, 3, border = "blue")
    IA(4, 5, 2, 3, border = "blue")
    IA(2, 3, 4, 5, border = "blue")
    IA(4, 5, 4, 5, border = "blue")

    invisible(dev.off())
    print(Output_Sample_Image_Name)
}

################################################################################
########  A Wrap-up of the Conditional Test Stimuli Generator
################################################################################
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
    Conditional_Test_Stimuli_Data <- Conditional_Information_Generator(
        Root_Directory, Input_Audio_Component_Folder, Sentential_Components_File)
    Audio_File_Information <- Conditional_Test_Stimuli_Data $Audio_File_Information
    print("========== Audio Info ==========")
    print(Audio_File_Information)
    Test_Image_File <- Conditional_Test_Stimuli_Data $ Test_Images_Information
    Test_Stimuli_File <- Conditional_Test_Stimuli_Data $ Test_Stimuli_Full
    Test_Stimuli <- Conditional_Test_Stimuli_Data $ Test_Stimuli
    write.csv(Test_Stimuli, file.path(Root_Directory, "Test_Stimuli.csv"), row.names = FALSE)
    write.table(Test_Stimuli, file.path(Root_Directory, "Test_Stimuli.txt"), sep = "\t", quote = FALSE, row.names = FALSE)
    
    print("======= Sample Image ==========")
    Cnd_Tst_Img_Gnr(
    Root_Directory, Input_Objects_Folder, Input_Boxes_Folder, Output_Test_Image_Folder,
    Object_Same_Image_Name = "ShanZi", Object_Different_Image_Name = "GuZheng", 
    Spatial_Order = "SD_SS_CS_DD", Test_Image_Name = "00_Example_Image.png")
    
    print("======= Sample Image with IAs ==========")
    Draw_IA_on_Sample_Image()
    
    print("======= Sample Test Audio ==========")
    Cnd_Tst_Aud_Gnr(
    Selected_Sentential_Connective = "IF", Mentioned_Object = "ShanZi", Agent_Mood = "Happy", Test_Audio_Name = "00_Example_Audio.wav",
    Root_Directory, Input_Audio_Component_Folder,  Output_Test_Audio_Folder,  
    Sentential_Components_File, Mentioned_Object_Expected_Length)
    
    Conditional_Test_Image_Generator_Function <- function(i) Conditional_Test_Image_Generator(Test_Image_Row = i, Test_Image_File,
        Root_Directory, Input_Objects_Folder, Input_Boxes_Folder, Output_Test_Image_Folder)
    print("========== Test Images ==========")
    invisible(sapply(1:nrow(Test_Image_File), Conditional_Test_Image_Generator_Function))

    Conditional_Test_Audio_Generator_Function <- function(i) Conditional_Test_Audio_Generator(
        Test_Stimuli_Row = i, Test_Stimuli_File, 
        Root_Directory, Input_Audio_Component_Folder,  Output_Test_Audio_Folder, Sentential_Components_File, 
        Mentioned_Object_Expected_Length)
    print("========== Test Audios ==========")
    invisible(sapply(1:nrow(Test_Stimuli_File), Conditional_Test_Audio_Generator_Function))	
}