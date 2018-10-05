####################################################################################################################################################################
########  Conditional_Information_Generator
####################################################################################################################################################################
Conditional_Information_Generator <- function(
Root_Directory = "~/Desktop/Conditional_New",
Objects_Audios_Folder = "Important_Originals/Objects_Audios",
Sentential_Components_File = "Important_Originals/Sentential_Components"
){
#################################### 1. Explore the length of the Audio Files ###########################
### a. Read the Audio Files ###########################
library(tuneR)
Audio_File_Directory <- file.path(Root_Directory, Objects_Audios_Folder)
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
