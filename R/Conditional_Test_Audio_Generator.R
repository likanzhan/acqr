#### Notes: 
## 1. The root directory: Root_Directory
## 3. Two Files Stored in the Root Directory and their Required Columns:
##    a. Sentential_Compoents: Component_Image, Component_Audio
##    b. Test_Stimuli: Sentential_Connective, Mentioned_Object, Agent_Mood, Object_Same_Image, Object_Different_Image, Test_Audio, Test_Image
## 2. Two folders in the root directory: 
##    a. Input_Audio_Component_Folder: The Folder storing the audio elements that are going to be concatenated to form the test audios. 
##       The names of the components should be the same as the column of "Component_Audio" in the "Sentential_Components" file.
##    b. Output_Test_Audio_Folder (Created one if it does not exist): 

Conditional_Test_Audio_Generator <- function(
  row = 1,
  Root_Directory = "~/Desktop/Conditional_New",
  Input_Audio_Component_Folder = "Important_Originals/Objects_Audios",
  Output_Test_Audio_Folder = "Test_Audios",
  Sentential_Components = "Important_Originals/Sentential_Components", 
  Test_Stimuli_Information = NULL,
  Mentioned_Object_Expected_Length = 1400
  ){
Sentential_Components <- read.csv(file.path(Root_Directory, paste0(Sentential_Components, ".csv")), header = TRUE, stringsAsFactors = FALSE)
if (is.null(Test_Stimuli_Information)) {
    Test_Stimuli <- as.data.frame(get("Test_Stimuli_Full", envir = parent.env(environment()))) # .GlobalEnv
  } else {
    Test_Stimuli <- read.csv(file.path(Root_Directory, paste0(Test_Stimuli_Information, ".csv")), header = TRUE, stringsAsFactors = FALSE)
  }
Audio_Component_Input_Directory <- file.path(Root_Directory, Input_Audio_Component_Folder)
Sentential_Connectives <- data.frame(Because = c("Because", "Therefore", "Very"), IF = c("IF", "Then", "Will"))
Sentential_Connective_1 <- Sentential_Connectives[1, Test_Stimuli[row, "Sentential_Connective"]]
Sentential_Connective_1_Audio_Name <- Sentential_Components[Sentential_Components$Component_Image == Sentential_Connective_1, "Component_Audio"]
Sentential_Connective_1_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, paste0(Sentential_Connective_1_Audio_Name, ".wav")))
Box_In_Audio_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, "XiangZiLi.wav"))
IS_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, "Shi.wav"))
Mentioned_Object <- Test_Stimuli[row, grepl(paste0(Test_Stimuli[row, "Mentioned_Object"], "_Image"), colnames(Test_Stimuli))]
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
Sentential_Connective_2 <- Sentential_Connectives[2, Test_Stimuli[row, "Sentential_Connective"]]
Sentential_Connective_2_Audio_Name <- Sentential_Components[Sentential_Components$Component_Image == Sentential_Connective_2, "Component_Audio"]
Sentential_Connective_2_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, paste0(Sentential_Connective_2_Audio_Name, ".wav")))
Agent_Name_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, "XiaoMing.wav"))
Sentential_Connective_3 <- Sentential_Connectives[3, Test_Stimuli[row, "Sentential_Connective"]]
Sentential_Connective_3_Audio_Name <- Sentential_Components[Sentential_Components$Component_Image == Sentential_Connective_3, "Component_Audio"]
Sentential_Connective_3_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, paste0(Sentential_Connective_3_Audio_Name, ".wav")))
Agent_Mood <- Test_Stimuli[row, "Agent_Mood"]
Agent_Mood_Audio_Name <- Sentential_Components[Sentential_Components$Component_Image == Agent_Mood, "Component_Audio"]
Agent_Mood_Audio_File <- tuneR::readWave(file.path(Audio_Component_Input_Directory, paste0(Agent_Mood_Audio_Name, ".wav")))
Test_Sentence_Audio <- tuneR::bind(
    Sentential_Connective_1_Audio_File, Box_In_Audio_Audio_File, IS_Audio_File,  Mentioned_Object_Audio_File,
    Sentential_Connective_2_Audio_File, Agent_Name_Audio_File, Sentential_Connective_3_Audio_File, Agent_Mood_Audio_File)
Test_Audios_Output_Directory <- file.path(Root_Directory, Output_Test_Audio_Folder)
dir.create(Test_Audios_Output_Directory, showWarnings = FALSE)
Test_Audios_Output_Name <- file.path(Test_Audios_Output_Directory, Test_Stimuli[row, "Test_Audio"])
print(Test_Stimuli[row, "Test_Audio"])
tuneR::writeWave(Test_Sentence_Audio, Test_Audios_Output_Name)
#tuneR::play(Test_Sentence_Audio, player = '/usr/bin/afplay')
}