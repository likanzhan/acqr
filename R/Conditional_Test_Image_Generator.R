## Notes: 
## This function requires 
## 1. A Root_Directory,
## 2. A Test_Images_Info File in the root directory with the following required columns: Object_Same_Image, Object_Different_Image, Spatial_Order, Test_Image
## 3. Three foulders in the Root_Directory:
##    a. Input_Objects_Folder: Objects used to concatenate the Test images
##    b. Input_Boxes_Foler: Boxes and the covers used to concatenate the test images.
##    c. Output_Test_Image_Folder: The folder used to store the created test images.

############ 0. Define the Function ################################################################################################
Conditional_Test_Image_Generator <- function(
    row = 1,
    Root_Directory = "~/Desktop/Conditional_New",
    Input_Objects_Folder = "Objects_Images",
    Input_Boxes_Folder = "Boxes",
    Output_Test_Image_Folder = "Test_Images",
    Test_Images_Info = NULL
){
suppressMessages(library(magick))
if (is.null(Test_Images_Info)) {
	Test_Images <- as.data.frame(Test_Images)
} else {
    Test_Images <- read.csv(paste0(Root_Directory, "/", Test_Images_Info, ".csv"), header = TRUE, stringsAsFactors = FALSE)	
}
########## 1. Read Objects into R ####################################################################################################
Object_Same_File <- paste0(Root_Directory, "/", Input_Objects_Folder, "/", Test_Images[row, "Object_Same_Image"], ".png")
Object_Same <- magick::image_read(Object_Same_File)
Object_Same <- magick::image_resize(Object_Same, geometry = geometry_size_percent(width = 200/600 * 100))
Brand_Same  <- magick::image_resize(Object_Same, geometry = geometry_size_percent(width = 70/200 * 100))

Object_Different_File <- paste0(Root_Directory, "/", Input_Objects_Folder, "/", Test_Images[row, "Object_Different_Image"], ".png")
Object_Different <- magick::image_read(Object_Different_File)
Object_Different <- magick::image_resize(Object_Different, geometry = geometry_size_percent(width = 200/600 * 100))
Brand_Different  <- magick::image_resize(Object_Different, geometry = geometry_size_percent(width = 70/200 * 100))

########## 2. Read Boxes into R and Define offsets ####################################################################################################
Box_Open_File <- paste0(Root_Directory, "/", Input_Boxes_Folder, "/", "Box_Open.pdf")
Box_Open    <- magick::image_read_pdf(Box_Open_File,   density = 323)
Box_Open <- magick::image_transparent(Box_Open, color = "white") # Remove background
Box_Closed_File <- paste0(Root_Directory, "/", Input_Boxes_Folder, "/", "Box_Closed.pdf")
Box_Closed  <- magick::image_read_pdf(Box_Closed_File, density = 323)
Box_Closed <- magick::image_transparent(Box_Closed, color = "white")
Box_Cover_File <- paste0(Root_Directory, "/", Input_Boxes_Folder, "/", "Box_Cover.pdf")
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
Condition <- Test_Images[row, "Spatial_Order"]
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
print(Test_Images[row, "Test_Image"])
magick::image_write(Image_Add_Numbers, paste0(File_Path, "/", Test_Images[row, "Test_Image"]))		
}
########## 7. Run the function to draw the image ####################################################################################################
#Conditional_Test_Image_Generator()