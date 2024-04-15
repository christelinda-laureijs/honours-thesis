# Useful R Snippets

# Paths may not work, since not all files are there anymore
# These are here mainly as examples

#--------- Merge PDFs ---------------------------------------------------------------------------------
library(pdftools)
pdf_combine(c("pdf2.pdf", "pdf1.pdf"), 
            output = "two_joined_pdfs.pdf")

#---------  Navigate up two parent folders from current wd:---------------------------------------
#../
# Although the here() package is better

# -------- Create ggplot object of colour palettes with hex codes -----------------------------

# https://stackoverflow.com/questions/55711467/print-some-hex-colors-to-view-them
library(ggplot2)
colors <- c("#6600cc", "#0093fb", "#55b323","#ffe70f")
show_colors2 <- function(colors) { 
  ggplot(data.frame(id=seq_along(colors), color=colors)) + 
    geom_tile(aes(1, id, fill=color)) + 
    geom_text(aes(1, id, label=color)) + 
    scale_fill_identity() +
    theme_void()
}
show_colors2(colors)


#--------- Fast ggplot for testing things like fonts -------------------------------------------

# Uses built-in cars dataset

font_test <- ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  labs(title = "My R plot testing fonts", x = "Arial font or Segoe UI", y = "g")
font_test

ggsave(
  plot = font_test,
  filename = "font-test.png",
  path = here("Figures"),
  width = 14,
  height = 10,
  units = "in",
  dpi = 300,
  scaling = 1.8)


# For example, in R-work, saving a plot to ../PPR-Excel-Backups
# enters the current WD, goes up one step, and then enters the folder PPR-Excel-Backups


#--------- Add new columns to existing CSV file------------------------------------------------

#I want to add age and location data to my files
setwd("C:/Users/cslau/Desktop/Honours-Project/Data-Analysis/R-Work")

## Import the raw data
data1 <- read.csv("../Data/PPR-After-HFS-Control.csv", header = T)
data2 <- read.csv("../Data/PPR-After-Adding-Insulin.csv", header = T)
data3 <- read.csv("../Data/PPR-After-HFS-With-Insulin.csv", header = T)

## 20240107 Add "Category" Column to dfs
data1$Category <- 1
data2$Category <- 2
data3$Category <- 3

## 20240107 Merge all 3 raw data sheets into one master sheet
master_df <- rbind(data1,data2,data3)
write.csv(master_df, "../Data/Master-Raw-Data.csv", row.names=F)

## Import the csv files which contain the columns "Letter" and another column like "Age"
ages1 <- read.csv("../Data/Animal-Numbers1.csv", header = T)
ages2 <- read.csv("../Data/Animal-Numbers2.csv", header = T)
ages3 <- read.csv("../Data/Animal-Numbers3.csv", header = T)

## Merge the dataframes using a left-join on the "Letter" column
df1 <- merge(data1,ages1, by = "Letter", all.x=T)
df2 <- merge(data2,ages2, by = "Letter", all.x=T)
df3 <- merge(data3,ages3, by = "Letter", all.x=T)

## Export the new CSV files

write.csv(df1, "../Data/PPR-After-HFS-Control-2.csv", row.names=F)
write.csv(df2, "../Data/PPR-After-Adding-Insulin-2.csv", row.names=F)
write.csv(df3, "../Data/PPR-After-HFS-With-Insulin-2.csv", row.names=F)


#--------- Filter to exclude text string --------------------------------------------------
# Use grepl to identify any text string that contains the word "depolarization" or "Gap"
# in the column named 'Experiment'
coordinates %>%
  filter(!grepl("depolarization",Experiment) & !grepl("Gap",Experiment))


#---------- Get separate legend from ggplot2 figure.-------------------------------------

## Step 1. Make a graph and assign it to a variable, but do not call the graph.
## Use the following code to isolate the legend from the figure and display it as an independent figure.
legend <- cowplot::get_legend(p3)

grid.newpage()
legend_image<- grid.draw(legend)
legend_image


#----------- Conditional echo based on document output type----------------------------------

# Goal: Only show something if it the output format is .docx
#Used asis function in knitr and echo=knitr::pandoc_to() to selectively display text in word or pdf output only
#Using LaTeX styling in the title made it invisible in Word, so I manually inserted text with the Title style

#{asis front-matter-word-output-only, echo=knitr::pandoc_to("docx"), purl=FALSE}
#::: {custom-style="Centered-Subtitle"}
#Christelinda Laureijs\
#Mount Allison University\
#Biology
#:::


#--------- Get list of named R colours ------------------------------------------------------
## No margin around chart
par(mar=c(0,0,0,0))

## Empty chart
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")

## Settings
line <- 25
col <- 5

## Add color background
rect(  
  rep((0:(col - 1)/col),line) ,  
  sort(rep((0:(line - 1)/line),col),decreasing=T),   
  rep((1:col/col),line) , 
  sort(rep((1:line/line),col),decreasing=T),  
  border = "white" , 
  col=colors()[seq(1,line*col)])

## Color names
text(  
  rep((0:(col - 1)/col),line)+0.1 ,  
  sort(rep((0:(line - 1)/line),col),decreasing=T)+0.015 , 
  colors()[seq(1,line*col)]  , 
  cex=1)

#--------- Get current pandoc latex template ------------------------------------------------------

library(pandoc)

pandoc_export_template("latex","20240108default.tex")

#--------- Remove a whole set of named-alike objects: ------------------------------------------------------
rm(list = ls(pattern = "^tmp"))
rm(list = ls()[grep("^tmp", ls())]) # A longer version

#--------- Create a label in a plot chunk caption for LaTeX cross-referencing

# Put this in the fig.cap field (double \\ to escape the first \):
# https://stackoverflow.com/questions/17167140/chunk-reference-using-knitr-r-markdown-and-pandoc-pander
# fig.cap="\\label{mylabel}Caption of my figure."
# Later use 'as seen in figure \ref{mylabel}'

# --------- Remove Excel-generated PPR column --------------
# In tidy data, no manipulations should be performed on the raw data
# Previously, I had used Excel to calculated PPR as P2/P1
# I ran this code to remove the Excel-generated PPR
# 20240117: From now on, I will use the mutate function
# within the data import chunk to calculate PPR.

# Import Data
raw_df <- read.csv("../Data/Master-Raw-Data.csv", header = T)

# Remove PPR column that was generated by Excel
raw_df_noPPR <- raw_df %>%
  select(-PPR)

# Write this as a csv file to the Data format
write.csv(raw_df_noPPR, "../Data/Raw-Data.csv", row.names=F)

# Double-check that the PPR function was the same
raw_df_noPPR2 <- raw_df_noPPR

raw_df_noPPR2 <- raw_df_noPPR2 %>%
  mutate(
    PPR = P2/P1
  )

# Look at recording L data
# I found slight differences between the old and new graphs
# because the previous one used the end of the minute as the point,
# while the other used the mid-point of each interval to calculate the mean