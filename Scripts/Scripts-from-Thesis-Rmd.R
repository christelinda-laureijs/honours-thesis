# This code achieves five things:

  # 1. Create plots of evoked currents versus time (P1 vs. Time) for individual cells and whole treatment groups
  # 2. Create a plot of the locations of all cells that you recorded from
  # 3. Run statistical tests
  # 4. Generate a thesis with auto-updating plots and p-values

# If this file is called Thesis.Rmd, you can knit the whole document into a PDF formatted according to MtA's Thesis guidelines.

# If this file is called Scripts-from-Thesis-Rmd.R, it contains only the code chunks within the thesis.
# Don't make any changes to Scripts-from-Thesis-Rmd.R because it will be overwritten when running purl() in Thesis.Rmd.


# ------------ Set chunk options --------------------------------------------------------------
# cairo_pdf encodes high-quality plots with searchable text
# Hide chunk warnings to reduce distractions
# comment = NA removes the double hashtags next to chunk output

knitr::opts_chunk$set(
  dev = c("cairo_pdf"),
  dpi = 600,
  fig.align = "center",
  comment = NA,
  echo = FALSE,
  message = FALSE,
  warning = FALSE
)

# ------------- Load packages ----------------------------------------------------------------------------
# In a .Rmd file, the default working directory is the folder that contains the .Rmd file
# However, in an .R file, the default working directory is the parent directory i.e. the project folder R-work/
# Use here() to define relative paths and ensure that everything is relative to the top-level directory
# Otherwise, R will not be able to find files when you are working with both Thesis.Rmd and .R script files
# Use here() anytime you are reading in an external file

# here() also makes it easier to transfer between different computers
# because each person's computer has a different file structure
# For example, instead of C:/Users/cslaureijs/Documents/R-work/Thesis/Thesis.Rmd, which only works on my computer
# You can just type here("Thesis/Thesis.Rmd")

library(here)

# Data Management

library(tidyr) # Reformat dataframes from wide to long and vice versa 
library(dplyr) # Data manipulation
library(stringr) # Manipulating text objects

# Making plots

library(ggplot2) # Make plots
library(viridis) # Accessible and beautiful colour palettes
library(extrafont) # Required for custom fonts in plots
library(grid) # Required for rasterGrob function to have a background image on a plot
library(gridExtra) # Required for more rasterGrob functions on cell coordinates plot
library(ggtext) # Required for formatting plot text (e.g. coloured text in title)

# Statistical packages

library(ggfortify) # Autoplot multiple diagnostic plots at once
library(nlme) # Required for linear mixed models and other statistical tests
library(MASS) # Get studentized residuals for linear regression
library(rcompanion) # Plot residuals against a normalized histogram
library(car) # Provides ncvTest and other functions
library(heplots) # Required for Box's M test for MANOVA
library(RVAideMemoire) # Required for multivariate normality tests
library(broom) # Required to format statistical test output into tidy dataframes
library(rstatix) # Required to use pipes with statistical tests
library(knitr) # Required to access better table formatting through kable()
library(kableExtra) # Required for full width tables
library(lazyWeave) # Provides the pvalString() function to format publication-ready p-values in a table 


# Redefine the function select() to prioritize dplyr select()
# This is because the MASS select() function masks dplyr's select()

select <- dplyr::select


# Generate a bibliography of all packages used in this file
# I will use this later to cite all packages that I used in my bibliography

knitr::write_bib(file = here("Templates","packages.bib"))

# ------------- Load scripts ---------------------------------------------------------------------------
# This script file contains my functions for pruning the data and creating plots of P1 vs. Time
# I sourced these to an external file to reduce the length of my code

# Set local = knitr::knit_global() to ensure that the script uses THIS document's directory,
# not the Scripts/ folder

source(here("Scripts","Functions.R"), local = knitr::knit_global())

# ---------- Set plot options --------------------------------------------------------------------------------
# Set save_choice to "yes" if you want to save plots (! this will increase the run time; generally only run this after adding new data)
# Saved plots will automatically go to Figures/Output-... folders depending on what type they are
  # E.g. Raw plots of the first evoked current over time (P1 vs. Time) will go into Figures/Output-individual-plots

save_choice = "no"

# Set colours here only for consistency

line_col = "#333333" # Sets colour of x and y-axes
my_colours = c("#6600cc", "#0093fb", "#55b323","#ffe70f","#e86c00","#333333")
my_colours_pale = c("#b080e0","#5cb9fa", "#92d46e","#ebdf05","#f58c31","#b1b1b1")

# Required for better contrast between the sexes in summary plots
# Do NOT use these for any of the raw P1 vs. Time plots because they are too pale and/or too dark
my_colours_very_dark = c("#4d0299","#026bb5","#398511","#a69502","#994700","#000000")
my_colours_very_pale = c("#d6b8f5", "#8fd0ff", "#aee691","#ebdf05","#ffc38f","#dcdcdc")


# Custom fonts may cause issues depending on what fonts you have in your system
# Troubleshooting steps: 
  # Try changing the font to one that you have on your computer
  # If it does not work, you could always delete 'family = plot_font_family' in the ggplot theme set below
plot_font_family = "Segoe UI"
plot_light_font_family = "Segoe UI Light"

# A consistent y-axis enables comparison across multiple experiments, treatments, etc.
# This is not applied to the raw plots of eEPSCs vs. time for individual cells
# If you change this from 175, make sure to regenerate all summary plots so that you can compare across the same y-axis.

y_axis_limit = 175

# ----------- Set ggplot theme ----------------------------------------------------------------------------------
# Formatting changes like increasing font size & removing gray background
# Requires the extrafont() package (loaded in the load-libraries chunk) for custom font

theme_set(
  theme_classic() %+replace%
    theme(
      text = element_text(
        family = plot_font_family
      ),
      plot.title = element_text(
        color = "black",
        size = 20,
        family = plot_font_family,
        #face = "plain",
        margin = margin(b = 25),
        hjust = 0.5
      ),
      plot.margin = margin(25, 25, 25, 25),
      plot.caption = element_text(
        hjust = 0,
        family = plot_font_family,
        size = 12
      ),
      plot.caption.position = "plot",
      axis.text = element_text(
        size = 12,
        color = "black"
      ),
      axis.title = element_text(
        size = 16,
        face = "bold"
      ),
      axis.title.y = element_text(
        margin = margin(r = 25),
        angle = 90,
        vjust = 0.5
      ),
      axis.title.x = element_text(margin = margin(b = 25, t = 20)),
      axis.ticks = element_blank(),
      strip.background = element_rect(color = NA, fill = NA),
      strip.text = element_text(size = 20)
    )
)









# --------------- Import data --------------------------------------------------------------------------------------

raw_df <- read.csv(here("Data/Raw-eEPSC-Data.csv"), header = T)


# Evoked current amplitudes were negative in Clampex
# Negative transform P1 and P2 for easier interpretation

raw_df <- raw_df %>%
  mutate(P1 = P1 * -1,
         P2 = P2 * -1)


# Add paired pulse ratio (PPR) column

raw_df <- raw_df %>%
  mutate(PPR = P2 / P1)


# Time filtering is required for consistency in plots

raw_df <- filter(raw_df, Time <= 30)


# Must define experimental categories as factors to reduce errors (R thinks these are numeric)

raw_df$Category <- factor(raw_df$Category)



# --------------- Normalize currents relative to baseline ---------------------------------------------------------

# The first part of the code below creates time intervals of 0-5, 6-10, 11-15, etc.
  # I included -1 in the list of breaks so that t = 0 would be included
  # Add a new column with the names for each interval.
  # I tried naming the intervals '0-5', but R thought I was trying to subtract and it did not recognize the names as characters. Instead, I used 't0to5'.

# The second part of this code normalizes each current as a percentage relative to its own baseline
  # Group by letter

  # The baseline is the mean P1 of the first 5 minutes
  # Used time <= 5 to include only the first 5 minutes in the baseline period
  # Get the mean current amplitude for the baseline (0-5 min) for each cell

  # Create a new variable called P1_transformed
  # Express each P1 relative as a percentage relative to its own baseline, which is ~100%
  # This normalizes the P1 to the baseline
  # Repeat this process for P2

raw_df <- raw_df %>%
  mutate(Interval = cut(
    Time,
    breaks = c(-1, 5, 10, 15, 20, 25, 30),
    labels = c("t0to5", "t5to10", "t10to15", "t15to20", "t20to25", "t25to30")
  )) %>%
  group_by(Letter) %>%
  mutate(
    baseline_range = (Time <= 5),
    baseline_mean = sum(P1 * baseline_range) / sum(baseline_range),
    P1_transformed = (P1 / baseline_mean) * 100,
    P2_transformed = (P2 / baseline_mean) * 100,
    Animal_factor = as.factor(unique(Animal))
  )

# Write the raw data to a RDS file for use in other documents/analyses
saveRDS(raw_df, file = here("Data/Output-Data-from-R/raw_df.RDS"))

# -------------- Make dataset for statistical testing -------------------------------------------------------
# Group by letter, sex, treatment and interval
# Get mean P1_transformed amplitude and standard error for each 5-min time interval
# Unfortunately, reframe() removes columns that do not have a function applied to them.
# Use unique() to retain the columns for age, animal, location for each cell and experimental category

summary_df <-
  raw_df %>%
  group_by(Letter, Sex, Treatment, Interval) %>%
  reframe(
    Mean = mean(P1_transformed),
    SE = sd(P1_transformed) / sqrt(n()),
    Age = unique(Age),
    Animal = unique(Animal),
    Animal_factor = as.factor(unique(Animal)),
    n = n(),
    X = unique(X),
    Y = unique(Y),
    Category = unique(Category)
  )

# ----------------- Plot P1 vs. Time --------------------------------------------------------------
# Use custom function make_raw_plots().
# 1st argument: Raw dataframe
# 2nd argument: The experimental category
# Recall from above if you set "save_choice" to yes or no

# This can take a long time to run, and I don't need to run it each time I run the thesis
# Uncomment the line below if you want to produce the raw plots

# make_raw_plots(raw_df, 1)
# make_raw_plots(raw_df, 2)
# make_raw_plots(raw_df, 3)

# ----------------- View raw plots -----------------------------------------------------------------------------
# Not for publication; just for quick viewing in R
# Written in the form of Individual_raw_plots_[category number]
# This number should match argument 2 of make_raw_plots(), or you will see an "object not found" error

# These plots are super useful for seeing individual recordings, but I do not want them in the thesis
# It also can take a while to run this line
# Uncomment the line below if you want to see the raw plots

# Individual_raw_plots_1
# Individual_raw_plots_2
# Individual_raw_plots_3

# ----------------- Prune dataset per minute ------------------------------------------------------------------
# GraphPad Prism has a "prune rows" command that outputs the mean and standard error of every n rows.
# I performed an equivalent pruning function in R.
# I used n = 12 because it will generate one summary point per minute.

# Group raw data by category
# Within each category, define intervals of 1-min each
# Used include.lowest = T so that t = 0 would not be deleted
# 1st group_by(): Get the mean amplitude for each minute within each cell
# 2nd group_by(): Get the mean amplitude for each minute across ALL cells

pruned_df_individual_cells <- raw_df %>%
  group_by(Category) %>%
  mutate(Interval_pruned = cut(
    Time,
    breaks = seq(0, 30, by = 1),
    include.lowest = T
  )) %>%
  group_by(Category, Letter, Sex, Treatment, Interval_pruned) %>%
  reframe(
    mean_P1 = mean(P1),
    sd_P1 = sd(P1),
    n = n(),
    se = sd_P1 / sqrt(n),
    Letter = unique(Letter),
    Category = unique(Category),
    Time = last(Time)
  )


# Write the pruned data to a RDS file for use in other documents/analyses
saveRDS(pruned_df_individual_cells, file = here("Data/Output-Data-from-R/pruned_df_individual_cells.RDS"))

# ----------------- Plot pruned P1 vs. Time --------------------------------------------------------------
# If save_choice = "yes", the plots will be saved in the form of '[Letter]-pruned.png'
# Use custom function make_individual_pruned_plots().
# 1st argument: Pruned individual cells dataframe
# 2nd argument: The experimental category
# 
# make_individual_pruned_plots(pruned_df_individual_cells, 1)
# make_individual_pruned_plots(pruned_df_individual_cells, 2)
# make_individual_pruned_plots(pruned_df_individual_cells, 3)


# -------------- View pruned plots ---------------------------------------------------------
# These plots are not part of the thesis, but they are helpful to monitor cells

# Individual_pruned_plots_1
# Individual_pruned_plots_2
# Individual_pruned_plots_3

# ---------------- Make pruned dataframe for multiple cells ------------------------------------------------------
# Within each category, define intervals of 1-min
# First group_by: Get the mean amplitude for each minute within each cell
# Second group_by: Get the mean amplitude for each minute across ALL cells 
pruned_df <- raw_df %>%
  group_by(Category) %>%
  mutate(Interval_pruned = cut(
    Time,
    breaks = seq(0, 30, by = 1),
    include.lowest = T
  )) %>%
  group_by(Category, Letter, Sex, Treatment, Interval_pruned) %>%
  reframe(
    mean_P1 = mean(P1_transformed),
    sd_P1 = sd(P1_transformed),
    n = n(),
    se = sd_P1 / sqrt(n),
    Letter = unique(Letter),
    Category = unique(Category),
    Time = last(Time)
  ) %>%
  group_by(Category, Interval_pruned, Sex, Treatment) %>%
  reframe(
    Mean_all = mean(mean_P1),
    sd_all = sd(mean_P1),
    n = n(),
    se = sd_all / sqrt(n),
    Time = last(Time),
    Category = unique(Category)
  )

# Write the pruned data to a RDS file for use in other documents/analyses
saveRDS(pruned_df, file = here("Data/Output-Data-from-R/pruned_df.RDS"))







# -------------------------- Get MANOVAR summary table as object -------------------
# print.Anova.mlm prints the dataframe but uses an invisible object
# This meant that I could see the dataframe, but it was not available as an object
# I could not extract the p-values from a class Anova.mlm object, and manually typing them was not ideal for reproducibility

# This answer shows how to extract the summary table as a dataframe from an Anova.mlm object
# https://stackoverflow.com/questions/65161615/extracting-p-value-column-from-output-anova-car-package
# The following function will remove the last two lines of the print.Anova.mlm function in the car package that would normally use an invisible function to delete the dataframe
# Using this modified function will return a dataframe, where you can extract the p-values directly

get_summary_for_print <-
  car:::print.Anova.mlm # copy the original print function (inclusive environment)
body(get_summary_for_print) <- # replace the code of our copy
  local({
    # to avoid pollution of environment by tmp
    tmp <-
      body(get_summary_for_print) # to avoid code duplication
    tmp[-(length(tmp) - (0:1))] # remove the last two code lines of the function
  })

# Use get_summary_for_print() to extract the dataframe
# Use tidy() to clean up the column names and dataframe structure
# Create reader-friendly DF column, rounded test statistics and F-values, and clearer column names
# Use pvalString to express small p-values as <0.0001 instead of exact values.

tidy_manovar <- get_summary_for_print(rep_measures_insulin) %>%
  tidy() %>%
  mutate(df = paste(num.Df, den.Df, sep = ", ")) %>%
  select(!c(num.Df, den.Df)) %>%
  mutate(across(c("test.stat", "approx.F"), \(x) round(x, 2)),
         p.value = pvalString(p.value)) %>%
  rename(
    Term = term,
    'Pillai\'s Trace' = test.stat,
    F = approx.F,
    'p-value' = p.value
  )

















# ---------------- Modify dataset for cell locations plot ------------------------------------------------------------------
# Filter out cells with missing coordinates
# Add a new column called percent change that looks at the percent change 20 minutes after adding insulin
cell_coordinates <-
  summary_df %>%
  group_by(Treatment, Interval, Letter) %>%
  select(!c(SE,n)) %>%
  pivot_wider(
    names_from = Interval,
    values_from = Mean
  ) %>%
  mutate(
    percent_change = t25to30/t0to5*100
  ) %>%
  filter_at(vars(X, Y), all_vars(!is.na(.)))


background_slice_image <- jpeg::readJPEG(here("Figures/DMH-Image.jpg"))


# ---------------- Plot cell locations ------------------------------------------------------------------------
# Using rastorGrob to insert the DMH slice image as a background picture
# The scale_colour_viridis_c() is only relevant if there are multiple categories
# If you want to show multiple categories, go to geom_point() and remove 'color = my_colours[4]'
# Filter (for now) to only include full datasets

cell_coordinates_plot <-
  ggplot(cell_coordinates %>% filter(!is.na(percent_change)) %>% filter(Category == 2) %>% filter(Treatment == "None" | Treatment == "HNMPA" | Treatment == "Fasting"), aes(x = X, y = Y, color = percent_change)) +
  annotation_custom(
    rasterGrob(background_slice_image,
      width = unit(1, "npc"),
      height = unit(1, "npc")
    ),
    -Inf, 
    Inf,
    -Inf,
    Inf
  ) +
  labs(y = "y (µM)", x = "x (µM)", color = "% Change in\ncurrent amplitude\nafter 20 min") +
  geom_point(size = 2.7,
             alpha = .99,
             shape = 16) +
  scale_color_viridis_c(
    limits = c(0, 150),
    begin = 0.27,
    end = 1,
    option = "plasma"
  ) +
  coord_fixed(ratio = 1) +
  xlim(0, 800) +
  scale_y_reverse(limits = c(800, 0)) +
  theme(
    plot.margin = margin(25, 0.5, 0.5, 15),
    axis.title.y = element_text(angle = 90, margin = margin(r = 17)),
    legend.title = element_text(size = 14, margin = margin(b = 15))
  )
cell_coordinates_plot

# Save plot
if (save_choice == "yes") {
  ggsave(
    plot = cell_coordinates_plot,
    filename = "Cell_coordinates_plot_add_insulin.png",
    path = here("Figures"),
    width = 14,
    height = 10,
    units = "in",
    dpi = 300,
    scaling = 1.8
  )
}







# -------------------------- Make publication-ready MANOVAR summary table -------------------
# Use booktabs package to get publication-quality table in LaTeX PDF output
tidy_manovar %>%
  kable(booktabs = T,
        linesep = '',
        col.names = c("Parameters","df","Pillai's Trace","F","\\textit{p}-value"),
        escape = F,
        caption = "A multivariated repeated measures ANOVA using Pillai's Trace shows that current amplitude decreases significantly over time, and treatment may affect neuronal responses to insulin. \\label{MANOVAR-Table}",
        align = c('l','r','r','r','r')) %>%
  kable_styling(full_width = T) %>%
  column_spec(1,
              width = "1.7in")

# ---------------- Get summary plots for all cells --------------------------------------------------------------
# Uses custom function get_pruned_summary_plots()
# 1st argument: Experiment category
# 2nd argument: Treatment
# 3rd argument: pruned dataframe
# 4th argument: Choose colour from colour palette lists defined at the top of the script
# e.g. 1 corresponds to the 1st item in the list, which is purple
get_pruned_summary_plots(2, "None", pruned_df, 1)







get_pruned_summary_plots(2, "HNMPA", pruned_df, 2)

# ---------------- Write R chunks to a script file ---------------------------------------------
# This code extracts code from Thesis.Rmd and saves it to Scripts/Scripts-from-Thesis-Rmd.R
# Useful if you want an independent script file for debugging

# I set documentation = 0 to extract R code within chunks only
# This code is commented out because it causes issues with knitting
# R thought that there were duplicate chunks

# knitr::purl(input = here("Thesis/Thesis.Rmd"), output = here("Scripts/Scripts-from-Thesis-Rmd.R"), documentation = 0)
