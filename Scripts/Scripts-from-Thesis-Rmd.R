# This code achieves five things:

  # 1. Create plots of evoked excitatory currents versus time (P1 vs. Time) for individual cells and whole treatment groups
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
library(abftools) # Required to read in .ABF files

# Data Management

library(tidyr) # Reformat dataframes from wide to long and vice versa 
library(dplyr) # Data manipulation
library(stringr) # Manipulating text objects
library(glue) # Required for easy mixing of variables and text when naming objects

# Making plots

library(ggplot2) # Make plots
library(viridis) # Accessible and beautiful colour palettes
library(extrafont) # Required for custom fonts in plots
library(grid) # Required for rasterGrob function to have a background image on a plot
library(gridExtra) # Required for more rasterGrob functions on cell coordinates plot
library(ggtext) # Required for formatting plot text (e.g. coloured text in title)
library(ggforce) # Required for sina plots
library(patchwork) # Required for multi-plot layout
library(ggsignif) # Required for significance stars and brackets on plots
library(ggh4x) # Required to create nested x-axis labels
library(ggpubr) # Required for significance brackets

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
library(knitr) # Required to access better table formatting through kable
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

source(here("Scripts/Functions.R"), local = knitr::knit_global())

# ---------- Set plot theme, colours & sizes --------------------------------------------------------------------------------
# Set save_choice to "yes" if you want to save plots (! this will increase the run time; generally only run this after adding new data)
# Saved plots will automatically go to Figures/Output-... folders depending on what type they are
  # E.g. Raw plots of the first evoked current over time (P1 vs. Time) will go into Figures/Output-individual-plots

save_choice <- "no"


treatment_shapes <- c(16, 17, 15, 18)
list_of_treatments <- c("Control", "Fasting", "HNMPA", "PPP")
list_of_significance_stars <- c(
  "***" = 0.001,
  "**" = 0.01,
  "*" = 0.05,
  "ns" = 2
)

# Set colours here only for consistency

line_col <- "#333333" # Sets colour of x and y-axes
my_colours <- c("#6600cc", "#0093fb", "#55b323","#ffe70f","#e86c00","#333333", "#411900","#e11584")
my_colours_pale <- c("#b080e0","#5cb9fa", "#92d46e","#ebdf05","#f58c31","#b1b1b1","#a16b4a","#f57dbe")

# Rectangle highlights intervals from 5-10 min and 15-20 min
rectangle_shading_colour <- "#f6f6f6"

# Required for better contrast between the sexes in summary plots
# Do NOT use these for any of the raw P1 vs. Time plots because they are too pale and/or too dark
my_colours_very_dark <- c("#4d0299","#026bb5","#398511","#a69502","#994700","#000000","#785138","#910150")
my_colours_very_pale <- c("#d6b8f5", "#8fd0ff", "#aee691","#ebdf05","#ffc38f","#dcdcdc","#bf9b84","#eba2c9")


# Custom fonts may cause issues depending on what fonts you have in your system
# Troubleshooting steps: 
  # Try changing the font to one that you have on your computer
  # If it does not work, you could always delete 'family = plot_font_family' in the ggplot theme set below
plot_font_family <- "Segoe UI"
plot_light_font_family <- "Segoe UI Light"
significance_stars_font <- plot_font_family


# Set default colours and point sizes for action potential summary plots
mean_point_colour <- "#000000"
baseline_group_colour <- "#727a85"
insulin_group_colour <- "#6600cc"
connecting_line_width <- 0.1
connecting_line_colour_aps <- "#bfbfbf"
connecting_line_width_PPR <- 0.2

mean_point_size <- 0.5
geom_sina_size <- 3
geom_signif_text_size <- 8

AP_trace_size <- 0.7
scale_bar_shift_y <- 5
scale_bar_shift_x <- 30

# These sizes work better for the multi-plot figure in the PDF output
if (knitr::is_latex_output()) {
  geom_sina_size <- 2
  geom_signif_text_size <- 4
  AP_trace_size <- 0.6
  scale_bar_shift_y <- 13
  scale_bar_shift_x <- 200
  connecting_line_width <- 0.04
  connecting_line_width_PPR <- 0.04
}

# A consistent y-axis enables comparison across multiple experiments, treatments, etc.
# This is not applied to the raw plots of eEPSCs vs. time for individual cells
# If you change this from 175, make sure to regenerate all summary plots so that you can compare across the same y-axis.

y_axis_limit <- 175
PPR_y_min <- 0
PPR_y_max <- 5

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













# --------------- AP Analysis --------------------------------------------------------------------------------------
ap_data <- read.csv(here("Data/Raw-CSVs/20240329-AP-Analysis.csv"), header = T)

# Remove recordings with known issues
# AH, AM (Current clamp steps 'After' recording was after Insulin and high-frequency stimulation)
# FO, FP (Lost the cell before I could record 'After') 

ap_data <- ap_data %>%
  filter(! Letter %in% c("AH", "AM", "FO", "FP"))

# Add new column with frequency of APs
# Each sweep lasts 0.5 seconds, so to get frequency in Hz, I divided the number of APs by 0.5

# Add a new column that states the current injection used at each step
# Rename column 't_x' to 'Threshold', which is more useful

ap_data <- ap_data %>%
  mutate(
    AP_frequency = No_of_APs / 0.5,
    Latency_to_fire = Time_to_Peak - 265.4,
    Antipeak_time_relative_to_threshold = Time_of_antipeak - Time_of_Threshold,
    Current_injection = case_when(
      Sweep == 1 ~ -50,
      Sweep == 2 ~ -40,
      Sweep == 3 ~ -30,
      Sweep == 4 ~ -20,
      Sweep == 5 ~ -10,
      Sweep == 6 ~ 0,
      Sweep == 7 ~ 10,
      Sweep == 8 ~ 20,
      Sweep == 9 ~ 30,
      Sweep == 10 ~ 40,
      T ~ 50
    )
  ) %>%
  rename(Threshold = t_x)

# Import CSV with info on sex, synapses, X, Y, treatment, animal, etc.
cell_characteristics <-
  read.csv(here("Data/Plaintext-Cell-Characteristics.csv"))

# Merge datasets to get complete AP dataset
full_ap_df <-
  merge(ap_data,
        cell_characteristics,
        by = "Letter")

# Create list of the order of AP parameters
# This ensures that the parameters are listed in a consistent order in the t-test and Shapiro-Wilk tables
list_of_AP_parameters <-
  c(
    "Peak_amplitude",
    "Threshold",
    "Half_width",
    "Latency_to_fire",
    "Antipeak_amplitude",
    "Antipeak_time_relative_to_threshold"
  )

# ----------------- AP Analysis: Get representative current clamp steps traces --------------------

# Set range (e.g. height and width) of scale bar
scale_bar_x_in_recording_time_units <- 500
scale_bar_y_in_mV <- 20

# Manually reposition the text labels on the scale bar
scale_bar_shift_y <- 10
scale_bar_shift_x <- 40

# Set starting point of the scale bar
# This determines the point where the horizontal and vertical bars meet
scale_bar_start_x <- 8200
scale_bar_start_y <- 0

# Set the x- and y-axis limits
# The x-limit shouldn't have to change much since all protocols started and ended at the same time
ap_traces_y_min <- -120
ap_traces_y_max <- 20
ap_traces_x_min <- 2500 #2500
ap_traces_x_max <- 8500 #8000


ap_trace_baseline <-
  make_AP_trace_plot(file_ID = "23623001_rescaled_0_01",
                     sweep1 = "epi10",
                     sweep2 = "e",
                     trace_color = baseline_group_colour) +
  annotate(
    "segment",
    x = scale_bar_start_x,
    xend = scale_bar_start_x + scale_bar_x_in_recording_time_units,
    y = scale_bar_start_y,
    yend = scale_bar_start_y,
    lwd = 0.4
  ) +
  annotate(
    "segment",
    x = scale_bar_start_x,
    xend = scale_bar_start_x,
    y = scale_bar_start_y-0.49,
    yend = scale_bar_start_y + scale_bar_y_in_mV,
    lwd = 0.4
  ) +
  annotate(
    "text",
    x = scale_bar_start_x - scale_bar_shift_x,
    y = scale_bar_start_y + 1/2*(scale_bar_y_in_mV),
    label = paste0(scale_bar_y_in_mV, " mV"),
    hjust = 1,
    vjust = 0.5,
    family = plot_font_family
  ) +
  annotate(
    "text",
    x = scale_bar_start_x + 1/2*(scale_bar_x_in_recording_time_units),
    y = scale_bar_start_y - scale_bar_shift_y,
    
    # Recall that 1 recording time unit = 0.1 ms
    # because it was filtered at 10 kHz
    label = paste0(scale_bar_x_in_recording_time_units/10, " ms"),
    hjust = 0.5,
    family = plot_font_family
  )

ap_trace_insulin <-
  make_AP_trace_plot(file_ID = "23623009_rescaled_0_01",
                     sweep1 = "epi10",
                     sweep2 = "e",
                     trace_color = insulin_group_colour)

dual_plot <- ap_trace_baseline / ap_trace_insulin


if (save_choice == "yes") {
  ggsave(
    plot = dual_plot,
    path = here("Figures", "Output-summary-plots"),
    file = "AP-step-10-representative-traces-AO.png",
    width = 7,
    height = 5,
    units = "in",
    dpi = 300
  )
}

# --------------- AP Analysis: Get max mean frequencies per step --------------------------------------------------------------------------------------
# Required for correct positioning of significance stars

max_mean_AP_frequencies <- full_ap_df %>%
  group_by(Current_injection, State) %>%
  summarize(
    mean_AP_frequency = mean(AP_frequency),
    SE = sd(AP_frequency) / sqrt(n())
  ) %>%
  group_by(Current_injection) %>%
  summarize(
    max_AP_frequency = max(mean_AP_frequency),
    max_se = max(SE)
  )

# --------------------- AP Analysis: T-Test of AP Frequency per Step
# I saved this to a dataframe so I could later add significance asterisks to my plot through geom_text()
# "ns" is distracting in plots, so I replaced it with whitespace

# Included here for future testing only
# The data are non-normal, so I used a Wilcoxon test for my analysis

t_test_current_clamp_steps <- full_ap_df %>%
  select(Letter, State, AP_frequency, Current_injection) %>%
  group_by(Current_injection) %>%
  pairwise_t_test(
    AP_frequency ~ State,
    ref.group = "Baseline",
    paired = T,
    p.adjust.method = "holm"
  ) %>%
  mutate(
    statistic = round(statistic, 2),
    p_string = pvalString(p),
    significance_stars = case_when(p.adj.signif == "ns" ~ "",
                             T ~ p.adj.signif)) %>%
  merge(., max_mean_AP_frequencies,
        by = "Current_injection")

t_test_current_clamp_steps %>% 
  select(c(Current_injection, statistic, df, p_string, p.adj.signif)) %>%
  kable(
    col.names = c("Current Injection", "Statistic", "DF", "p-value", ""),
    caption = "Pairwise T-Test with Holm's Correction"
  )

# Each letter contains 10 rows (one for each sweep)
# This means that the data in columns like 'Threshold', 'Peak_amplitude', 'Time_of_peak' are repeated 10 times
# Create a dataframe for the AP sina plots that does not contain repeated rows

distinct_aps <- full_ap_df %>%
  distinct(Letter, State, .keep_all = TRUE)


# ---------------- AP Analysis: Get differences between control and insulin ------------------------

# For each recording, get the difference in before vs. after values for
# Peak amplitude, latency, etc.
# Use arrange() to ensure that the states are arranged in the proper order within each letter
# These are in the form of
  # (latency_diff) = (latency_insulin) - (latency_baseline)
# Ungroup data and remove NAs to avoid issues later


# Note that there are more values for peak amplitude than the other AP parameters
# This is because some recordings did not have any action potentials
# For these recordings, I wrote "0" for peak amplitude and "NA" for the other AP parameters like half-width

distinct_aps_differences <- distinct_aps %>%
  arrange(Letter, State) %>%
  group_by(Letter) %>%
  mutate(
    Peak_amplitude_diff = Peak_amplitude - lag(Peak_amplitude),
    Latency_diff = Latency_to_fire - lag(Latency_to_fire),
    Threshold_diff = Threshold - lag(Threshold),
    Antipeak_amplitude_diff = Antipeak_amplitude - lag(Antipeak_amplitude),
    Antipeak_time_relative_to_threshold_diff = Antipeak_time_relative_to_threshold - lag(Antipeak_time_relative_to_threshold),
    Half_width_diff = Half_width - lag(Half_width)
  ) %>%
  ungroup() %>%
  drop_na(
    c(
      Peak_amplitude_diff,
      Latency_diff,
      Threshold_diff,
      Antipeak_amplitude_diff,
      Antipeak_time_relative_to_threshold_diff,
      Half_width_diff
    )
  )

# ---------------------- AP Analysis: Pivot data to longer form -----------------
# Makes it easier to run t-tests across separate AP parameters at once through tidyr framework

wide_ap_df <- distinct_aps %>%
  pivot_longer(
    cols = c(
      "Peak_amplitude",
      "Threshold",
      "Half_width",
      "Latency_to_fire",
      "Antipeak_amplitude",
      "Antipeak_time_relative_to_threshold"
    ),
    names_to = "AP_parameter_name",
    values_to = "AP_parameter_value"
  )

wide_ap_df_differences <- distinct_aps_differences %>%
  pivot_longer(
    cols = c(
      "Peak_amplitude_diff",
      "Threshold_diff",
      "Half_width_diff",
      "Latency_diff",
      "Antipeak_amplitude_diff",
      "Antipeak_time_relative_to_threshold_diff"
    ),
    names_to = "AP_parameter_diff_name",
    values_to = "AP_parameter_diff"
  )

# --------------- AP Analysis: T-test for parameters --------------------------------------------------------------------------------------
wide_ap_df %>%
  group_by(AP_parameter_name) %>%
  t_test(
    AP_parameter_value ~ State,
    ref.group = "Baseline",
    paired = T
  ) %>%
  mutate(
    statistic = round(statistic, 2),
    p_original = p,
    p_string = pvalString(p),
    significance_stars = symnum(p, 
                     symbols   = c("***","**","*",""),
                     cutpoints = c(0,  .001,.01,.05, 1),
                     corr      = FALSE
                   )
  ) %>%
  arrange(match(AP_parameter_name, list_of_AP_parameters)) %>%
  select(c(AP_parameter_name, statistic, df, p_string, significance_stars)) %>%
  kable(
    col.names = c("Parameter", "Statistic", "DF", "p-value", ""),
    caption = "Paired T-Test"
  )

# --------------- AP Analysis: Testing normality --------------------------------------------------------------------------------------
wide_ap_df_differences %>%
  group_by(AP_parameter_diff_name) %>%
  shapiro_test(AP_parameter_diff) %>%
  select(!variable) %>%
  mutate(
    p_string = pvalString(p),
    p_true = p,
    statistic = round(statistic, 2),
    significance_stars = symnum(
      p,
      symbols   = c("***", "**", "*", ""),
      cutpoints = c(0,  .001, .01, .05, 1),
      corr      = FALSE
    )
  ) %>%
  select(c(
    AP_parameter_diff_name,
    statistic,
    p_string,
    significance_stars
  )) %>%
  arrange(match(
    AP_parameter_diff_name,
    c(
      "Peak_amplitude_diff",
      "Threshold_diff",
      "Half_width_diff",
      "Latency_diff",
      "Antipeak_amplitude_diff",
      "Antipeak_time_relative_to_threshold_diff"
    )
  )) %>%
  kable(col.names = c("Parameter", "Statistic", "P-value", ""),
        caption = "Shapiro-Wilk Normality Test")


## # make_qq_plot(data = distinct_aps_differences,
## #              parameter_title = "Peak amplitude",
## #              parameter = Peak_amplitude_diff)
## #
## # make_qq_plot(data = distinct_aps_differences,
## #              parameter_title = "Threshold",
## #              parameter = Threshold_diff)
## #
## # make_qq_plot(data = distinct_aps_differences,
## #              parameter_title = "Half Width",
## #              parameter = Half_width_diff)
## #
## # make_qq_plot(data = distinct_aps_differences,
## #              parameter_title = "Latency",
## #              parameter = Latency_diff)
## #
## # make_qq_plot(data = distinct_aps_differences,
## #              parameter_title = "Antipeak amplitude",
## #              parameter = Antipeak_amplitude_diff)
## #
## # make_qq_plot(data = distinct_aps_differences,
## #              parameter_title = "Antipeak Time",
## #              parameter = Antipeak_time_relative_to_threshold_diff)
## 
## 
## 
## # The data did not pass normality, and a log-transformation did not work for the data (some of the data were negative, and even when I transformed it, the data were still not normal). Instead, I ran a Wilcoxon signed-rank test.

# --------------------- AP Analysis: Wilcoxon Test -------------------------------
wilcoxon_table_ap <- wide_ap_df %>%
  group_by(AP_parameter_name) %>%
  wilcox_test(AP_parameter_value ~ State,
              ref.group = "Baseline",
              paired = T) %>%
  mutate(
    statistic = round(statistic, 2),
    p_string = pvalString(p),
    significance_stars = symnum(
      p,
      symbols   = c("***", "**", "*", ""),
      cutpoints = c(0,  .001, .01, .05, 1),
      corr      = FALSE
    ),
    df = paste(n1, n2, sep = ", "),
    AP_parameter_name = str_replace_all(AP_parameter_name, "_", " "),
    AP_parameter_name = case_when(
      AP_parameter_name == "Half width" ~ "Half-width",
      AP_parameter_name == "Antipeak amplitude" ~ "After-hyperpolarization amplitude",
      AP_parameter_name == "Antipeak time relative to threshold" ~ "After-hyperpolarization time",
      T ~ AP_parameter_name
    )
  ) %>%
  arrange(match(
    AP_parameter_name,
    c(
      "Peak amplitude",
      "Threshold",
      "Latency to fire",
      "Half-width",
      "After-hyperpolarization amplitude",
      "After-hyperpolarization time"
    )
  )) %>%
  select(c(
    AP_parameter_name,
    df,
    statistic,
    p_string
  ))

kable_tidy_wilcoxon <- wilcoxon_table_ap %>%
  kable(booktabs=T,
        linesep = '',
        col.names = c("Parameter", "df", "Statistic", "\\textit{p}-value"),
        escape = F,
        caption = "A Wilcoxon signed-rank test shows that insulin significantly affects action potential amplitudes, thresholds, and half-widths. Insulin does not significantly affect the latency to fire, after-hyperpolarization amplitude, or the time of afterhyperpolarization. \\label{AP-parameters-Wilcoxon-Table}",
        caption.short = "Wilcoxon signed-rank summary table of action potential parameters before and after insulin exposure",
        align = c('l','c','l','l')) %>% 
  kable_styling(full_width = T) %>% 
  column_spec(1,
              width = "2.5in")

# ----------------- AP Analysis: Make AP Frequency Plot ---------------------------
full_ap_frequency_plot <- full_ap_df %>%
  group_by(State, Current_injection) %>%
  summarize(
    mean_AP_frequency = mean(AP_frequency),
    SE = sd(AP_frequency) / sqrt(n())
  ) %>%
  ggplot(
    aes(
      x = Current_injection,
      y = mean_AP_frequency,
      ymin = mean_AP_frequency - SE,
      ymax = mean_AP_frequency + SE,
      color = State,
      shape = State
    )
  ) +
  geom_pointrange(size = 1.1,
                  linewidth = 0.6,
                  position = position_dodge(width = 0)) +
  labs(x = "Current Injection (pA)",
       y = "AP Frequency (Hz)",
       color = NULL,
       shape = NULL) +
  scale_color_manual(values = c(baseline_group_colour, insulin_group_colour)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.15, 0.8),
        legend.key.spacing.y = unit(1.5, "lines"),
        axis.title = element_text(face = "plain")) +
  scale_y_continuous(expand = expansion(mult = c(0.1, .1))) +
  geom_text(
      data = t_test_current_clamp_steps,
      aes(
        x = Current_injection,
        y = max_AP_frequency + max_se + 1,
        label = significance_stars
      ),
      inherit.aes = F,
      size = 5,
      family = significance_stars_font
    )

raw_ap_frequency_plot <- full_ap_df %>%
  ggplot(aes(x = Current_injection, y = AP_frequency, color = State)) +
  geom_point(position = "jitter") +
  labs(x = "Current Injection (pA)",
       y = "AP Frequency (Hz)",
       color = NULL) +
  scale_color_manual(values = c(baseline_group_colour, insulin_group_colour)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.11, 0.8),
        axis.title = element_text(face = "plain")) +
  scale_y_continuous(expand = expansion(mult = c(0.1, .1)))

# ------------------ AP Analysis: Make AP Characteristics Plots ---------------------
peak_amplitude_plot <- make_AP_plot(data = distinct_aps,
                                    y = Peak_amplitude,
                                    y_axis_title = "Amplitude (mV)")

latency_plot <- make_AP_plot(data = distinct_aps,
                             y = Latency_to_fire,
                             y_axis_title = "Latency (ms)")

threshold_plot <- make_AP_plot(data = distinct_aps,
                               y = Threshold,
                               y_axis_title = "Threshold (mV)")

antipeak_amplitude_plot <- make_AP_plot(data = distinct_aps,
                                        y = Antipeak_amplitude,
                                        y_axis_title = "Afterhyperpolarization\nAmplitude (mV)")

time_of_antipeak_plot <- make_AP_plot(
  data = distinct_aps,
  y = Antipeak_time_relative_to_threshold,
  y_axis_title = "Time of After-\nhyperpolarization (ms)"
)

half_width_plot <- make_AP_plot(data = distinct_aps,
                                y = Half_width,
                                y_axis_title = "Half Width (ms)")


# Save Plots
if (save_choice == "yes") {
  save_AP_plot(full_ap_frequency_plot, "AP-frequency")
  save_AP_plot(peak_amplitude_plot, "Peak-amplitude")
  save_AP_plot(latency_plot, "Latency")
  save_AP_plot(threshold_plot, "Threshold")
  save_AP_plot(half_width_plot, "Half-width")
  save_AP_plot(antipeak_amplitude_plot, "Antipeak-amplitude")
  save_AP_plot(time_of_antipeak_plot, "Time-of-Antipeak")
}

# --------------- eEPSC Analysis --------------------------------------------------------------------------------------

raw_df <- read.csv(here("Data/Raw-eEPSC-Data.csv"), header = T) %>%
  
  # Remove letters with known issues
  filter(!Letter %in% c("GE", "GN", "GJ"))


# Evoked current amplitudes were negative in Clampex
# Negative transform P1 and P2 for easier interpretation
# Add paired pulse ratio (PPR column)

# Must define experimental categories as factors to reduce errors (R thinks these are numeric)
# Time filtering (Time <= 25) is required for consistency in plots


raw_df <- raw_df %>%
  mutate(P1 = P1 * -1,
         P2 = P2 * -1,
         PPR = P2 / P1,
         Category = factor(Category)) %>%
  filter(Time <= 25)


# Must define experimental categories as factors to reduce errors (R thinks these are numeric)



raw_df$Category <- factor(raw_df$Category)


# --------------- eEPSC Analysis: Normalize currents relative to baseline ---------------------------------------------------------

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
    breaks = c(-1, 5, 10, 15, 20, 25),
    labels = c("t0to5", "t5to10", "t10to15", "t15to20", "t20to25")
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

# -------------- eEPSC Analysis: Make dataset for statistical testing -------------------------------------------------------
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

# ----------------- eEPSC Analysis: Plot P1 vs. Time --------------------------------------------------------------
# Use custom function make_raw_plots().
# 1st argument: Raw dataframe
# 2nd argument: The experimental category
# Recall from above if you set "save_choice" to yes or no

# This can take a long time to run, and I don't need to run it each time I run the thesis
# Uncomment the line below if you want to produce the raw plots

# make_raw_plots(dataframe = raw_df,
#                treatment = "AM251",
#                category = 2,
#                color_choice = 8)

# ----------------- eEPSC Analysis: View raw plots -----------------------------------------------------------------------------
# Not for publication; just for quick viewing in R
# Written in the form of Individual_raw_plots_[category number]
# This number should match argument 2 of make_raw_plots(), or you will see an "object not found" error

# These plots are super useful for seeing individual recordings, but I do not want them in the thesis
# It also can take a while to run this line
# Uncomment the line below if you want to see the raw plots

#Individual_raw_plots_Fasting_category_2

# ----------------- eEPSC Analysis: Prune dataset per minute ------------------------------------------------------------------
# GraphPad Prism has a "prune rows" command that outputs the mean and standard error of every n rows.
# I performed an equivalent pruning function in R.
# I used by = 1 because it will generate one summary point per minute.

# Group raw data by category
# Within each category, define intervals of 1-min each
# Used include.lowest = T so that t = 0 would not be deleted
# 1st group_by(): Get the mean amplitude for each minute within each cell
# 2nd group_by(): Get the mean amplitude for each minute across ALL cells
# Keep baseline_mean because this is required to create horizontal annotation of the baseline

pruned_df_individual_cells <- raw_df %>%
  group_by(Category) %>%
  mutate(Interval_pruned = cut(
    Time,
    breaks = seq(0, 25, by = 1.5),
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
    Time = last(Time),
    baseline_mean = unique(baseline_mean)
  )


# Write the pruned data to a RDS file for use in other documents/analyses
saveRDS(pruned_df_individual_cells, file = here("Data/Output-Data-from-R/pruned_df_individual_cells.RDS"))

# ----------------- eEPSC Analysis: Plot pruned P1 vs. Time --------------------------------------------------------------
# If save_choice = "yes", the plots will be saved in the form of '[Letter]-pruned.png'
# Use custom function make_individual_pruned_plots().
# 1st argument: Pruned individual cells dataframe
# 2nd argument: The experimental category

# make_individual_pruned_plots(data = pruned_df_individual_cells,
#                              treatment = "AM251",
#                              category = 2,
#                              color_choice = 8)


# -------------- eEPSC Analysis: View pruned plots ---------------------------------------------------------
# These plots are not part of the thesis, but they are helpful to monitor cells

#Individual_pruned_plots_Control_category_1


# ---------------- eEPSC Analysis: Make pruned dataframe for multiple cells ------------------------------------------------------
# Within each category, define intervals of 1-min
# First group_by: Get the mean amplitude for each minute within each cell
# Second group_by: Get the mean amplitude for each minute across ALL cells 
pruned_df <- raw_df %>%
  group_by(Category) %>%
  mutate(Interval_pruned = cut(
    Time,
    breaks = seq(0, 25, by = 1),
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







# -------------------------- eEPSC Analysis: Get MANOVAR summary table as object -------------------
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

# -------------------------- eEPSC Analysis: Make publication-ready MANOVAR summary table -------------------
# Use booktabs package to get publication-quality table in LaTeX PDF output
kable_manovar_table_eEPSC <- tidy_manovar %>%
  kable(booktabs = T,
        linesep = '',
        col.names = c("Parameters","df","Pillai's Trace","F","\\textit{p}-value"),
        escape = F,
        caption = "A multivariate repeated measures ANOVA using Pillai's Trace shows that current amplitude varies significantly with time, and there are no significant interactions between factors like sex and treatment. \\label{MANOVAR-Table}",
        caption.short = "MANOVAR summary table of evoked excitatory currents before and during insulin exposure",
        align = c('l','r','r','r','r')) %>%
  kable_styling(full_width = T) %>%
  column_spec(1,
              width = "1.7in")















# -------------------- PPR Analysis: Create dataframe for t-tests ----------------
PPR_df <- raw_df %>%
  filter(Category == 2) %>%
  filter(Treatment %in% list_of_treatments) %>%
  filter(PPR < 5 & PPR > 0) %>%
  filter(Interval == "t0to5" | Interval == "t20to25") %>%
  mutate(
    Treatment = factor(Treatment, levels = list_of_treatments),
    State = case_when(
      Interval == "t0to5" ~ "Baseline",
      Interval == "t20to25" ~ "Insulin",
      T ~ Interval
    )
  )

t_test_PPR <- PPR_df %>%
  group_by(Treatment, Letter, State) %>%
  summarise(mean_PPR_cell = mean(PPR),
            n = n()) %>%
  ungroup() %>%
  group_by(Treatment) %>%
  t_test(mean_PPR_cell ~ State,
         ref.group = "Baseline",
         paired = T) %>%
  mutate(
    statistic = round(statistic, 2),
    p_original = p,
    p_string = pvalString(p),
    significance_stars = symnum(
      p,
      symbols   = c("***", "**", "*", "ns"),
      cutpoints = c(0,  .001, .01, .05, 1),
      corr      = FALSE
    )
  ) %>%
  arrange(match(Treatment, list_of_treatments))


kable_t_test_table_PPR <- t_test_PPR %>%
  select(c(Treatment, statistic, df, p_string)) %>%
  kable(
    booktabs = T,
    linesep = '',
    col.names = c("Parameter", "Statistic", "DF", "\\textit{p}-value"),
    caption = "A paired t-test comparing the mean paired pulse ratio (PPR) per cell during the baseline period (0-5 min) to the mean PPR after insulin exposure (20-25 min) shows that insulin does not significantly affect the PPR during experiments with no additional treatments, HNMPA, or fasting. Insulin significantly increases the PPR when the insulin receptor blocker HNMPA is applied. \\label{PPR-paired-t-test}",
    caption.short = "Paired T-test summary table examining paired pulse ratios before and after insulin exposure",
    escape = F
  ) %>%
  kable_styling(full_width = T) %>%
  column_spec(1, width = "2.5in")

# -------------- PPR Analysis: Get differences between control and insulin ------------------------
# Create a dataframe containing the differences in PPR from insulin to baseline
# arrange() sorts the data in the form of baseline -> insulin within each letter
# This enables me to use lag() to get:
# PPR_diff = PPR_insulin - PPR_baseline

PPR_differences_df <- PPR_df %>%
  group_by(Treatment, Letter, State) %>%
  summarise(mean_PPR_cell = mean(PPR),
            n = n()) %>%
  ungroup() %>% 
  arrange(Letter, State) %>%
  group_by(Letter) %>%
  mutate(
    PPR_diff = mean_PPR_cell - lag(mean_PPR_cell)
    ) %>%
  ungroup() %>%
  drop_na(PPR_diff)


# ----------------PPR Analysis: Assumption test - Normality
shapiro.test(PPR_differences_df$PPR_diff)

# Sat Mar 30 17:45:29 2024
# The data are normal, indicating that I can use the results from the t-test

# ---------------- Cell Locations: Modify dataset for cell locations plot ------------------------------------------------------------------
# Filter out cells with missing coordinates
# Add a new column called percent change that looks at the percent change 20 minutes after adding insulin
# Filter (for now) to only include full datasets for the experiments where I added insulin (Category == 2)

cell_coordinates <-
  summary_df %>%
  group_by(Treatment, Interval, Letter) %>%
  select(!c(SE,n)) %>%
  pivot_wider(
    names_from = Interval,
    values_from = Mean
  ) %>%
  mutate(
    percent_change = t20to25/t0to5*100
  ) %>%
  filter_at(vars(X, Y), all_vars(!is.na(.))) %>% 
  filter(Category == 2) %>%
  filter(Treatment %in% list_of_treatments)

saveRDS(cell_coordinates, file = here("Data/Output-Data-from-R/cell_coordinates_with_percent_change.RDS"))

background_slice_image <- jpeg::readJPEG(here("Figures/DMH-Image.jpg"))

# --------------------- eEPSC Analysis: Get max percent change after 20 min -----------------------

# Get the value of the maximum percent change after 20 minutes
# I filtered this to only include the treatments of interest
# This is required to set the scale in the cell locations plot.
# plyr::round_any rounds this value up to the nearest multiple of 10
# Required to create 'nice' scale limits
max_percent_change <- cell_coordinates %>%
  ungroup() %>%
  summarise(
    max(percent_change)) %>%
  pull() %>%
  plyr::round_any(10, ceiling)

# ----------------------- AP Analysis: Full Multiplot Figure -----------------------------------
full_ap_multiplot <-
  (full_ap_frequency_plot |
     (ap_trace_baseline / ap_trace_insulin)) / (peak_amplitude_plot + threshold_plot + half_width_plot) /
  (latency_plot + antipeak_amplitude_plot + time_of_antipeak_plot) +
  plot_layout(heights = c(0.8, 0.5, 0.5)) &
  theme(
    axis.title = element_text(size = 12, face = "plain"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 8),
    plot.margin = margin(10, 20, 10, 5)
  )


# Apply theme_void to just the representative traces plots since one already has a scale bar and both of them have been set to the same y-axes

full_ap_multiplot[[1]][[2]][[1]] <- full_ap_multiplot[[1]][[2]][[1]] + theme_void()
full_ap_multiplot[[1]][[2]][[2]] <- full_ap_multiplot[[1]][[2]][[2]] + theme_void()


# Manually applied a list of labels because I wanted the second trace to be blank
# baseline and insulin sweeps both count as figure "B"
# Styled tags separately since theme_void on plots "B" and "[c]" changed the style

full_ap_multiplot <- full_ap_multiplot +
  plot_annotation(tag_levels = list(c('A','B','','C','D','E','F','G','H'))) &
  theme(
    plot.tag = element_text(
      size = 18,
      margin = margin(r = 10, b = 10),
      face = "bold"
    )
  )

full_ap_multiplot

kable_tidy_wilcoxon

# Import representative traces
male_control_trace <- png::readPNG(here("Figures/Representative-Traces/Male-Control-Trace-H.png")) %>% rasterGrob()
female_control_trace <- png::readPNG(here("Figures/Representative-Traces/Female-Control-Trace-AV.png")) %>% rasterGrob()
male_HNMPA_trace <- png::readPNG(here("Figures/Representative-Traces/Male-HNMPA-Trace-BO.png")) %>% rasterGrob()



# ---------------- Get summary plots for all cells --------------------------------------------------------------
# Uses custom function make_pruned_summary_plots()
# 1st argument: Experiment category
# 2nd argument: Treatment
# 3rd argument: pruned dataframe
# 4th argument: Choose colour from colour palette lists defined at the top of the script
# e.g. 1 corresponds to the 1st item in the list, which is purple
make_pruned_summary_plots(category = 2,
                         treatment = "Control",
                         data = pruned_df,
                         color_choice = 1) +
 annotation_custom(female_control_trace, xmin = 1, xmax = 8, ymin = 0, ymax = 40)


kable_manovar_table_eEPSC

make_pruned_summary_plots(category = 2,
                          treatment = "HNMPA",
                          data = pruned_df,
                          color_choice = 5) +
 annotation_custom(male_HNMPA_trace, xmin = 1, xmax = 8, ymin = 0, ymax = 40)

make_pruned_summary_plots(category = 2,
                         treatment = "PPP",
                         data = pruned_df,
                         color_choice = 2)

make_pruned_summary_plots(category = 2,
                         treatment = "Fasting",
                         data = pruned_df,
                         color_choice = 3)

ppr_summary_plot <- PPR_df %>% 
  mutate(
    Treatment_and_state = paste(Treatment, State, sep = "_"),
    Treatment_and_state = factor(
      Treatment_and_state,
      levels = c(
        "Control_Baseline",
        "Control_Insulin",
        "Fasting_Baseline",
        "Fasting_Insulin",
        "HNMPA_Baseline",
        "HNMPA_Insulin",
        "PPP_Baseline",
        "PPP_Insulin"
      )
    )
  ) %>%
  group_by(Treatment_and_state, Treatment, State, Letter, Sex) %>%
  summarise(mean_PPR_cell = mean(PPR)) %>%
  ungroup() %>%
  ggplot(aes(x = interaction(State,Treatment, sep = "!"), y = mean_PPR_cell, color = Treatment, shape = Sex)) +
  geom_point(size = 2, position = position_jitter(0.04), alpha = 0.9) +
  geom_line(aes(group = Letter), linewidth = connecting_line_width_PPR, alpha = 0.3) +
  stat_summary(
    fun.data = "mean_se",
    geom = "pointrange",
    color = mean_point_colour,
    size = mean_point_size + 0.2,
    alpha = 0.7
  ) +
  scale_color_manual(values = c(my_colours[1],
                                my_colours[3],
                                my_colours[5],
                                my_colours[2])) +
  theme(legend.position = "bottom") +
  scale_x_discrete(guide = guide_axis_nested(delim = "!")) +
  coord_cartesian(ylim = c(0,3)) +
  theme(
    ggh4x.axis.nestline = element_line(color = my_colours_pale[6]),
    axis.text.x = element_text(margin = margin(b = 5, t = 5))
  ) +
  add_bracket(treatment = "Control", y_position = 2.5) +
  add_bracket(treatment = "Fasting", y_position = 2.5) +
  add_bracket(treatment = "HNMPA", y_position = 2.5) +
  add_bracket(treatment = "PPP", y_position = 2.5) +
  labs(
    x = NULL,
    y = "Paired pulse ratio",
    shape = NULL
  ) +
  scale_shape_manual(values = c(17, 16)) +
  guides(color = "none", shape = guide_legend(reverse = T))

if (save_choice == "yes") {
  ggsave(
    plot = ppr_summary_plot,
    filename = "PPR-Summary-Plot-paired.png",
    path = here("Figures/Output-summary-plots"),
    width = 7,
    height = 5,
    units = "in",
    dpi = 300
  )
}

ppr_summary_plot

kable_t_test_table_PPR

# ----------------------- Final treatment comparisons summary plot ----------------------------
treatment_comparisons_plot <- cell_coordinates %>%
  group_by(Treatment) %>%
  mutate(
    Treatment = factor(Treatment, levels = list_of_treatments),
    mean_change = mean(percent_change),
    sd_change = sd(percent_change)
  ) %>%
  ggplot(aes(
    x = Treatment,
    y = percent_change,
    color = Treatment,
    shape = Sex
  )) +
  geom_sina(
    bw = 12,
    alpha = 0.8,
    maxwidth = 0.5,
    size = 2
  ) +
  scale_color_manual(values = c(my_colours[1], my_colours[3], my_colours[5], my_colours[2])) +
  scale_shape_manual(values = c(17, 16)) +
  stat_summary(
    fun.data = mean_se,
    geom = "pointrange",
    color = mean_point_colour,
    size = mean_point_size + 0.2,
    alpha = 0.8
  ) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  labs(y = "Change in eEPSC amplitude\n25 min after insulin (%)") +
  theme(legend.position = "right",
        axis.title.y = element_text(angle = 90, hjust = 0.5)) +
  guides(color = "none", shape = guide_legend(reverse = T))

treatment_comparisons_plot

if (save_choice == "yes") {
  ggsave(
    treatment_comparisons_plot,
    path = here("Figures", "Output-summary-plots"),
    file = "Treatment-comparison-plot-percent-change-sex.png",
    width = 7,
    height = 5,
    units = "in",
    dpi = 300
  )
}

# ---------------- Plot cell locations ------------------------------------------------------------------------
# Using rastorGrob to insert the DMH slice image as a background picture
# The scale_colour_viridis_c() is only relevant if there are multiple categories
# If you want to show multiple categories, go to geom_point() and remove 'color = my_colours[4]'

cell_coordinates_plot <-
  ggplot(cell_coordinates %>% filter(!is.na(percent_change)), aes(x = X, y = Y, color = percent_change)) +
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
  labs(y = "y (µM)", x = "x (µM)", color = "% Change in\ncurrent amplitude\nafter 20 min\nof insulin exposure") +
  geom_point(size = 2.7,
             alpha = .99,
             shape = 16) +
  scale_color_viridis_c(
    limits = c(0, max_percent_change),
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
  ) +
  guides(color = guide_colorbar(draw.ulim = T, draw.llim = F, theme = theme(
    legend.key.width = unit(0.7, "lines"),
    legend.key.height = unit(10, "lines")
  )))


cell_coordinates_plot

# Save plot
if (save_choice == "yes") {
  ggsave(
    plot = cell_coordinates_plot,
    filename = "Cell_coordinates_plot_add_insulin.png",
    path = here("Figures/Cell_coordinates_plot"),
    width = 14,
    height = 10,
    units = "in",
    dpi = 300,
    scaling = 1.8
  )
}

knitr::knit_exit()

# ---------------- Write R chunks to a script file ---------------------------------------------
# This code extracts code from Thesis.Rmd and saves it to Scripts/Scripts-from-Thesis-Rmd.R
# Useful if you want an independent script file for debugging

# I set documentation = 0 to extract R code within chunks only
# This code is commented out because it causes issues with knitting
# R thought that there were duplicate chunks

# knitr::purl(input = here::here("Thesis/Thesis.Rmd"), output = here::here("Scripts/Scripts-from-Thesis-Rmd.R"), documentation = 0)



male_point_shape <- 16
female_point_shape <- 15
male_point_color <- my_colours_very_dark
female_point_color <- my_colours_very_pale


# Presentation plot for no treatment condition
presentation_plot_no_treatment_FEMALES <-
  make_presentation_plots_one_sex(
    sex = "Female",
    category = 2,
    point_shape = female_point_shape,
    treatment = "Control",
    data = pruned_df,
    point_color = female_point_color[1]
  ) +
  # For Dr. Crosby's Presentation
  theme(axis.title = element_text(face = "plain"),
        axis.title.y = element_text(angle = 90))

presentation_plot_no_treatment_FEMALES

presentation_plot_no_treatment_MALES <-
  make_presentation_plots_one_sex(
    sex = "Male",
    category = 2,
    point_shape = male_point_shape,
    treatment = "Control",
    data = pruned_df,
    point_color = male_point_color[1]
  ) +
  theme(axis.title = element_text(face = "plain"),
        axis.title.y = element_text(angle = 90))
  
presentation_plot_no_treatment_MALES

save_choice <- "no"
if (save_choice == "yes") {
  ggsave(
    presentation_plot_no_treatment_FEMALES,
    path = here("Figures/Output-summary-plots"),
    file = "Presentation-Plot-No-Treatment-FEMALES.png",
    width = 12,
    height = 7,
    units = "in",
    dpi = 300,
    scaling = 1.5
  )
  
  ggsave(
    presentation_plot_no_treatment_MALES,
    path = here("Figures/Output-summary-plots"),
    file = "Presentation-Plot-No-Treatment-MALES.png",
    width = 12,
    height = 7,
    units = "in",
    dpi = 300,
    scaling = 1.5
  )
}

save_choice <- "no"











all_AP_steps_plot <- abf2_load("../Data/ABF-Files/23d15001.abf") %>%
  MeltAbf() %>%
  rename("Voltage" = chan1,
         "Current" = chan2) %>%
  mutate(
    Sweep = str_sub(Episode, 4),
    Sweep = factor(Sweep, levels = c(
      "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"
    )),
    Current_injection = case_when(
      Sweep == 1 ~ -50,
      Sweep == 2 ~ -40,
      Sweep == 3 ~ -30,
      Sweep == 4 ~ -20,
      Sweep == 5 ~ -10,
      Sweep == 6 ~ 0,
      Sweep == 7 ~ 10,
      Sweep == 8 ~ 20,
      Sweep == 9 ~ 30,
      Sweep == 10 ~ 40,
      T ~ 50
    ),
    Current_injection = factor(
      Current_injection,
      levels = c(-50,-40,-30 , -20,-10, 0, 10, 20, 30, 40)
    )
  ) %>%
  ggplot(aes(
    x = Time,
    y = Voltage,
    group = Current_injection,
    color = Current_injection
  )) +
  geom_line(linewidth = 0.5) +
  labs(x = NULL,
       y = NULL) +
  coord_cartesian(
    xlim  = c(ap_traces_x_min, ap_traces_x_max+500),
    ylim = c(ap_traces_y_min, ap_traces_y_max)
  ) +
  theme_void() +
  scale_color_viridis_d(begin = 0,
                        end = 0.8,
                        option = "plasma",
                        direction = -1) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.2), reverse = T)) +
  theme(
    text = element_text(family = plot_font_family),
    legend.key.width = unit(0.8, "line"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    color = "Current Injection (pA)"
  ) +
  theme(
    legend.position = "none"
  )

all_AP_steps_plot

ggsave(
    plot = all_AP_steps_plot,
    path = here("Figures", "Representative-Traces"),
    file = "All-AP-steps-plot-23d15001.svg",
    width = 7,
    height = 5,
    units = "in",
    dpi = 300
  )
