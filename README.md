About the Project
================

<img src="Figures/Presentation-Images/Cover-Image.png" alt="Cover image displaying the text 'Insulin in the Brain' with background graphics of a rat, a neuron and a cartoon representation of insulin" width="1280" style="display: block; margin: auto;" />

## Background

Insulin is a hormone associated with processes like blood glucose
regulation, and it also acts in the brain. For my honours project, I
explored the effect of insulin on neurons in a brain region called the
dorsomedial hypothalamus (DMH). DMH neurons stimulate appetite and they
also have insulin receptors, but almost nothing is known about the
effect of insulin on neuronal activity in the DMH.

I used whole-cell patch clamp electrophysiology to record from the
living DMH neurons of young, male and female Sprague-Dawley rats. I
recorded excitatory evoked currents (a measure of synaptic transmission)
and action potentials (a measure of excitability) before and after
exposing neurons to 500 nM of insulin.

## Goals

I developed this code to achieve the following goals:

- Analyze changes in current amplitudes (evoked excitatory post-synaptic
  currents; eEPSCs) over time
- Analyze changes in action potential parameters before and after
  insulin exposure
- Group and summarize the raw data into publication-ready plots
- Perform appropriate statistical tests on the raw data
- Compile my thesis in a print-ready format with embedded code and
  figures

## Instructions

First, click on the `R-Work.Rproj` file. This will open RStudio and set
the working directory to this folder
`(...your computer path here.../R-Work/)`.

> IMPORTANT: If you don’t open RStudio through the .Rproj file, R will
> not know where to find any of the scripts, and you will see error
> messages about missing files.

Read through the comments in the file (`Thesis.Rmd` or
`Scripts-from-Thesis-Rmd.R`) and run each section in groups (press
Ctrl+Enter to run chunks at a time). To minimize error messages, go from
the top to the bottom and don’t skip lines.

The file `Scripts-from-Thesis-Rmd.R` in the **Scripts/** folder contains
just the R code contained within the `Thesis.Rmd` file. This is useful
if you want to quickly run the same script without needing to scroll
past the thesis text. It is also useful for debugging; just make sure
that you make changes within the main `Thesis.Rmd`file, not the script.

When adding new recordings, use the `P1-Analysis-Script.Rmd` in the
`Scripts` folder. Be sure to read the instructions contained within the
comments there.

If you want to quickly view the plots, make sure that you have run the
`Thesis.Rmd` file at least up to the “Begin statistical analysis” code.
This will ensure that the data are processed and ready for plotting.
Then, you can open and run the `Quick-Plot-Viewer.Rmd` file in the
Figures subfolder.

## Important variables

There are two variables near the top of the scripts that you may want to
change.

`save_choice`: Change this to “no” if you do not want to save the plots
each time the file is run. This is useful for debugging or when
repeatedly running the script, since it will significantly reduce the
run time.

`my_colours`: Use these to define the colours globally.

To generate the thesis as a PDF file, go to the **Thesis/** folder, open
the `Thesis.Rmd` file and click the `Knit` button. If you just want to
run the script and get statistical output and plots, you could click on
the `Run` button.

> Note: The thesis is best viewed as a PDF. LaTeX features (like figure
> cross-referencing) will not work in the other outputs and you will
> receive an error message.

## Where to find things

**Data/** - Contains raw data including metadata (`eEPSC-metadata.csv`),
which can also be found below in the [metadata table](#metadata).

- `Output-Data-from-R` contains the processed and cleaned data as R
  object files (`.RDS`). These are required to run the plots document.
- `Raw-CSVs` contains the CSV files from copying and pasting from
  Clampfit on the lab computer.

**Figures/** - Folder containing all figures. Figures generated from the
`Thesis.Rmd` document will automatically go into folders with names
starting with **Output-**. Also contains the folders
**Gap-Free-Plots/**, **Pruned Summary Plots/**, and
**Representative-Traces/**.

**Scripts/** - Folder containing R scripts.

- *Functions.r* contains custom functions that I wrote to create the
  summary plots. I put this as an external .R file instead of keeping it
  within `Thesis.Rmd` so that I could use it elsewhere.

- *Scripts-from-Thesis-Rmd.R* contains just the R code contained within
  code chunks of the `Thesis.Rmd` file. This is useful if you want to
  quickly run the same script without needing to scroll past the thesis
  text. It is also useful for debugging; just make sure that you make
  changes within the main `Thesis.Rmd`file, not the script.

``` r
knitr::purl(input = here::here("Thesis/Thesis.Rmd"), output = here::here("Scripts/Scripts-from-Thesis-Rmd.R"), documentation = 0)
```

    ## 
    ## 
    ## processing file: C:/Users/cslau/Desktop/Honours-Project/honours-thesis/Thesis/Thesis.Rmd

    ##   |                            |                    |   0%  |                            |                    |   1%                                                   [set-global-options]                               |                            |                    |   2%                                                    |                            |.                   |   3% [load-libraries]                                   |                            |.                   |   4%                                                   [load-functions]                                   |                            |.                   |   5%                                                    |                            |.                   |   6% [set-plot-options]                                                                                   |                            |.                   |   7% [set-ggplot2-theme]                                |                            |..                  |   8%                                                    |                            |..                  |   9% [insulin-schematic]                                                                                  |                            |..                  |  10% [Include-figure-DMH]                               |                            |..                  |  11%                                                    |                            |..                  |  12% [Include-figure-pipettes]                                                                            |                            |...                 |  13% [Include-figure-ap-schematic]                      |                            |...                 |  14%                                                   [include-figure-evoked-currents]                   |                            |...                 |  15%                                                    |                            |...                 |  16% [Include-figure-methods]                           |                            |...                 |  17%                                                   [AP-data-import-and-merge]                         |                            |....                |  18%                                                    |                            |....                |  19% [AP-data-get-representative-traces]                                                                  |                            |....                |  20% [AP-data-get-max-frequencies-per-step]             |                            |....                |  21%                                                    |                            |....                |  22% [AP-data-t-test-AP-frequency]                                                                        |                            |.....               |  23% [AP-data-create-dataset-for-statistical-analysis]  |                            |.....               |  24%                                                   [AP-data-pivot-longer]                             |                            |.....               |  25%                                                    |                            |.....               |  26% [AP-data-parameters-t-test]                        |                            |.....               |  27%                                                   [AP-data-parameters-shapiro-test]                  |                            |......              |  28%                                                    |                            |......              |  29% [AP-data-QQ-plots]                                                                                   |                            |......              |  30% [AP-data-wilcoxon-test]                            |                            |......              |  31%                                                    |                            |......              |  32% [AP-data-make-summary-ap-frequency-plot]                                                             |                            |.......             |  33% [AP-data-make-raw-ap-frequency-plot]               |                            |.......             |  34%                                                    |                            |.......             |  35% [AP-data-make-AP-characteristics-plots]                                                              |                            |.......             |  36% [eEPSC-data-import-data]                           |                            |.......             |  37%                                                   [eEPSC-data-make-raw-plots]                        |                            |........            |  38%                                                    |                            |........            |  39% [eEPSC-data-individual-pruned-df]                  |                            |........            |  40%                                                   [eEPSC-data-make-individual-pruned-plots]          |                            |........            |  41%                                                    |                            |........            |  42% [eEPSC-data-make-pruned-df-full]                                                                     |                            |.........           |  43% [eEPSC-data-create-wide-format]                    |                            |.........           |  44%                                                    |                            |.........           |  45% [eEPSC-data-create-insulin-only-df]                                                                  |                            |.........           |  46% [eEPSC-data-MANOVAR-adding-insulin]                |                            |.........           |  47%                                                   [eEPSC-data-get-manovar-dataframe]                 |                            |..........          |  48%                                                    |                            |..........          |  49% [eEPSC-get-publication-ready-manovar-table]        |                            |..........          |  50%                                                   [eEPSC-data-show-sphericity-output]                |                            |..........          |  51%                                                    |                            |..........          |  52% [eEPSC-data-univariate-levene-s-test]              |                            |...........         |  53%                                                   [eEPSC-data-univariate-model]                      |                            |...........         |  54%                                                    |                            |...........         |  55% [eEPSC-data-univariate-normality]                                                                    |                            |...........         |  56% [eEPSC-data-multivariate-box-s-M]                  |                            |...........         |  57%                                                    |                            |............        |  58% [eEPSC-data-multi-shapiro]                                                                           |                            |............        |  59% [eEPSC-data-t-tests]                               |                            |............        |  60%                                                   [PPR-data-create-dataframe-for-t-test]             |                            |............        |  61%                                                    |                            |............        |  62% [PPR-data-t-test]                                  |                            |.............       |  63%                                                   [PPR-data-create-differences-df]                   |                            |.............       |  64%                                                    |                            |.............       |  65% [PPR-data-shapiro]                                                                                   |                            |.............       |  66% [cell-coordinates-df]                              |                            |.............       |  67%                                                    |                            |..............      |  68% [eEPSC-get-maximum-percent-change]                                                                   |                            |..............      |  69% [AP-data-multiplot-figure]                         |                            |..............      |  70%                                                    |                            |..............      |  71% [unnamed-chunk-5]                                                                                    |                            |..............      |  72% [import-representative-traces]                     |                            |...............     |  73%                                                   [plot-control]                                     |                            |...............     |  74%                                                    |                            |...............     |  75% [unnamed-chunk-6]                                  |                            |...............     |  76%                                                   [plot-HNMPA]                                       |                            |...............     |  77%                                                    |                            |................    |  78% [plot-PPP]                                                                                           |                            |................    |  79% [plot-Fasting]                                     |                            |................    |  80%                                                    |                            |................    |  81% [PPR-data-final-summary-plot]                                                                        |                            |................    |  82% [unnamed-chunk-7]                                  |                            |.................   |  83%                                                   [treatment-comparisons-plot]                       |                            |.................   |  84%                                                    |                            |.................   |  85% [plot-cell-coordinates]                            |                            |.................   |  86%                                                   [exit-knitting]                                    |                            |.................   |  87%                                                    |                            |..................  |  88% [extract-r-code]                                                                                     |                            |..................  |  89% [get-presentation-plot-no-treatment]               |                            |..................  |  90%                                                    |                            |..................  |  91% [one-sex-only-plots]                                                                                 |                            |..................  |  92% [get-presentation-plot-HNMPA]                      |                            |................... |  93%                                                    |                            |................... |  94% [get-presentation-plot-fasting]                                                                      |                            |................... |  95% [get-presentation-plot-PPP]                        |                            |................... |  96%                                                   [cell-coordinates-no-axes]                         |                            |................... |  97%                                                    |                            |....................|  98% [cell-coordinates-no-axes-by-treatment]            |                            |....................|  99%                                                   [get-representative-current-clamp-steps-image]     |                            |....................| 100%                                                  

    ## output file: C:/Users/cslau/Desktop/Honours-Project/honours-thesis/Scripts/Scripts-from-Thesis-Rmd.R

    ## [1] "C:/Users/cslau/Desktop/Honours-Project/honours-thesis/Scripts/Scripts-from-Thesis-Rmd.R"

**Templates/** - Contains my custom style templates: -
*MtA-Thesis-Preamble.tex* for PDF - *my-CSS-theme.css* for HTML -
*thesis-citations.bib* for all in-text citations. - *packages.bib* for
all R packages used in my scripts

> The MtA-Thesis-Preamble.tex requires additional LaTeX packages, and it
> may take some time to run the first time as it installs them. This
> also requires the font EB Garamond, which is a sub-folder in
> **Templates/**.

**Thesis/** - Contains the `Thesis.Rmd` file used to generate my thesis,
as well as its PDF output.

**R-Work.Rproj -** The project file, which sets the working directory
for scripts and ensures that the relative paths work properly. Please
click on this to open up R, otherwise the file paths will not work.

## Metadata

These are all variables included the dataframe. Values marked with a \*
will be automatically generated during the R script.

| Variable       | Type                | Description                                                                     |
|:---------------|:--------------------|:--------------------------------------------------------------------------------|
| Letter         | character           | Letter ID of the recording for easy cross-referencing to my lab book            |
| Synapses       | character           | Synapses being observed                                                         |
| Sex            | character           | Biological sex of the animal                                                    |
| Treatment      | character           | Treatment applied, see README file                                              |
| Time           | numeric             | Time in minutes                                                                 |
| ID             | character           | ID from Clampex; not used in this file                                          |
| P1             | numeric             | The first evoked excitatory post-synaptic current (eEPSC) amplitude             |
| P2             | numeric             | The second eEPSC amplitude                                                      |
| X              | numeric             | Location of the cell in $\mu$M lateral to the top of the third ventricle        |
| Y              | numeric             | Location of the cell in $\mu$M ventral to the top of the third ventricle        |
| Age            | integer             | Age of the animal in days                                                       |
| Animal         | numeric             | Unique ID of the animal; could be adapted to include both n and N on graphs     |
| Category       | factor: 3 levels    | Experiment category; see below in README for further explanations               |
| PPR            | numeric             | \*The paired pulse ratio is the ratio of P2/P1                                  |
| Interval       | factor: 6 levels    | \*Divides time into six 5-min intervals for MANOVAR                             |
| baseline_range | logic               | \*Returns TRUE if Time \<= 5 min. Required for the normalization function later |
| baseline_mean  | numeric             | \*Mean current amplitude within the baseline period of each recording           |
| P1_transformed | numeric             | \*P1 normalized as a percentage relative to the baseline                        |
| P2_transformed | numeric             | \*P2 normalized as a percentage relative to the baseline                        |
| Animal_factor  | factor: many levels | \*Animal column coded as a factor                                               |

Additional notes about some variables:

**Category**: I assigned each experiment type with its own number ID and
used this convention for all scripts.

1.  HFS in control conditions
2.  Adding insulin in control conditions
3.  HFS in the presence of insulin

**HFS**: High frequency stimulation was 100 Hz applied for 4 seconds,
which was repeated again after a 20-second interval.

**Treatment**:

- *None* indicates no additional drugs or modifications.
- *HNMPA* is a tyrosine kinase inhibitor that blocks insulin receptors.
- *PPP* is an insulin-like growth factor 1 receptor blocker.
- *AM251* blocks CB1 cannabinoid receptors.
- *Fasting* marks experiments in which I removed food from the rats 24
  hours before euthanization.

**P1 and P2**: Although the raw values were in pA, I normalized these
values relative to the baseline current for each cell. The resulting
percent data has \*\_transformed\* in the variable name.

**Interval**: I named each interval ‘t_to\_’ to avoid using numbers and
special characters in variable names.
