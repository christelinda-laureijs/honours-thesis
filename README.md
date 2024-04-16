README
================
Christelinda Laureijs
Last updated: April 15, 2024

## Project Overview

This file folder contains the data and code used to analyze recordings
from whole-cell patch clamp electrophysiology experiments. The first
goal of the project is to analyze changes in current amplitudes (evoked
excitatory post-synaptic currents; eEPSCs) over time. A second goal is
to create plots of the raw data for each cell (scatterplots of eEPSCs
vs. Time) as well as summary plots for groups of cells.

## Instructions

First, click on the `R-Work.Rproj` file. This will open RStudio and set
the working directory to this folder
`(...your computer path here.../R-Work/)`.

> IMPORTANT: If you skip this step and don’t open RStudio through the
> .Rproj file, R will not know where to find any of the scripts, and you
> will see error messages about missing files.

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
`Scripts` folder. Be sure to read the instructions there.

If you want to quickly view the plots, make sure that you have run the
`Thesis.Rmd` file at least up to the “Begin statistical analysis” code.
This will ensure that the data are processed and ready for plotting.

Then, you can open the `Quick-Plot-Viewer.Rmd` file in the Figures
subfolder.

## Important variables

There are two variables near the top of the script that you may want to
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

> Note: The thesis is best viewed as a PDF. You could also knit to word
> or HTML, but LaTeX features (like figure cross-referencing) will not
> work, and the output will not look good.

## File Structure

**Data/** - Folder containing the raw recording data and animal weights.
The file `eEPSC-metadata.csv` contains explanations of each variable,
which is also in the [metadata table](#metadata) in this document. There
are also subfolders:

- `Output-Data-from-R` contains the processed and cleaned data as R
  object files (.RDS). These are required to run the plots document.
- `Raw-CSVs` contains the CSV files from copying and pasting from
  Clampfit on the lab computer.

**Drafts/** - Folder containing draft data sheets, images, papers, and
scripts. Some scripts came from troubleshooting sessions as I was
learning how to make the analyses work.

**Figures/** - Folder containing all figures. Figures generated from the
`Thesis.Rmd` document will automatically go into folders with names
starting with **Output-**. Also contains the folders
**Gap-Free-Plots/**, **Pruned Summary Plots/**, and
**Discarded-Cells/**.

**Scripts/** - Folder containing R scripts.

- *Functions.r* contains custom functions that I wrote to create the
  summary plots. I put this as an external .R file instead of keeping it
  within `Thesis.Rmd` so that I could use it elsewhere.

- *Scripts-from-Thesis-Rmd.R* contains just the R code contained within
  code chunks of the `Thesis.Rmd` file. This is useful if you want to
  quickly run the same script without needing to scroll past the thesis
  text. It is also useful for debugging; just make sure that you make
  changes within the main `Thesis.Rmd`file, not the script. You can
  regenerate this file this by running the code in the last chunk of the
  `Thesis.Rmd` file (the one beginning with `knitr::purl()`).

<!-- -   *Useful-R-Snippets.R* contains notes to myself about useful functions, and code snippets to solve problems. -->

**Templates/** - Contains my custom style templates for Word
(Thesis-Template.docx), PDF (preamble.tex for `includes: in_header:` in
the YAML) and HTML (my-CSS-theme.css). This folder also contains the
bibliography BibTeX file (thesis-citations.bib) and citation style
language file for APA format (apa.csl). Ignore if you do not need to run
the `Thesis.Rmd` file.

*Notes about the preamble.tex*: This requires some additional LaTeX
packages, and it may take some time to run the first time as it installs
them. This also requires the font EB Garamond, which is a sub-folder in
**Templates/**.

**Thesis/** - Contains the `Thesis.Rmd` file used to generate my thesis,
as well as its Word and PDF outputs. The Word file is mainly for track
changes, and it is missing components like figure numbers and the table
of contents (these can only be rendered properly in LaTeX). To generate
plots without running the entire thesis, see **Scripts/**.

**README.html** - This file. If you want to edit it, or see the code
used to make it, click on **README.html**

**R-Work.Rproj -** The project file, which sets the working directory
for scripts and ensures that the relative paths work properly. Please
click on this to open up R, otherwise the file paths will not work.

## Metadata

These are all variables included the dataframe. Values marked with a \*
will be automatically generated during the R script.

| Variable.Name  | Type                | Description                                                                     |
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
- *AM251* blocks CB1 cannabinoid receptors.
- *Fasting* marks experiments in which I removed food from the rats 24
  hours before euthanization.

**P1 and P2**: Although the raw values were in pA, I normalized these
values relative to the baseline current for each cell. The resulting
percent data has \*\_transformed\* in the variable name.

**Interval**: I named each interval ‘t_to\_’ to avoid using numbers and
special characters in variable names.
