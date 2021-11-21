# Artefact-Detection-And-Removal-Benchmarking-Tool

DOI:10.5281/zenodo.5716306

Different sources of noise are present in the brain, and to accurately understand the brain functionality, they must be removed a priory of the study. Due to the nature and complexity of both neuronal signals and artefacts, computational techniques provide powerful tools for their detection and removal. Despite many studies available in the literature, there is no study that facilitates the selection of an appropriate method for a given signal. Here we present the code of an online benchmarking tool that has been developed to aid an appropriate selection of machine learning based methods and to compare obtained results with the state-of-the-art.

The source code is in the file BMT_plotly_mod.R, which takes the alert message from intro_text.html, the dataset from online table v2.xlsx and the corresponding references from otr v2.xlsx

Dependencies:
R version 4.0.5 (2021-03-31)

Packages : The application use the following packages : shiny, shinyjs, DT, readxl, shinyWidgets, ggplot2, dplyr, plotly.

## Downloading the repository :arrow_down:

For users unfamiliar with git, the current version of the repository can be downloaded by simply clicking the green _Code_ button, and then clicking _Download ZIP_.

Otherwise, by using the command line

$ git clone --bare https://github.com/IgnacioFabietti/Artefact-Removal-Benchmarking-Tool

Makes a bare clone of the external repository in a local directory


## Online site:
https://nachodev.shinyapps.io/ARPCT/

**Feel free to [email me](mailto:n0892706@my.ntu.ac.uk) for help**
