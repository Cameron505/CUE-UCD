# NIFA Soil Health Project (McMillan_Rodrigues)

## Overview
This repository contained the code, data, and analytical workflows for the NIFA Soil Health Project. Soil samples were collected from Russell Ranch in early 2024 to investigate the influence of management practices and plant community composition on soil biogeochemical properties and microbial dynamics.

The study utilized trenched plots designed to minimize the impact of roots and plant communities. Despite being fallowed, conventional plots were treated with herbicides, leading to distinct differences in plant abundance and community composition across fields.

## Experimental Methodology
All experimental procedures were conducted in the past tense to reflect the completed nature of the data collection:

* **Sample Preparation:** Soils were sieved through an 8 mm mesh and stored at -80°C until fractionation. 
* **Fractionation:** Two rounds of fractionation were performed. In the first, aggregates were sieved in large batches and dried to assess physical properties. In the second, soils were processed moist (adjusted to ~68% water holding capacity) to maintain microbial integrity.
* **Microbial Analysis:** 2 g of soil was incubated with $^{18}\text{O}$-labeled $\text{H}_2\text{O}$ to assess carbon use efficiency (CUE). Microbial biomass C and N were determined via chloroform fumigation.
* **Assays:** Enzyme activities were measured via MUF and MC fluorometric microplate assays. FDA (cuvettes) and POXC (microplates) were measured using colorimetric assays.
* **Omics & Fractionation:** DNA was extracted for metagenome analysis and $^{18}\text{O}$ incorporation. Freeze-dried soils were sent for metabolomic analysis. Soil organic matter was separated into fPOM, oPOM, and MAOM fractions for $^{13}\text{C}$ analysis and total C:N analysis.



## Repository Structure
The project was managed by the `targets` package to ensure a reproducible and automated workflow:

* **`_targets.R`**: Located in the root directory; defined the pipeline targets and dependencies.
* **`run.R`**: The script used to execute the pipeline.
* **`Code/`**: Contained R scripts for data processing, cleaning, and analysis.
* **`Data/`**: Contained all summary and intermediate data files.
* **`Reports/`**: Contained generated outputs, including HTML summaries and GitHub-formatted report documents.

## Usage and Dependencies
The analysis was performed using **R**. To reproduce the pipeline, run the following in the root directory:

```r
source("run.R")
# Or alternatively
targets::tar_make()
```
### Key R Packages
The following libraries were utilized within the `Code/packages.R` script:

| Category | Packages |
| :--- | :--- |
| **Data Science** | `tidyverse`, `plyr`, `dplyr`, `reshape2`, `janitor`, `pracma` |
| **Ecology & Stats** | `vegan`, `nlme`, `agricolae`, `rstatix`, `ropls` |
| **Bioinformatics** | `pmartR`, `edgeR`, `pathview`, `clusterProfiler`, `enrichplot` |
| **Visualization** | `ggbiplot`, `ggpubr`, `cowplot`, `trelliscopejs`, `knitr` |

## Data Access
* **Summary Data:** All processed summary data are available within the `/Data` folder of this repository.
* **Raw Sequence Data:** Raw metagenomic sequencing data will be uploaded to the **NCBI** BioProject database.

## Contact
* **Cameron McMillan** — University of California, Davis  
* **Email:** [cmcmillan@ucdavis.edu](mailto:cmcmillan@ucdavis.edu)


