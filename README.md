# Bridges over troubled water: A meta-analysis of the associations of character strengths with well-being and common mental health disorders

## Direct links of interest

Shiny app for navigating results: https://feracoshiny.shinyapps.io/character-meta-v3/

OSF project: https://doi.org10.17605/OSF.IO/PRJXB

PROSPERO preregistration: https://www.crd.york.ac.uk/prospero/display_record.php?ID=CRD42023488089 and https://feracotommaso.github.io/CSmetaAnalysis/PROSPERO_preregistration.pdf

Supplementary results: https://feracotommaso.github.io/CSmetaAnalysis/supplementary/SupplementaryResults.pdf

Author version of the paper: https://feracotommaso.github.io/CSmetaAnalysis/paper/paper.pdf

## Project Structure

- `Data_and_code/`             
  - Contains the scripts for running the main analyses (`1.MetaAnalisi.R`) and creating the plots (`0.Prisma.R`, `2.Figures.R`)
  - `metaData/`: Contains the final dataset for the analyses (`finalMetaData.xlsx`) and the **data dictionary** (`dataDictionary.xlsx`)
- `results/`: Contains the already-computed results of the metaanalysis in .RDS format
- `paper/`: Contains the author version of the manuscript
- `Shiny_app_code/`  
  - Contains the R script to run the app locally (`app.R`)
  - `R/`: Contains custom functions for the Shiny app
  - `results/`: Contains the meta-analytic results used in the Shiny app for the plots
- `supplementary/`: Contains the markdown summary of the complete and supplementary results
- `citations`: Contains the citations of all the studies included in the meta-analysis
- `R`: Contains custom functions
- `PROSPERO_preregistration.pdf`
- `index.Qmd`
- `README.md`
- `GitMeta..Rproj`

## How to run the analysis

- This requires R or Rstudio (install them if needed)
- Download the entire folder 
- Open the project 'GitMeta.Rproj'
- Access the 'Data_and_code' folder and open the '1.metaAnalisis.R' script. This script allows you to entirely replicate each analyses performed. 
- To reproduce the plots use the '2.Figures.R' script available in the 'Data_and_code' folder.
- To reproduce the tables, open the 'SupplementaryResults.qmd' file within the 'supplementary' folder. This generates the corresponding pdf file with all tables.
- The Shiny app code is available in the 'Shiny_app_code' folder. You can run the 'app.R' file to open the app locally.