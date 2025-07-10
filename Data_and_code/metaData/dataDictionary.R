library(datadictionary)
library(readxl)

dtot = read_excel(here::here("Data_and_code/metaData/finalMetaData.xlsx"))
names(dtot)[12] <- "Gender"
df_labels <- list(
  ID = "Study ID",
  sampleID = "Sample ID within each study",
  sample = "Unique identifier of study and sample",
  k = "Study citation",
  kk = "Sample citation",
  authors = "List of authors",
  year = "Year of publication of the study",
  title = "Title of the publication",
  doi = "DOI of the publication",
  Study_design = "Design of the study",
  Country = "Country of the sample",
  Gender = "Percentage of female participants",
  Mean_age = "Mean age of the sample",
  N = "Sample size",
  Age_classification = "Whether the sample is composed by youths, adults, or older adults",
  Population = "Whether the sample is a clinical or non-clinical sample",
  Population_type = "If clinical, which specific clinical sample",
  CS_measure = "VIA measure used",
  CS_measure_type = "Whether the VIA measure is in a long or short format",
  Outcome = "Whether the outcome is a well-being or mental health outcome",
  Outcome_type = "The specific outcome category (e.g., anxiety, happiness, life satisfaction)",
  domsat_type = "If outcome type is domain satisfaction, the domain of reference",
  Outcome_measure = "The measure used to assess the outcome",
  Outcome_measure_other = "If not included in the list, the specific measure used",
  Valence = "Whether higher values in the outcome indicate lower well-being",
  out_rel = "Reliability index (alpha) of the outcome",
  Appreciation_of_beauty = "For each strength, the Pearson correlation with the outcome"
)

dataDictionary <- datadictionary::create_dictionary(dtot, var_labels = df_labels)
writexl::write_xlsx(dataDictionary,"Data_and_code/metaData/dataDictionary.xlsx")
