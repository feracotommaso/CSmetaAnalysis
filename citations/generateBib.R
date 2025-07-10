library(RefManageR)
library(readxl)
# Load your Excel file (change the path if needed)
df = read_excel(here::here("Data_and_code/metaData/finalMetaData.xlsx"))

# Optional - Filter unique entries (based on DOI or title)
df_unique <- df[!duplicated(df$doi), ]
df_unique <- df[!duplicated(df$ID), ]

# Read the bib of all articles included in screening
bibAll <- RefManageR::ReadBib("citations/articles.bib")

# Clean titles from your dataset
titles_df <- trimws(tolower(df_unique$title))

# Clean titles from BibEntry object
titles_bib <- tolower(sapply(bibAll, function(x) x$title))
titles_bib <- as.character(titles_bib)

# Titles that do not match exactly
unmatched_titles <- titles_df[titles_df %in% titles_bib == FALSE]

# Fuzzy matching: only unmatched vs all BibTeX titles
dist_matrix <- stringdistmatrix(unmatched_titles, titles_bib, method = "osa")

# Get best match (lowest distance) for each unmatched title
closest_match_index <- apply(dist_matrix, 1, which.min)
closest_distances <- apply(dist_matrix, 1, min)

# Set a distance threshold for fuzzy match acceptance
threshold <- 20

# Build a data frame of fuzzy matches
fuzzy_matches <- data.frame(
  original_title = unmatched_titles,
  matched_bib_title = sapply(closest_match_index, function(i) bibAll[[i]]$title), #titles_bib[closest_match_index],
  distance = closest_distances,
  stringsAsFactors = FALSE
)

# Filter to only strong fuzzy matches
fuzzy_matches <- fuzzy_matches[fuzzy_matches$distance <= threshold, ]

# Exact matches
titles_exact <- titles_df[titles_df %in% titles_bib]

# Fuzzy matches — use the matched bib title column
titles_fuzzy <- tolower(fuzzy_matches$matched_bib_title)

# Combine all matched titles
all_matched_titles <- unique(c(titles_exact, titles_fuzzy))

# Filter the bib
matched <- titles_bib %in% all_matched_titles
filtered_bib <- bibAll[matched]

# Write the final bib
WriteBib(filtered_bib, file = "filtered_articles.bib")

