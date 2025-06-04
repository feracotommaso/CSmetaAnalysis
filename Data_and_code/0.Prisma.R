# install.packages("PRISMA2020")
library(PRISMA2020)
csvFile<-system.file("extdata","PRISMA.csv",package="PRISMA2020") 
data<-read.csv(csvFile) 
data<-PRISMA_data(data)

# Title
data$newstud_text <- "PRISMA flowchart"

# My data
scopus = "225 [215 + 10]"
wos =    "1151 [1081 + 70]"
psy = "1174 [1125 + 49]"
tot = "2550"
dupl = "653 [610 + 43]"
screened = paste0(1811+86, " [1811 + 86]")
included = 308
missingText = 11
final = 130

# Enter data manually
data$database_specific_results <- rbind.data.frame(data$database_specific_results, data$database_specific_results[1,])
data$database_specific_results$reason <- c("Scopus", "Web of science", "Psycinfo", "Total")
data$database_specific_results$n <- c(scopus,wos,psy,tot)
data$duplicates <- dupl
data$records_screened <- screened
data$records_excluded <- (1811+86) - included
data$dbr_sought_reports <- included
data$dbr_notretrieved_reports <- missingText
data$dbr_assessed <- included - missingText

data$dbr_excluded <- data.frame(reason = c("Language","Missing data","No character strengths measures",
                                           "No new data", "No outcome measures", "Other", "Total"),
                                n = c(7,67,79,4,7,3,167))
data$new_studies = "130"

# Remove not used info
data$new_reports = NA
data$database_results <- NA
data$register_results <- NA
data$excluded_automatic <- NA
data$excluded_other <- NA

p<-PRISMA_flowdiagram(data,
                   interactive = FALSE,
                   previous = FALSE,
                   other = FALSE,
                   detail_databases = TRUE,
                   detail_registers = FALSE,
                   fontsize = 14,
                   font = "times",
                   title_colour = "Goldenrod1",
                   greybox_colour = "Gainsboro",
                   main_colour = "Black",
                   arrow_colour = "Black",
                   arrow_head = "normal",
                   arrow_tail = "none",
                   side_boxes = TRUE)
p
