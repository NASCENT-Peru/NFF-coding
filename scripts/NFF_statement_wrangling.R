
library(readxl)

#load excel sheets
Merged_statements <- read_excel("data/NFF_statements_combined.xlsx", sheet = "Merges")
Contributions_statements <- read_excel("data/NFF_statements_combined.xlsx", sheet = "Contributions")
Values_statements <- read_excel("data/NFF_statements_combined.xlsx", sheet = "Values")
NFF_statements <- read_excel("data/NFF_statements_combined.xlsx", sheet = "NFF_unique_statements")

# seperate all entries in Merged_statements$'Original ID' that contain 'C'
Merged_IDs <- grep("C", Merged_statements$'Original ID', value = TRUE)

#vector of all IDs in Contributions_statements
Contributions_IDs <- grep("C", Contributions_statements$ID, value = TRUE)

#remove 'C' from all entries
Merged_IDs <- gsub("C", "", Merged_IDs)
Contributions_IDs <- gsub("C", "", Contributions_IDs)

#convert to numeric
Merged_IDs <- as.numeric(Merged_IDs)
Contributions_IDs <- as.numeric(Contributions_IDs)

#sort both vectors
Merged_IDs <- sort(Merged_IDs)
Contributions_IDs <- sort(Contributions_IDs)


#identify IDs in contributions that are not in Merged
Contributions_not_in_Merged <- Contributions_IDs[!(Contributions_IDs %in% Merged_IDs)]

# Seperate the statements for these IDs
Contributions_statements_not_in_Merged <- Contributions_statements[Contributions_statements$ID %in% Contributions_not_in_Merged, "Contributions"]








