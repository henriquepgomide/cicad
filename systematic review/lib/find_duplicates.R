# Find duplicates
# author: Henrique Gomide
# license: GNU V3

# Load libraries
library(tm)
library(dplyr)

# Open data
df <- read.csv("db/all_records.csv", stringsAsFactors = FALSE)

# Duplicate title column
df$title.cleaned <- df$title
df$title.cleaned <- tolower(df$title.cleaned) ## Convert to lowercase
df$title.cleaned <- removePunctuation(df$title.cleaned) ## Remove punctuation

# Arrange by title.cleaned
df <- arrange(df, title.cleaned)

# Find duplicates
df$duplicated <- duplicated(df$title.cleaned)
df <- df[ -c(5)]
