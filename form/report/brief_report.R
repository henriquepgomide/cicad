# *------------------------------------------------------------------
# | PROGRAM NAME: Analysis of CICAD questionnaires
# | CREATED BY: Henrique Gomide
# *----------------------------------------------------------------
# | PURPOSE: To create descriptive analysis of all Cicad Questionnaires
# *------------------------------------------------------------------
# |*------------------------------------------------------------------
# | DATA USED:               
# | CICAD Questionnaires
# |*------------------------------------------------------------------

# Open data
db <- list.files("../db")[2]
cicad <- read.csv(paste0("../db/",db), na.strings = "n/a", stringsAsFactors = FALSE); rm(db)

# ********************
# DATA WRANGLING ----
# ********************

# Select time frame
cicad$start <- strptime(cicad$start, "%Y-%m-%dT%H:%M:%S")
cicad$end <- strptime(cicad$end, "%Y-%m-%dT%H:%M:%S")

# Subset with only valid data
cicad <- subset(cicad, cicad$today > "2017-01-27")

# Center's vs. Professionals vs. None
table(cicad$intro_question.initial_question)

# Center's names
table(cicad$center.center_training_questions.center_name)

# Center's countries
table(cicad$center.center_training_questions.training_place)