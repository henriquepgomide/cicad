# *------------------------------------------------------------------
# | PROGRAM NAME: Analysis of CICAD questionnaires - Centers
# | CREATED BY: Henrique Gomide
# *----------------------------------------------------------------
# | PURPOSE: To clean data
# *------------------------------------------------------------------
# |*------------------------------------------------------------------
# | DATA USED:               
# | CICAD Questionnaires
# |*------------------------------------------------------------------

# Libraries
library(car)

# Open data
db <- list.files("../db")[length(list.files("../db"))-1]
cicad <- read.csv(paste0("../db/",db), na.strings = "n/a", stringsAsFactors = FALSE); rm(db)

# ********************
# DATA WRANGLING ----
# ********************

# Select time frame
cicad$start <- strptime(cicad$start, "%Y-%m-%dT%H:%M:%S")
cicad$end <- strptime(cicad$end, "%Y-%m-%dT%H:%M:%S")

# Checkout all numeric and integer data
summary(cicad[, sapply(cicad, function(x) is.numeric(x) | is.integer(x))])

# Checkout all character data
sapply(cicad[, sapply(cicad, is.character)], function(x) table(as.factor(x)))

# Age
# Convert wrong age input into NA
# n73 made a mistake on reporting age. After removal, data is okay.
cicad$professionals.demographic_questions.d_age <- ifelse(cicad$professionals.demographic_questions.d_age > 100, NA, 
                                                          cicad$professionals.demographic_questions.d_age)

# professionals.demographic_questions.d_main_occupation
summary(as.factor(cicad$professionals.demographic_questions.d_main_occupation))
table(cicad$professionals.demographic_questions.d_main_occupation_other)

# Which kind of training have you attended?
## Variables like professionals.training_questions.training_skip.training_name.super needs to be recoded. Many professionals answered multiple courses which were not previously inserted. "Inspect professionals.training_questions.training_skip.training_name_other"

# Training Length
plot(cicad$professionals.training_questions.training_skip.training_length) # Four professionals told they had done more than 4999 of training, which is quite unlikely. Then, we treated these values as missing.

cicad$professionals.training_questions.training_skip.training_length <- 
  ifelse(cicad$professionals.training_questions.training_skip.training_length > 4000, NA, 
         cicad$professionals.training_questions.training_skip.training_length)

# professionals.screening_questions.screening_instruments_other must be recoded in future. Several instruments as CAD - Cuestionario de Abuso de Drogas, POSIT, ISCA, CRAFFT, LIBARE, were pointed by participants.

cicad$center.center_training_questions.training_experience <- ifelse(cicad$center.center_training_questions.training_experience <= 0, NA, 
                                                                     cicad$center.center_training_questions.training_experience)

cicad$center.center_training_questions.training_length <- ifelse(cicad$center.center_training_questions.training_length <= 0, NA, 
                                                                     cicad$center.center_training_questions.training_length)

# Inspect Center Inputs
## Comment: We had multiple inputs from Centro Regional de Referência da Aliança de Redução de Danos Fátima Cavalcanti da Universidade Federal da Bahia which are valid. We removed professionals who were not affiliated with any center: "Como Persona Natural", "Realmente no existen en ecuador centros d Capacitación o formación especifica para estos temas, lo hice cuando fui Coordinador de Salud Mental dirigido a los médicos y personal de salud de Cotopaxi y desde hace 12 años, colaboro y conozco los procesos de alcoholismo y deshabituación alcohólica y drogas con grupos auto-organizados de Alcohólicos Anónimos y Narcóticos Anónimos". 
## Multiple entries: formaensenada@hotmail.com

table(table(cicad$center.center_training_questions.center_name) > 1)

# Subset with only valid data
cicad <- cicad[-c(5, 17, 54, 105, 518, 623, 629, 659), ]
cicad <- subset(cicad, cicad$today > "2017-01-27" | 
                  cicad$X_submitted_by != "henriquepgomide")
#subset(cicad, cicad$center.center_training_questions.full_questionnaire.training_leader_medi >= 1.111111e+12)[1,1:4]

# STORE DATA AS CSV
write.csv(subset(cicad, cicad$intro_question.initial_question == "student"), "../db/toCICAD/health_professionals.csv", row.names = FALSE)
write.csv(subset(cicad, cicad$intro_question.initial_question == "center"), "../db/toCICAD/centers.csv", row.names = FALSE)

