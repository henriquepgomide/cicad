# ********************
# RECODE DATA 
# ********************

# Recode Sex
#cicad$professionals.demographic_questions.d_sex <- Recode(cicad$professionals.demographic_questions.d_sex, 
#                                                   "'male' = 'Masculino'; 'female' = 'Femenino'")

# Recode Countries
countries <- "'arge' = 'Argentina';
'bras' = 'Brasil';
'boli' = 'Bolivia';
'chil' = 'Chile';
'colo' = 'Colombia';
'cost' = 'Costa Rica';
'cuba' = 'Cuba';
'el_s' = 'El Salvador';
'equa' = 'Ecuador';
'guat' = 'Guatemala';
'hait' = 'Haiti';
'hond' = 'Honduras';
'mexi' = 'México';
'nica' = 'Nicaragua';
'pana' = 'Panama';
'para' = 'Paraguay';
'peru' = 'Perú';
'porto_rico' = 'Puerto Rico';
'repu' = 'República Dominicana';
'sain' = 'Santa Lucia';
'urug' = 'Uruguay';
'vene' = 'Venezuela';"

cicad$center.countries <- Recode(cicad$center.center_training_questions.training_place, countries)
cicad$professionals.demographic_questions.d_country <- Recode(cicad$professionals.demographic_questions.d_country, countries)
rm(countries)

# Transform NA's into zeroes
cicad[, 151:163] <- sapply(cicad[, 151:163], function(x) as.integer(x))
cicad[, 151:163] <- sapply(cicad[, 151:163], function(x) ifelse(x < 0, NA, x))
cicad[, 151:163] <- sapply(cicad[, 151:163], function(x) ifelse(is.na(x), 0, x))
# Create var trained professionals
cicad$center.trained.professionals <- rowSums(cicad[, 151:163])

# Transform type of training as string
cicad$center.training.type.p <- ifelse(cicad$center.center_training_questions.training_type.pres == "True", 'p,','') 
cicad$center.training.type.d <- ifelse(cicad$center.center_training_questions.training_type.dist == "True", 'd,','') 
cicad$center.training.type.s <- ifelse(cicad$center.center_training_questions.training_type.semi == "True", 's,','') 
cicad$center.training.type <- paste0(cicad$center.training.type.p, cicad$center.training.type.d, cicad$center.training.type.s)
cicad$center.training.type <- ifelse(cicad$center.training.type == "NANANA", NA, cicad$center.training.type)

# Transform boolean current
cicad$center.training.isprovided <- Recode(cicad$center.center_training_questions.training_current,  "'no'='No';'yes'='Si'") 
# Set address
cicad$address <- paste(cicad$center.identification_questions.address, cicad$center.countries)

# ********************
# DATA ANALYSIS ----
# ********************

# Load libraries
library(formattable)
library(ggmap)

# Center's vs. Professionals vs. None
table(cicad$intro_question.initial_question)

# Center's names
table(cicad$center.center_training_questions.center_name)
cat("Number of centers with names: ", table(is.na(cicad$center.center_training_questions.center_name))[1])

# Training Type
table(cicad$center.center_training_questions.training_type.dist)
table(cicad$center.center_training_questions.training_type.semi)
table(cicad$center.center_training_questions.training_type.pres)

# Country
countries <- cbind(table(cicad$center.countries))

# Average time to fill in the questionnaire
summary(cicad$X_duration)/60

# ********************
# TABLE PRESENTATION ----
# ********************

table0 <- data.frame(Nombre = cicad$center.center_training_questions.center_name,
                     Pais = cicad$center.countries,
                     Atual = cicad$center.training.isprovided,
                     Desde = cicad$center.center_training_questions.training_since,
                     Modalidade = cicad$center.training.type,
                     N_treinado = cicad$center.trained.professionals)

table1 <- subset(table0, table0$N_treinado != 0)
table1 <- table1[with(table1, order(-N_treinado)), ]
write.csv(table1, "../db/summary_table_1.csv", row.names = FALSE)

# ********************
# MAP ----
# ********************

geo.info <- geocode(cicad$address)
map1 <- cbind(table0, geo.info)
write.csv(map1, "map.csv")

## CENTER DATA #--------
center <- subset(cicad, cicad$intro_question.initial_question == "center")
center <- center[, grepl("center.", names(center))]
center <- center[,colSums(is.na(center))<nrow(center)]
