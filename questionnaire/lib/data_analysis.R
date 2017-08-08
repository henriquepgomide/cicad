# ********************
# RECODE DATA 
# ********************

# Load libraries
library(car)
library(formattable)
library(ggmap)
library(tm)

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
cicad$center.training.type.p <- ifelse(cicad$center.center_training_questions.training_type.pres == "True", 'presencial,','') 
cicad$center.training.type.d <- ifelse(cicad$center.center_training_questions.training_type.dist == "True", 'a distancia,','') 
cicad$center.training.type.s <- ifelse(cicad$center.center_training_questions.training_type.semi == "True", 'semipresencial,','') 
cicad$center.training.type <- paste0(cicad$center.training.type.p, cicad$center.training.type.d, cicad$center.training.type.s)
cicad$center.training.type <- ifelse(cicad$center.training.type == "NANANA", NA, cicad$center.training.type)

# Transform boolean current
cicad$center.training.isprovided <- Recode(cicad$center.center_training_questions.training_current,  "'no'='No';'yes'='Si'") 

# Recode boolean certification
cicad$center.center_training_questions.full_questionnaire.training_certification <- Recode(cicad$center.center_training_questions.training_current,  "'no'='No';'yes'='Si'")

# Set address
cicad$address <- paste(cicad$center.identification_questions.address, cicad$center.countries)

# ********************
# DATA ANALYSIS ----
# ********************

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

table0 <- data.frame(Nombre_centro = cicad$center.center_training_questions.center_name,
                     Nombre_capacitacion = cicad$center.center_training_questions.training_name_center,
                     Capacitacion_contenido = cicad$center.center_training_questions.training_content,
                     Capacitation_horas = cicad$center.center_training_questions.training_length,
                     Capacitation_certificacion = cicad$center.center_training_questions.full_questionnaire.training_certification,
                     Pais = cicad$center.countries,
                     Atual = cicad$center.training.isprovided,
                     Desde = cicad$center.center_training_questions.training_since,
                     Modalidade = cicad$center.training.type,
                     Publico.agente = cicad$center.center_training_questions.full_questionnaire.training_public.agen,
                     Publico.assistente.social = cicad$center.center_training_questions.full_questionnaire.training_public.assi,
                     Publico.enfermeiro = cicad$center.center_training_questions.full_questionnaire.training_public.enfe,
                     Publico.fisioterapeuta = cicad$center.center_training_questions.full_questionnaire.training_public.fisi,
                     Publico.medico = cicad$center.center_training_questions.full_questionnaire.training_public.medi,
                     Publico.nutricionista = cicad$center.center_training_questions.full_questionnaire.training_public.nutr,
                     Publico.direito = cicad$center.center_training_questions.full_questionnaire.training_public.dire,
                     Publico.policia = cicad$center.center_training_questions.full_questionnaire.training_public.poli,
                     Publico.docencia = cicad$center.center_training_questions.full_questionnaire.training_public.doce,
                     Publico.tecnico.enfermagem = cicad$center.center_training_questions.full_questionnaire.training_public.tecn,
                     Publico.tecnico.psicologia = cicad$center.center_training_questions.full_questionnaire.training_public.psic,
                     Publico.outros = cicad$center.center_training_questions.full_questionnaire.training_public.other,
                     Target.audience.atencao.primaria.saude = cicad$center.center_training_questions.full_questionnaire.training_targetaudience.aps,
                     Target.audience.saude.geral = cicad$center.center_training_questions.full_questionnaire.training_targetaudience.generalhealth,
                     Target.audience.comunidade = cicad$center.center_training_questions.full_questionnaire.training_targetaudience.com,
                     Target.audience.ambientes.trabalho = cicad$center.center_training_questions.full_questionnaire.training_targetaudience.workplace,
                     Target.audience.populacao.carceraria = cicad$center.center_training_questions.full_questionnaire.training_targetaudience.prison,
                     Target.audience.populacao.rua = cicad$center.center_training_questions.full_questionnaire.training_targetaudience.street,
                     Target.audience.escola = cicad$center.center_training_questions.full_questionnaire.training_targetaudience.school,
                     Target.audience.outra = cicad$center.center_training_questions.full_questionnaire.training_targetaudience.other,
                     Ferramentas.assist = cicad$center.center_training_questions.full_questionnaire.training_tools.assist,
                     Ferramentas.audit = cicad$center.center_training_questions.full_questionnaire.training_tools.audit,
                     Ferramentas.cage = cicad$center.center_training_questions.full_questionnaire.training_tools.cage,
                     Ferramentas.dusi = cicad$center.center_training_questions.full_questionnaire.training_tools.dusi,
                     Ferramentas.ftnd = cicad$center.center_training_questions.full_questionnaire.training_tools.ftnd,
                     Ferramentas.otros = cicad$center.center_training_questions.full_questionnaire.training_tools.other,
                     Acompanhamento.cursistas = cicad$center.center_training_questions.full_questionnaire.training_followup_type,
                     #N.treinado = cicad$center.trained.professionals,
                     #Responsable.name = cicad$center.identification_questions.fullname,
                     Responsable.email = cicad$center.identification_questions.center_email,
                     Center.telefono = cicad$center.identification_questions.telnumber,
                     Center.direccion = cicad$address, 
                     stringsAsFactors = FALSE)

table0 <- data.frame(lapply(table0, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

table1 <- subset(table0, !is.na(table0$Nombre_centro))
table1 <- table1[with(table1, order(Nombre_centro)), ]
write.csv(table1, "../summary_table_1.csv", row.names = FALSE)

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

## DESCRIPTIVES #--------
# Checkout all numeric and integer data
summary(cicad[, sapply(cicad, function(x) is.numeric(x) | is.integer(x))])

# Checkout all character data
sapply(cicad[, sapply(cicad, is.character)], function(x) table(as.factor(x)))
