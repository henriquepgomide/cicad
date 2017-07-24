# Load packages
require(mosaic)   
library(knitr)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(scales)
library(car)
library(dplyr)
library(tidyr)
library(tibble)
library(sjPlot)

# Some customization.  You can alter or delete as desired (if you know what you are doing).
options(OutDec= ",")
trellis.par.set(theme=theme.mosaic()) # change default color scheme for lattice
knitr::opts_chunk$set(
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code

# Set makeTable function 
makeTable <- function(var, table_caption) {
tb <- cbind(prop.table(sort(table(var), decreasing = TRUE)))*100
tb <- cbind(sort(table(var), decreasing = TRUE), tb)
tb <- data.frame(tb)
tb <- rownames_to_column(tb, "options")
tb <- arrange(tb, -X2)

print(kable(tb, 
      caption = table_caption, 
      digits = 1, align = c("l","c","c"),
      col.names = c("", "N", "%")))

}

# Carregar banco de dados da cicad
cicad <- read.csv("../../db/cicad.csv", stringsAsFactors = FALSE)
students <- subset(cicad, cicad$intro_question.initial_question == "student")
centers <- subset(cicad, cicad$intro_question.initial_question == "center")
email_marketing <- read.csv("../../../questionnaire/recruitment/email_marketing_results/email_marketing_results.csv", stringsAsFactors = FALSE, dec = ",")
kable(email_marketing[, c(1,2,5,7,8)], 
      caption = "Resultados do recrutamento feito através de campanhas de e-mail marketing.", 
      digits = 2, align = c("l","c", "c","c","c"),
      col.names = c("Campanha", "N de Destinatários", "Taxa de Abertura", "Cliques", "Taxa de cliques"))
table0 <- cbind(prop.table(sort(table(cicad$intro_question.initial_question), decreasing = TRUE)))*100
table0 <- cbind(sort(table(cicad$intro_question.initial_question), decreasing = TRUE), table0)
colnames(table0) <- c("Frequência (%)","N")
rownames(table0) <- c("Profissionais", "Nenhum", "Centros")
table0 <- data.frame(table0)

kable(table0,
      caption = "Frequência de respostas dos questionários por tipo.",
      digits = 1,
      col.names = c("N", "%"),
      align = "cc")
countries_data <- data.frame(table(centers$center.center_training_questions.training_place))
# Recode Countries
countries <- "'arge' = 'Argentina';
'bras' = 'Brazil';
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
'mexi' = 'Mexico';
'nica' = 'Nicaragua';
'pana' = 'Panama';
'para' = 'Paraguay';
'peru' = 'Peru';
'porto_rico' = 'Puerto Rico';
'repu' = 'Dominican Republic';
'sain' = 'Santa Lucia';
'urug' = 'Uruguay';
'vene' = 'Venezuela';"

colnames(countries_data) <- c("country", "freq")
countries_data$country <- as.character(Recode(countries_data$country, countries))

# Plot map
map.world <- map_data("world")
map.world_joined <- left_join(map.world, countries_data, by = c("region" = "country"))
ggplot() + geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = cut(freq, 5))) + 
  xlim(-130, -20) + ylim(-60,40) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  coord_equal() + theme_map(base_size = 4) + scale_fill_brewer(name = "Frequência", type = "seq", palette = 5, na.value = "#E5E5E5")
countries_table <- filter(countries_data, country != "other")
countries_table <- mutate(countries_table, freq.per = freq / sum(freq)*100)

kable(arrange(countries_table, - freq), 
      caption = "Distribuição de resposta dos centros de formação por país", 
      digits = 1, align = c("l","c","c"),
      col.names = c("País", "N", "%"))
merged_table <- data.frame(a = c("Oferecida atualmente", "", "Módulo destinado a IB", "", "Modalidade", "", ""),
                           b = c("Sí", "No", "Sí", "No", "Presencial", "À distância", "Semipresencial"),
                           N = c(76,26,76,26,93,14,6),
                           freq = c(74.5,25.5,74.5,25.5,91.2,13.7,5.9))

kable(merged_table, 
      caption = "Características das capacitações oferecidas pelos centros", 
      digits = 1, align = c("l", "l", "c", "c"),
      col.names = c("", "", "N", "%"))
# ¿A qué tipo de servicio pertenece el centro de capacitación?
training_sector <- centers$center.center_training_questions.full_questionnaire.training_sector
training_sector <- Recode(training_sector,
                           "'public' = 'Público'; 
                           'private' = 'Privado';
                            'ngos' = 'Organizaciones no gubernamentales (ONG)';
                           ")
makeTable(training_sector, 
          "Origem/tipo do Centro de Capacitação")
training_public <- centers[, 138:150]
training_public <- data.frame(cbind(t(sapply(training_public, function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))
rownames(training_public) <- c('Agente/líder/educador (a) comunitario (a) en salud',
                               'Trabajo social',
                              'Enfermería',
                              'Fisioterapia',
                              'Medicina',
                              'Nutrición',
                              'Derecho',
                              'Policía',
                              'Docencia',
                              'Auxiliar/ técnico de enfermería',
                              'Psicología',
                              'Estudiante',
                              'Otro')

training_public <- rownames_to_column(training_public, "options")
colnames(training_public) <- c("options", "Sí")

kable(arrange(training_public, -Sí), 
      caption = "Categorías profesionales capacitadas pelos Centros", 
      digits = 1, align = c("l", "c"),
      col.names = c("", "Sí (%)"))

merged_table <- data.frame(a = c("Uso de instrumentos", "", "Instrumentos", "", "", "", "", ""),
                           b = c("Sí", "No", "AUDIT", "ASSIST", "FAGERSTROM", "CAGE", "DUSI", "Outros"),
                           N = c(56,20,48,42,33,18,5,41),
                           freq = c(73.7,26.3,85.7,75.0,58.9,32.1,8.9,26.8))

kable(merged_table, 
      caption = "Inclusão de Instrumentos de Triagem na Capacitação? Qual (is)?", 
      digits = 1, align = c("l", "l", "c", "c"),
      col.names = c("", "", "N", "%"))
# ¿Qué tipo de recursos son utilizados?
teaching_sources <- centers[, 181:185]
teaching_sources <- data.frame(cbind(t(sapply(teaching_sources, function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))
rownames(teaching_sources) <- c("Libros o folletos informativos",
                          "Videos",
                          "Audios",
                          "Cartillas",
                          "Otros"
                          )
teaching_sources <- rownames_to_column(teaching_sources, "options")
colnames(teaching_sources) <- c("options", "Sí")
kable(arrange(teaching_sources, -Sí), 
      caption = "Recursos pedagógicos utilizados nas capacitações", 
      digits = 1, align = c("l", "c"),
      col.names = c("", "Sí (%)"))
# ¿Qué tipo de recursos son utilizados?
courses_characteristcs <- centers[, c(187,194,196,197, 199,200)]
courses_characteristcs <- data.frame(cbind(t(sapply(courses_characteristcs, function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))
rownames(courses_characteristcs) <- c("Certificado oficialmente válido",
                          "Respaldos oficiales de autoridades educativas",
                          "Actualización continuada",
                          "Acompañamiento o seguimiento para las personas capacitadas",
                          "Realizada alguna práctica como complemento",
                          "Situaciones prácticas que permitan verificar lo aprendido"
                          )
colnames(courses_characteristcs) <- c("Sí")
courses_characteristcs <- rownames_to_column(courses_characteristcs, "options")
colnames(courses_characteristcs) <- c("options", "Sí")

kable(arrange(courses_characteristcs, -Sí), 
      caption = "Características e estratégias adotadas pelos centros", 
      digits = 1, align = c("l", "c"),
      col.names = c("", "Sí (%)"))
# ¿Cómo se difunde la oferta académica del Centro de Formación?
training_marketing <- centers[, 188:192]
training_marketing <- data.frame(cbind(t(sapply(training_marketing, function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))
rownames(training_marketing) <- c("Redes sociales",
                          "Página web del Centro de Formación",
                          "Lista de correos electrónicos",
                          "Referencia de autoridades locales, nacionales o internacionales",
                          "Otros"
                          )
colnames(training_marketing) <- c("Sí")
kable(training_marketing, 
      caption = "Estratégias de divulgação das capacitações", 
      digits = 1, align = c("c"),
      col.names = c("Sí (%)"))
df <- subset(students, !is.na(students$professionals.demographic_questions.d_sex))
df$professionals.demographic_questions.d_sex <- Recode(df$professionals.demographic_questions.d_sex, "'female'='Feminino';'male'='Masculino'")
df$professionals.demographic_questions.d_age <- as.numeric(df$professionals.demographic_questions.d_age)
df <- subset(students, !is.na(students$professionals.demographic_questions.d_sex))
df$professionals.demographic_questions.d_sex <- Recode(df$professionals.demographic_questions.d_sex, "'female'='Feminino';'male'='Masculino'")

ggplot(df, aes(x = factor(professionals.demographic_questions.d_sex), y = (..count..)/sum(..count..))) + 
  geom_bar(na.rm = TRUE, fill = "#7cb5ec", width = .5) + xlab("Sexo") + ylab("Frequência (%)") + 
  theme_hc() + scale_colour_hc() + 
  scale_y_continuous(labels = scales::percent)
students$professionals.demographic_questions.d_age <- as.numeric(students$professionals.demographic_questions.d_age)
ggplot(subset(students, !is.na(students$professionals.demographic_questions.d_age)), aes(professionals.demographic_questions.d_age, fill = professionals.demographic_questions.d_sex)) + 
  geom_histogram(stat = "count") + 
  xlab("Idade") + ylab("Frequência") + 
  theme_hc() + scale_colour_hc() + 
  scale_fill_manual(name = "Sexo", values = c("#f7a35c", "#7cb5ec"),labels = c("Feminino", "Masculino"))
countries_data <- data.frame(table(students$professionals.demographic_questions.d_country))
# Recode Countries
countries <- "'arge' = 'Argentina';
'bras' = 'Brazil';
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
'mexi' = 'Mexico';
'nica' = 'Nicaragua';
'pana' = 'Panama';
'para' = 'Paraguay';
'peru' = 'Peru';
'porto_rico' = 'Puerto Rico';
'repu' = 'Dominican Republic';
'sain' = 'Santa Lucia';
'urug' = 'Uruguay';
'vene' = 'Venezuela';"

colnames(countries_data) <- c("country", "freq")
countries_data$country <- as.character(Recode(countries_data$country, countries))

# Plot map
map.world <- map_data("world")
map.world_joined <- left_join(map.world, countries_data, by = c("region" = "country"))
ggplot() + geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = cut(freq, 5))) + 
  xlim(-130, -20) + ylim(-60,40) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  coord_equal() + theme_map(base_size = 4) + scale_fill_brewer(name = "Frequência", type = "seq", palette = 1, na.value = "#E5E5E5")
countries_table <- filter(countries_data, country != "other")
countries_table <- mutate(countries_table, freq.per = freq / sum(freq)*100)

kable(arrange(countries_table, - freq), 
      caption = "Distribuição dos respondentes por país", 
      digits = 1, align = c("l","c","c"),
      col.names = c("País", "N", "%"))
tb_main_ocupation <- filter(students,
                            !is.na(professionals.demographic_questions.d_main_occupation))
tb_main_ocupation <- data.frame(table(students$professionals.demographic_questions.d_main_occupation))

ocupations <- "
'agen' = 'Agente/líder/educador (a) comunitario (a) en salud';
'assi' = 'Trabajo social';
'enfe' = 'Enfermería';
'fisi' = 'Fisioterapia';
'medi' = 'Medicina';
'nutr' = 'Nutrición';
'dire' = 'Derecho';
'poli' = 'Policía';
'doce' = 'Docencia';
'tecn' = 'Auxiliar/ técnico de enfermería';
'psic' = 'Psicología';
'estud' = 'Estudiante';
'other' = 'Otro';"

colnames(tb_main_ocupation) <- c("Ocupation", "freq")
tb_main_ocupation$Ocupation <- as.character(Recode(tb_main_ocupation$Ocupation, ocupations))

ocupation_table <- mutate(tb_main_ocupation, freq.per = freq / sum(freq)*100)
ocupation_table <- arrange(ocupation_table, - freq)
kable(ocupation_table, 
      caption = "Categoria Profissional dos participantes", 
      digits = 1, align = c("l","c","c"),
      col.names = c("Actuación profesional", "N", "%"))
workposition <- data.frame(cbind(t(sapply(students[,11:16], function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))
rownames(workposition) <- c("Dirección de la institución",
                            "Coordinación de equipo", 
                            "Hago parte de un equipo de trabajo",
                            "Miembro del cuadro de funcionarios",
                            "Trabajador independiente",
                            "Otro")

workposition <- rownames_to_column(workposition, "options")
colnames(workposition) <- c("options", "Sí")

kable(arrange(workposition, -Sí), 
      caption = "Atuação Profissional dentro das instituições", 
      digits = 1, align = c("l", "c"),
      col.names = c("", "Sí (%)"))
# Frequência de profissionais que realizaram treinamento, foram avaliados e certicados
training_table <- data.frame(cbind(t(sapply(students[,18:20], function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))
rownames(training_table) <- c("¿Realizó algún curso de formación específica?",
                          "¿Usted fue evaluado en el curso?",
                          "¿Recibió certificado por su participación en el curso?"
                          )

training_table <- rownames_to_column(training_table, "options")
colnames(training_table) <- c("options", "Sí")

kable(arrange(training_table, -Sí), 
      caption = "Frequência de profissionais que realizaram treinamento, foram avaliados e certicados", 
      digits = 1, align = c("l", "c"),
      col.names = c("", "Sí (%)"))
# ¿Cuál fue la capacitación que usted realizó?
training_name <- data.frame(cbind(t(sapply(students[,24:29], function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))
rownames(training_name) <- c("Supera",
                          "Treatnet",
                          "ASSIST",
                          "Fé na Prevenção",
                          "Centro Regional de Referência sobre Drogas",
                          "Outro"
                          )

training_name <- rownames_to_column(training_name, "options")
colnames(training_name) <- c("options", "Sí")

kable(arrange(training_name, -Sí), 
      caption = "¿Cuál fue la capacitación que usted realizó?", 
      digits = 1, align = c("l", "c"),
      col.names = c("", "Sí (%)"))
since_year <- as.numeric(students$professionals.training_questions.work_skip.work_sinceyear)
since_year <- subset(since_year, since_year > 1900 & since_year <= 2017)
ggplot() + aes(since_year, y = (..count..)/sum(..count..)) + geom_histogram(binwidth=1, fill = "#7cb5ec") + 
  theme_hc() + scale_colour_hc() +
  labs(x = "Ano", y = "Frequência (%)") +
  scale_y_continuous(labels = scales::percent)
servicetype <- data.frame(mapply(c, students[, 36:38], students[, 110:112]))
servicetype <- data.frame(cbind(t(sapply(servicetype, function(x) prop.table(table(x))*100, simplify = TRUE))))
rownames(servicetype) <- c("Público","Privado", "ONG's")
colnames(servicetype) <- c("No", "Sí")
servicetype$names <-rownames(servicetype)
servicetype <- servicetype %>%
                  gather(Resposta, Valor, No:Sí)

ggplot(servicetype, aes(x = names, y = Valor, fill = factor(Resposta))) + geom_bar(stat = "identity", position = "stack") +
  coord_flip() + theme_hc() + scale_colour_hc() + ylim(0,100) + 
  scale_fill_manual(name = "Resposta", values = c("#f7a35c", "#7cb5ec"),labels = c("No", "Sí")) + 
  labs(  
       x = "",
       y = "Frequência (%)")

worksector <- data.frame(mapply(c, students[,39:43], students[, 113:117]))
worksector <- data.frame(cbind(t(sapply(worksector, function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))

rownames(worksector) <- c("Serviço Social",
                          "Saúde", 
                          "Educação", 
                          "Justiça", 
                          "Outro")

worksector <- rownames_to_column(worksector, "options")
colnames(worksector) <- c("options", "Sí")

kable(arrange(worksector, -Sí), 
      caption = "Setor de atuação dos profissionais", 
      digits = 1, align = c("l", "c"),
      col.names = c("", "Sí (%)"))

# Sector salud
worktype <- data.frame(c(students$professionals.training_questions.work_skip.sector_health.work_type_health, students$professionals.screening_questions.screening_skip_3.sector_health_no.work_type_health_no))
worktype <- Recode(worktype[,1],
                   "'amb' = 'Ambulatorio/consulta externa'; 
                    'hospitals' = 'Hospitales';
                    'mentalhealth' = 'Centro Comunitario de Tratamento de salud mental/adicciones';
                    'primarycare' = 'Atención Primaria en Salud';
                    'other' = 'Otros'
                   ")
makeTable(worktype, "Sector salud")
# Sector educativo
educationtype <- data.frame(c(students$professionals.training_questions.work_skip.sector_education.work_type_education, students$professionals.screening_questions.screening_skip_3.sector_education_no.work_type_education_no))

educationtype <- Recode(educationtype[,1],
                       "'prim' = 'Educación Primaria'; 
                        'sec' = 'Educación Secundaria';
                        'sup' = 'Educación Superior/ Universitaria';
                        'other' = 'Otros'
                       ")
makeTable(educationtype, "Sector educativo")
# ¿Para qué tipo de población están orientadas las capacitaciones?
target_audience <- data.frame(cbind(t(sapply(students[,50:57], function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))
rownames(target_audience) <- c("Usuarios de Atención Primaria en Salud",
                          "Usuarios de otros servicios especializados (excepto alcohol y otras drogas )",
                          "Usuarios de servicios comunitarios",
                          "Usuarios en contextos laborales",
                          "Población carcelaria",
                          "Población en situación de calle",
                          "Escuela",
                          "Otros"
                          )

target_audience <- rownames_to_column(target_audience, "options")
colnames(target_audience) <- c("options", "Sí")

kable(arrange(target_audience, -Sí), 
      caption = "Tipo de população alvo em as intervenção eram direcionadas nas capacitações", 
      digits = 1, align = c("l", "c"),
      col.names = c("", "Sí (%)"))
# Cuando indaga por el consumo de drogas ¿utiliza algún cuestionario para la detección temprana (AUDIT, ASSIST, CAGE, DUSI, FAGERSTROM)?
screening_question <- students$professionals.screening_questions.screening_question
screening_question <- Recode(screening_question,
                             "'yes' = 'Sí'; 
                             'no' = 'No';
                             'nobut' = 'No, pregunto sin utilizar un cuestionario específico'
                             ")
makeTable(screening_question, 
          "Cuando indaga por el consumo de drogas ¿utiliza algún cuestionario para la detección temprana (AUDIT, ASSIST, CAGE, DUSI, FAGERSTROM)?")
# ¿Cuáles son los instrumentos de detección temprana (despistaje/cribado/tamización) que suele utilizar durante sus intervenciones?
screen_tools <- students[, 60:65]
screen_tools <- data.frame(cbind(t(sapply(screen_tools, function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))
rownames(screen_tools) <- c("ASSIST – Prueba de Detección de Consumo de Alcohol, Tabaco y Sustancias",
                          "AUDIT – Test de Identificación de Trastornos debido al Consumo de Alcohol",
                          "CAGE – Cuestionario detección de problemas relacionados con el consumo de alcohol",
                          "DUSI – Inventario de Despistaje de Uso de Drogas",
                          "FAGERSTROM – Test de Fagerström de dependencia de la nicotina",
                          "Otros"
                          )
colnames(screen_tools) <- c("Sí")
screen_tools <- rownames_to_column(screen_tools, "options")
kable(arrange(screen_tools, - Sí), 
      caption = "¿Cuáles son los instrumentos de detección temprana (despistaje/cribado/tamización) que suele utilizar durante sus intervenciones?", 
      digits = 1, align = c("l", "c"),
      col.names = c("", "(%)"))
# ¿Cuáles son las sustancias abordadas?
substances <- students[, 67:78]
substances_table <- data.frame(cbind(t(sapply(substances, function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))
rownames(substances_table) <- c("Alcohol",
                          "Tabaco",
                          "Cocaína inhalada",
                          "Cocaína fumable",
                          "Marihuana",
                          "Anfetaminas",
                          "Alucinógenos",
                          "Inhalantes",
                          "Hipnóticos/sedantes",
                          "Drogas inyectables",
                          "Opiáceos",
                          "Otros"
                          )

colnames(substances_table) <- c("Sí")
substances_table <- rownames_to_column(substances_table, "options")
kable(arrange(substances_table, - Sí), 
      caption = "¿Cuáles son las sustancias abordadas?", 
      digits = 1, align = c("l", "c"),
      col.names = c("", "(%)"))
merged_table <- data.frame(a = c("Utiliza cartilhas/folhetos", "", "", "Realiza seguimento?", "", "Frecuecia del acompañamiento", "", "", ""),
                           b = c("Sí", "No","Otros recursos", "Sí", "No", "Semanal", "Mensual", "Semestralmente", "Anual"),
                           N = c(504, 57, 27, 489, 100, 171, 268, 45, 5),
                           freq = c(85.7,9.7,4.6,83,17,35.0,54.8,9.2,1.0))

kable(merged_table, 
      caption = "Uso de recursos, realização e frequência de seguimento", 
      digits = 1, align = c("l", "l", "c", "c"),
      col.names = c("", "", "N", "%"))
# ¿Cuál es el protocolo adoptado para casos de bajo riesgo de consumo de alcohol, tabaco y otro tipo de sustancias?
low_risk <- students$professionals.screening_questions.screening_skip_2.intervention_knowledge.intervention_lowrisk
low_risk <- Recode(low_risk,
                           "'referralspecial' = 'Remisión/derivación a servicios especializados'; 
                           'referralselfhelp' = 'Remisión/derivación a grupo de ayuda–mutua o soporte comunitario';
                            'quickinterventionno' = 'Realizar intervención educativa y entrega de material informativo (folletos, tarjetas, cartillas)';
                            'quickinterventionyes' = 'Realizar abordaje educativo SIN material de apoyo (folletos, tarjetas, cartillas)';
                            'none' = 'No adopto ningún procedimiento'")
makeTable(low_risk, 
          "Procedimentos realizados para casos de bajo riesgo de consumo de alcohol, tabaco y otro tipo de sustancias")
# ¿Cuál es el protocolo seguido para los casos de consumo de riesgo de alcohol, tabaco y otro tipo de sustancias?
at_risk <- students$professionals.screening_questions.screening_skip_2.intervention_knowledge.intervention_atrisk
at_risk <- Recode(at_risk,
                           "'referralspecial' = 'Remisión/derivación a servicios especializados'; 
                           'referralselfhelp' = 'Remisión/derivación a grupo de ayuda–mutua o soporte comunitario';
                            'quickinterventionno' = 'Realizar intervención educativa y entrega de material informativo (folletos, tarjetas, cartillas)';
                            'quickinterventionyes' = 'Realizar abordaje educativo SIN material de apoyo (folletos, tarjetas, cartillas)';
                            'none' = 'No adopto ningún procedimiento'")
makeTable(at_risk, 
          "Procedimentos realizados para casos de riesgo de consumo de alcohol, tabaco y otro tipo de sustancias")
# ¿Cuál es el protocolo en el caso de usuarios que cumplen los criterios diagnósticos de dependencia de drogas?
dependence <- students$professionals.screening_questions.screening_skip_2.intervention_knowledge.intervention_dependence
dependence <- Recode(dependence,
                           "'referralspecial' = 'Remisión/derivación a servicios especializados'; 
                           'referralselfhelp' = 'Remisión/derivación a grupo de ayuda–mutua o soporte comunitario';
                            'quickinterventionno' = 'Realizar intervención educativa y entrega de material informativo (folletos, tarjetas, cartillas)';
                            'quickinterventionyes' = 'Realizar abordaje educativo SIN material de apoyo (folletos, tarjetas, cartillas)';
                            'none' = 'No adopto ningún procedimiento'")
makeTable(dependence, 
          "Procedimentos realizados para usuários com diagnóstico de dependência")
# Escoja la opción que mejor representa la práctica de detección temprana e intervención breve en su institución de trabajo
screening_work <- students$professionals.screening_questions.screening_skip.screening_work
screening_work <- Recode(screening_work,
                   "'onlyone' = 'Soy el único profesional de mi equipo que realiza detección temprana e IB'; 
                   'afew' = 'Pocos profesionales de mi equipo realizan detección temprana e IB';
                    'more' = 'Muchos profesionales de mi equipo realizan detección temprana e IB';
                    'almost' = 'Casi todos los profesionales de mi equipo realizan detección temprana e IB';
                    'donot' = 'No actúo con equipos de trabajo.'")
makeTable(screening_work, 
          "Percepção sobre a prática de sua equipe  de triagem e intervenção breve")
# Percepção de efetividade da IB e auto-avaliação da prática profissional
perception_ib <- students[, c(89, 91)]
perception_ib <- data.frame(cbind(t(sapply(perception_ib, function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))
rownames(perception_ib) <- c("¿Considera que la detección temprana e intervención breve contribuyen para reducir el consumo?",
                          "¿Evalúa los resultados en su práctica cotidiana de trabajo?"
                          )

colnames(perception_ib) <- c("Sí")
perception_ib <- rownames_to_column(perception_ib, "options")
kable(arrange(perception_ib, - Sí), 
      caption = "Percepção de efetividade da IB e se profissional avalia durante a prática", 
      digits = 1, align = c("l", "c"),
      col.names = c("", "(%)"))
# ¿Cuáles de las siguientes opciones facilitan la implementación de detección temprana e intervención breve en la práctica profesional?
roadblocks1 <- students[, 93:96]
roadblocks1 <- data.frame(cbind(t(sapply(roadblocks1, function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))
rownames(roadblocks1) <- c("Apoyo por parte de las autoridades, gestores, gerentes o responsables del servicio",
                          "Inclusión del procedimiento en las pautas y normas obligatorias",
                          "Programa de actualización para el equipo de atención ",
                          "Otros"
                          )

colnames(roadblocks1) <- c("Sí")
roadblocks1 <- rownames_to_column(roadblocks1, "options")
kable(arrange(roadblocks1, - Sí), 
      caption = "Fatores atribuídos pelos profissionais como facilitadores de implementação da triagem e intervenção breve", 
      digits = 1, align = c("l", "c"),
      col.names = c("", "(%)"))
# ¿Cuáles de las siguintes opciones dificultan la implementación de las actividades de detección temprana e intervención breve?
roadblocks2 <- students[, 98:105]
roadblocks2 <- data.frame(cbind(t(sapply(roadblocks2, function(x) prop.table(table(x))*100, simplify = TRUE))[,2]))
rownames(roadblocks2) <- c("Falta de apoyo por parte de las autoridades, gestores, gerentes o responsables",
                          "Falta de tiempo (sobrecarga de actividades)",
                          "Falta de entrenamiento",
                          "Falta de supervisión",
                          "Falta de apoyo por parte de otros miembros del equipo",
                          "Falta de una red de servicios para remitir/derivar los casos",
                          "Falta de conocimientos sobre el consumo de alcohol y drogas en la población",
                          "Otros"
                          )

colnames(roadblocks2) <- c("Sí")

roadblocks2 <- rownames_to_column(roadblocks2, "options")
kable(arrange(roadblocks2, - Sí), 
      caption = "Fatores atribuídos pelos profissionais como obstáculos para a implementação de ações de triagem e intervenção breve", 
      digits = 1, align = c("l", "c"),
      col.names = c("", "(%)"))
readLines(purl("full_report.Rmd", documentation = 0))
