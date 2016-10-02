# Query pubmed
library(RISmed)

search_query <- EUtilsSummary(readLines("pubmed/user_query.txt"), type="esearch", db="pubmed", datetype='pdat', mindate=2005, maxdate=2016, retmax=500)

# Query result
summary(search_query)

# Fetch data
records <- EUtilsGet(search_query)

# PMID, Title, Author, Abstract, affiliation, Country,  YearAccepted, ELocationID
pubmed_data <- data.frame(PMID = PMID(records), title = ArticleTitle(records), abstract = AbstractText(records), affiliation = Affiliation(records),  year =  YearPubmed(records), link = ELocationID(records))

# write data as csv file
write.csv(pubmed_data, "db/pubmed.csv")
