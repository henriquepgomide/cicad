## What is this about?

This repository contains data and code used for the research project "Screening, Brief Intervention and Referral to Treatment (SBIRT) programmes for Substances in the Americas".

The project is aimed to:

1. Map and describe centers that provide training on SBIRT in the Americas but US and Canada.
2. Identify all experience on training published in scientific sources.

Some data are protected and may be made available upon request. Reach me at henriquepgomide[at]gmail.com

## Folder structure

The project has the following structure:
.
├── form
│   ├── cover-letter
│   ├── email_marketing_results
│   ├── html-template
│   ├── logos
│   ├── report
│   └── XLSConverter
├── service_list
└── systematic review
    ├── lib
    ├── raw_data
    └── supp

**form** has all files related to the form used to gather data from health professionals who have experience on SBIRT  implementation and institutions that provide training on SBIRT. You can find in the folder: the email template available in Spanish and Portuguese, logo of [CICAD](http://www.cicad.oas.org/main/default_eng.asp) and [CREPEIA](http://www.ufjf.br/crepeia/), the report code, written mainly in R, and the [XLSConverter](https://opendatakit.org/use/xlsform/) tool that was used to create the form. All code and tools are freeware or open-source. Have fun!

**service_list** is a list of almost one thousand drug services available on [COPOLAD](https://www.copolad.eu/en/que-es-copolad) website. According to the COPOLAD's website the data can be fetched and retrieved without request. Code is written in Python as a notebook. So, you may have Conda installed to run it or adapt it as a pure Python script. We used data from list to reach out professionals and health services and invite them join our study.

**systematic review** contains code used to retrieve results scientific papers from different databases such as PubMed, EMBASE, PsycINFO, Redalyc, Scielo. The systematic review is aimed to explore all experience on SBIRT in the Americas but US and Canada. We adapted the methods used in a previous systematic review:

Kaner, E. F. S., Dickinson, H. O., Beyer, F., Pienaar, E., Schlesinger, C., Campbell, F., … Heather, N. (2009). The effectiveness of brief alcohol interventions in primary care settings: A systematic review: Brief intervention effectiveness. Drug and Alcohol Review, 28(3), 301–323. [Link](https://doi.org/10.1111/j.1465-3362.2009.00071.x)


## Installation

You need [R](https://www.r-project.org) and [Python](https://www.python.org/downloads/) installed to run the scripts. A text editor is sufficient, even though I'd recommend an HTML editor (e.g., Atom), [Anaconda](https://www.continuum.io/downloads) and [RStudio](https://www.rstudio.com/products/rstudio/download/). All software is open-source and cross-platform.

## License

MIT License
