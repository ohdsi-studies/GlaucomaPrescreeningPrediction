[Study title]
=============

<img src="https://img.shields.io/badge/Study%20Status-Repo%20Created-lightgray.svg" alt="Study Status: Repo Created">

- Analytics use case(s): **-**
- Study type: **-**
- Tags: **-**
- Study lead: **-**
- Study lead forums tag: **[[Lead tag]](https://forums.ohdsi.org/u/[Lead tag])**
- Study start date: **-**
- Study end date: **-**
- Protocol: **-**
- Publications: **-**
- Results explorer: **-**

[Description (single paragraph)]

[You can add other text at this point]

# Code to run study 
```
# first install the package
remotes::install_github('ohdsi-studies/GlaucomaPrescreeningPrediction')

# set up python environment to use 
reticulate::virtualenv_create("glaucoma-screen", requirements = system.file('requirements.txt', package = 'GlaucomaPrescreeningPrediction'))
reticulate::use_virtualenv("glaucoma-screen")

# Specify the connection stuff 
cohortTable <- 'glau_screen_cohort'
cdmDatabaseSchema <- '<add schema with OMOP CDM data>'
cohortDatabaseSchema <- '<add schema with read/write access>'
tempEmulationSchema <- Sys.getenv("DATABRICKS_SCRATCH_SCHEMA")
options(sqlRenderTempEmulationSchema = tempEmulationSchema)


# get data
connectionDetails <- <add connection details>

# TODO: add code to create cohorts here

# run the model
results <- GlaucomaPrescreeningPrediction::execute(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    cohortTable = cohortTable,
    targetId = 23884,
    outcomeId = 23933,
    sampleSize = 40000
)
```
