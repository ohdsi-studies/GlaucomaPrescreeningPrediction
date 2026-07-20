#' Generate study cohorts
#'
#' @details
#' The user specifies the connection details for the OMOP CDM, the CDM database schema
#' the cohort table schema and the name of the table you want the cohorts to be generated into
#' optionally you can also enter the temp schema that is used to create temporary tables.
#'
#' @param connectionDetails The OMOP CDM connection details created using DatabaseConnector::createConnectionDetails
#' @param cdmDatabaseSchema The schema containing the OMOP CDM
#' @param cohortDatabaseSchema The schema to add the cohort table to
#' @param cohortTableName The name of the cohort table to create for this study
#' @param tempEmulationSchema Optional temp schema
#'
#' @return
#' TRUE
#' @export
generateCohorts <- function(
    connectionDetails,
    cdmDatabaseSchema,
    cohortDatabaseSchema,
    cohortTableName  = 'glaucoma_prescreen',
    tempEmulationSchema = NULL
    ){

  cohortDefinitionSet <- ParallelLogger::loadSettingsFromJson(
    system.file('cohorts/all.json',package = 'GlaucomaPrescreeningPrediction')
  )

  studyTableNames <- CohortGenerator::getCohortTableNames(
    cohortTable = cohortTableName
    )

  message('Creating empty cohort tables')
  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortTableNames = studyTableNames,
    cohortDatabaseSchema = cohortDatabaseSchema
      )

  message('Populating cohort tables')
  CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cohortTableNames = studyTableNames,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    cohortDefinitionSet = cohortDefinitionSet
  )

  message('Done')
  return(invisible(TRUE))
}
