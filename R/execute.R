#' Validates the glaucoma model
#'
#' @details
#' The user specifies the connectionDetails to the new OMOP CDM data,
#'
#' @param connectionDetails The connection details for the OMOP CDM data
#' @param cdmDatabaseSchema The schema containing the OMOP CDM data
#' @param cohortDatabaseSchema A schema with read/write access that contains the target and outcome cohorts
#' @param tempEmulationSchema The schema to use for temp tables if the dbms does not support temp tables
#' @param cohortTable The name of the cohort table that contains the target and outcome cohorts
#' @param targetId The validation target cohort definition id
#' @param outcomeId The validation outcome cohort definition id
#' @param sampleSize Number of patients to sample from the full target cohort
#'
#' @return
#' A list with the prediction data.frame and evaluation list
#' @export
execute <- function(
    connectionDetails,
    cdmDatabaseSchema,
    cohortDatabaseSchema,
    tempEmulationSchema = Sys.getenv("DATABRICKS_SCRATCH_SCHEMA"),
    cohortTable = 'glau_screen_cohort',
    targetId = 23884,
    outcomeId = 23933,
    sampleSize = NULL
    ){

  plpModel <- GlaucomaPrescreeningPrediction::getModel()

  newData <- PatientLevelPrediction::getPlpData(
    databaseDetails = PatientLevelPrediction::createDatabaseDetails(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      outcomeDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      outcomeTable = cohortTable,
      targetId = targetId,
      outcomeIds = outcomeId
    ),
    covariateSettings = plpModel$modelDesign$covariateSettings,
    restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(
      sampleSize = sampleSize,
      washoutPeriod = 180
    )
  )

  newPopulation <- PatientLevelPrediction::createStudyPopulation(
    plpData = newData,
    outcomeId = plpModel$modelDesign$outcomeId,
    populationSettings = plpModel$modelDesign$populationSettings
  )

  prediction <- PatientLevelPrediction::predictPlp(
    plpModel = plpModel,
    plpData = newData,
    population = newPopulation
  )

  # AUC
  prediction$evaluationType <- 'Validation'
  attr(prediction, "metaData")$modelType <- 'binary'
  evaluation <- PatientLevelPrediction::evaluatePlp(prediction = prediction)

return(list(
  prediction = prediction,
  evaluation = evaluation
))
}
