# add a function that runs all the parts (with optional inputs)




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
#' A list with the prediction data.frame and evaluation list and the data
#' @export
execute <- function(
    connectionDetails,
    cdmDatabaseSchema,
    cohortDatabaseSchema,
    tempEmulationSchema = Sys.getenv("DATABRICKS_SCRATCH_SCHEMA"),
    cohortTable = 'glau_screen_cohort',
    targetId = 23884,
    outcomeId = 23933,
    sampleSize = NULL,
    model = c('ALL_OF_US_8_17_26_221','ALL_OF_US')[1],
    modelType = c('.keras','.h5')[1]
    ){

  plpModel <- GlaucomaPrescreeningPrediction::getModel(
    modelName = paste0('model_',model,modelType),
    conditionFile = paste0('diag_codes_',model,'.pkl'),
    conditionAutoFile = paste0('diag_autoencoder_model_',model,modelType),
    drugFile = paste0('drugs_codes_',model,'.pkl'),
    drugAutoFile = paste0('drugs_autoencoder_model_',model,modelType)
  )

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
  evaluation = evaluation,
  plpData = newData
))
}

# run a baseline LR model
developBaselineLr <- function(plpData){

  # do not restrict to the same features the model used


  # restrict to the same features that the model used

}
