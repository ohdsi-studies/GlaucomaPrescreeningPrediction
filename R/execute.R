#' Executes all the analyses
#'
#' @details
#' The user specifies the connectionDetails to the new OMOP CDM data,
#'
#' @param connectionDetails The connection details for the OMOP CDM data
#' @param cdmDatabaseSchema The schema containing the OMOP CDM data
#' @param cohortDatabaseSchema A schema with read/write access that contains the target and outcome cohorts
#' @param tempEmulationSchema The schema to use for temp tables if the dbms does not support temp tables
#' @param cohortTable The name of the cohort table that contains the target and outcome cohorts
#' @param outputFolder The location of the folder to save all the results to
#' @param minCellCount The minimum cell count to save (otherwise it is saved as -minCellCount to represent <)
#' @param runCohortGeneration Whether to run the cohort generation
#' @param runCharacterization Whether to run the characterization
#' @param runValidation Whether to run the model validation
#' @param runFineTune Whether to run the model fineTuning
#' @param runModelDevelopment Whether to run a benchmark LR model
#' @param sampleSize Number of patients to sample from the full target cohort for validation and development
#'
#' @return
#' A location where all the results are saved to.
#' @export
executeGlaucoma <- function(
    connectionDetails,
    cdmDatabaseSchema,
    cohortDatabaseSchema,
    tempEmulationSchema = Sys.getenv("DATABRICKS_SCRATCH_SCHEMA"),
    cohortTable = 'glau_screen_cohort',
    outputFolder = tempdir(),
    minCellCount = 0,

    runCohortGeneration = TRUE,
    runCharacterization = TRUE,
    runValidation = TRUE,
    runFineTune = TRUE,
    runModelDevelopment = TRUE,
    sampleSize = NULL
){


  if(runCohortGeneration){
    message('Generating study cohorts')
    generateCohorts(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTableName  = cohortTable,
      tempEmulationSchema = tempEmulationSchema
    )
  }

  if(runCharacterization){
    message('Running Characterization')
    runCharacterization(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      cohortTable = cohortTable,
      minCellCount = minCellCount,
      saveLoc = outputFolder
    )
  }

  if(runValidation){
    message('Running Validation')
    validationResults <- validationModel(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      cohortTable = cohortTable,
      targetId = 23884,
      outcomeId = 23933,
      sampleSize = sampleSize,
      model = 'ALL_OF_US_8_17_26_221',
      modelType = '.keras'
    )

    # code to do performance across race/ethnicity/age/sex
    # and save as csv files using minCellCount
    exportValidation(
      validationResults = validationResults,
      minCellCount = minCellCount,
      outputFolder = outputFolder
    )

  }

  # add the fine tuning here

  if(runModelDevelopment){
    message('Running Model Development')
    developBaselineModels(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema ,
      cohortTable = cohortTable,
      targetId = 23884,
      outcomeId = 23933,
      sampleSize = sampleSize,
      model = 'ALL_OF_US_8_17_26_221',
      modelType = '.keras',
      minCellCount = minCellCount,
      outputFolder = outputFolder
    )
  }

  if(runFineTune){
    message('Running Fine Tuning')
    executeFineTuning(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema ,
      cohortTable = cohortTable,
      targetId = 23884,
      outcomeId = 23933,
      sampleSize = sampleSize,
      model = 'ALL_OF_US_8_17_26_221',
      modelType = '.keras',
      minCellCount = minCellCount,
      outputFolder = outputFolder
    )
  }

  return(invisible(outputFolder))
}


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
#' @param model The name of the model to run
#' @param modelType either '.keras' or '.h5'
#'
#' @return
#' A list with the prediction data.frame and evaluation list and the data
#' @export
validationModel <- function(
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
  prediction$evaluationType <- 'Validation'

  covRef <- as.data.frame(newData$covariateData$covariateRef)

  for(subGroup in c('gender', 'race', 'ethnicity',
                    'ageYear: 40-49','ageYear: 50-59',
                    'ageYear: 60-69','ageYear: 70-79',
                    'ageYear: 80-89')){

    if(subGroup == 'gender'){
      genders <- covRef[covRef$analysisId == 1,]

      if(nrow(genders) > 0){

        for(genI in 1:nrow(genders)){

          subGroupName <- genders$covariateName[genI]
          covOfInt <- genders$covariateId[genI]

          subOfInt <- newData$covariateData$covariates %>%
            dplyr::filter(.data$covariateId == !!covOfInt) %>%
            dplyr::collect()

          ind <- prediction$rowId %in% subOfInt$rowId
          predictionTemp <- prediction[ind,]
          predictionTemp$evaluationType <- subGroupName
          prediction <- rbind(prediction, predictionTemp)
        }
      }

    } else if(subGroup == 'race'){
      race <- covRef[covRef$analysisId == 4,]

      if(nrow(race) > 0){

        for(raceI in 1:nrow(race)){

          subGroupName <- race$covariateName[raceI]
          covOfInt <- race$covariateId[raceI]

          subOfInt <- newData$covariateData$covariates %>%
            dplyr::filter(.data$covariateId == !!covOfInt) %>%
            dplyr::collect()

          ind <- prediction$rowId %in% subOfInt$rowId
          predictionTemp <- prediction[ind,]
          predictionTemp$evaluationType <- subGroupName
          prediction <- rbind(prediction, predictionTemp)
        }
      }

    } else if(subGroup == 'ethnicity'){
      ethnicity <- covRef[covRef$analysisId == 5,]

      if(nrow(ethnicity) > 0){

        for(ethnicityI in 1:nrow(ethnicity)){

          subGroupName <- ethnicity$covariateName[ethnicityI]
          covOfInt <- ethnicity$covariateId[ethnicityI]

          subOfInt <- newData$covariateData$covariates %>%
            dplyr::filter(.data$covariateId == !!covOfInt) %>%
            dplyr::collect()

          ind <- prediction$rowId %in% subOfInt$rowId
          predictionTemp <- prediction[ind,]
          predictionTemp$evaluationType <- subGroupName
          prediction <- rbind(prediction, predictionTemp)
        }
      }

    } else{
      years <- gsub('ageYear: ','',subGroup)
      ageRange <- strsplit(years, '-')[[1]]

      ind <- prediction$ageYear >= ageRange[1] & prediction$ageYear <= ageRange[2]
      predictionTemp <- prediction[ind,]
      predictionTemp$evaluationType <- subGroup
      prediction <- rbind(prediction, predictionTemp)
    }

  }

  # AUC
  attr(prediction, "metaData")$modelType <- 'binary'
  evaluation <- PatientLevelPrediction::evaluatePlp(prediction = prediction)

return(list(
  prediction = prediction,
  evaluation = evaluation,
  plpData = newData
))
}

exportValidation <- function(
  validationResults = validationResults,
  minCellCount = minCellCount,
  outputFolder = outputFolder
){

  model <- list(
    modelDesign = list(
      modelSettings = list(
        settings = list(
          saveType = "RtoJson"
        )
      )
    ))
  class(model) <- 'plpModel'

  # code to export results here
  formattedResults <- list(
    executionSummary = list(
      PackageVersion = list(),
      PlatformDetails = list()
    ),
    model = model,
    performanceEvaluation = validationResults$evaluation
  )

  PatientLevelPrediction::savePlpShareable(
    result = formattedResults,
    minCellCount = minCellCount,
    saveDirectory = file.path(outputFolder, 'validation')
  )

  return(invisible(file.path(outputFolder, 'validation')))
}

# run a baseline LR + GBM model
developBaselineModels <- function(
    connectionDetails,
    cdmDatabaseSchema,
    cohortDatabaseSchema,
    tempEmulationSchema = Sys.getenv("DATABRICKS_SCRATCH_SCHEMA"),
    cohortTable = 'glau_screen_cohort',
    targetId = 23884,
    outcomeId = 23933,
    sampleSize = NULL,
    model = c('ALL_OF_US_8_17_26_221','ALL_OF_US')[1],
    modelType = c('.keras','.h5')[1],
    minCellCount = minCellCount,
    outputFolder = outputFolder
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


  lr <- PatientLevelPrediction::runPlp(
    plpData = newData,
    outcomeId = outcomeId,
    populationSettings = plpModel$modelDesign$populationSettings,
    splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
    modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
    analysisName = 'benchmark_lr',
    analysisId = 1,
    preprocessSettings = PatientLevelPrediction::createPreprocessSettings(),
    saveDirectory = tempdir(),
    executeSettings =  PatientLevelPrediction::createExecuteSettings(
      runSplitData = TRUE,
      runModelDevelopment = TRUE,
      runPreprocessData = TRUE,
      )
    )

  PatientLevelPrediction::savePlpShareable(
    result = lr,
    minCellCount = minCellCount,
    saveDirectory = file.path(outputFolder, 'lr_model')
  )

  gbm <- PatientLevelPrediction::runPlp(
    plpData = newData,
    outcomeId = outcomeId,
    populationSettings = plpModel$modelDesign$populationSettings,
    splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
    modelSettings = PatientLevelPrediction::setGradientBoostingMachine(ntrees = 200),
    analysisName = 'benchmark_gbm',
    analysisId = 1,
    preprocessSettings = PatientLevelPrediction::createPreprocessSettings(),
    saveDirectory = tempdir(),
    executeSettings =  PatientLevelPrediction::createExecuteSettings(
      runSplitData = TRUE,
      runModelDevelopment = TRUE,
      runPreprocessData = TRUE,
    )
  )

  PatientLevelPrediction::savePlpShareable(
    result = gbm,
    minCellCount = minCellCount,
    saveDirectory = file.path(outputFolder, 'gbm_model')
  )

  return(invisible(outputFolder))
}
