#' Extracts measurement covariates
#'
#' @details
#' The user specifies a cohort and time period and then a covariate is constructed whether they are in the
#' cohort during the time periods relative to target population cohort index
#'
#' @param modelName filename of the model
#' @param conditionFile filename of the pickle with the condition codes
#' @param conditionAuto filename of the condition autoencoder
#' @param drugFile filename of the pickle with the drug codes
#' @param drugAuto filename of the drug autoencoder
#' @param sampleSize Number of patients to sample from the full target cohort
#'
#' @return
#' CovariateData object with covariates, covariateRef, and analysisRef tables
#' @export
getModel <- function(
    modelName = 'model_ALL_OF_US.h5',
    conditionFile = 'diag_codes.pkl',
    conditionAuto = 'diag_autoencoder_model_ALL_OF_US.h5',
    drugFile = 'drugs_codes.pkl',
    drugAuto = 'drugs_autoencoder_model_ALL_OF_US.h5',
    sampleSize = NULL
    ){

  measurements <- getMeasurements()

  covariateSettings <- lapply(
    X = 1:length(measurements),
    FUN = function(i){
      createMeasurementCovariateSettings(
        covariateName = measurements[[i]]$name,
        conceptSet = measurements[[i]]$concepts,
        unitSet = NULL,
        startDay=-365*5,
        endDay=-1,
        aggregateMethod = 'recent',
        covariateId = i*1000+444,
        analysisId = 444
      )
    }
  )

  covariateSettings[[length(covariateSettings) + 1]] <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAge = TRUE,
    useDemographicsRace = TRUE,
    useDemographicsEthnicity = TRUE,
    useConditionOccurrenceAnyTimePrior = TRUE,
    useDrugExposureAnyTimePrior = TRUE,
    endDays = -1
  )

  modelSettings <- list(
    param <- list(none = "true"),
    settings = list(
      modelName = 'validation model',
      modelType = "binary",
      requiresDenseMatrix = FALSE
    )
  )
  class(modelSettings) <- "modelSettings"

  # TODO note: need to do measurement processing in side the covariate extraction or
  # as a FeatureEngineering function or via plpModel$preprocessing$tidyCovariates

plpModel <- list(
  model = system.file(paste0('models/',modelName), package = 'GlaucomaPrescreeningPrediction'),

  modelDesign = PatientLevelPrediction::createModelDesign(
    targetId = 23884, outcomeId = 23933,
    restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(
      sampleSize = sampleSize
    ),
    populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
      washoutPeriod = 180,
      firstExposureOnly = TRUE,
      removeSubjectsWithPriorOutcome = TRUE,
      priorOutcomeLookback = 9999,
      includeAllOutcomes = FALSE,
      riskWindowStart = 0,
      riskWindowEnd = 365*3
    ),
    covariateSettings = covariateSettings,
    # add a dummy setting here
    modelSettings = modelSettings
  ),

  preprocessing = list(
    requiresDenseMatrix = FALSE,
    featureEngineering = list(
      list(
        funct = 'GlaucomaPrescreeningPrediction::condAuto',
        settings = list(
          trainData = NULL,
          featureEngineeringSettings = list(
            conditionAnalysisId = 101,
            conditionCodeLoc = system.file(paste0('models/',conditionFile), package = 'GlaucomaPrescreeningPrediction'),
            autoEncoderLoc = system.file(paste0('models/',conditionAuto), package = 'GlaucomaPrescreeningPrediction')
          )
        )
      ),
      list(
        funct = 'GlaucomaPrescreeningPrediction::drugAuto',
        settings = list(
          trainData = NULL,
          featureEngineeringSettings = list(
            drugAnalysisId = 301,
            drugCodeLoc = system.file(paste0('models/',drugFile), package = 'GlaucomaPrescreeningPrediction'),
            autoEncoderLoc = system.file(paste0('models/',drugAuto), package = 'GlaucomaPrescreeningPrediction')
          )
        )
      )
    )
  ),

  # meds_diag_demo_labs_cohort 128+128
  # demo (12 features):
  #.    age (years),
  #
  #.    ohe ethnicity (38003564: 'nonhispanic',38003563: 'hispanic',0: 'other')
  #.    ohe sx_birth (8507: 'male', 8532: 'female')
  #.                  8507001, 8532001
  #.    ohe race. (0: 'other', 8515: 'asian', 8527: 'white', 8516: 'black', 8557: 'nhpi', 8657: 'aian')
  # labs = 31
  covariateImportance = data.frame(
    columnId = 1:299, # have 256 from drugs/conds - 43 others
    covariateId = c(
      (1:128)*1000+341, # drug embed
      (1:128)*1000+141, # condition embed
      1002, # age (years)
      38003564005, # 'nonhispanic'
      38003563005, # 'hispanic'
      -1, # 'other'
      8507001, # 'male'
      8532001, #' female'
      8522004,   # 'other'
      8515004,   # 'asian'
      8527004, # 'white'
      8516004,   # 'black'
      -1,  # 'nhpi'
      -1, # 'aian',
      (1:31)*1000+444
    )
  )
)
attr(plpModel, "predictionFunction") <- predictKeras
class(plpModel) <- 'plpModel'

return(plpModel)
}



# helpers
loadKerasModel <- function(modelLocation){
  model <- keras::load_model_hdf5(modelLocation,
                                  compile = FALSE)
  return(model)
}

predictKeras <- function (plpModel, data, cohort)
{
  if (inherits(data, "plpData")) {
    matrixObjects <- PatientLevelPrediction::toSparseM(
      plpData = data,
      cohort = cohort,
      map = plpModel$covariateImportance %>%
        dplyr::select("columnId", "covariateId")
    )
    newData <- matrixObjects$dataMatrix
    cohort <- matrixObjects$labels
  }
  else {
    newData <- data
  }

  if (inherits(plpModel, "plpModel")) {
    if (is.character(plpModel$model)) {
      model <- loadKerasModel(plpModel$model)
    }
    else {
      model <- plpModel$model
    }
  }
  else {
    model <- plpModel
  }

  cohort <- predictValuesKeras(
    model = model,
    data = newData,
    cohort = cohort
  )

  return(cohort)
}

predictValuesKeras <- function (model, data, cohort)
{
  # requires 299 variables
  # do the prediction in batches as casting to dense matrix
  numRows <- dim(data)
  maxInd <- ceiling(numRows[1]/20000)
  cohort$value <- 0 # initialize it to 0
  for(ind in 1:maxInd){
    start <- (ind-1)*20000+1
    end <-   min((ind)*20000, numRows)
    predictionValue <- model$predict(as.matrix(data[start:end,]))
    cohort$value[start:end] <- predictionValue[,1]
  }

  cohort <- cohort %>% dplyr::select(-"rowId") %>% dplyr::rename(rowId = "originalRowId")
  attr(cohort, "metaData")$modelType <- "binary"
  return(cohort)
}
