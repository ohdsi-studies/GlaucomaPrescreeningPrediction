#' Implement the condition autoencoder
#'
#' @description
#' Call the condition autoencoder to get 128 features
#'
#' @details
#' Used by applyFeatureEngineering to add the condition autoencoders
#'
#' @param trainData The training data to apply the autoencoder to
#' @param featureEngineeringSettings settings for loading the autoencoder
#'
#' @return
#' The plp data with the auto encoder features added
#'
#'
#' @export
condAuto <- function(
    trainData,
    featureEngineeringSettings = list(
      conditionAnalysisId = 101,
      conditionCodeLoc = system.file("models/diag_codes.pkl", package = 'GlaucomaPrescreeningPrediction'),
      autoEncoderLoc = system.file('models/diag_autoencoder_model_ALL_OF_US.h5', package = 'GlaucomaPrescreeningPrediction')
    )){

  pd <- reticulate::import("pandas")

  diag <- pd$read_pickle(featureEngineeringSettings$conditionCodeLoc)
  conConcepts <- gsub('omop_', '', diag)

  conditionAutoMap <- data.frame(
    columnId = 1:length(conConcepts),
    covariateId = as.double(conConcepts)*1000+featureEngineeringSettings$conditionAnalysisId
  )

  # convert to sparse matrix
  sparseData <- PatientLevelPrediction::toSparseM(
    plpData = trainData,
    cohort = trainData$cohort,
    map = conditionAutoMap
  )

  # now load the autoencoder
  diagAuto <- keras$models$load_model(
    filepath = featureEngineeringSettings$autoEncoderLoc,
    compile = FALSE
  )


  nrows <- dim(sparseData$dataMatrix)[1]
  maxRind <- ceiling(nrows/20000)

  # now apply the autoencoder - need to do this in batches as have to convert to dense matrix
  maxCol <- 0
  for(rind in 1:maxRind){
    start <- (rind-1)*20000+1
    end <- min((rind)*20000,nrows)
    mappedFeatures <- diagAuto$predict(as.matrix(sparseData$dataMatrix[start:end,]))

    if(maxCol < ncol(mappedFeatures)){
      maxCol = ncol(mappedFeatures)
    }

    newCovariates <- c()
    for(i in 1:ncol(mappedFeatures)){
      newCovariates <- rbind(
        newCovariates,
        data.frame(
          rowId = start:end,
          covariateId = i*1000+141,
          covariateValue = mappedFeatures[,i]
        )
      )
    }

    # append the new data
    Andromeda::appendToTable(
      tbl = trainData$covariateData$covariates,
      data = newCovariates
    )
  }

  # remove the condition covariates as no longer needed
  trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
    dplyr::filter(!.data$covariateId %in% conditionAutoMap$covariateId)

  # update covariate Ref
  trainData$covariateData$covariateRef <- trainData$covariateData$covariateRef %>%
    dplyr::filter(!.data$covariateId %in% conditionAutoMap$covariateId)

  newCovRef <- data.frame(
    covariateId = (1:maxCol)*1000+141,
    covariateName = paste0('diag_embed',1:maxCol),
    analysisId = 141
  )

  Andromeda::appendToTable(
    tbl = trainData$covariateData$covariateRef,
    data = newCovRef
  )

  return(trainData)

}

#' Implement the drug autoencoder
#'
#' @description
#' Call the drug autoencoder to get 128 features
#'
#' @details
#' Used by applyFeatureEngineering to add the drug autoencoders
#'
#' @param trainData The training data to apply the autoencoder to
#' @param featureEngineeringSettings settings for loading the autoencoder
#'
#' @return
#' The plp data with the auto encoder features added for the drugs
#'
#'
#' @export
drugAuto <- function(
    trainData,
    featureEngineeringSettings = list(
      drugAnalysisId = 301,
      drugCodeLoc = system.file("models/drugs_codes.pkl", package = 'GlaucomaPrescreeningPrediction'),
      autoEncoderLoc = system.file('models/drugs_autoencoder_model_ALL_OF_US.h5', package = 'GlaucomaPrescreeningPrediction')
    )){

  pd <- reticulate::import("pandas")

  drugs <- pd$read_pickle(featureEngineeringSettings$drugCodeLoc)
  drugConcepts <- gsub('omop_', '', drugs)

  drugAutoMap <- data.frame(
    columnId = 1:length(drugConcepts),
    covariateId = as.double(drugConcepts)*1000+featureEngineeringSettings$drugAnalysisId
  )

  # convert to sparse matrix
  sparseData <- PatientLevelPrediction::toSparseM(
    plpData = trainData,
    cohort = trainData$cohort,
    map = drugAutoMap
  )

  # now load the autoencoder
  drugAuto <- keras$models$load_model(
    filepath = featureEngineeringSettings$autoEncoderLoc,
    compile = FALSE
  )


  nrows <- dim(sparseData$dataMatrix)[1]
  maxRind <- ceiling(nrows/20000)

  # now apply the autoencoder - need to do this in batches as have to convert to dense matrix
  maxCol <- 0
  for(rind in 1:maxRind){
    start <- (rind-1)*20000+1
    end <- min((rind)*20000,nrows)
    mappedFeatures <- drugAuto$predict(as.matrix(sparseData$dataMatrix[start:end,]))

    # 128 features
    if(maxCol < ncol(mappedFeatures)){
      maxCol = ncol(mappedFeatures)
    }

    newCovariates <- c()
    for(i in 1:ncol(mappedFeatures)){
      newCovariates <- rbind(
        newCovariates,
        data.frame(
          rowId = start:end,
          covariateId = i*1000+341,
          covariateValue = mappedFeatures[,i]
        )
      )
    }

    # append the new data
    Andromeda::appendToTable(
      tbl = trainData$covariateData$covariates,
      data = newCovariates
    )
  }

  # remove the condition covariates as no longer needed
  trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
    dplyr::filter(!.data$covariateId %in% drugAutoMap$covariateId)

  # update covariate Ref
  trainData$covariateData$covariateRef <- trainData$covariateData$covariateRef %>%
    dplyr::filter(!.data$covariateId %in% drugAutoMap$covariateId)

  newCovRef <- data.frame(
    covariateId = (1:maxCol)*1000+341,
    covariateName = paste0('drugs_embed',1:maxCol),
    analysisId = 341
  )

  Andromeda::appendToTable(
    tbl = trainData$covariateData$covariateRef,
    data = newCovRef
  )

  return(trainData)

}
