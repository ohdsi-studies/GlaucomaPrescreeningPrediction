#' Extracts measurement covariates
#'
#' @details
#' The user specifies a cohort and time period and then a covariate is constructed whether they are in the
#' cohort during the time periods relative to target population cohort index
#'
#' @param connection  The database connection
#' @param tempEmulationSchema The schema to use for temp tables
#' @param oracleTempSchema  DEPRECATED The temp schema if using oracle
#' @param cdmDatabaseSchema  The schema of the OMOP CDM data
#' @param cdmVersion  version of the OMOP CDM data
#' @param cohortTable  the table name that contains the target population cohort
#' @param rowIdField  string representing the unique identifier in the target population cohort
#' @param aggregated  whether the covariate should be aggregated
#' @param cohortId cohort id for the target cohort
#' @param covariateSettings  settings for the covariate cohorts and time periods
#' @param ...  additional arguments from FeatureExtraction
#'
#' @return
#' CovariateData object with covariates, covariateRef, and analysisRef tables
#' @export
getMeasurementCovariateData <- function(connection,
                                        tempEmulationSchema = NULL,
                                        oracleTempSchema = NULL,
                                        cdmDatabaseSchema,
                                        cdmVersion = "5",
                                        cohortTable = "#cohort_person",
                                        rowIdField = "row_id",
                                        aggregated,
                                        cohortId,
                                        covariateSettings,
                                        ...
) {

  # to get table 1 - take source values and then map them - dont map in SQL
  message(paste0('running getMeasurementCovariateData'))
  # Some SQL to construct the covariate:
  sql <- paste("WITH measurement_val AS (SELECT c.@row_id_field AS row_id,
               measurement_concept_id,
               unit_concept_id,",
               "(value_as_number - @scale_min)*1.0/@scale_range as value_as_number,",
               "measurement_date,",
               "YEAR(GETDATE()) - p.year_of_birth AS age_in_years,",
               "ABS(datediff(dd, measurement_date, c.cohort_start_date)) AS index_time",
               "FROM @cdm_database_schema.measurement m INNER JOIN @cohort_temp_table c
                ON c.subject_id = m.person_id",
               "AND measurement_date >= dateadd(day, @startDay, cohort_start_date) ",
               "AND measurement_date <= dateadd(day, @endDay, cohort_start_date) ",
               "INNER JOIN @cdm_database_schema.person p ON p.person_id=c.subject_id",
               "WHERE m.measurement_concept_id IN (@concepts)
                AND value_as_number IS NOT NULL
               {@use_min}?{AND value_as_number >= @min_val}
               {@use_max}?{AND value_as_number <= @max_val}
               {@restrict_units}?{
               AND (unit_concept_id IN (@units) {@na_unit}?{OR unit_concept_id  is NULL})
               }
  )

{@impute}?{
  ,missing_val AS (SELECT
               c.@row_id_field AS row_id,
               0 AS measurement_concept_id,
               0 AS unit_concept_id,",
               "(@imputated_value - @scale_min)*1.0/@scale_range as value_as_number,",
               "c.cohort_start_date AS measurement_date,",
               "YEAR(GETDATE()) - p.year_of_birth AS age_in_years,",
               "0 AS index_time",
               "FROM @cohort_temp_table c
                INNER JOIN @cdm_database_schema.person p ON p.person_id=c.subject_id",
               "WHERE NOT EXISTS (SELECT 1 from measurement_val b where c.@row_id_field = b.row_id)",
  ")
}

  SELECT * from measurement_val
{@impute}?{
  UNION
  SELECT * from missing_val
}


               ;
               "
  )

  sql <- SqlRender::render(sql,
                           cohort_temp_table = cohortTable,
                           row_id_field = rowIdField,
                           startDay=covariateSettings$startDay,
                           endDay=covariateSettings$endDay,
                           concepts = paste(covariateSettings$conceptSet, collapse = ','),
                           cdm_database_schema = cdmDatabaseSchema,
                           scale_min = ifelse(is.null(covariateSettings$scaleMin), 0, covariateSettings$scaleMin),
                           scale_range = ifelse(is.null(covariateSettings$scaleMax), 1, covariateSettings$scaleMax - covariateSettings$scaleMin),
                           impute = !is.null(covariateSettings$imputatedValue),
                           imputated_value = covariateSettings$imputatedValue,
                           use_min  = !is.null(covariateSettings$minVal),
                           min_val  = covariateSettings$minVal,
                           use_max = !is.null(covariateSettings$maxVal),
                           max_val  = covariateSettings$maxVal,
                           restrict_units = !is.null(covariateSettings$unitSet),
                           na_unit = NA %in% covariateSettings$unitSet,
                           units = paste(covariateSettings$unitSet[!is.na(covariateSettings$unitSet)], collapse = ',')
  )
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"),
                              oracleTempSchema = oracleTempSchema)
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))

  # map data:
  covariates <- covariates[!is.na(covariates$valueAsNumber),]

  # if scaleMap is a function call it otherwise eval the string function
  if(inherits(covariateSettings$scaleMap, 'function')){
    covariates <- covariateSettings$scaleMap(covariates)
  } else{
    scaleMap <- eval(parse(text = covariateSettings$scaleMap))
    covariates <- scaleMap(covariates)
  }

  # aggregate data:
  if(covariateSettings$aggregateMethod == 'max'){
    covariates <- covariates %>%
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue = max(.data$valueAsNumber,na.rm = TRUE))
  } else if(covariateSettings$aggregateMethod == 'min'){
    covariates <- covariates %>%
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue = min(.data$valueAsNumber,na.rm = TRUE))
  } else if(covariateSettings$aggregateMethod == 'mean'){
    covariates <- covariates %>%
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue = mean(.data$valueAsNumber,na.rm = TRUE))
  } else if(covariateSettings$aggregateMethod == 'median'){
    covariates <- covariates %>%
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue = median(.data$valueAsNumber,na.rm = TRUE))
  } else if(covariateSettings$aggregateMethod == 'stdev'){
    covariates <- covariates %>%
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue = sd(.data$valueAsNumber,na.rm = TRUE))
  } else{
    last <- covariates %>%
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(lastTime = min(.data$indexTime, na.rm = TRUE))

    covariates <- merge(covariates,last,
                        by.x = c('rowId','indexTime'),
                        by.y = c('rowId','lastTime') )

    covariates <- covariates %>%
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(
        covariateValue = mean(.data$valueAsNumber)
      )
  }

  # do age interaction or log age
  if(covariateSettings$ageInteract){
    covariates <- covariates %>%
      dplyr::mutate(covariateValue = .data$covariateValue*.data$ageInYears/covariateSettings$ageScale)
  }
  if(covariateSettings$logAgeInteract){
    covariates <- covariates %>%
      dplyr::mutate(covariateValue = .data$covariateValue*log(.data$ageInYears/covariateSettings$ageScale))
  }

  # add covariateID:
  covariates$covariateId <- covariateSettings$covariateId


  covariates <- covariates %>% dplyr::select(rowId, covariateId, covariateValue)


  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = covariateSettings$covariateId,
                             covariateName = paste('Measurement during day',
                                                   covariateSettings$startDay,
                                                   'through',
                                                   covariateSettings$endDay,
                                                   'days relative to index:',
                                                   covariateSettings$covariateName
                             ),
                             analysisId = covariateSettings$analysisId,
                             conceptId = 0,
                             valueAsConceptId = 0,
                             collisions = NA
  )

  analysisRef <- data.frame(analysisId = covariateSettings$analysisId,
                            analysisName = "measurement covariate",
                            domainId = "measurement covariate",
                            startDay = covariateSettings$startDay,
                            endDay = covariateSettings$endDay,
                            isBinary = "N",
                            missingMeansZero = "Y")

  result <- Andromeda::andromeda(covariates = covariates,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  class(result) <- "CovariateData"
  return(result)
}

#' Create covariates settings for measurements
#'
#' @details
#' The user specifies
#'
#' @param covariateName The name of the covariate
#' @param conceptSet A vector of conceptIds that are the measurement concepts
#' @param unitSet NULL or a vector of conceptIds to restrict the units to (can include NA)
#' @param startDay the start time before index to look for the measurement
#' @param endDay the end time before index to look for the measurement
#' @param scaleMap A function that lets you concept units into a standard unit and do any scaling
#' @param ageInteract Whether to do interaction with age/ageScale in years
#' @param logAgeInteract Whether to do interaction with log(age/ageScale) in years
#' @param ageScale A value to divide age by for the age/logAge interaction
#' @param minVal NULL or the min valid value for the measurements (value less than this are excluded)
#' @param maxVal NULL or the max valid value for the measurements (value more than this are excluded)
#' @param aggregateMethod one of max/min/mean/median/recent how to handle multiple measurements
#' @param covariateId a unique value for the covariateId
#' @param analysisId a unique value for the analysisId
#' @param scaleMin The min value to scale by
#' @param scaleMax The max value to scale by
#' @param imputatedValue (optional) The value to impute if there is no value
#'
#' @return
#' An object of class `covariateSettings` specifying how to create the cohort covariate with the covariateId
#'  cohortId x 100000 + settingId x 1000 + analysisId
#'
#' @export
createMeasurementCovariateSettings <- function(
    covariateName,
    conceptSet,
    unitSet = NULL,
    startDay=-30,
    endDay=0,
    scaleMap = function(x){return(x)},
    ageInteract = FALSE,
    logAgeInteract = FALSE,
    ageScale = 1,
    minVal = NULL,
    maxVal = NULL,
    aggregateMethod = 'recent',
    covariateId = 1444,
    analysisId = 444,
    scaleMin = NULL,
    scaleMax = NULL,
    imputatedValue = NULL
) {

  if(ageInteract & logAgeInteract){
    stop(paste0('Error - max of one of logAgeInteract and ageInteract can be TRUE'))
  }

  if(!inherits(ageScale, 'numeric')){
    stop('ageScale must be numeric')
  }
  if(ageScale == 0){
    stop('ageScale cannot be 0')
  }

  covariateSettings <- list(covariateName=covariateName,
                            conceptSet=conceptSet,
                            unitSet = unitSet,
                            startDay=startDay,
                            endDay=endDay,
                            scaleMap=scaleMap,
                            ageInteract = ageInteract,
                            logAgeInteract = logAgeInteract,
                            ageScale = ageScale,
                            aggregateMethod = aggregateMethod,
                            minVal = minVal,
                            maxVal = maxVal,
                            covariateId = covariateId,
                            analysisId = analysisId,
                            scaleMin = scaleMin,
                            scaleMax = scaleMax,
                            imputatedValue = imputatedValue
  )

  attr(covariateSettings, "fun") <- "GlaucomaPrescreeningPrediction::getMeasurementCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}



getMeasurements <- function(){

  labs <- list(
    list(concepts =3038553, name = 'bmi', min = 0, max = 3234.2, median = 30.2),
    list(concepts =3012888, name = 'dbp', min = 0, max = 181, median = 76),
    list(concepts =3004249, name = 'sbp', min = 0, max = 248, median = 127),
    list(concepts =3027018, name = 'heart_rate', min = 0, max = 591, median = 75),
    list(concepts =3004410, name = 'a1c', min = 0, max = 10000000, median = 6.2),
    # modified tsh to add concepts
    list(concepts =c(3009201,4197602,4193708, 37399332,37394134, 37393873, 4197602), name = 'tsh', min = 0, max = 10000000, median = 2),
    list(concepts =3027114, name = 'total_chol', min = 0, max = 10000000, median = 188),
    # modified ldl_chol to add concepts
    list(concepts =c(3028288, 3028437, 4012479), name = 'ldl_chol', min = -33, max = 10000000, median = 100),
    list(concepts =3007070, name = 'hdl_chol', min = 3, max = 10000000, median = 54),
    list(concepts =3044491, name = 'nonhdl_chol', min = 0, max = 685, median = 122),
    list(concepts =3022192, name = 'triglyceride', min = 7, max = 10000000, median = 113),
    # modified red_blood to add concepts
    list(concepts = c(3020416,4030871,37393849), name = 'red_blood', min = 0, max = 10000000, median = 5),
    # modified white_blood to add concepts
    list(concepts =c(3000905, 4298431), name = 'white_blood', min = 0, max = 10000000, median = 1078),
    list(concepts =3000963, name = 'hemoglobin', min = 0.084, max = 10000000, median = 13.4),
    # modified hematocrit to add concepts
    list(concepts = c(3023314, 3009542, 40789179), name = 'hematocrit', min = 0, max = 10000000, median = 42),
    # modified platelets to add concepts
    list(concepts = c(3024929, 3007461), name = 'platelets', min = 0, max = 10000000, median = 992),
    list(concepts =c(3019550,3000285), name = 'sodium', min = 20, max = 169, median = 140),
    list(concepts =c(3005456, 3023103), name = 'potassium', min = 0, max = 10000000,median = 165),
    list(concepts =c(3018572,3014576), name = 'chloride', min = 5.2, max = 130, median = 103),
    list(concepts =c(3014094,3015632), name = 'co2', min = 0, max = 50, median = 28),
    list(concepts =c(3024561), name = 'albumin', min = 0, max = 41000, median = 40),
    list(concepts =c(3001110, 3035995), name = 'alk_phos', min = 0.7, max = 6000, median = 83),
    list(concepts =c(3028833, 3024128), name = 'bilirubin', min = 0, max = 10000000, median = 0.5),
    list(concepts =c(3013721, 36305398, 3037081), name = 'aspartate_trans', min = 0, max = 10000000, median = 22),
    list(concepts =c(46235106, 3006923, 3027388, 3005755 ), name = 'alaine_trans', min = 3, max = 10000000, median = 24),
    list(concepts =c(3013682), name = 'blood_urea_nit', min = 0, max = 213),
    list(concepts =c(3020630), name = 'protein', min = 0, max = 10000000, median = 7),
    list(concepts =c(3006906), name = 'calcium', min = 1.18, max = 26, median = 9.4),
    list(concepts =c(3016723), name = 'creatinine', min = 0, max = 274.5, median = 0.84),
    list(concepts =c(3000483, 3004501), name = 'glucose', min = 0, max = 10000000, median = 98),
    list(concepts = c(3015501, 3015736, 3029305, 3022621), name = 'ph', min = 0, max = 10000000, median = 6)
  )

return(labs)
}


createAgeScale <- function(min = 0, max = 120) {
  # create list of inputs to implement function
  featureEngineeringSettings <- list(
    min = min,
    max = max
  )

  # specify the function that will implement the sampling
  attr(featureEngineeringSettings, "fun") <- "implementAgeScale"

  # make sure the object returned is of class "sampleSettings"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}

#' function to scale age 1002 - min:21	max:109	range:88
#'
#' @description
#' Call the age scaling function
#'
#' @details
#' Used by applyFeatureEngineering to scale the age in years
#'
#' @param trainData The training data to apply the autoencoder to
#' @param featureEngineeringSettings settings for loading the autoencoder
#' @param model The plp model
#'
#' @return
#' The plp data with the scaled age added as covariate id 2002
#'
#'
#' @export
implementAgeScale <- function(trainData, featureEngineeringSettings, model = NULL){

  if (is.null(model)) {
    ageData <- trainData$cohorts
    ageYear <- ageData$ageYear

    min <- min(ageYear)
    featureEngineeringSettings$min <- min
    max <- max(ageYear)
    featureEngineeringSettings$max <- max

    # scale
    newData <- data.frame(
      rowId = ageData$rowId,
      covariateId = 2002,
      covariateValue = (ageYear-min)/(max-min)
    )
  } else {
    # use existing min/max
    min <- featureEngineeringSettings$min
    max <- featureEngineeringSettings$max

    ageData <- trainData$cohorts
    ageYear <- trainData$cohorts$ageYear
    newData <- data.frame(
      rowId = trainData$cohorts$rowId,
      covariateId = 2002,
      covariateValue = (ageYear-min)/(max-min)
    )
  }

  # remove existing age if in covariates
  ##trainData$covariateData$covariates <- trainData$covariateData$covariates |>
  ##  dplyr::filter(!.data$covariateId %in% c(1002))

  # update covRef
  Andromeda::appendToTable(
    trainData$covariateData$covariateRef,
    data.frame(
      covariateId = 2002,
      covariateName = "Scaled age",
      analysisId = 2,
      conceptId = 2002
    )
  )

  # update covariates
  Andromeda::appendToTable(trainData$covariateData$covariates, newData)

  featureEngineering <- list(
    funct = "implementAgeScale",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      model = model
    )
  )

  feLen <- length(attr(trainData$covariateData, "metaData")$featureEngineering)
  attr(trainData$covariateData, "metaData")$featureEngineering[[feLen + 1]] <- featureEngineering

  return(trainData)
}
