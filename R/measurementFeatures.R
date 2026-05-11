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
  sql <- paste("SELECT c.@row_id_field AS row_id,
               measurement_concept_id,
               unit_concept_id,",
               "value_as_number,",
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
    analysisId = 444
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
                            analysisId = analysisId
  )

  attr(covariateSettings, "fun") <- "GlaucomaPrescreeningPrediction::getMeasurementCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}



getMeasurements <- function(){

  labs <- list(
    list(concepts =3038553, name = 'bmi'),
    list(concepts =3012888, name = 'dbp'),
    list(concepts =3004249, name = 'sbp'),
    list(concepts =3027018, name = 'heart_rate'),
    list(concepts =3004410, name = 'a1c'),
    list(concepts =3009201, name = 'tsh'),
    list(concepts =3027114, name = 'total_chol'),
    list(concepts =3028288, name = 'ldl_chol'),
    list(concepts =3007070, name = 'hdl_chol'),
    list(concepts =3044491, name = 'nonhdl_chol'),
    list(concepts =3022192, name = 'triglyceride'),
    list(concepts =3020416, name = 'red_blood'),
    list(concepts =3000905, name = 'white_blood'),
    list(concepts =3000963, name = 'hemoglobin'),
    list(concepts =3023314, name = 'hematocrit'),
    list(concepts =3024929, name = 'platelets'),
    list(concepts =c(3019550,3000285), name = 'sodium'),
    list(concepts =c(3005456, 3023103), name = 'potassium'),
    list(concepts =c(3018572,3014576), name = 'chloride'),
    list(concepts =c(3014094,3015632), name = 'co2'),
    list(concepts =c(3024561), name = 'albumin'),
    list(concepts =c(3001110, 3035995), name = 'alk_phos'),
    list(concepts =c(3028833, 3024128), name = 'bilirubin'),
    list(concepts =c(3013721, 36305398, 3037081), name = 'aspartate_trans'),
    list(concepts =c(46235106, 3006923, 3027388, 3005755 ), name = 'alaine_trans'),
    list(concepts =c(3013682), name = 'blood_urea_nit'),
    list(concepts =c(3020630), name = 'protein'),
    list(concepts =c(3006906), name = 'calcium'),
    list(concepts =c(3016723), name = 'creatinine'),
    list(concepts =c(3000483, 3004501), name = 'glucose'),
    list(concepts = c(3015501, 3015736, 3029305, 3022621), name = 'ph')
  )

return(labs)
}
