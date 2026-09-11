#' run characterization
#'
#' @details
#' The user specifies the connectionDetails to the new OMOP CDM data,
#'
#' @param connectionDetails The connection details for the OMOP CDM data
#' @param cdmDatabaseSchema The schema containing the OMOP CDM data
#' @param cohortDatabaseSchema A schema with read/write access that contains the target and outcome cohorts
#' @param tempEmulationSchema The schema to use for temp tables if the dbms does not support temp tables
#' @param cohortTable The name of the cohort table that contains the target and outcome cohorts
#' @param minCellCount The minCellCount for results
#' @param saveLoc The location to save the results to
#'
#' @return
#' The location where the csv files for the characterization are
#' @export
runCharacterization <- function(
    connectionDetails,
    cdmDatabaseSchema,
    cohortDatabaseSchema,
    tempEmulationSchema = Sys.getenv("DATABRICKS_SCRATCH_SCHEMA"),
    cohortTable = 'glau_screen_cohort',
    minCellCount = 0,
    saveLoc = tempdir()
    ){

  targetIds <- 23884
  caseId <- 23884001
  nonCaseId <- 23884002
  outcomeIds <- 23933

  covariateSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAgeGroup = TRUE,
    useDemographicsAge = TRUE,
    useDemographicsRace = TRUE,
    useDemographicsEthnicity = TRUE,
    useDemographicsIndexYear = TRUE,
    useCharlsonIndex = TRUE,
    useVisitCountLongTerm = TRUE,
    useConditionGroupEraAnyTimePrior = TRUE,
    useDrugGroupEraAnyTimePrior = TRUE,
    useProcedureOccurrenceAnyTimePrior = TRUE,
    useObservationAnyTimePrior = TRUE
    #, add more ...
  )

  # all occurrences of target index
  studyPopulationSettingsAll <- Characterization::createStudyPopulationSettings(
    targetIds = targetIds,
    limitToFirstInNDays = 0,
    minPriorObservation = 365
  )

  # first occurrence of target index
  studyPopulationSettingsFirst <- Characterization::createStudyPopulationSettings(
    targetIds = targetIds,
    limitToFirstInNDays = 99999,
    minPriorObservation = 365
  )

  studyPopulationSettingsFirstBoth <- Characterization::createStudyPopulationSettings(
    targetIds = c(targetIds, outcomeIds, caseId, nonCaseId),
    limitToFirstInNDays = 99999,
    minPriorObservation = 365
  )


  # target baseline for target, outcome, ?
  targetBaselineSettings <- Characterization::createTargetBaselineSettings(
    studyPopulationSettings = studyPopulationSettingsFirstBoth,
    covariateSettings = covariateSettings
  )

  # time to event for target and outcome
  timeToEventSettings <- Characterization::createTimeToEventSettings(
    outcomeIds = outcomeIds,
    studyPopulationSettings = studyPopulationSettingsAll
    )

  # risk factors for target and outcome with 3-year TAR
  riskFactorSettings <- Characterization::createRiskFactorSettings(
    studyPopulationSettings = studyPopulationSettingsFirst,
    outcomeIds = outcomeIds,
    outcomeWashoutDays = 99999,
    riskWindowStart = 0,
    startAnchor = 'cohort start',
    riskWindowEnd = 365*3,
    endAnchor = 'cohort start',
    covariateSettings = covariateSettings
      )

  # cohort incidence for target and outcome with 3-year TAR
  cohortIncidenceSettings <- Characterization::createCohortIncidenceSettings(
    studyPopulationSettings = studyPopulationSettingsFirst,
    outcomeIds = outcomeIds,
    outcomeWashoutDays = 99999,
    riskWindowStart = 0,
    startAnchor = 'cohort start',
    riskWindowEnd = 365*3,
    endAnchor = 'cohort start',
    byAge = TRUE,
    ageBreaks = c(0,18,65,100),
    byGender = TRUE,
    byYear = TRUE
  )

  # dechal-rechal not needed

  charSet <- Characterization::createCharacterizationSettings(
    riskFactorSettings = riskFactorSettings,
    timeToEventSettings = timeToEventSettings,
    targetBaselineSettings = targetBaselineSettings,
    cohortIncidenceSettings = cohortIncidenceSettings
  )

  # run the characterizations
  Characterization::runCharacterizationAnalyses(
    connectionDetails = connectionDetails,
    characterizationSettings = charSet,
    mode = 'CohortIncidence',
    minCharacterizationMean = 0.01,
    minCovariateCount = 2,
    minSMD = 0.1,
    minCellCount = minCellCount,
    outputDirectory = file.path(saveLoc, 'characterization'),
    targetDatabaseSchema = cohortDatabaseSchema,
    targetTable = cohortTable,
    outcomeDatabaseSchema = cohortDatabaseSchema,
    outcomeTable = cohortTable,
    nestingCohortDatabaseSchema = cohortDatabaseSchema,
    nestingCohortTable = cohortTable,
    outputDatabaseSchema = cohortDatabaseSchema,
    outputTable = 'glau_screen_cohort_char',
    tempEmulationSchema = tempEmulationSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    incremental = FALSE,
    databaseId = cdmDatabaseSchema
    )

return(invisible(file.path(saveLoc, 'characterization')))
}
