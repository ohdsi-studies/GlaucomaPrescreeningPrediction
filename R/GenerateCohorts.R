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

  # add case and non-case people
  cohortDefinitionSet <- addCaseNonCases(
    cohortDefinitionSet = cohortDefinitionSet,
    targetId = 23884,
    outcomeId = 23933,
    caseId = 23884001,
    nonCaseId = 23884002
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





addCaseNonCases <- function(
    cohortDefinitionSet,
    targetId,
    outcomeId,
    caseId,
    nonCaseId
) {
  sql <- "
INSERT INTO @cohort_database_schema.@cohort_table
  (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
SELECT DISTINCT
    @definition_id AS cohort_definition_id,
    has_outcome_tar.subject_id,
    has_outcome_tar.cohort_start_date,
    has_outcome_tar.cohort_end_date

    FROM

    (
    SELECT
    t.subject_id,
    t.cohort_start_date,
    t.cohort_end_date


    FROM @cohort_database_schema.@cohort_table t
    INNER JOIN @cohort_database_schema.@cohort_table o
    ON t.subject_id = o.subject_id

    WHERE t.cohort_definition_id IN (@target_id)
    AND o.cohort_definition_id IN (@outcome_id)
    AND o.cohort_start_date >= DATEADD(d, 0, t.cohort_start_date) AND o.cohort_start_date <= DATEADD(d, 1095, t.cohort_start_date)
    ) has_outcome_tar


    lEFT JOIN

    (
    SELECT
    t.subject_id,
    t.cohort_start_date,
    t.cohort_end_date


    FROM @cohort_database_schema.@cohort_table t
    INNER JOIN @cohort_database_schema.@cohort_table o
    ON t.subject_id = o.subject_id

    WHERE t.cohort_definition_id IN (@target_id)
    AND o.cohort_definition_id IN (@outcome_id)
    AND o.cohort_start_date <= DATEADD(d, -1, t.cohort_start_date)
    ) has_outcome_prior

    ON has_outcome_tar.subject_id = has_outcome_prior.subject_id
    WHERE has_outcome_prior.cohort_start_date is NULL;

"

  cohortDefinitionSet <- cohortDefinitionSet |>
    CohortGenerator::addSqlCohortDefinition(sql = sql,
                                            cohortId = caseId,
                                            cohortName = 'Cases',
                                            translateSql = TRUE,
                                            warnOnMissingParameters = FALSE,
                                            definition_id = caseId,
                                            target_id = targetId,
                                            outcome_id = outcomeId
    )



  sql <- "
INSERT INTO @cohort_database_schema.@cohort_table
  (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
SELECT DISTINCT
    @definition_id AS cohort_definition_id,
    t.subject_id,
    t.cohort_start_date,
    t.cohort_end_date

    FROM  (
    SELECT
    subject_id,
    cohort_start_date,
    cohort_end_date
    FROM @cohort_database_schema.@cohort_table
    WHERE cohort_definition_id IN (@target_id)
    ) t

    LEFT JOIN

    (
    SELECT
    t.subject_id,
    1 as dummy_var

    FROM @cohort_database_schema.@cohort_table t
    INNER JOIN @cohort_database_schema.@cohort_table o
    ON t.subject_id = o.subject_id

    WHERE t.cohort_definition_id IN (@target_id)
    AND o.cohort_definition_id IN (@outcome_id)
    AND o.cohort_start_date >= DATEADD(d, 0, t.cohort_start_date) AND o.cohort_start_date <= DATEADD(d, 1095, t.cohort_start_date)
    ) has_outcome_tar

    ON t.subject_id = has_outcome_tar.subject_id

    lEFT JOIN

    (
    SELECT
    t.subject_id,
    1 as dummy_var

    FROM @cohort_database_schema.@cohort_table t
    INNER JOIN @cohort_database_schema.@cohort_table o
    ON t.subject_id = o.subject_id

    WHERE t.cohort_definition_id IN (@target_id)
    AND o.cohort_definition_id IN (@outcome_id)
    AND o.cohort_start_date <= DATEADD(d, -1, t.cohort_start_date)
    ) has_outcome_prior

    ON t.subject_id = has_outcome_prior.subject_id

    WHERE has_outcome_prior.dummy_var is NULL
    AND has_outcome_tar.dummy_var is NULL
    ;

"

  cohortDefinitionSet <- cohortDefinitionSet |>
    CohortGenerator::addSqlCohortDefinition(sql = sql,
                                            cohortId = nonCaseId,
                                            cohortName = 'Non Cases',
                                            translateSql = TRUE,
                                            warnOnMissingParameters = FALSE,
                                            definition_id = nonCaseId,
                                            target_id = targetId,
                                            outcome_id = outcomeId
    )

  return(cohortDefinitionSet)
}
