#!/usr/bin/env Rscript

required_env <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    stop(sprintf("Missing required environment variable: %s", name), call. = FALSE)
  }
  value
}

optional_env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = default)
  if (!nzchar(value)) default else value
}

as_optional_integer <- function(value) {
  if (!nzchar(value)) {
    return(NULL)
  }
  out <- suppressWarnings(as.integer(value))
  if (is.na(out)) {
    stop(sprintf("Expected an integer value, received: %s", value), call. = FALSE)
  }
  out
}

as_bool <- function(value, default = FALSE) {
  if (!nzchar(value)) {
    return(default)
  }
  normalized <- tolower(trimws(value))
  if (normalized %in% c("1", "true", "yes", "y")) {
    return(TRUE)
  }
  if (normalized %in% c("0", "false", "no", "n")) {
    return(FALSE)
  }
  stop(sprintf("Expected boolean value for flag, received: %s", value), call. = FALSE)
}

library(DatabaseConnector)
library(GlaucomaPrescreeningPrediction)
library(reticulate)

reticulate_env <- optional_env("RETICULATE_ENV", "/opt/venv/glaucoma-screen")
reticulate::use_virtualenv(reticulate_env, required = TRUE)

dbms <- required_env("DBMS")
connection_string <- required_env("CONNECTION_STRING")
db_user <- optional_env("DB_USER", "")
db_password <- optional_env("DB_PASSWORD", "")

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = dbms,
  user = db_user,
  password = db_password,
  connectionString = connection_string
)

cdmDatabaseSchema <- required_env("CDM_DATABASE_SCHEMA")
cohortDatabaseSchema <- required_env("COHORT_DATABASE_SCHEMA")
cohortTable <- optional_env("COHORT_TABLE", "glau_screen_cohort")
tempEmulationSchema <- optional_env("TEMP_EMULATION_SCHEMA", "")
if (!nzchar(tempEmulationSchema)) {
  tempEmulationSchema <- NULL
} else {
  options(sqlRenderTempEmulationSchema = tempEmulationSchema)
}

targetId <- as.integer(optional_env("TARGET_ID", "23884"))
outcomeId <- as.integer(optional_env("OUTCOME_ID", "23933"))
sampleSize <- as_optional_integer(optional_env("SAMPLE_SIZE", ""))

generate <- as_bool(optional_env("GENERATE_COHORTS", "false"), default = FALSE)

if (isTRUE(generate)) {
  message("Generating cohorts...")
  GlaucomaPrescreeningPrediction::generateCohorts(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableName = cohortTable,
    tempEmulationSchema = tempEmulationSchema
  )
}

message("Running glaucoma prescreening model...")
results <- GlaucomaPrescreeningPrediction::execute(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  tempEmulationSchema = tempEmulationSchema,
  cohortTable = cohortTable,
  targetId = targetId,
  outcomeId = outcomeId,
  sampleSize = sampleSize
)

output_file <- optional_env("OUTPUT_FILE", "/output/results.rds")
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(results, output_file)

message("Study complete. Results written to: ", output_file)