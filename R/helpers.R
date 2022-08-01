#helper functions
getTargetCohort <- function(connectionDetails,
                            resultsDatabaseSchema,
                            cohortTable,
                            targetCohortId) {

  #set connection
  suppressMessages(connection <- DatabaseConnector::connect(connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  sql <- "SELECT * FROM @resultsDatabaseSchema.@cohortTable WHERE cohort_definition_id = @targetCohortId"
  cohort <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    resultsDatabaseSchema = resultsDatabaseSchema,
    cohortTable = cohortTable,
    targetCohortId = targetCohortId,
    snakeCaseToCamelCase = TRUE
  )
  return(cohort)
}


getUniquePersons <- function(cohort) {
  cohort %>%
    dplyr::distinct(.data$subjectId) %>%
    dplyr::pull()
}

getDob <- function(connectionDetails,
                         cdmDatabaseSchema,
                         personIds) {
  #set connection
  suppressMessages(connection <- DatabaseConnector::connect(connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  sql1 <- "SELECT person_id, year_of_birth, month_of_birth
           FROM @cdmDatabaseSchema.person
           WHERE person_id IN (@personIds)"

  dob <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql1,
    cdmDatabaseSchema = cdmDatabaseSchema,
    personIds = personIds,
    snakeCaseToCamelCase = TRUE) %>%
    dplyr::mutate(
      monthOfBirth = stringr::str_pad(monthOfBirth, 2, side = "left", pad = "0"),
      dob = lubridate::mdy(paste(monthOfBirth, "01", yearOfBirth, sep = "/"))
    ) %>%
    dplyr::select(.data$personId, .data$dob)


  return(dob)
}

getCdmElement <- function(connectionDetails,
                          cdmDatabaseSchema,
                          vocabularyDatabaseSchema,
                          cdmTable,
                          cdmTableElement,
                          cdmTableValue,
                          personIds) {
  #set connection
  suppressMessages(connection <- DatabaseConnector::connect(connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  sql <- "SELECT a.person_id, b.concept_id, b.concept_name
  FROM @cdmDatabaseSchema.@cdmTable a
  JOIN @vocabularyDatabaseSchema.concept b
  ON a.@cdmTableElement = b.concept_id
  WHERE person_id IN (@personIds)
  AND @cdmTableElement IN (@cdmTableValue)"
  dat <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    cdmDatabaseSchema = cdmDatabaseSchema,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    cdmTable = cdmTable,
    cdmTableElement = cdmTableElement,
    cdmTableValue = cdmTableValue,
    personIds = personIds,
    snakeCaseToCamelCase = TRUE
  )
  return(dat)
}

checkOp <- function(op) {
  opTable <- data.frame(
    op_id = 1:6,
    op_symbol = c(">", ">=", "<", "<=", "==", "!="),
    op_charShort = c("gt", "gte", "lt", "lte", "eq", "neq"),
    op_char = c("greater than", "greater than or equal to", "less than", "less than or equal to",
                "equal to", "not equal to")
  )

  opCheck <- opTable %>%
    dplyr::select(.data$op_id:.data$op_char) %>%
    purrr::map(~purrr::map_lgl(.x, ~grepl(op, .x)))
  opCheck_col <- purrr::map_lgl(opCheck, ~any(.x)) %>% purrr::detect_index(isTRUE)
  opCheck_row <- purrr::detect_index(opTable[[opCheck_col]] %in% op, isTRUE)

  ll <- list(
  op = opTable[opCheck_row, 2],
  opcharShort = opTable[opCheck_row, 3]
  )

  return(ll)
}



covariateId_to_conceptId <- function(covariateId) {
  as.integer(substring(as.character(covariateId), 1, nchar(covariateId) - 3))
}

formatDemographics <- function(dat) {

  dat %>%
    dplyr::mutate(
      conceptName = dplyr::case_when(
        conceptId == 8507 & is.na(conceptName) ~ "Male",
        conceptId == 8532 & is.na(conceptName) ~ "Female",
        conceptId == 1 & is.na(conceptName) ~ "5 - 9",
        conceptId == 2 & is.na(conceptName) ~ "10 - 14",
        conceptId == 3 & is.na(conceptName) ~ "15 - 19",
        conceptId == 4 & is.na(conceptName) ~ "20 - 24",
        conceptId == 5 & is.na(conceptName) ~ "25 - 29",
        conceptId == 6 & is.na(conceptName) ~ "30 - 34",
        conceptId == 7 & is.na(conceptName) ~ "35 - 39",
        conceptId == 8 & is.na(conceptName) ~ "40 - 44",
        conceptId == 9 & is.na(conceptName) ~ "45 - 49",
        conceptId == 10 & is.na(conceptName) ~ "50 - 54",
        conceptId == 11 & is.na(conceptName) ~ "55 - 59",
        conceptId == 12 & is.na(conceptName) ~ "60 - 64",
        conceptId == 13 & is.na(conceptName) ~ "65 - 69",
        conceptId == 14 & is.na(conceptName) ~ "70 - 74",
        conceptId == 15 & is.na(conceptName) ~ "75 - 79",
        conceptId == 16 & is.na(conceptName) ~ "80 - 84",
        conceptId == 17 & is.na(conceptName) ~ "85 - 89",
        conceptId == 18 & is.na(conceptName) ~ "90 - 94",
        conceptId == 19 & is.na(conceptName) ~ "95 - 99",
        conceptId == 20 & is.na(conceptName)  ~ "100 - 104",
        TRUE ~ conceptName
      ),
      categoryName = dplyr::case_when(
        is.na(categoryName) ~ "Demographics",
        TRUE ~ categoryName
      ),
      categoryId = if_else(categoryId != 0, categoryId, -analysisId, -analysisId)
    )
}



rollupConditions <- function(connectionDetails,
                             cdmDatabaseSchema,
                             conceptIds) {

  #connect to database
  suppressMessages(connection <- DatabaseConnector::connect(connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  #change this to system file when turned into package
  #conditionSql <- readr::read_file(system.file("sql/conditionRollup.sql", package = "ariadne"))
  conditionSql <- readr::read_file("~/R/ideas/characterizationTables/sql/conditionRollup.sql")

  conditionRollup <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = conditionSql,
    snakeCaseToCamelCase = TRUE,
    cdmDatabaseSchema = cdmDatabaseSchema,
    conceptIds = conceptIds
  )
  names(conditionRollup) <- c("conceptId", "conceptName", "categoryId", "categoryName")
  return(conditionRollup)

}


rollupDrugs <- function(connectionDetails,
                        cdmDatabaseSchema,
                        conceptIds) {

  #connect to database
  suppressMessages(connection <- DatabaseConnector::connect(connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  #change this to system file when turned into package
  drugSql <- readr::read_file("~/R/ideas/characterizationTables/sql/drugRollup.sql")
  #drugSql <- readr::read_file(system.file("sql/drugRollup.sql", package = "ariadne"))

  drugRollup <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = drugSql,
    snakeCaseToCamelCase = TRUE,
    cdmDatabaseSchema = cdmDatabaseSchema,
    conceptIds = conceptIds
  ) %>%
    mutate(categoryName = stringr::str_to_title(categoryName))

  names(drugRollup) <- c("conceptId", "conceptName", "categoryId", "categoryName")

  return(drugRollup)

}
