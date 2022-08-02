
#' A function to define a date strata
#'
#' @param name the name of the strata
#' @param op the operator for the strata
#' @param value the value for the date strata
#' @include helpers.R
#' @return a date strata object
#' @export
define_date_strata <- function(name,
                               op,
                               date) {

  strata_type <- "date_strata"
  kk <- checkOp(op)
  fn_call <- paste("get", strata_type, sep ="_") %>%
    get()

  args <- rlang::fn_fmls_syms(fn_call)
  args$name <- name
  args$op <- kk$op
  args$date <- date

  call <- rlang::call2(paste("get", strata_type, sep ="_"),
                     !!!args)


  dateStrata <- list(name = name,
                     op = kk$op,
                     date = as.Date(date),
                     call = call)
  structure(dateStrata, class = c("strata","date_strata"))
}


#' A function to define a concept strata
#'
#' @param name the name of the strata
#' @param domain the domain of the concept
#' @param element the element in the table to search
#' @param concept the OMOP concept id for the strata
#' @return a concept strata object
#' @include helpers.R
#' @export
define_concept_strata <- function(name,
                                  domain,
                                  element,
                                  concept) {

  strata_type <- "concept_strata"
  fn_call <- paste("get", strata_type, sep ="_") %>%
    get()

  args <- rlang::fn_fmls_syms(fn_call)
  args$name <- name
  args$domain <- domain
  args$element <- element
  args$concept <- concept

  call <- rlang::call2(paste("get", strata_type, sep ="_"),
                     !!!args)

  conceptStrata <- list(name = name,
                        domain = domain,
                        element = element,
                        concept = concept,
                        call = call)

  structure(conceptStrata, class = c("strata", "concept_strata"))
}

#' A function to define an age strata
#'
#' @param name the name of the strata
#' @param op the operator for the strata
#' @param value the value for the age strata
#' @return an age strata object
#' @include helpers.R
#' @export
define_age_strata <- function(name,
                              op,
                              value) {

  strata_type <- "age_strata"
  kk <- checkOp(op)

  fn_call <- paste("get", strata_type, sep ="_") %>%
    get()

  args <- rlang::fn_fmls_syms(fn_call)
  args$name <- name
  args$op <- kk$op
  args$value <- value

  call <- rlang::call2(paste("get", strata_type, sep ="_"),
                     !!!args)

  ageStrata <- list(name = name,
                    op = kk$op,
                    value = value,
                    call = call)
  structure(ageStrata, class = c("strata", strata_type))
}

#' Function that creates an age strata
#' @param connectionDetails the connection Details object to connect to the database
#' @param resultsDatabaseSchema the results schema holding the cohort table
#' @param cohortTable the cohort table holding the cohort
#' @param targetCohortId the cohort Id used for the strata
#' @param name the name of the strata
#' @param op the operator that defines the inequality for the age
#' @param value the value boundary
#' @include helpers.R
#' @importFrom rlang !!
#' @return a dataframe representing the strata for the cohort
#' @export
get_age_strata <- function(connectionDetails,
                           resultsDatabaseSchema,
                           cohortTable,
                           targetCohortId,
                           name,
                           op,
                           value) {

  #get target cohort
  cohort <- getTargetCohort(connectionDetails = connectionDetails,
                            resultsDatabaseSchema = resultsDatabaseSchema,
                            cohortTable = cohortTable,
                            targetCohortId = targetCohortId)

  #check operator
  kk <- checkOp(op)

  #create symbols for dplyr
  strata_name_full <- paste("strata", name, sep ="_")
  strata_name_sym <- rlang::sym(strata_name_full)
  startDate <- grep("start", names(cohort), value = TRUE, ignore.case = TRUE) %>%
    rlang::sym()
  rowId <- grep("subject", names(cohort), value = TRUE, ignore.case = TRUE) %>%
    rlang::sym()

  #define logic test
  logicTest <- rlang::call2(kk$op, rlang::sym("age"), value)


  #get dob
  personIds <- getUniquePersons(cohort)

  dob <- getDob(connectionDetails = connectionDetails,
                cdmDatabaseSchema = cdmDatabaseSchema,
                personIds = personIds)
  #get strata data
  strata_df <- cohort %>%
    left_join(dob, by = c("subjectId" = "personId")) %>%
    dplyr::mutate(
      age = floor(lubridate::time_length(difftime(!!startDate, dob), "years"))
    ) %>%
    dplyr::mutate(!! strata_name_sym := dplyr::if_else(eval(logicTest), 1L, 0L, 0L)) %>%
    dplyr::select(!!rowId, !!strata_name_sym)

  return(strata_df)
}

#' Function that creates a date strata
#' @param connectionDetails the connection Details object to connect to the database
#' @param resultsDatabaseSchema the results schema holding the cohort table
#' @param cohortTable the cohort table holding the cohort
#' @param targetCohortId the cohort Id used for the strata
#' @param name the name of the strata
#' @param op the operator that defines the inequality for the age
#' @param date the date value boundary
#' @include helpers.R
#' @importFrom rlang !!
#' @return a dataframe representing the strata for the cohort
#' @export
get_date_strata <- function(connectionDetails,
                            resultsDatabaseSchema,
                            cohortTable,
                            targetCohortId,
                            name,
                            op,
                            date) {

  cohort <- getTargetCohort(connectionDetails = connectionDetails,
                            resultsDatabaseSchema = resultsDatabaseSchema,
                            cohortTable = cohortTable,
                            targetCohortId = targetCohortId)
  #check operator
  kk <- checkOp(op)

  #create symbols for dplyr
  strata_name_full <- paste("strata", name, sep ="_")
  strata_name_sym <- rlang::sym(strata_name_full)
  startDate <- grep("start", names(cohort), value = TRUE, ignore.case = TRUE) %>%
    rlang::sym()
  rowId <- grep("subject", names(cohort), value = TRUE, ignore.case = TRUE) %>%
    rlang::sym()

  #define logic test
  logicTest <- rlang::call2(op, startDate, date)


  #get strata data
  strata_df <- cohort %>%
    dplyr::mutate(!! strata_name_sym := dplyr::if_else(eval(logicTest), 1L, 0L, 0L)) %>%
    dplyr::select(!!rowId, !!strata_name_sym)

  return(strata_df)

}

#' Function that creates a date strata
#' @param connectionDetails the connection Details object to connect to the database
#' @param cdmDatabaseSchema the schema that holds the cdm
#' @param resultsDatabaseSchema the results schema holding the cohort table
#' @param cohortTable the cohort table holding the cohort
#' @param targetCohortId the cohort Id used for the strata
#' @param name the name of the strata
#' @param domain the cdm table to search for a concept
#' @param element the element in a table to search for a concept
#' @param concept the concept Id to search in the cdm
#' @include helpers.R
#' @importFrom rlang !!
#' @return a dataframe representing the strata for the cohort
#' @export
get_concept_strata <- function(connectionDetails,
                                  cdmDatabaseSchema,
                                  resultsDatabaseSchema,
                                  vocabularyDatabaseSchema,
                                  cohortTable,
                                  targetCohortId,
                                  name,
                                  domain,
                                  element,
                                  concept) {

  cohort <- getTargetCohort(connectionDetails = connectionDetails,
                            resultsDatabaseSchema = resultsDatabaseSchema,
                            cohortTable = cohortTable,
                            targetCohortId = targetCohortId)

  strata <- cohort %>%
    getUniquePersons() %>%
    getCdmElement(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                  cdmTable = domain,
                  cdmTableElement = element,
                  cdmTableValue = concept,
                  personIds = .) %>%
    tibble::tibble()

  #create symbols for dplyr
  strata_name_full <- paste("strata", name, sep ="_")
  strata_name_sym <- rlang::sym(strata_name_full)
  rowId <- grep("subject", names(cohort), value = TRUE, ignore.case = TRUE) %>%
    rlang::sym()
  #define logic test
  logicTest <- rlang::call2("==", rlang::sym("conceptId"), concept)

  #get strata data
  strata_df <- cohort %>%
    dplyr::left_join(strata, by = c("subjectId" = "personId")) %>%
    dplyr::mutate(!! strata_name_sym := dplyr::if_else(eval(logicTest), 1L, 0L, 0L)) %>%
    dplyr::select(!!rowId, !!strata_name_sym)

  return(strata_df)
}

#' Function that builds the strata
#' @param strata the strata dataframe object
#' @param cohortId a cohort Id to define which cohort is used for the strata
#' @return a dataframe representing the strata for the cohort
#' @export
build_strata <- function(strata,
                         cohortId = NULL) {
  if (!is.null(cohortId)) {
    strata$call$targetCohortId <- as.integer(cohortId)
  }

  return(eval(strata$call))
}

#' Function to expand the strata interaction
#'
#' This function creates all combinations for the strata
#'
#' @param ... multiple strata objects
#' @param type either combine or expand. Expand option looks at iteractions
#' @return a tibble with all combinations of the strata for the unique persons in the cohort
#' @export
bind_strata <- function(..., type = c("combine","expand")) {

  strata_list <- list(...)
  dd <- do.call('cbind', strata_list)
  dd <- dd[ ,!duplicated(names(dd))]


  type <- switch(type,
                 combine = "+",
                 expand = "*")

  subjectId <- dd$subjectId

  ff <- as.formula(paste0("~",
                          dd %>%
                            dplyr::select(tidyr::starts_with("strata")) %>%
                            names() %>%
                            paste0(collapse = type)))

  gg <- dd %>%
    modelr::model_matrix(ff) %>%
    dplyr::rename(strata_total = `(Intercept)`) %>%
    dplyr::rename_with(~gsub(":", "+", .x, fixed = TRUE)) %>%
    dplyr::mutate(dplyr::across(tidyr::starts_with("strata"), as.integer)) %>%
    tibble::add_column(subjectId, .before = 1)

  return(gg)

}
