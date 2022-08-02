#' A function that defines the covariates for ariadne
#'
#' @param temporalStartDays an integer vector of days relative to index depicting the start window.
#' Positive number corresponds to days after index and a negative corresponds
#' to days before index
#' @param temporalEndDays an integer vector of days relative to index depicting the end window.
#' Positive number corresponds to days after index and a negative corresponds
#' to days before index
#' @return a ariadne covariates object that defines the covariates to extract
#' @export
define_covariates <- function(temporalStartDays,
                              temporalEndDays) {



  #define temporal covariates -----------------------
  covariateSettings <- FeatureExtraction::createTemporalCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAgeGroup = TRUE,
    useConditionEraGroupOverlap = TRUE,
    useDrugEraGroupOverlap = TRUE,
    temporalStartDays = temporalStartDays,
    temporalEndDays = temporalEndDays,
    excludedCovariateConceptIds = c(21600001, 21600959, 21601237,
                                    21601907, 21602359, 21602681,
                                    21602795, 21601386, 21603931,
                                    21604180, 21604847, 21605007,
                                    21603550, 21605212) #remove ATC 1st class
  )



  args <- rlang::fn_fmls(FeatureExtraction::getDbCovariateData)
  args$connectionDetails <- rlang::sym("connectionDetails")
  args$cdmDatabaseSchema <- rlang::sym("cdmDatabaseSchema")
  args$cohortTable <- rlang::sym("cohortTable")
  args$cohortDatabaseSchema <- rlang::sym("resultsDatabaseSchema")
  args$cohortId <- rlang::sym("targetCohortId")
  args$covariateSettings <- covariateSettings

  call <- rlang::call2("%>%",
    rlang::call2("getDbCovariateData", !!!args, .ns = "FeatureExtraction"),
    rlang::call2("convert_covariates_arrow"))


  ariadne_covariates <- structure(list(
    temporalStartDays = temporalStartDays,
    temporalEndDays = temporalEndDays,
    covariateSettings = covariateSettings,
    call = call), class = "ariadne_covariates_definition")

  return(ariadne_covariates)

}

convert_covariates_arrow <- function(covDat) {
  sqlite <- RSQLite::SQLite()
  con <- DBI::dbConnect(sqlite, covDat@dbname)

  covInfo <- dplyr::tbl(con, "covariates") %>%
    dplyr::left_join(dplyr::tbl(con, "covariateRef"), by = c("covariateId")) %>%
    dplyr::left_join(dplyr::tbl(con, "analysisRef"), by = c("analysisId")) %>%
    dplyr::select(.data$rowId, .data$covariateId,
                  .data$covariateName, .data$covariateValue,
                  .data$analysisId, .data$timeId) %>%
    tidyr::replace_na(list(timeId = 999)) %>%
    dplyr::collect() %>%
    dplyr::mutate(covariateId = as.character(covariateId),
                  analysisId = as.integer(analysisId),
                  timeId = as.integer(timeId))

  datMeta <- structure(list(
    dbDirectory = tempfile(),
    analysisRef = dplyr::tbl(con, "analysisRef") %>% dplyr::collect(),
    timeRef = dplyr::tbl(con, "timeRef") %>% dplyr::collect()
  ), class = "ariadne_covariates")

  arrow::write_feather(covInfo, datMeta$dbDirectory)


  return(datMeta)
}


#' Function that builds covariates
#'
#' @param covariates an ariadne_covariates class object
#' @param cohortId an integer that defines which cohort to build covariates
#' @return an ariadne_covariates object
#' @export
build_covariates <- function(covariates,
                             cohortId) {
  covariates$call[[2]]$cohortId <- as.integer(cohortId)
  eval(covariates$call)
}

#' Function that aggregates the covariates in the ariadne object
#'
#' This function takes the ariadne object to aggregate and joins with
#' condition and drug rollups of the covariates
#'
#' @param ariadne an ariadne reference class with a feather file containing the data
#' @param strata_df a dataframe with the strata. This parameter can be null if so
#' it will find the unique subjects from the cohort and make a total strata
#' @param connectionDetails a set of connectionDetails
#' @param cdmDatabaseSchema the schema of the cdm
#' @param outputFolder a folder where the aggregated covariates are stored
#' @return an aggregated_ariadne_covariates class object
#' @include helpers.R utils.R
#' @importFrom rlang !! !!!
#' @export
aggregate_ariadne <- function(ariadne,
                                   strata = NULL,
                                   connectionDetails,
                                   cdmDatabaseSchema,
                                   outputFolder) {

  #Step 1: Setup strata ---------------------------

  if(is.null(strata)) {
    #get target cohort
    strata <- arrow::read_feather(ariadne$dbDirectory) %>%
      dplyr::distinct(.data$rowId) %>%
      dplyr::rename(subjectId = rowId) %>%
      dplyr::mutate(strata_total = 1L)
  }

  # create strata symbol
  strata_sym <- strata %>%
    dplyr::select(tidyr::starts_with("strata")) %>%
    names() %>%
    rlang::sym()

  #find counts per strata
  strata_count <- strata %>%
    dplyr::group_by(!!strata_sym) %>%
    dplyr::count(name = "tot")

  #Step 2: aggregate by covariate by strata by timeId--------------------
  dat <- arrow::read_feather(ariadne$dbDirectory) %>%
    dplyr::left_join(strata,
              by = c("rowId" = "subjectId")) %>%
    dplyr::group_by(timeId, analysisId, covariateId, !!strata_sym) %>%
    dplyr::summarize(nn = sum(covariateValue)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(conceptId = covariateId_to_conceptId(covariateId)) %>%
    dplyr::left_join(strata_count, by = c(rlang::as_string(strata_sym))) %>%
    dplyr::mutate(pct = nn / tot)

  #Step 3: Join on rollups------------------

  #find condition rollup
  condition_concepts <- dat %>%
    dplyr::filter(.data$analysisId == 203) %>%
    dplyr::distinct(.data$conceptId) %>%
    dplyr::pull() %>%
    rollupConditions(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    conceptIds = .
  )

  #find drug rollup
  drug_concepts <- dat %>%
    dplyr::filter(.data$analysisId == 403) %>%
    dplyr::distinct(.data$conceptId) %>%
    dplyr::pull() %>%
    rollupDrugs(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      conceptIds = .
    )

  # get rollup
  dat <- dat %>%
    left_join(condition_concepts, by = c("conceptId")) %>%
    left_join(drug_concepts, by = c("conceptId"),
              suffix = c("_condition", "_drug"))

  #Step 4: format table ------------------
  dat <- dat %>%
  dplyr::mutate(
      conceptId = as.integer(conceptId),
      conceptName = dplyr::coalesce(conceptName_condition, conceptName_drug),
      categoryName = dplyr::coalesce(categoryName_condition, categoryName_drug),
      categoryId = as.integer(dplyr::coalesce(categoryId_condition, categoryId_drug)),
      .keep = "unused"
    ) %>%
    formatDemographics() %>%
    dplyr::select(.data$conceptName, .data$conceptId, .data$categoryName,
                  .data$categoryId, .data$timeId, .data$analysisId,
                  !!strata_sym, .data$nn, .data$pct)

  dir.create(file.path(outputFolder, rlang::as_string(strata_sym)))

  arrow::write_dataset(dataset = dat,
                       path = file.path(outputFolder, rlang::as_string(strata_sym)),
                       format = "csv",
                       partitioning = c("analysisId", "timeId"))


  arrow_meta <- structure(list(
    dbDirectory = file.path(outputFolder, rlang::as_string(strata_sym)),
    dbFormat = "csv",
    analysisRef = ariadne$analysisRef,
    timeRef = ariadne$timeRef,
    strata = rlang::as_string(strata_sym)
  ), class = "aggregated_ariadne_covariates")

  return(arrow_meta)

}

#' The UI for aggregating ariadne covariates
#'
#' @param ariadne an ariadne reference class with a feather file containing the data
#' @param strata_df a dataframe with the strata. This parameter can be null if so
#' it will find the unique subjects from the cohort and make a total strata
#' @param output_folder a folder where the aggregated covariates are stored
#' @return an aggregated ariadne covariates class object
#' @export
aggregate_covariates <- function(ariadne,
                                 strata = NULL,
                                 output_folder) {

  call <- rlang::call2("aggregate_ariadne",
               ariadne = ariadne,
               strata = strata,
               connectionDetails = rlang::sym("connectionDetails"),
               cdmDatabaseSchema = rlang::sym("cdmDatabaseSchema"),
               outputFolder = output_folder)

  return(eval(call))

}
