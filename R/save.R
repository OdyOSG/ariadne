#' Function to save the survival_analysis as an rds object
#'
#' @param survival_analysis a survival_analysis object with information about the treatment patterns
#' @param output_folder an output location to save the files
#' @import usethis
#' @export
save_survival_analysis <- function(survival_analysis,
                                output_folder) {

  analysis_settings <- survival_analysis$analysis_settings
  strata_nm <- survival_analysis$strata_name
  nm <- analysis_settings$targetCohortName
  path <- file.path(output_folder, "survival_analysis")
  filePath <- file.path(path, paste0("survival_analysis_", nm, strata_nm,".rds"))
  #save treatment patterns to treatment patterns folder
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  #save treatment_pathways
  readr::write_rds(survival_analysis, file = filePath)
  usethis::ui_info(
    "survival_analysis rds saved to: {ui_path(filePath)}"
  )

}

#' Function to save the treatment patterns as an rds object
#'
#' @param treatment_patterns a treatment_patterns object with information about the treatment patterns
#' @param output_folder an output location to save the files
#' @import usethis
#' @export
save_treatment_patterns <- function(treatment_patterns,
                                    output_folder) {

  analysis_settings <- treatment_patterns$analysis_settings
  nm <- analysis_settings$targetCohortName
  path <- file.path(output_folder, "treatment_patterns")
  filePath <- file.path(path, paste0("treatment_patterns_", nm,".rds"))
  #save treatment patterns to treatment patterns folder
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  #save treatment_pathways
  readr::write_rds(treatment_patterns, file = filePath)
  usethis::ui_info(
    "treatment_patterns rds saved to: {ui_path(filePath)}"
  )

}

#' Function to load aggregated ariadne covariate object from file
#'
#' @param ariadne an aggregated ariadne covariate object with arrow files of information
#' @export
load_ariadne <- function(ariadne) {

  strata_sym <- rlang::sym(ariadne$strata)

  ariadne_schema <- arrow::schema(
    conceptName = arrow::string(),
    conceptId = arrow::int64(),
    categoryName = arrow::string(),
    categoryId = arrow::int64(),
    timeId = arrow::int16(),
    analysisId = arrow::int16(),
    strata = arrow::int16(),
    nn = arrow::int64(),
    pct = arrow::float64()
  )

  gg <- arrow::open_dataset(ariadne$dbDirectory,
                      format = ariadne$dbFormat,
                      partitioning = c("analysis_id", "time_id"),
                      schema = ariadne_schema,
                      skip = 1) %>%
    rename(!!strata_sym := strata)
  return(gg)
}
