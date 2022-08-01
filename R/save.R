#' Function to save the treatment history as csv
#'
#' @param treatment_history the dataframe output from create_treatment_history
#' @param analysisSettings a DrugUtilizationAnalysisSettings object that defines the elements of the analysis
#' @import usethis
#' @export
save_treatment_history <- function(treatment_history,
                                   analysisSettings) {

  treatmentPatternsFolder <- analysisSettings$outputFolder
  database <- analysisSettings$database

  #save treatment history to treatment patterns folder
  if(!dir.exists(file.path(treatmentPatternsFolder, database))) {
    dir.create(file.path(treatmentPatternsFolder, database), recursive = TRUE)
  }
  readr::write_csv(treatment_history,
                   file = file.path(treatmentPatternsFolder, database,"treatmentHistory.csv"))
  usethis::ui_info(
    "data.frame of treatment history saved to: {ui_path(file.path(treatmentPatternsFolder, database, \"treatmentHistory.csv\"))}"
  )
}

# TODO change the save settings to split the outcomes by analysis
#' Function to save the survival analysis as multiple csv
#'
#' @param survival_table a SurvivalAnalysis object with information about the survival analysis
#' @param analysisSettings a DrugUtilizationAnalysisSettings object that defines the elements of the analysis
#' @include utils.R
#' @import usethis
#' @export
save_survival_table <- function(survival_table,
                                analysisSettings) {

  #extract relevant tables
  survTab <- survival_table$survTab
  surv_summary <- survival_table$survFit
  surv_curves <- survival_table$survInfo


  treatmentPatternsFolder <- analysisSettings$outputFolder
  database <- analysisSettings$database

  #save treatment patterns to treatment patterns folder
  if(!dir.exists(file.path(treatmentPatternsFolder, database))) {
    dir.create(file.path(treatmentPatternsFolder, database), recursive = TRUE)
  }
  #save survival data
  readr::write_csv(survTab,
                   file = file.path(treatmentPatternsFolder, database,"survivalData.csv"))
  usethis::ui_info(
    "data.frame of survival data saved to: {ui_path(file.path(treatmentPatternsFolder, database, \"survivalData.csv\"))}"
  )

  #save surv_summary
  readr::write_csv(surv_summary,
                   file = file.path(treatmentPatternsFolder, database,"survivalSummary.csv"))
  usethis::ui_info(
    "data.frame of survival summary saved to: {ui_path(file.path(treatmentPatternsFolder, database, \"survivalSummary.csv\"))}"
  )


  #save surv_curves
  readr::write_csv(surv_curves,
                   file = file.path(treatmentPatternsFolder, database,"survivalCurves.csv"))
  usethis::ui_info(
    "data.frame of survival curves saved to: {ui_path(file.path(treatmentPatternsFolder, database, \"survivalCurves.csv\"))}"
  )

}

#' Function to save the treatment patterns as multiple csv
#'
#' @param pathway_analysis a PathwayAnalysis object with information about the treatment patterns
#' @param analysisSettings a DrugUtilizationAnalysisSettings object that defines the elements of the analysis
#' @import usethis
#' @export
save_treatment_patterns <- function(treatment_patterns,
                                    analysisSettings) {

  treatment_pathways <- treatment_patterns$treatmentPathways
  pathway_attrition <- treatment_patterns$attrition

  treatmentPatternsFolder <- analysisSettings$outputFolder
  database <- analysisSettings$database

  #save treatment patterns to treatment patterns folder
  if(!dir.exists(file.path(treatmentPatternsFolder, database))) {
    dir.create(file.path(treatmentPatternsFolder, database), recursive = TRUE)
  }
  #save treatment_pathways
  readr::write_csv(treatment_pathways,
                   file = file.path(treatmentPatternsFolder, database,"treatmentPathways.csv"))
  usethis::ui_info(
    "data.frame of treatment pathways saved to: {ui_path(file.path(treatmentPatternsFolder, database, \"treatmentPathways.csv\"))}"
  )

  #save pathway_attrition
  readr::write_csv(pathway_attrition,
                   file = file.path(treatmentPatternsFolder, database,"pathwayAttrition.csv"))
  usethis::ui_info(
    "data.frame of pathway attrition saved to: {ui_path(file.path(treatmentPatternsFolder, database, \"pathwayAttrition.csv\"))}"
  )

}
