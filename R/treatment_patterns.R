# Modified version of addPathwaySettings from TreatmentPatterns

#' Function to create analysis settings for drug utilization
#'
#' This function adopts ideas from TreatmentPatterns::addPathwayAnalysis and
#' builds the settings needed for a drug utilization study.
#' @param targetCohortId the Id of the target cohort
#' @param targetCohortName the name of the target cohort
#' @param eventCohortIds the ids of the event cohorts
#' @param eventCohortNames the names of the event cohorts
#' @param includeTreatments Include treatments starting ('startDate') or ending ('endDate') after target cohort start date
#' @param periodPriorToIndex 	Number of days prior to the index date of the target cohort that event cohorts are allowed to start
#' @param minEraDuration Minimum time an event era should last to be included in analysis
#' @param eraCollapseSize Window of time between which two eras of the same event cohort are collapsed into one era
#' @param combinationWindow 	Window of time two event cohorts need to overlap to be considered a combination treatment
#' @param minPostCombinationDuration 	Minimum time an event era before or after a generated combination treatment should last to be included in analysis
#' @param filterTreatments 	Select first occurrence of ("First") / changes between ("Changes') / all event cohorts ("All")
#' @param maxPathLength Maximum number of steps included in treatment pathway (max 5)
#' @param minCellCount Minimum number of persons with a specific treatment pathway for the pathway to be included in analysis
#' @param minCellMethod Select to completely remove / sequentially adjust (by removing last step as often as necessary) treatment pathways below minCellCount
#' @param groupCombinations Select to group all non-fixed combinations in one category 'other’ in the sunburst plot
#' @param addNoPaths Select to include untreated persons without treatment pathway in the sunburst plot
#' @param strata a strata object of stratification settings
#' @return a DrugUtilizationAnalysisSettings object defining the treatment analysis
#' @export
define_treatment_history <- function(targetCohortId,
                                     targetCohortName = NULL,
                                     eventCohortIds,
                                     eventCohortNames = NULL,
                                     includeTreatments = "startDate",
                                     periodPriorToIndex = 0,
                                     minEraDuration = 0,
                                     eraCollapseSize = 30,
                                     combinationWindow = 30,
                                     minPostCombinationDuration = 30,
                                     filterTreatments = "First",
                                     maxPathLength = 5,
                                     minCellCount = 5,
                                     minCellMethod = "Remove",
                                     groupCombinations = 10,
                                     addNoPaths = FALSE,
                                     strata = NULL) {

  #argument checks
  # checkmate::assert_int(targetCohortId)
  # checkmate::assert_integer(eventCohortIds)
  #
  if (is.null(eventCohortNames)) {
    eventCohortNames <- paste("Event_Cohort", eventCohortIds, sep = "_")
  }

  if (is.null(targetCohortName)) {
    targetCohortName <- paste("Target Cohort", targetCohortId, sep = "_")
  }
  # checkmate::assert_set_equal(length(eventCohortIds), length(eventCohortNames))
  checkmate::assert_choice(includeTreatments, c("startDate", "endDate"))
  checkmate::assert_choice(filterTreatments, c("First", "Changes", "All"))


  #create settings
  settings <- structure(list(
    targetCohortId = targetCohortId,
    targetCohortName = targetCohortName,
    eventCohortIds = eventCohortIds,
    eventCohortNames = eventCohortNames,
    includeTreatments = includeTreatments,
    periodPriorToIndex = periodPriorToIndex,
    minEraDuration = minEraDuration,
    eraCollapseSize = eraCollapseSize,
    combinationWindow = combinationWindow,
    minPostCombinationDuration = minPostCombinationDuration,
    filterTreatments = filterTreatments,
    maxPathLength = maxPathLength,
    minCellCount = minCellCount,
    minCellMethod = minCellMethod,
    groupCombinations = groupCombinations,
    addNoPaths = addNoPaths), class = "ariadne_treatment_history_settings")

  return(settings)
}

#' Function to build survival table
#' @param analysisSettings an ariadne_treatment_history_settings object
#' @export
build_treatment_history <- function(analysisSettings) {
  #create treatment history call
  args <- rlang::fn_fmls_syms(treatment_history)
  args$analysisSettings <- analysisSettings
  call <- rlang::call2("treatment_history",
                       !!!args)
  return(eval(call))
}


#' Function that creates the treatment history
#'
#' This function adapts the underlying calculations for treatment patterns from TreatmentPatterns package
#'
#' @param connectionDetails a list of connectionDetails for DatabaseConnector
#' @param cdmDatabaseSchema the schema that hosts the cdm tables, defaults to value
#' in config.yml of the active configuration
#' @param resultsDatabaseSchema the schema that hosts the users writeable results tables (or scratch)
#' @param cohortTable the name where the cohorts are stored in the results schema
#' @param analysisSettings an object that defines the elements of the analysis
#' @include utils.R strata.R
#' @importFrom data.table :=
#' @return a data.table with the treatment history
#' @export
treatment_history <- function(connectionDetails,
                                     cdmDatabaseSchema,
                                     resultsDatabaseSchema,
                                     cohortTable,
                                     analysisSettings) {

  checkmate::assert_class(analysisSettings, "ariadne_treatment_history_settings")
  # checkmate::assert_list(generatedCohorts)

  suppressMessages(connection <- DatabaseConnector::connect(connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  # cohortId <- purrr::map_dbl(generatedCohorts, ~.x$cohort_id) %>%
  #   as.integer()
  targetCohortId <- analysisSettings$targetCohortId
  eventCohortIds <- analysisSettings$eventCohortIds

  # targetGeneratedCohort <- generatedCohorts[[targetId]]
  # eventGeneratedCohorts <- generatedCohorts[eventCohortIds]

  #database schemas
  # resultsDatabaseSchema <- targetGeneratedCohort$cohortTableRef$cohortDatabaseSchema
  # cohortTable <- targetGeneratedCohort$cohortTableRef$cohortTableNames$cohortTable
  #set parameters

  #extract cohorts
  sql <-"WITH CTE_person AS(
          SELECT SUBJECT_ID FROM @resultsDatabaseSchema.@cohortTable WHERE COHORT_DEFINITION_ID = @targetCohortId
          )
         SELECT a.* FROM @resultsDatabaseSchema.@cohortTable a
         INNER JOIN CTE_person b ON a.SUBJECT_ID = b.SUBJECT_ID;"

  current_cohorts <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    resultsDatabaseSchema = resultsDatabaseSchema,
    cohortTable = cohortTable,
    targetCohortId = targetCohortId
  ) %>%
    data.table::as.data.table()
  colnames(current_cohorts) <- c("cohort_id", "person_id", "start_date", "end_date")

  #create treatment history using TreatmentPatterns package

  th <- quiet_th(current_cohorts = current_cohorts,
                 targetCohortId = targetCohortId,
                 eventCohortIds = eventCohortIds,
                 periodPriorToIndex = analysisSettings$periodPriorToIndex,
                 includeTreatments = analysisSettings$includeTreatments,
                 minEraDuration = analysisSettings$minEraDuration,
                 eraCollapseSize = analysisSettings$eraCollapseSize,
                 combinationWindow = analysisSettings$combinationWindow,
                 minPostCombinationDuration = analysisSettings$minPostCombinationDuration,
                 filterTreatments = analysisSettings$filterTreatments)

  log <- str_split(th$output, "\n")[[1]]
  th <- th$result

  # Add event_seq number to determine order of treatments in pathway
  th <- th[order(person_id, event_start_date, event_end_date),]
  th[, event_seq:=seq_len(.N), by= .(person_id)]

  quiet_doMaxPathLength <- purrr::quietly(doMaxPathLength)

  th <- quiet_doMaxPathLength(th, analysisSettings$maxPathLength)
  log <- c(log, str_split(th$output, "\n")[[1]])
  th <- th$result %>%
    addLabels(eventCohortIds, analysisSettings$eventCohortNames)
    # Add event_cohort_name (instead of only event_cohort_id)

  #some clean up for the combination names
  combi <- grep("+", th$event_cohort_name, fixed=TRUE)
  cohort_names <- strsplit(th$event_cohort_name[combi], split="+", fixed=TRUE)
  th$event_cohort_name[combi] <- sapply(cohort_names, function(x) paste(sort(x), collapse = "+"))
  th$event_cohort_name <- unlist(th$event_cohort_name)
  th$duration_era <- as.integer(th$duration_era)

  #add strata
  # if (!is.null(analysisSettings$strata)) {
  # th <- th %>%
  #   dplyr::left_join(analysisSettings$strata,
  #                    by = c("person_id" = "subjectId"))
  # }

#
#   th_Meta <- structure(list(
#     th_directory = tempfile(),
#     th_log = log,
#     analysis_settings = analysisSettings
#   ), class = "ariadne_treatment_history")
#
#   arrow::write_feather(th, th_Meta$th_directory)

  return(th)
}

#' Function to build treatment patterns
#' @param treatment_history an ariadne_treatment_history object
#' @export
build_treatment_patterns <- function(treatment_history) {
  #create treatment history call
  args <- rlang::fn_fmls_syms(treatment_patterns)
  args$treatment_history <- treatment_history
  call <- rlang::call2("treatment_patterns",
                       !!!args)
  return(eval(call))
}

#' Function to construct the treatment patterns
#'
#' @param treatment_history the dataframe output from treatment_history
#' @return a PathwayAnalysis object with the treatment patterns and attrition
#' @include utils.R
#' @export
treatment_patterns <- function(treatment_history) {

  checkmate::assert_class(treatment_history, "ariadne_treatment_history")

  minCellCount <- treatment_history$analysis_settings$minCellCount

  th <- arrow::read_feather(treatment_history$th_directory)

  tp <- th %>%
    tidyr::pivot_wider(id_cols = person_id,
                       names_from = event_seq,
                       names_prefix = "event_cohort_name",
                       values_from = event_cohort_name)
  #get number of individuals
  numPersons <- nrow(tp)

  #
  tp <- tp %>%
    dplyr::count(dplyr::across(tidyselect::starts_with("event_cohort_name"))) %>%
    dplyr::mutate(End = "end", .before = "n")


  numPathways <- nrow(tp)

  tp <- tp %>%
    dplyr::filter(n >= minCellCount)

  numPersons2 <- sum(tp$n)
  numPathways2 <- nrow(tp)


  df <- data.frame(
    'TotalNumberOfPersons' = numPersons,
    'TotalNumberOfPathways' = numPathways,
    'minCellCount' = minCellCount,
    'FilteredNumberOfPersons' = numPersons2,
    'FilteredNumberOfPathways' = numPathways2
  )


  pathway_analysis <- structure(
    list(
      treatmentPathways = tp,
      attrition = df,
      th_log = treatment_history$th_log,
      analysis_settings = treatment_history$analysis_settings
    ),
    class = "ariadne_treatment_pathway")

  return(pathway_analysis)

}

#' Function to build survival table
#' @param treatment_history an ariadne_treatment_history object
#' @export
build_survival_table <- function(treatment_history, strata) {
  #create treatment history call
  args <- rlang::fn_fmls_syms(create_survival_table)
  args$treatment_history <- treatment_history
  args$strata <- strata
  call <- rlang::call2("create_survival_table",
                       !!!args)
  return(eval(call))
}

#' Function that creates survival tables from the treatment history
#'
#' @param treatment_history the dataframe output from create_treatment_history
#' @param connectionDetails a list of connectionDetails for DatabaseConnector
#' @param resultsDatabaseSchema the schema that hosts the users writeable results tables (or scratch)
#' @param cohortTable the name where the cohorts are stored in the results schema
#' @param strata a strata object to stratify the survival analysis
#' @include utils.R helpers.R
#' @return a SurvivalAnalysis object containing the data for the survival
#' analysis from survminer
#' @export
create_survival_table <- function(treatment_history,
                                  connectionDetails,
                                  resultsDatabaseSchema,
                                  cohortTable,
                                  strata) {

  # resultsDatabaseSchema <- generatedTargetCohort$cohortTableRef$cohortDatabaseSchema
  # cohortTable <- generatedTargetCohort$cohortTableRef$cohortTableNames$cohortTable

  targetCohortId <- treatment_history$analysis_settings$targetCohortId
  minCellCount <- treatment_history$analysis_settings$minCellCount

  #extract target cohort for censoring
  targetCohort <- getTargetCohort(connectionDetails = connectionDetails,
                                  resultsDatabaseSchema = resultsDatabaseSchema,
                                  cohortTable = cohortTable,
                                  targetCohortId = targetCohortId)



  if (is.null(strata)) {
    targetCohort <- targetCohort %>%
      dplyr::mutate(strata_total = 1L)

    strata_sym <- rlang::sym("strata_total")
  } else {

    targetCohort <- targetCohort %>%
      dplyr::left_join(strata, by = c("subjectId"))

    strata_sym <- strata %>%
      dplyr::select(tidyr::starts_with("strata")) %>%
      names() %>%
      rlang::sym()
  }



  #create survival table
  survTab <- arrow::read_feather(treatment_history$th_directory) %>%
    dplyr::left_join(targetCohort, by = c("person_id" = "subjectId")) %>%
    dplyr::mutate(event = ifelse(event_end_date < cohortEndDate, 1, 0)) %>%
    dplyr::select(event_cohort_id, duration_era, event, !!strata_sym) %>%
    tidyr::nest(data = !event_cohort_id) %>%
    dplyr::mutate(nn = purrr::map_int(data, ~nrow(.x))) %>%
    dplyr::filter(nn >= minCellCount) %>%
    dplyr::select(event_cohort_id, data) %>%
    dplyr::mutate(survFit = purrr::map(
      data, ~survival::survfit(
        survival::Surv(
          duration_era, event
        ) ~ !!strata_sym,
        data = .x))) %>%
    dplyr::mutate(survInfo = purrr::map2(
      survFit,
      data,
      ~survminer::surv_summary(.x, data = .y))) %>%
    dplyr::select(-.data$survFit)



  survival_table <- structure(
    list(strata_name = rlang::as_string(strata_sym),
         survival_analysis = survTab,
         th_log = treatment_history$th_log,
         analysis_settings = treatment_history$analysis_settings),
    class = "ariadne_survival_analysis")

  return(survival_table)

}


# Treatment history helper functions -----------------

th <- function(current_cohorts,
                     targetCohortId,
                     eventCohortIds,
                     periodPriorToIndex,
                     includeTreatments,
                     minEraDuration,
                     eraCollapseSize,
                     combinationWindow,
                     minPostCombinationDuration,
                     filterTreatments) {
  doCreateTreatmentHistory(current_cohorts,
                           targetCohortId,
                           eventCohortIds,
                           periodPriorToIndex,
                           includeTreatments) %>%
    doEraDuration(minEraDuration) %>%
    doEraCollapse(eraCollapseSize) %>%
    doCombinationWindow(combinationWindow, minPostCombinationDuration) %>%
    doFilterTreatments(filterTreatments)
}

quiet_th <- purrr::quietly(th)




#Functions from TreatmentPatterns ConstructPathways.R

doCreateTreatmentHistory <- function(current_cohorts, targetCohortId, eventCohortIds, periodPriorToIndex, includeTreatments) {

  # Add index year column based on start date target cohort
  targetCohort <- current_cohorts[current_cohorts$cohort_id %in% targetCohortId,,]
  targetCohort$index_year <- as.numeric(format(targetCohort$start_date, "%Y"))

  # Select event cohorts for target cohort and merge with start/end date and index year
  eventCohorts <- current_cohorts[current_cohorts$cohort_id %in% eventCohortIds,,]
  current_cohorts <- merge(x = eventCohorts, y = targetCohort, by = c("person_id"), all.x = TRUE, allow.cartesian = TRUE)

  # Only keep event cohorts starting (startDate) or ending (endDate) after target cohort start date
  if (includeTreatments == "startDate") {
    current_cohorts <- current_cohorts[current_cohorts$start_date.y - as.difftime(periodPriorToIndex, unit="days") <= current_cohorts$start_date.x & current_cohorts$start_date.x < current_cohorts$end_date.y,]
  } else if (includeTreatments == "endDate") {
    current_cohorts <- current_cohorts[current_cohorts$start_date.y - as.difftime(periodPriorToIndex, unit="days") <= current_cohorts$end_date.x & current_cohorts$start_date.x < current_cohorts$end_date.y,]
    current_cohorts$start_date.x <- pmax(current_cohorts$start_date.y - as.difftime(periodPriorToIndex, unit="days"), current_cohorts$start_date.x)
  } else {
    warning("includeTreatments input incorrect, return all event cohorts ('includeTreatments')")
    current_cohorts <- current_cohorts[current_cohorts$start_date.y - as.difftime(periodPriorToIndex, unit="days") <= current_cohorts$start_date.x & current_cohorts$start_date.x < current_cohorts$end_date.y,]
  }

  # Remove unnecessary columns
  current_cohorts <- current_cohorts[,c("person_id", "index_year", "cohort_id.x", "start_date.x", "end_date.x")]
  colnames(current_cohorts) <- c("person_id", "index_year", "event_cohort_id", "event_start_date", "event_end_date")

  # Calculate duration and gap same
  current_cohorts[,duration_era:=difftime(event_end_date, event_start_date, units = "days")]

  current_cohorts <- current_cohorts[order(event_start_date, event_end_date),]
  current_cohorts[,lag_variable:=data.table::shift(event_end_date, type = "lag"), by=c("person_id", "event_cohort_id")]
  current_cohorts[,gap_same:=difftime(event_start_date, lag_variable, units = "days"),]
  current_cohorts$lag_variable <- NULL

  return(current_cohorts)
}


doEraDuration <- function(treatment_history, minEraDuration) {
  treatment_history <- treatment_history[duration_era >= minEraDuration,]
  ParallelLogger::logInfo(paste0("After minEraDuration: ", nrow(treatment_history)))

  return(treatment_history)
}


doCombinationWindow <- function(treatment_history, combinationWindow, minPostCombinationDuration) {

  time1 <- Sys.time()

  treatment_history$event_cohort_id <- as.character(treatment_history$event_cohort_id)

  # Find which rows contain some overlap
  treatment_history <- selectRowsCombinationWindow(treatment_history)

  # While rows that need modification exist:
  iterations <- 1
  while(sum(treatment_history$SELECTED_ROWS)!=0) {

    # Which have gap previous shorter than combination window OR min(current duration era, previous duration era) -> add column switch
    treatment_history[SELECTED_ROWS == 1 & (-GAP_PREVIOUS < combinationWindow  & !(-GAP_PREVIOUS == duration_era | -GAP_PREVIOUS == data.table::shift(duration_era, type = "lag"))), switch:=1]

    # For rows selected not in column switch -> if treatment_history[r - 1, event_end_date] <= treatment_history[r, event_end_date] -> add column combination first received, first stopped
    treatment_history[SELECTED_ROWS == 1 & is.na(switch) & data.table::shift(event_end_date, type = "lag") <= event_end_date, combination_FRFS:=1]

    # For rows selected not in column switch -> if treatment_history[r - 1, event_end_date] > treatment_history[r, event_end_date] -> add column combination last received, first stopped
    treatment_history[SELECTED_ROWS == 1 & is.na(switch) & data.table::shift(event_end_date, type = "lag") > event_end_date, combination_LRFS:=1]

    ParallelLogger::logInfo(paste0("Iteration ", iterations, " modifying  ", sum(treatment_history$SELECTED_ROWS), " selected rows out of ", nrow(treatment_history), ": ", sum(!is.na(treatment_history$switch)) , " switches, ", sum(!is.na(treatment_history$combination_FRFS)), " combinations FRFS and ", sum(!is.na(treatment_history$combination_LRFS)), " combinations LRFS"))
    if (sum(!is.na(treatment_history$switch)) + sum(!is.na(treatment_history$combination_FRFS)) +  sum(!is.na(treatment_history$combination_LRFS)) != sum(treatment_history$SELECTED_ROWS)) {
      warning(paste0(sum(treatment_history$SELECTED_ROWS), ' does not equal total sum ', sum(!is.na(treatment_history$switch)) +  sum(!is.na(treatment_history$combination_FRFS)) +  sum(!is.na(treatment_history$combination_LRFS))))
    }

    # Do transformations for each of the three newly added columns
    # Construct helpers
    treatment_history[,event_start_date_next:=data.table::shift(event_start_date, type = "lead"),by=person_id]
    treatment_history[,event_end_date_previous:=data.table::shift(event_end_date, type = "lag"),by=person_id]
    treatment_history[,event_end_date_next:=data.table::shift(event_end_date, type = "lead"),by=person_id]
    treatment_history[,event_cohort_id_previous:=data.table::shift(event_cohort_id, type = "lag"),by=person_id]

    # Case: switch
    # Change end treatment_history of previous row -> no minPostCombinationDuration
    treatment_history[data.table::shift(switch, type = "lead")==1,event_end_date:=event_start_date_next]

    # Case: combination_FRFS
    # Add a new row with start date (r) and end date (r-1) as combination (copy current row + change end date + update concept id) -> no minPostCombinationDuration
    add_rows_FRFS <- treatment_history[combination_FRFS==1,]
    add_rows_FRFS[,event_end_date:=event_end_date_previous]
    add_rows_FRFS[,event_cohort_id:=paste0(event_cohort_id, "+", event_cohort_id_previous)]

    # Change end date of previous row -> check minPostCombinationDuration
    treatment_history[data.table::shift(combination_FRFS, type = "lead")==1,c("event_end_date","check_duration"):=list(event_start_date_next, 1)]

    # Change start date of current row -> check minPostCombinationDuration
    treatment_history[combination_FRFS==1,c("event_start_date", "check_duration"):=list(event_end_date_previous,1)]

    # Case: combination_LRFS
    # Change current row to combination -> no minPostCombinationDuration
    treatment_history[combination_LRFS==1,event_cohort_id:=paste0(event_cohort_id, "+", event_cohort_id_previous)]

    # Add a new row with end date (r) and end date (r-1) to split drug era (copy previous row + change end date) -> check minPostCombinationDuration
    add_rows_LRFS <- treatment_history[data.table::shift(combination_LRFS, type = "lead")==1,]
    add_rows_LRFS[,c("event_start_date", "check_duration"):=list(event_end_date_next,1)]

    # Change end date of previous row -> check minPostCombinationDuration
    treatment_history[data.table::shift(combination_LRFS, type = "lead")==1,c("event_end_date", "check_duration"):=list(event_start_date_next,1)]

    # Combine all rows and remove helper columns
    treatment_history <- rbind(treatment_history, add_rows_FRFS, fill=TRUE)
    treatment_history <- rbind(treatment_history, add_rows_LRFS)

    # Re-calculate duration_era
    treatment_history[,duration_era:=difftime(event_end_date, event_start_date, units = "days")]

    # Check duration drug eras before/after generated combination treatments
    treatment_history <- doStepDuration(treatment_history, minPostCombinationDuration)

    # Preparations for next iteration
    treatment_history <- treatment_history[,c("person_id", "index_year", "event_cohort_id", "event_start_date", "event_end_date", "duration_era")]
    treatment_history <- selectRowsCombinationWindow(treatment_history)
    iterations <- iterations + 1

    gc()
  }

  ParallelLogger::logInfo(paste0("After combinationWindow: ", nrow(treatment_history)))

  treatment_history[,GAP_PREVIOUS:=NULL]
  treatment_history[,SELECTED_ROWS:=NULL]

  time2 <- Sys.time()
  ParallelLogger::logInfo(paste0("Time needed to execute combination window ", difftime(time2, time1, units = "mins")))

  return(treatment_history)
}


selectRowsCombinationWindow <- function(treatment_history) {
  # Order treatment_history by person_id, event_start_date, event_end_date
  treatment_history <- treatment_history[order(person_id, event_start_date, event_end_date),]

  # Calculate gap with previous treatment
  treatment_history[,GAP_PREVIOUS:=difftime(event_start_date, data.table::shift(event_end_date, type = "lag"), units = "days"), by = person_id]
  treatment_history$GAP_PREVIOUS <- as.integer(treatment_history$GAP_PREVIOUS)

  # Find all rows with gap_previous < 0
  treatment_history[treatment_history$GAP_PREVIOUS < 0, ALL_ROWS:=which(treatment_history$GAP_PREVIOUS < 0)]

  # Select one row per iteration for each person
  rows <- treatment_history[!is.na(ALL_ROWS),head(.SD,1), by=person_id]$ALL_ROWS

  treatment_history[rows,SELECTED_ROWS:=1]
  treatment_history[!rows,SELECTED_ROWS:=0]
  treatment_history[,ALL_ROWS:=NULL]

  return(treatment_history)
}
doStepDuration <- function(treatment_history, minPostCombinationDuration) {
  treatment_history <- treatment_history[(is.na(check_duration) | duration_era >= minPostCombinationDuration),]
  ParallelLogger::logInfo(paste0("After minPostCombinationDuration: ", nrow(treatment_history)))

  return(treatment_history)
}

doEraCollapse <- function(treatment_history, eraCollapseSize) {
  # Order treatment_history by person_id, event_cohort_id, start_date, end_date
  treatment_history <- treatment_history[order(person_id, event_cohort_id,event_start_date, event_end_date),]

  # Find all rows with gap_same < eraCollapseSize
  rows <- which(treatment_history$gap_same < eraCollapseSize)

  # For all rows, modify the row preceding, loop backwards in case more than one collapse
  for (r in rev(rows)) {
    treatment_history[r - 1,"event_end_date"] <- treatment_history[r,event_end_date]
  }

  # Remove all rows with gap_same < eraCollapseSize
  treatment_history <- treatment_history[!rows,]
  treatment_history[,gap_same:=NULL]

  # Re-calculate duration_era
  treatment_history[,duration_era:=difftime(event_end_date , event_start_date, units = "days")]

  ParallelLogger::logInfo(paste0("After eraCollapseSize: ", nrow(treatment_history)))
  return(treatment_history)
}


doFilterTreatments <- function(treatment_history, filterTreatments) {

  # Order treatment_history by person_id, event_start_date, event_end_date
  treatment_history <- treatment_history[order(person_id, event_start_date, event_end_date),]

  if (filterTreatments == "All") {} # Do nothing
  else {
    # Order the combinations
    ParallelLogger::logInfo("Order the combinations.")
    combi <- grep("+", treatment_history$event_cohort_id, fixed=TRUE)
    if (length(combi) != 0) {
      concept_ids <- strsplit(treatment_history$event_cohort_id[combi], split="+", fixed=TRUE)
      treatment_history$event_cohort_id[combi] <- sapply(concept_ids, function(x) paste(sort(x), collapse = "+"))
    }

    if (filterTreatments == "First") {
      treatment_history <- treatment_history[, head(.SD,1), by=.(person_id, event_cohort_id)]

    } else if (filterTreatments == "Changes") {
      # Group all rows per person for which previous treatment is same
      tryCatch(treatment_history <- treatment_history[, group:=data.table::rleid(person_id,event_cohort_id)],
               error = function(e){print(paste0("Check if treatment_history contains sufficient records: ", e))})

      # Remove all rows with same sequential treatments
      treatment_history <- treatment_history[,.(event_start_date=min(event_start_date), event_end_date=max(event_end_date), duration_era=sum(duration_era)), by = .(person_id,index_year,event_cohort_id,group)]
      treatment_history[,group:=NULL]
    } else {
      warning("filterTreatments input incorrect, return all event cohorts ('All')")
    }
  }

  ParallelLogger::logInfo(paste0("After filterTreatments: ", nrow(treatment_history)))

  return(treatment_history)
}

doMaxPathLength <- function(treatment_history, maxPathLength) {

  # Apply maxPathLength
  treatment_history <- treatment_history[event_seq <= maxPathLength,]

  ParallelLogger::logInfo(paste0("After maxPathLength: ", nrow(treatment_history)))

  return(treatment_history)
}

addLabels <- function(treatment_history, eventCohortIds, eventCohortNames) {

  labels <- tibble(event_cohort_id = eventCohortIds,
                   event_cohort_name = eventCohortNames) %>%
    mutate(event_cohort_id = as.character(event_cohort_id))

  th <- treatment_history %>%
    dplyr::left_join(labels, by = c("event_cohort_id"))

  th$event_cohort_name[is.na(th$event_cohort_name)] <- sapply(th$event_cohort_id[is.na(th$event_cohort_name)], function(x) {

    # Revert search to look for longest concept_ids first
    for (l in nrow(labels):1)
    {
      # If treatment occurs twice in a combination (as monotherapy and as part of fixed-combination) -> remove monotherapy occurrence
      if (any(grep(labels$event_cohort_name[l], x))) {
        x <- gsub(labels$event_cohort_id[l], "", x)
      } else {
        x <- gsub(labels$event_cohort_id[l], labels$event_cohort_name[l], x)
      }
    }

    return(x)
  })


  # Filter out + at beginning/end or repetitions
  th$event_cohort_name <- gsub("\\++", "+", th$event_cohort_name)
  th$event_cohort_name <- gsub("^\\+", "", th$event_cohort_name)
  th$event_cohort_name <- gsub("\\+$", "", th$event_cohort_name)

  return(th)
}

