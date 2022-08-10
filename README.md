# ariadne

This is an experimental R package focusing on providing tools to aid OHDSI characterization studies. Thus far it has two areas of functionality: 1) providing tools to analyze covariates and 2) tools for assessing treatment patterns. 

## Covariates

Ariadne formats covariates into a setup that is more conducive to providing tables and figures to summarize information in comparison to the default FeatureExtraction tools. Ariadne creates dynamic and static summary tables for reports and apps. It also has two types of plots (manhattan and cleveland) to assess features in a characterization analysis.

## Treatment Patterns

Another common type of characterization study is to look at treatment patterns. How patients switch between different drugs and understand how they are cared for. Ariadne adapts functions from the TreatmentPatterns package to create a treatment history table. From this treatment history table, we can either create a pathway analysis or a time to event analysis. For the pathway analysis, Ariadne provides summary information about the kinds of treatment pathways patients in the cohort experience and depicts this visually using a sankey plot. For the time to event analysis, Ariadne helps calculate the time to a treatment discontinuation and creates a kaplan meier plot. 

## Stratifications

Ariadne provides functionality to improve stratification analysis in OHDSI studies. It takes the unique patients in a cohort and builds strata in three ways: 1) cohort start date, 2) age, and 3) based on presence of a concept. We provide three examples of how these three strata may be defined: 1) the person enters the cohort before or during the covid-19 pandemic, 2) the person is age greater than or equal to 65 upon entry into the cohort, and 3) the person is a female defined by the concept Id 8532. We plan to add functionality to build stratification based on a presence of event defined by an external cohort. 


## Code Block

```

#UI for Ariadne----------------------
library(ariadne)
#create strata

female_strata <- define_concept_strata(name = "female",
                                       domain = "person",
                                       element = "gender_concept_id",
                                       concept = 8532L) %>%
  build_strata(cohortId = 10L)

#build covariates -------------------------------
ariadne <- define_covariates(
  temporalStartDays = c(-365, 0, 91, 184, 366),
  temporalEndDays = c(-1, 90, 183, 365, 730),
  targetCohortId = 10L
) %>%
  build_covariates() %>%
  aggregate_covariates(
    strata = female_strata,
    output_folder = "~/R/ideas/characterizationTables/andromeda/tst"
  )

# gets full data from files
aggregated_ariadne <- load_ariadne(ariadne)

# treatment patterns ---------------------------------------



tp <- define_treatment_history(targetCohortId = 10,
                               targetCohortName = "target",
                               eventCohortIds = c(1,3,5,6:9,13,14),
                               includeTreatments = "startDate",
                               periodPriorToIndex = 0,
                               minEraDuration = 0,
                               eraCollapseSize = 90,
                               combinationWindow = 30,
                               minPostCombinationDuration = 30,
                               filterTreatments = "Changes",
                               maxPathLength = 3,
                               minCellCount = 5,
                               minCellMethod = "Remove",
                               groupCombinations = 10,
                               addNoPaths = FALSE) %>%
  build_treatment_history() %>%
  build_treatment_patterns() #%>%
  #save_treatment_patterns()
ariadne::plot_treatment_patterns(tp)


survTab <- define_treatment_history(targetCohortId = 10,
                                    targetCohortName = "target",
                                    eventCohortIds = c(1,3,5,6:9,13,14),
                                    includeTreatments = "startDate",
                                    periodPriorToIndex = 0,
                                    minEraDuration = 0,
                                    eraCollapseSize = 90,
                                    combinationWindow = 30,
                                    minPostCombinationDuration = 30,
                                    filterTreatments = "Changes",
                                    maxPathLength = 3,
                                    minCellCount = 5,
                                    minCellMethod = "Remove",
                                    groupCombinations = 10,
                                    addNoPaths = FALSE) %>%
  build_treatment_history() %>%
  build_survival_table(strata = female_strata)
```
