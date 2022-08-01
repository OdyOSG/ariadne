# ariadne

This is an experimental R package focusing on providing tools to aid OHDSI characterization studies. Thus far it has two areas of functionality: 1) providing tools to analyze covariates and 2) tools for assessing treatment patterns. 

## Covariates

Ariadne formats covariates into a setup that is more conducive to providing tables and figures to summarize information in comparison to the default FeatureExtraction tools. Ariadne creates dynamic and static summary tables for reports and apps. It also has two types of plots (manhattan and cleveland) to assess features in a characterization analysis.

## Treatment Patterns

Another common type of characterization study is to look at treatment patterns. How patients switch between different drugs and understand how they are cared for. Ariadne adapts functions from the TreatmentPatterns package to create a treatment history table. From this treatment history table, we can either create a pathway analysis or a time to event analysis. For the pathway analysis, Ariadne provides summary information about the kinds of treatment pathways patients in the cohort experience and depicts this visually using a sankey plot. For the time to event analysis, Ariadne helps calculate the time to a treatment discontinuation and creates a kaplan meier plot. 

## Stratifications

Ariadne provides functionality to improve stratification analysis in OHDSI studies. It takes the unique patients in a cohort and builds strata in three ways: 1) cohort start date, 2) age, and 3) based on presence of a concept. We provide three examples of how these three strata may be defined: 1) the person enters the cohort before or during the covid-19 pandemic, 2) the person is age greater than or equal to 65 upon entry into the cohort, and 3) the person is a female defined by the concept Id 8532. We plan to add functionality to build stratification based on a presence of event defined by an external cohort. 
