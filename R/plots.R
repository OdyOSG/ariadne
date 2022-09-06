#' Function to plot the sankey for treatment patterns
#'
#' @param pathway_analysis a PathwayAnalysis object with information about the treatment patterns
#' @param min_count select a minimum count to include in the diagram
#' @return a sankey diagram using networkD3::sankeyNetwork. We implement a color blind friendly palette
#' @export
plot_treatment_patterns <- function(pathway_analysis,
                                    min_count = 30) {


   treatment_pathways <- pathway_analysis$treatmentPathways %>%
     dplyr::filter(n >= min_count) %>%
     dplyr::mutate(dplyr::across(dplyr::contains("cohort"), ~gsub("_era", "", .)))

  #set up data for sankey
  links <- treatment_pathways %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = c(-row, -n),
                        names_to = 'column', values_to = 'source') %>%
    dplyr::mutate(column = match(column, names(treatment_pathways))) %>%
    tidyr::drop_na(source) %>%
    dplyr::mutate(source = paste0(source, '__', column)) %>%
    dplyr::group_by(row) %>%
    dplyr::mutate(target = dplyr::lead(source, order_by = column)) %>%
    tidyr::drop_na(target, source) %>%
    dplyr::group_by(source, target) %>%
    dplyr::summarise(value = sum(n), .groups = 'drop') %>%
    dplyr::arrange(desc(value))

  nodes <- data.frame(name = unique(c(links$source, links$target)))
  nodes <- data.table::data.table(nodes)
  links <- data.table::data.table(links)
  links$source <- match(links$source, nodes$name) - 1
  links$target <- match(links$target, nodes$name) - 1
  nodes$name <- sub('__[0-9]+$', '', nodes$name)
  links$type <- sub(' .*', '',
                    as.data.frame(nodes)[links$source + 1, 'name'])
  data.table::setkey(links, type)
  data.table::setorder(links, cols = - "value")
  #create custom colors
  label <- unique(links$type)
  label2 <- paste0("'", paste(label, collapse = "','"), "',", "'end'")


  martin_colors <- unname(colorBlindness::paletteMartin)[-1]


  col <- martin_colors[seq_along(label)]
  col2 <- paste0("'", paste(col, collapse = "','"), "',", "'#1B1919FF'")



  myCol <- glue::glue('d3.scaleOrdinal() .domain([{label2}]) .range([{col2}])')


  #plot sankeyNetwork
  networkD3::sankeyNetwork(Links=links, Nodes=nodes, Source='source', Target='target',
                           Value='value', NodeID='name', fontSize = 11, colourScale = myCol)
}

#' Function to plot the survival analysis for time to discontinuation
#'
#' @param survival_table a SurvivalAnalysis object with information about the survival analysis
#' @param type identify the treatment line type to display, Either single, only combinations or all lines
#' @param selectTop display an integer number of lines of treatment to display in the kaplan meier plot
#' @return a kaplan meier plot suing survminer. We implement the lancet color schema from ggsci
#' @include utils.R
#' @export
plot_kaplan_meier <- function(survival_table,
                              treatmentRegimentId) {


  fit <- survTab$survival_analysis$survInfo %>%
    purrr::pluck(as.character(treatmentRegimentId))


  dd <- survTab$survival_analysis$data %>%
    purrr::pluck(as.character(treatmentRegimentId))


  survminer::ggsurvplot(fit = fit,
                        data = dd,
                        size = 0.8,
                        palette = ggsci::pal_lancet()(2),
                        xlab = "Time in days",
                        break.time.by = 30,
                        conf.int = TRUE) +
    ggplot2::labs(
      title = "Time To Discontinuation"
    )



}

#' Function to plot the covariates distribution using manhattan plot
#'
#' @param timeId window id to plot
#' @param analysisId analisis id to plot
#' @param strata_name strata to plot, if NULL will be plotted without strata
#' @param output_folder folder where stored aggregated covariates
#' @return a manhattan plot
#' @include utils.R
#' @export
manhattan_covariates <- function(
  timeId,
  analysisId,
  strata_name = NULL,
  output_folder
) {
  analysisId = paste0('analysis_id=', analysisId)
  timeId = paste0('time_id=', timeId)

  pathToCsv <- paste(
    gsub('/', '\\\\',output_folder),
    ifelse(is.null(strata_name), 'strata_total', paste0('strata_', strata_name)),
    analysisId,
    timeId,
    'part-0.csv',
    sep = '\\'
  )
  ggplot2::ggplot(data = read.csv(pathToCsv),
                  ggplot2::aes(x = conceptName, y = pct,
                               color = categoryName)) +
    ggplot2::labs( title = '_____',  color= "") +
    ggplot2::geom_rect(min = 0, ymax = 1, xmin=-Inf, xmax=Inf, color = "gray90", fill = "gray90") +
    ggplot2::theme(axis.title.x = ggplot2::element_text(color = "black",
                                                        size = 14, angle = 0,
                                                        hjust = .5, vjust = 0, face = "bold",
    ),
    axis.title.y = ggplot2::element_text(
      color = "black",
      size = 14, angle = 90,
      hjust = .5, vjust = .5,
      face = "bold"))+
    ggplot2::geom_hline(ggplot2::aes(
      yintercept = 0), color = "black", alpha=1) +
    ggplot2::geom_point(alpha = 0.75,
                        position = ggplot2::position_dodge(0.2)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.25),
      legend.position = "bottom",
      legend.key.size = ggplot2::unit(0.25, "cm"),
      legend.text = ggplot2::element_text(size=8),
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank()
    )  +  ggplot2::geom_abline(intercept = 150, slope = 20,
                               color = "gray",
                               size = 0.5,
                               alpha = 0.8) +
    ggplot2::xlab("Covariates") + ggplot2::ylab("Frequency(%)") +
    ggplot2::labs(title = "")
}


#' Function to plot the covariates distribution using cleveland plot
#'
#' @param timeId window id to plot
#' @param analysisId analisis id to plot
#' @param strata_name strata to plot, if NULL will be plotted without strata
#' @param output_folder folder where stored aggregated covariates
#' @return a cleveland plot
#' @include utils.R
#' @export
cleveland_covariates <- function(
  timeId,
  analysisId,
  strata_name = NULL,
  output_folder
) {
  analysisId = paste0('analysis_id=', analysisId)
  timeId = paste0('time_id=', timeId)

  pathToCsv <- paste(
    gsub('/', '\\\\',output_folder),
    ifelse(is.null(strata_name), 'strata_total', paste0('strata_', strata_name)),
    analysisId,
    timeId,
    'part-0.csv',
    sep = '\\'
  )
  ggplot2::ggplot(data = read.csv(pathToCsv),
                  ggplot2::aes(x    = pct,
                               xend = max(pct),
                               y    = reorder(categoryName, pct),
                               yend = reorder(categoryName, pct)
                  ),
                  show.legend = F) +

    ggplot2::geom_point(ggplot2::aes(
      x = pct,
      y = categoryName,
      colour = categoryName
    ),
    show.legend = F,
    position = ggplot2::position_jitter(w = 0.01, h = 0.2)
    ) +
    ggplot2::labs(title = 'Distribution of covariates',
                  x = "Frequency (%)", y = NULL) +
    ggplot2::theme_bw()
}
