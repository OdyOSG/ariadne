#' Function to plot the sankey for treatment patterns
#'
#' @param pathway_analysis a PathwayAnalysis object with information about the treatment patterns
#' @return a sankey diagram using networkD3::sankeyNetwork. We implement the lancet color schema from ggsci
#' @export
plot_treatment_patterns <- function(pathway_analysis, type = "sankey") {


  treatment_pathways <- pathway_analysis$treatmentPathways %>%
    dplyr::slice_max(n, n = 18)

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
    dplyr::summarise(value = sum(n), .groups = 'drop')

  nodes <- data.frame(name = unique(c(links$source, links$target)))
  nodes <- data.table::data.table(nodes)
  links <- data.table::data.table(links)
  links$source <- match(links$source, nodes$name) - 1
  links$target <- match(links$target, nodes$name) - 1
  nodes$name <- sub('__[0-9]+$', '', nodes$name)
  links$type <- sub(' .*', '',
                    as.data.frame(nodes)[links$source + 1, 'name'])
  data.table::setkey(links, type)

  #create custom colors
  label <- unique(links$type)
  label2 <- paste0("'", paste(label, collapse = "','"), "',", "'end'")


  allCols <- c(ggsci::pal_lancet("lanonc")(8),
               ggsci::pal_lancet("lanonc", alpha = 0.45)(9))


  col <- allCols[seq_along(label)]
  col2 <- paste0("'", paste(col, collapse = "','"), "',", "'#1B1919FF'")



  myCol <- glue::glue('d3.scaleOrdinal() .domain([{label2}]) .range([{col2}])')


  #plot sankeyNetwork
  networkD3::sankeyNetwork(Links=links, Nodes=nodes, Source='source', Target='target',
                           Value='value', NodeID='name', fontSize=10, colourScale = myCol)
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


  fit <- survival_table$survInfo %>%
    purrr::pluck(as.character(treatmentRegimentId))


  dd <- survival_table$data %>%
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
