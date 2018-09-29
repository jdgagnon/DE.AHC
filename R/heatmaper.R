
#' Hetmap function
#' Create a heatmap based on data with specified cuffoff values
#'
#' @import tidyverse
#' @import viridis
#' @import egg
#' @import magrittr
#' @param data Dataframe with expression data and HITS-CLIP data
#' @param filter Filter based on PValue or FDR
#' @param cuttoff Cuttoff value to use with filter
#' @param y.text Labels for gene names
#' @export
heatmaper <- function(data,
                   filter = 'PValue',
                   cuttoff = 0.05,
                   start = 9,
                   y.text = ggplot2::element_blank())
{

  theme_john <- ggplot2::theme(panel.background = ggplot2::element_blank(),
                      axis.line = ggplot2::element_line(colour = "black", size = 0.5),
                      line = ggplot2::element_line(colour = "black", size = 0.5),
                      panel.grid.major = ggplot2::element_blank(),
                      strip.background = ggplot2::element_blank(),
                      axis.ticks = ggplot2::element_line(colour = "black", size = 0.5),
                      panel.grid.minor = ggplot2::element_blank(),
                      legend.background = ggplot2::element_blank(),
                      legend.key = ggplot2::element_blank(),
                      legend.text = ggplot2::element_text(size = 16, colour = "black"),
                      legend.title = ggplot2::element_text(size = 16, colour = "black"),
                      axis.text = ggplot2::element_text(size = 16, colour = "black"),
                      plot.background = ggplot2::element_blank(),
                      text = ggplot2::element_text(size = 16, colour = "black"))

  heat <- data %>%
    dplyr::filter(get(filter) <= cuttoff) %>%
    dplyr::mutate(AHC = scale(Sum)) %>%
    tidyr::gather(Sample, log2CPM, start:length(data)) %>%
    dplyr::group_by(Gene) %>%
    dplyr::mutate(Z.score = scale(log2CPM)) %>%
    dplyr::arrange(logFC) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = Sample, y = stats::reorder(Gene, logFC), fill = Z.score)) +
    ggplot2::ylab(NULL) +
    viridis::scale_fill_viridis(begin = 0, end = 1,option = "A") +
    ggplot2::xlab(NULL) +
    ggplot2::scale_x_discrete(position = 'top') +
    theme_john +
    ggplot2::theme(
      axis.text.y = y.text,
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      legend.position = 'left',
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 0))

  bar <- data %>%
    dplyr::filter(get(filter) <= cuttoff) %>%
    dplyr::mutate(Sum = ifelse(Location != '3UTR', NA, Sum),
           AHC = scale(Sum)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = stats::reorder(Gene, logFC), y = log10(Sum + 1)),
             stat = 'identity',
             fill = 'black',
             color = NA,
             position = ggplot2::position_dodge(0.9),
             width = 0.9) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(position = 'right') +
    ggplot2::xlab(NULL) +
    ggplot2::ylab('log10(seed-match AHC reads + 1)') +
    theme_john +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(colour = "black", size = 0.5),
      axis.line = ggplot2::element_blank())
  plot <- egg::ggarrange(plots = list(heat, bar), ncol = 2)
  return(plot)
}
