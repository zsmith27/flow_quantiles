#' @title Historical Flow Quanitle Plot
#' @description FUNCTION_DESCRIPTION
#' @param quant.df PARAM_DESCRIPTION
#' @param x.col PARAM_DESCRIPTION
#' @param y.col PARAM_DESCRIPTION
#' @param y.log10 PARAM_DESCRIPTION, Default: TRUE
#' @param col.pal PARAM_DESCRIPTION, Default: 'rdbu'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[rlang]{quotation}}
#'  \code{\link[stringr]{case}}
#'  \code{\link[dplyr]{tidyeval}}
#' @rdname gg_quant
#' @export 
#' @importFrom rlang enquo
#' @importFrom stringr str_to_title
#' @importFrom dplyr quo_name %>% 

gg_quant <- function(quant.df, x.col, y.col, y.log10 = TRUE, col.pal = "rdbu") {
  # RColorBrewer::brewer.pal(7, "RdBu")
  # viridis::viridis(7)
  name.vec <- c(
    "00-05",
    "05-10",
    "10-25",
    "25-75",
    "75-90",
    "90-95",
    "90-100"
  )
  ord.df <- data.frame(legend = name.vec)
  
  x.col <- rlang::enquo(x.col)
  y.col <- rlang::enquo(y.col)
  final.plot <- quant.df %>% 
    # mutate(legend = factor(name.vec, level = name.vec)) %>% 
    ggplot(aes(!!x.col)) +
    geom_ribbon(aes(ymin = quant_00, ymax = quant_05, fill = "1")) +
    geom_ribbon(aes(ymin = quant_05, ymax = quant_10, fill = "2")) +
    geom_ribbon(aes(ymin = quant_10, ymax = quant_25, fill = "3")) +
    geom_ribbon(aes(ymin = quant_25, ymax = quant_75, fill = "4")) +
    geom_ribbon(aes(ymin = quant_75, ymax = quant_90, fill = "5")) +
    geom_ribbon(aes(ymin = quant_90, ymax = quant_95, fill = "6")) +
    geom_ribbon(aes(ymin = quant_95, ymax = quant_100, fill = "7")) +
    scale_x_continuous(expand = c(0, 0)) +
    xlab(stringr::str_to_title(dplyr::quo_name(x.col))) +
    ylab(stringr::str_to_title(dplyr::quo_name(y.col))) +
    labs(fill = "Quantile Ranges")
  
  if (col.pal == "viridis") {
    final.plot <- final.plot +
      scale_fill_manual(labels = rev(name.vec),
                        breaks = as.character(7:1),
                        values = c(
                          "1" = "#440154FF",
                          "2" = "#443A83FF",
                          "3" = "#31688EFF",
                          "4" = "#21908CFF",
                          "5" = "#35B779FF",
                          "6" = "#8FD744FF",
                          "7" = "#FDE725FF"
                        ))
  } else {
    final.plot <- final.plot +
      scale_fill_brewer(palette = col.pal,
                        labels = rev(name.vec),
                        breaks = as.character(7:1))
  }

  
  if (y.log10 == TRUE) {
    suppressMessages(
      final.plot <- final.plot + scale_y_log10()
    )
  }
  
  return(final.plot)
}

