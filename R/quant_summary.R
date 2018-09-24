#' @title Quantile Summary
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param value.col PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
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
#'  \code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}}
#' @rdname quant_summary
#' @export 
#' @importFrom rlang enquo quos
#' @importFrom dplyr group_by summarize ungroup quo_name %>% 
#' @importFrom tidyr gather

quant_summary <- function(df, value.col, ...) {
  value.col <- rlang::enquo(value.col)
  group.col <- rlang::quos(...)
  
  final.df <- df %>% 
    dplyr::group_by(!!!group.col) %>% 
    dplyr::summarize(
      quant_00 = quantile(x = !!value.col, probs = 0.00, na.rm = TRUE),
      quant_05 = quantile(x = !!value.col, probs = 0.05, na.rm = TRUE),
      quant_10 = quantile(x = !!value.col, probs = 0.10, na.rm = TRUE),
      quant_25 = quantile(x = !!value.col, probs = 0.25, na.rm = TRUE),
      quant_50 = quantile(x = !!value.col, probs = 0.50, na.rm = TRUE),
      quant_75 = quantile(x = !!value.col, probs = 0.75, na.rm = TRUE),
      quant_90 = quantile(x = !!value.col, probs = 0.90, na.rm = TRUE),
      quant_95 = quantile(x = !!value.col, probs = 0.95, na.rm = TRUE),
      quant_100 = quantile(x = !!value.col, probs = 1.00, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup() %>% 
    tidyr::gather(quant,
                  !!dplyr::quo_name(value.col),
                  quant_00:quant_100)
  
  return(final.df)
}
