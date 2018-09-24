#' @title Quantile LOESS Smooth
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param param.col PARAM_DESCRIPTION
#' @param group.col PARAM_DESCRIPTION
#' @param smooth.span PARAM_DESCRIPTION, Default: 0.3
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[rlang]{quotation}},\code{\link[rlang]{new_formula}},\code{\link[rlang]{sym}}
#'  \code{\link[dplyr]{tidyeval}}
#' @rdname quant_smooth
#' @export 
#' @importFrom rlang enquo new_formula sym
#' @importFrom dplyr quo_name %>% 

quant_smooth <- function(df, param.col, group.col, smooth.span = 0.3) {
  param.col <- rlang::enquo(param.col)
  group.col <- rlang::enquo(group.col)
  
  final.df <- df %>% 
    quant_summary(!!param.col, !!group.col) %>% 
    group_by(quant) %>%
    mutate(
      flow = predict(
        loess(rlang::new_formula(rlang::sym(dplyr::quo_name(param.col)), 
                                 rlang::sym(dplyr::quo_name(group.col))),
              span = smooth.span)
      )
    ) %>%
    ungroup() %>%
    spread(quant, !!param.col)
  
  return(final.df)
}
