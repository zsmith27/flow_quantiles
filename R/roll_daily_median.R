#' @title Rolling Window Daily Median
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param datetime.col PARAM_DESCRIPTION
#' @param param.col PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION, Default: 30
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
#'  \code{\link[lubridate]{round_date}},\code{\link[lubridate]{day}}
#'  \code{\link[dplyr]{tidyeval}}
#'  \code{\link[zoo]{rollmean}}
#' @rdname roll_daily_median
#' @export 
#' @importFrom rlang enquo
#' @importFrom lubridate round_date yday
#' @importFrom dplyr quo_name %>% 
#' @importFrom zoo rollmedian

roll_daily_median <- function(df, datetime.col, param.col, k = 30) {
  datetime.col <- rlang::enquo(datetime.col)
  param.col <- rlang::enquo(param.col)
  
  daily.median.df <- complete_datetime(df, !!datetime.col) %>%
    mutate(datetime = lubridate::round_date(!!datetime.col, "day"),
           day = lubridate::yday(!!datetime.col)) %>%
    group_by(!!datetime.col, day) %>%
    summarize(m_param = median(!!param.col, na.rm = TRUE)) %>%
    ungroup()
  
  final.df <- daily.median.df %>% 
    mutate(
      !!dplyr::quo_name(param.col) := c(
        rep(as.numeric(NA), k - 1),
        zoo::rollmedian(m_param, k = k, na.rm = TRUE)
      )) %>% 
    select(-m_param)
  
  return(final.df)
}

