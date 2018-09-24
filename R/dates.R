#' @title Datetiem Data Frame
#' @description FUNCTION_DESCRIPTION
#' @param start.date PARAM_DESCRIPTION
#' @param end.date PARAM_DESCRIPTION
#' @param seq.by PARAM_DESCRIPTION, Default: 'hour'
#' @param tz PARAM_DESCRIPTION, Default: 'EST'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname datetime_df
#' @export 

datetime_df <- function(start.date, end.date, seq.by = "hour", tz = "EST") {
  if(is(start.date, "POSIXct") != TRUE) stop("start.date must be class POSIXct")
  if(is(start.date, "POSIXct") != TRUE) stop("end.date must be class POSIXct")
  data.frame(datetime = seq.POSIXt(start.date, end.date, by = seq.by, tz = tz))
}

#' @title Complete Datetime
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param datetime.col PARAM_DESCRIPTION
#' @param seq.by PARAM_DESCRIPTION, Default: 'hour'
#' @param tz PARAM_DESCRIPTION, Default: 'EST5EDT'
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
#'  \code{\link[dplyr]{tidyeval}},\code{\link[dplyr]{join}}
#' @rdname complete_datetime
#' @export 
#' @importFrom rlang enquo
#' @importFrom dplyr quo_name full_join %>% 

complete_datetime <- function(df, datetime.col, seq.by = "hour", tz = "EST5EDT") {
  datetime.col <- rlang::enquo(datetime.col)
  
  min.scl <- min(df[, dplyr::quo_name(datetime.col)], na.rm = TRUE)
  max.scl <- max(df[, dplyr::quo_name(datetime.col)], na.rm = TRUE)
  
  datetime_df(start.date = min.scl, 
              end.date = max.scl,
              seq.by,
              tz) %>% 
  dplyr::full_join(df, by = dplyr::quo_name(datetime.col))
}


