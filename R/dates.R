#' @title FUNCTION_TITLE
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

#' @title FUNCTION_TITLE
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
#'  \code{\link[dplyr]{join}}
#' @rdname complete_datetime
#' @export 
#' @importFrom dplyr full_join %>% 

complete_datetime <- function(df, datetime.col, seq.by = "hour", tz = "EST5EDT") {
  datetime_df(start.date = min(df[, datetime.col]), 
              end.date = max(df[, datetime.col]),
              seq.by,
              tz) %>% 
  dplyr::full_join(df, by = datetime.col)
}


