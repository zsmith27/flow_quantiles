library(tidyverse)
library(dataRetrieval)
org.df <- dataRetrieval::readNWISdata(service = "iv",
                                      site = "01315500",
                                      startDate = "1900-01-01",
                                      #startDate = "2016-01-01",
                                      endDate = "2018-09-01",
                                      asDateTime = TRUE,
                                      #tz = "America/New_York",
                                      # Dischrage Code.
                                      parameterCd = "00060")

gage.df <- org.df
gage.df <- dplyr::mutate(gage.df,
                         datetime = lubridate::with_tz(dateTime, tzone = "EST5EDT"))

test <- complete_datetime(gage.df, "datetime") %>% 
  mutate(datetime = lubridate::round_date(datetime, "day"),
         day = lubridate::yday(datetime)) %>% 
  group_by(datetime, day) %>% 
  summarize(flow = median(X_00060_00000, na.rm = TRUE)) %>% 
  ungroup()


ggplot(test, aes(datetime, X_00060_00000)) +
  geom_line()