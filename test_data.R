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
                         datetime = lubridate::with_tz(dateTime,
                                                       tzone = "EST5EDT")) %>% 
  rename(flow = X_00060_00000)
  

roll.df <- roll_daily_median(gage.df, datetime, flow, k = 50)
quant.df <- quant_smooth(roll.df, flow, day, smooth.span = 0.01)
curr.df <- roll.df %>% 
  filter(datetime >= as.Date("2017-01-01")) %>% 
  mutate(year = lubridate::year(datetime))


gg_quant(quant.df, day, flow, col.pal = "RdBu") +
  geom_line(data = curr.df, aes(day, flow), na.rm = TRUE)
  

plotly::ggplotly(my.plot)

