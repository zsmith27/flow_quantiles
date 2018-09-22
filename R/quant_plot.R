library(tidyverse)
df <- test %>% 
  filter(datetime < as.Date("2018-01-01")) %>% 
  quant_summary(flow, day) %>% 
  gather(quant, flow, quant_00:quant_100) %>% 
  group_by(quant) %>% 
  mutate(flow = predict(loess(flow ~ day, span = 0.3))) %>% 
           ungroup() %>% 
  spread(quant, flow)
current <- test %>% 
  filter(datetime >= as.Date("2018-01-01")) %>% 
  group_by(day) %>% 
  summarize(flow = median(flow, na.rm = TRUE)) %>% 
  ungroup()

x.col <- rlang::quo(day)
y.col <- rlang::quo(X_00060_00000)
library(ggplot2)
 my.plot <- 
  ggplot2::ggplot(df, aes(!!x.col)) +
  geom_ribbon(aes(ymin = quant_00, ymax = quant_05), fill = "#B2182B") +
  geom_ribbon(aes(ymin = quant_05, ymax = quant_10), fill = "#EF8A62") +
  geom_ribbon(aes(ymin = quant_10, ymax = quant_25), fill = "#FDDBC7") +
  geom_ribbon(aes(ymin = quant_25, ymax = quant_75), fill = "#F7F7F7") +
  geom_ribbon(aes(ymin = quant_75, ymax = quant_90), fill = "#D1E5F0") +
  geom_ribbon(aes(ymin = quant_90, ymax = quant_95), fill = "#67A9CF") +
  geom_ribbon(aes(ymin = quant_95, ymax = quant_100), fill = "#2166AC") +
  scale_y_log10() +
  scale_x_continuous(expand = c(0, 0)) +
  geom_line(data = current, aes(!!x.col, flow))

plotly::ggplotly(my.plot)
  
  
  
RColorBrewer::brewer.pal(7, "RdBu")
