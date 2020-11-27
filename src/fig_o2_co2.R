# O2 CO2 figure

library(ggplot2)
library(cowplot)
library(dplyr)
library(suncalc)
library(lubridate)
library(scales)

data = read.csv('data/Harp2014.csv', stringsAsFactors = F, sep = ';') %>% as_tibble() %>%
  mutate(DateTime = as.POSIXct(DateTime))

# Harp Lake latitude longitude
lat = 45.38002
lon = -79.1339
time_zone = 'America/New_York'

day = as.Date(c('2014-08-01'), tz= time_zone)
plot_data = dplyr::filter(data, as.Date(DateTime, tz=time_zone) %in% day)

sun = suncalc::getSunlightTimes(date = day, lat = lat, lon = lon)
sun_df = tibble(x1 = c(as_datetime(day-1, tz = time_zone), as_datetime(sun$sunset, tz = time_zone)),
                x2 = c(as_datetime(sun$sunrise, tz = time_zone), as_datetime(day+2, tz = time_zone)),
                y1 = c(8,8),
                y2 = c(10, 10))

plot_data$CO2_scaled = plot_data$CO2^(1/6) + 6.2
plot_out = ggplot() +
  geom_rect(data = sun_df, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
            fill = 'grey60', alpha = .2) +
  annotate('rect', xmin = sun_df$x2[1], xmax = sun_df$x1[2], ymin = 8, ymax = 10, fill ='yellow', alpha =.2)+
  annotate('text', x = as_datetime('2014-08-01 02:00:00', tz = time_zone),
           y = 8.95, label = 'night', color = 'grey30', size =6)+
  annotate('text', x = as_datetime('2014-08-01 16:00:00', tz = time_zone),
           y = 8.95, label = 'day', color = 'gold3', size =6)+
  geom_point(data = plot_data, aes(x = DateTime, y = O2, color = 'O2'),
             size = 4) +
  geom_line(data = plot_data, aes(x = DateTime, y = O2, color = 'O2'),
            size = 1, linetype=  'dashed') +
  geom_point(data = plot_data, aes(x = DateTime, y = CO2_scaled, color = 'CO2'),
             size = 4) +
  geom_line(data = plot_data, aes(x = DateTime, y = CO2_scaled, color = 'CO2'),
            size = 1, linetype=  'dashed') +
  theme_bw() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  xlab('Time') + ylab(expression(O[2]~(mg~L^-1))) +
  scale_color_manual(name = '',
                     values = c('orange','blue'),
                     labels = c(expression(CO[2]), expression(O[2]))) +
  scale_x_datetime(labels = date_format('%H:%M', tz = time_zone)) +
  scale_y_continuous(name = expression(O[2]~(mg~L^-1)), sec.axis = sec_axis(~(. - 6.2)^6,
                                                                            name = expression(CO[2]~(mu~atm)))) +
  coord_cartesian(ylim = range(c(plot_data$O2,plot_data$CO2_scaled)), xlim = range(plot_data$DateTime))

plot_out

ggsave(filename = 'figures/fig_co2_o2.png', plot = plot_out, width = 8, height = 6, units = 'in', dpi = 320)
