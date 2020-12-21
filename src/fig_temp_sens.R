# temperature sensativity figure

library(ggplot2)
library(cowplot)
library(tidyverse)
library(suncalc)
library(lubridate)
library(scales)


# k = 8.62E-5
# temp = seq(0, 30, by = .1) +273.15
# E = seq(0.1,.7,by=.1)
#
# metab = 1
#
# metab_t = crossing(k, temp, E, metab) %>%
#   mutate(metab_t = metab*exp(-E/(k*temp)))
#
# ggplot(metab_t, aes(x = temp, y = log(metab_t/temp), group = E, color = E)) + geom_line()
r20 = 1
temp = seq(5,31,by = .1)

r<-r20*1.047^(temp-20)


# scale = c(1.025, 1.05, 1.075, 1.1)
q10 = c(1.6, 2.5)

r = crossing(temp, r20, q10) %>%
  mutate(r = r20*q10^((temp - 15)/10))

double_r = min(r$temp[r$r>=2&r$q10==2.5])
double_gpp = min(r$temp[r$r>=2&r$q10==1.6])

windows()
plot_out = ggplot(r, aes(x = temp, y = r, group = as.character(q10), color = as.character(q10))) +
  geom_line(size = 2)+
  theme_classic() +
  ylab(expression(Sensativity~(metab[t] / metab[15]))) +
  xlab(expression(Water~Temperature~(degree~C))) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 16)) +
  scale_color_manual(name ='', values = c('dark green', 'darkgoldenrod2'), labels=c('GPP', 'R')) +
  geom_segment(aes(x = 0, y = 2, xend = double_gpp, yend = 2),color = 'black', linetype ='dashed' )+
  geom_segment(aes(x = double_r, y = 0, xend = double_r, yend = 2),color = 'darkgoldenrod2', linetype ='dashed' )+
  geom_segment(aes(x = double_gpp, y = 0, xend = double_gpp, yend = 2),color = 'dark green', linetype ='dashed' ) +
  coord_cartesian(ylim = range(r$r), xlim = range(r$temp))

plot_out

ggsave(filename = 'figures/fig_temp_sens.png', plot = plot_out, width = 6, height = 6, units = 'in', dpi = 320)


