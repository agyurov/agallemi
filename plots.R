# plots

rm(list = ls())

df = read.csv('clean.csv')

library(ggplot2)



#  ------------------------------------------------------------------------

g = ggplot(df, aes(date, measure))
g + geom_point(aes(color = nform, pch = reactor))

# "nh3"  "nh3n" "nh4"  "nh4n" "no2"  "no2n" "no3"  "no3n"

g = ggplot(df[df$nform == 'nh3', ], aes(date, d_nform))
g + geom_point(aes(color = d_nform > 0 )) + facet_wrap(~reactor, nrow = 3) +
  coord_cartesian(ylim = c(-5, 5)) 
  
g = ggplot(df[df$nform == 'no3n', ], aes(date, d_nform))
g + geom_point(aes(color = d_nform > 0 )) + facet_wrap(~reactor, nrow = 3) +
  coord_cartesian(ylim = c(-30, 90)) 

