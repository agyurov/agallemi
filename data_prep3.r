# Data prep3

rm(list = ls())
cat('\014')

library(reshape2)
library(tidyr)

df0 = read.csv('dane.csv', header = T, stringsAsFactors = T)

names(df0) = tolower(names(df0))


# split and to long format ------------------------------------------------

general_date = df0$data.badania

df0 = df0[,-1]
rp = c('r', paste0('p', 1:6), 'date')
chem = c('nh4n', 'nh4', 'nh3n', 'nh3', 'no2n', 'no2',
         'no3n', 'no3', 'temp', 'ph', 'o2', 'sat', 'co2')
nmz = unlist(lapply(chem, function(x, y) paste(x, y, sep='_'), rp))
names(df0) = nmz


sq1 = seq(1, 104, by = 8)
sq2 = seq(0,104,by= 8) + 8
df.list = list() 
for(i in 1:13){
  df.list[[i]] = df0[,sq1[i]:sq2[i]]
}




#  ------------------------------------------------------------------------

# fix moronic 'NA' in o2 data

df.list[[11]]$o2_p4[124] = NA
df.list[[11]]$o2_p4 = as.numeric(df.list[[11]]$o2_p4)




#  ------------------------------------------------------------------------


# gather(df.list[[1]], cond, p, nh4n_p1:nh4n_p6, factor_key=TRUE)

aleix = function(x){
  out = gather(x, reactor, outflow, 2:7, factor_key=TRUE)
  levels(out$reactor) = gsub('.*_', '', levels(out$reactor))
  return(out)
}

df.list2 = list()
for(i in 1:12){
  df.list2[[i]] = aleix(df.list[[i]])
}

for(i in 1:length(df.list2)){
  names(df.list2[[i]])[4] = gsub('*_.', '', names(df.list2[[i]])[1])
}

tmp = df.list2
df.list2 = lapply(df.list2, function(x) x = x[,c(2,3,1,4)])

# Join all N forms --------------------------------------------------------
# first the outflow
nforms_out = df.list2[[1]]

for(i in 2:8){
  nforms_out = cbind(nforms_out, df.list2[[i]][,4])
}
names(nforms_out)[4:11] = paste0(chem[1:8], '_out')
nforms_out = gather(nforms_out, nform, outflow, 4:11, factor_key=TRUE)

# first the inflow
nforms_in = df.list2[[1]]

for(i in 2:8){
  nforms_in = cbind(nforms_in, df.list2[[i]][,4])
}
names(nforms_in)[4:11] = paste0(chem[1:8], '_in')
nforms_in = gather(nforms_in, nform, inflow, 4:11, factor_key=TRUE)
nforms = cbind(nforms_in, outflow = nforms_out$outflow)
names(nforms)[1] = 'date'



# expand abiotics ---------------------------------------------------------
##### lapply(df.list2, function(x) range(x[,1], na.rm = T))
df.list3 = list()
freq = rep(length(levels(nforms$nform)), nrow(df.list2[[11]]))
for(i in 1:4){
  df.list3[[i]] = df.list2[[i+8]][,c(1,4)]
  df.list3[[i]]$freq = freq
}


df.list4 = list()
for(i in 1:length(df.list3)){
  df.list4[[i]] = df.list3[[i]][rep(row.names(df.list3[[i]]), df.list3[[i]]$freq), 1:2]
}



# join them ---------------------------------------------------------------

# clean = cbind(nforms, do.call(cbind.data.frame, df.list4))
xx = unlist(lapply(1:8, function(x)seq(x, 22608, by = 8)))
clean = cbind(nforms,
              do.call(cbind.data.frame, df.list4)[xx,])

write.csv(clean, 'clean.csv', row.names = F)
