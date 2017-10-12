# Data prep2

rm(list = ls())
cat('\014')


library(XLConnect)
library(reshape2)
library(tidyr)


# read in the data --------------------------------------------------------


wb = loadWorkbook("original_data_cut.xlsx")
dl = readWorksheet(wb, sheet = getSheets(wb))
dl = dl[1:12]

lapply(dl, head)
names(dl) = c('nh4n', 'nh4', 'nh3n', 'nh3', 'no2n', 'no2',
                     'no3n', 'no3', 'temp', 'ph', 'o2', 'sat')
newnames = c('date', 'inflow', paste0('p', 1:6))

rename = function(x, newnames = newnames){
  names(x) = newnames
  return(x)
}
dl2 = lapply(dl, rename, newnames = newnames)

# chop off the head of the snake -------------------------------------------
# cuz the dates are repeated and also in a diff format

dl3 = dl2
for(i in 1:length(dl3)){
  dl3[[i]] = dl3[[i]][-c(1:22),]
}


# examination of dates ----------------------------------------------------

dl4 = dl3
for(i in 1:length(dl4)){
  dl4[[i]]$date = substr(dl3[[i]]$date, 1, 10)
}

# date cleaning (date var mostly) -----------------------------------------

clean_trash = function(x){
  y = as.numeric(substr(x$date, 1, 1))
  if(length(which(is.na(y))) == 0){
    return(x)
  }
  x = x[-which(is.na(y)),]
  return(x)
}

dl5 = lapply(dl4, clean_trash)


# keep 8 columns ----------------------------------------------------------

for(i in 1:length(dl3)){
  dl5[[i]] = dl5[[i]][,1:8]
}


# add nform ---------------------------------------------------------------

dl6 = dl5
for(i in 1:length(dl5)){
  dl6[[i]]$nform = factor(names(dl5)[i])
}


# list of dates -----------------------------------------------------------

listofdates = list()
listofdates = lapply(dl6, function(x) x$date)
common_dates = Reduce(intersect, listofdates)

dl7 = dl6
for(i in 1:length(dl6)){
  tmp = dl7[[i]]$date %in% common_dates
  dl7[[i]] = dl6[[i]][tmp,]
}


df = do.call(rbind.data.frame, dl7)
df$date = as.Date(df$date, format = '%Y-%m-%d')
rownames(df) = NULL
# write.csv(df, 'preped_data2.csv', row.names = F)


# aggregate dates ---------------------------------------------------------

dfa = gather(dfa, key, value, -date, -nform, -inflow)
# write.csv(dfa, 'preped_data3.csv', row.names = F)
# NOTE! 
# co2 is virtually useless