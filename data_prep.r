# Data prep

rm(list = ls())
cat('\014')


library(XLConnect)
library(reshape2)


# read in the data --------------------------------------------------------


wb = loadWorkbook("original_data.xlsx")
data.list = readWorksheet(wb, sheet = getSheets(wb))
data.list = data.list[1:13]


# function to add checmical column name (name of the data)
chemical_naming = function(df, chemname){
  df$chemname = chemname
  return(df)
}



# the new data list with the new column -----------------------------------


data.list2 = list()
for(i in 1:length(names(data.list))){
  data.list2[[i]] = chemical_naming(data.list[[i]], names(data.list)[i])
}
names(data.list2) = names(data.list)



# renaming the variables in the data --------------------------------------



newnames = c('date', 'inflow', paste0('p',1:6), 'nform')
data.list2b = data.list2
for(i in 1:length(data.list2b)){
  names(data.list2b[[i]]) = newnames
}



# caluclate differences ---------------------------------------------------

calc_diff = function(x){
  # x is a data frame
  y = data.frame(x$p1 - x$inflow, x$p2 - x$inflow, x$p3 - x$inflow,
                 x$p4 - x$inflow, x$p5 - x$inflow, x$p6 - x$inflow)
  names(y) = paste0('dp', 1:6)
  return(cbind.data.frame(x,y))
}

data.list3 = lapply(data.list2b, calc_diff)

# reshape the data --------------------------------------------------------

pain = function(x){
  # x is the data frame
  y = melt(x, c('date', 'nform', paste0('dp', 1:6)), paste0('p', 1:6))
  return(y)
}

data.list4 = lapply(data.list3, pain)
names(data.list4) = c('nh4n', 'nh4', 'nh3n', 'nh3', 'no2n', 'no2', 'no3n',
                      'no3', 'temp', 'ph', 'o2', 'sat', 'co2')


# chop off the head of the snake -------------------------------------------
# cuz the dates are repeated and also in a diff format

data.list4a = data.list4
for(i in 1:length(data.list4a)){
  if(names(data.list4a)[i] == 'temp'){
    next
  }
  data.list4a[[i]] = data.list4a[[i]][-c(1:22),]
}



# examination of dates ----------------------------------------------------

data.list5 = data.list4a
for(i in 1:length(data.list4)){
  data.list5[[i]]$date = substr(data.list4a[[i]]$date, 1, 10)
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

data.list6 = lapply(data.list5, clean_trash)

# Compare dates -----------------------------------------------------------

# find shortest unique dates
listofdates = list()
listofdates = lapply(data.list6, function(x) x$date)
common_dates = Reduce(intersect, listofdates)

data.list7 = data.list6
for(i in 1:length(data.list6)){
  test = data.list6[[i]]$date %in% common_dates
  print(which(!test))
  data.list7[[i]] = data.list6[[i]][test,]
}

# aggregate the repeating dates -------------------------------------------
# cast to factor
cast_to_factor = function(x){
  x$date = factor(x$date)
  x$nform = factor(x$nform)
  return(x)
}
data.list8 = lapply(data.list7, cast_to_factor)


df = do.call(rbind.data.frame, data.list8)
rownames(df) = NULL
df$date = as.Date(df$date, format = '%Y-%m-%d')
# data concatenation ------------------------------------------------------
# first join vertically the nforms

levels(df$nform) = c('nh4n', 'nh4', 'nh3n', 'nh3', 'no2n', 'no2', 'no3n',
                     'no3', 'temp', 'ph', 'o2', 'sat', 'co2')


write.csv(df, 'preped_data.csv', row.names = F)


