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

for(i in 1:length(data.list2)){
  names(data.list2[[i]]) = newnames
}
lapply(data.list2, names)



# caluclate differences ---------------------------------------------------

calc_diff = function(x){
  # x is a data frame
  y = data.frame(x$p1 - x$inflow, x$p2 - x$inflow, x$p3 - x$inflow,
                 x$p4 - x$inflow, x$p5 - x$inflow, x$p6 - x$inflow)
  names(y) = paste0('dp', 1:6)
  return(cbind.data.frame(x,y))
}

data.list3 = lapply(data.list2, calc_diff)

# # reshape the data --------------------------------------------------------
# 
# pain = function(x){
#   # x is the data frame
#   y = melt(x, c('date'), c('inflow', paste0('p', 1:6)))
#   names(y) = c('date', 'bf', lapply(x, unique)$nform)
#   return(y)
# } 
# 
# data.list3 = lapply(data.list2, pain)

# data concatenation ------------------------------------------------------



df = do.call(rbind.data.frame, data.list2)
df$nform = factor(df$nform)

# REMARK!@!!!!!!!
# Check manually that the below names match reaity
# !!!!!!!!!!!!!!!!!!!
levels(df$nform) = c('c', 'co2', 'nh3', 'nh3n', 'nh4', 'nh4n', 'no2', 'no2n', 'no3', 'no3n', 'o2', 'ph', 'sat')
str(df)



# date cleaning (date var mostly) -----------------------------------------



x = as.numeric(substr(df$date, 1, 1))
df = df[-which(is.na(x)),]
df$nform = gsub('\\..*', '', as.character(df$nform))
df$nform = factor(df$nform)
rownames(df) = NULL



# fixing the date variable ------------------------------------------------



df$date = substr(df$date, 1, 10)
df$date = as.Date(df$date, format = '%Y-%m-%d')



write.csv(df, 'preped_data.csv', row.names = F)


# REUIQRED FORMAT ---------------------------------------------------------

# DATE CONCENTRATION      NH ..... NO3 C... CO2 SAT
# {}   {INTAKE, P1 .. P6}


