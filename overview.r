# overview

rm(list = ls())

df = read.csv('preped_data.csv', header = T, stringsAsFactors = F)
df$date = as.Date(df$date, format = '%Y-%m-%d')
df$nform = factor(df$nform)
str(df)

plot.na = function(df,col=NULL,...){
  df = is.na(df)
  if(is.null(col)){col=grey.colors(2)}
  image(t(apply(df,2,rev)),col=grey.colors(2),yaxt="n",xaxt="n",xlab="",ylab="",...)
}



# super general summary ---------------------------------------------------

str(df)
dim(df)
plot.na(df)
lapply(df, function(x) length(unique(x)))

