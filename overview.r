# overview

rm(list = ls())

df = read.csv('preped_data.csv', header = T, stringsAsFactors = F)
df$date = as.Date(df$date, format = '%Y-%m-%d')
df$nform = factor(df$nform)
df$variable = factor(df$variable)
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
head(df)


# some plots --------------------------------------------------------------

p.set = split(df, df$variable)

plot(p.set$p1$dp1[p.set$p1$nform == 'nh3'], ylim = c(-150,150), pch = 20)
j = 1
for(i in c('nh4', 'no2', 'no3')){
  j = j + 1
  lines(p.set$p1$dp1[p.set$p1$nform == i], col = j)
  print(length(p.set$p1$value[p.set$p1$nform == i]) - sum(is.na(p.set$p1$value[p.set$p1$nform == i])))
}
