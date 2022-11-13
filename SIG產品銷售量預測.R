library(forecast)
library(ggplot2)
library(lubridate)
library(xts)
library(dplyr)

df <- read.csv(""SIG Europe data - SIG Europe data.csv"" )%>% as.data.frame()
df$Link..Cust...PH.<-as.character(df$Link..Cust...PH.)
df.t <- t(df)
df.t<-df.t[-c(2:4),]

colnames(df.t)<-df.t[1,]
df.t<- df.t[-1,]

df.t<-apply(df.t, MARGIN = 2,FUN = as.character)%>%
  apply(MARGIN = 2, FUN = as.numeric)%>%
  as.data.frame()  


# tranform each sale of product in to ts objects
for (i in c(1:ncol(df.t))) {
  df.t[,i]<-
    df.t[,i]%>%
    ts(start = c(2014,1),end = c(2017, 12), frequency = 12)
}

#
new.ts<- c()

for(i in c(1:ncol(df.t))){
  
  sou.ts<-df.t[,i]
  snaive.pred <- snaive(sou.ts, h = 12)
  
  f.value<-snaive.pred$mean%>%
    as.numeric()%>%
    round(., digits = 4)%>%
    as.data.frame()
  
  
  if(i==1){
    result<-rbind(colnames(df.t)[i], f.value)
  }else{
    result1<-rbind(colnames(df.t)[i], f.value)
    result<-cbind(result, result1)
  }
}


method2.result<-t(result)%>%as.data.frame()
View(method2.result)

colnames(method2.result)=c(""Link..Cust...PH."", 1:12)
method2.result$Link..Cust...PH.<-as.character(method2.result$Link..Cust...PH.)



answer.sheet<-read.csv(""SIG Europe data - Team 1.csv"" )
answer.sheet$Link..Cust...PH.<-
  answer.sheet$Link..Cust...PH.%>%
  as.character()
method2<-left_join(answer.sheet, method2.result, ""Link..Cust...PH."")
View(method2)

names<-colnames(answer.sheet[1:13])
method2<-method2[,-c(2:13)]
colnames(method2)<-names



# final result with the same order online
method2



# Analysis 2
library(tidyr)
library(dplyr)
library(lubridate)
library(reshape)
library(forecast)

sig.df<-read.csv( ""SIG Europe data - SIG Europe data.csv"")

# I firstly added four colunns in excel (which is not in the R code)
# ""count.0"", ""ratio.0"", ""X0.in.2017"" and ""ratio.0.2017""



#use the boxplot to help us see the distribution the zero record
total.boxplot<-boxplot(sig.df$ratio.0)
last.boxplot<-boxplot(sig.df$ratio.0.2017)

# filter out the product that contain few record and expecially almost no record in the las year (2017)
#group that contain almost no sales ==> copy the last value as the forecast (sero group)

# zero.group
ma.group<-sig.df%>%
  filter(ratio.0> total.boxplot$stats[4] &  ratio.0.2017>last.boxplot$stats[3])



# Filter our the zero.group and those which do not have record in the last yaer (2017)
sig.new<-sig.df%>%
  filter(!c(ratio.0> total.boxplot$stats[4] &  ratio.0.2017>last.boxplot$stats[3]))%>%
  filter(ratio.0.2017!=1)


sig.new<-sig.new[,-c(2:8)]
sig.new<-t(sig.new)
colnames(sig.new)<-sig.new[1,]
sig.new<- sig.new[-1,]


Date<-rownames(sig.new)%>%
  substr(start = 2, stop = 7)%>%
  paste(""01"", sep = """")%>%
  as.data.frame()


colnames(Date)<-""Date""
sig<-cbind2(sig.new, Date)
rownames(sig)<-c(1:nrow(sig.new))

sig<-apply(sig, MARGIN = 2,FUN = as.character)%>%
  apply(MARGIN = 2, FUN = as.numeric)%>%
  as.data.frame()  

sig$Date<-ymd(sig$Date)

# tranform each sale of product in to ts objects
for (i in c(1:c(ncol(sig)-1))) {
  sig[,i]<-
    sig[,i]%>%
    ts(start = c(2014,1),end = c(2017, 12), frequency = 12)
}





# use smoothing method to categorize
for (i in c(1:c(ncol(sig)-1))) {
  class<-
    sig[,i]%>%
    ets(model = ""ZZZ"")%>%
    substr(., start = 5, stop =9 )%>%
    gsub("","", """", .)
  
  if(i==1){
    ets.result<-data.frame(""Product""=colnames(sig[i]), ""model""=class)
  }else{
    ets.result2<-data.frame(""Product""=colnames(sig[i]), ""model""=class)
    ets.result<-rbind(ets.result, ets.result2)
  }
  
}

View(ets.result)

unique(ets.result$model)



#### auto arima

for (i in c(1:c(ncol(sig)-1))) {
  model<-sig[,i]%>%
    auto.arima()%>%
    substr(., start = 7, stop = 60)
  
  if(i==1){
    arima.result<-data.frame(""Product""=colnames(sig[i]), ""result""= model)
  }else{
    arima.result2<-data.frame(""Product""=colnames(sig[i]), ""result""=model)
    arima.result<-rbind(arima.result, arima.result2)
  }
  
}

View(arima.result)
auto.result<-cbind(ets.result, arima.result)
auto.result[,-3]%>%
  group_by(model, result)%>%
  count(sort = TRUE)%>%View()

unique(auto.result$result)

# ANN group
ann.index<-auto.result[which(auto.result$model==""ANN"" | auto.result$model==""MNN""),-3]
ann.group<-sig[,which(colnames(sig)%in% as.character(ann.index$Product))] 

#Other group
non.ann.group<- sig[,-which(colnames(sig)%in% as.character(ann.index$Product))]



# Forecast of teh non.ann.group
zzz.result<-c()
for (i in c(1:c(ncol(non.ann.group)-1))) {
  ts<-non.ann.group[,i]
  ts.pred<-ts%>%
    ets(., model = ""ZZZ"")%>%
    forecast(., h=12)
  f.value<-ts.pred$mean%>%
    as.numeric()%>%
    round(., digits = 4)%>%
    as.data.frame()
  
  
  if(i==1){
    zzz.result<-rbind(colnames(non.ann.group[i]), f.value)
  }else{
    zzz.result1<-rbind(colnames(non.ann.group[i]), f.value)
    zzz.result<-cbind(zzz.result, zzz.result1)
  }
}


# forecast of the ann group

lm.result<-c()
for (i in c(1:c(ncol(ann.group)-1))) {
  ts<-ann.group[,i]
  
  ts.pred<-
    tslm(ts~trend + season)%>%
    forecast(., h=12)
  
  f.value<-ts.pred$mean%>%
    as.numeric()%>%
    round(., digits = 4)%>%
    as.data.frame()
  
  
  if(i==1){
    lm.result<-rbind(colnames(ann.group[i]), f.value)
  }else{
    lm.result1<-rbind(colnames(ann.group[i]), f.value)
    lm.result<-cbind(lm.result, lm.result1)
  }
}

#forecast of the zero group
zero.result<-c()
for (i in c(1:c(ncol(ma.group)-1))) {
  f.value =rep(0,12)%>%
    as.data.frame()
  
  if(i==1){
    zero.result<- rbind(""Product"" = as.character(ma.group[i,1]), f.value)
  }else{
    zero.result1<-rbind(""Product"" = as.character(ma.group[i,1]), f.value)
    zero.result<-cbind(zero.result, zero.result1)
  }
}
View(zero.result)


#bind the whole datset toghether
zzz.result
lm.result
zero.result

final.result<-cbind(zzz.result, cbind(lm.result, zero.result))%>%
  t()%>%
  as.data.frame()

colnames(final.result)=c(""Link..Cust...PH."", 1:12)
final.result$Link..Cust...PH.<-final.result$Link..Cust...PH.%>%
  as.character()

answer.sheet<-read.csv(""SIG Europe data - Team 1.csv"" )
answer.sheet$Link..Cust...PH.<-
  answer.sheet$Link..Cust...PH.%>%
  as.character()
final.sheet<-left_join(answer.sheet, final.result, ""Link..Cust...PH."")
View(final.sheet)

names<-colnames(answer.sheet[1:13])
final.sheet<-final.sheet[,-c(2:13)]
colnames(final.sheet)<-names



# final result with the same order online
final.sheet

final.sheet[which(is.na(final.sheet)), ]<-0

write.csv(final.sheet, file = ""/home/johnliao/BAFT/Sig/ team1.sig.csv"")"



